#![deny(unused_imports)]

use std::collections::hash_map::Entry;
use std::collections::BTreeSet;
use std::hash::{Hash, Hasher};

use ast::NodeId;
use atoms::JsWord;
use global_common::SyntaxContext;
use index::bit_set::GrowableBitSet;
use index::vec::IndexVec;
use petgraph::graph::EdgeReference;
use petgraph::prelude::DiGraph;
use petgraph::visit::EdgeRef;
use petgraph::Direction::Incoming;
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};

use crate::control_flow::node::{Node, NodeKind};
use crate::control_flow::ControlFlowGraph::Branch;
use crate::utils::unwrap_as;
use crate::DataFlowAnalysis::{
    LatticeElementId, LinearFlowState, PrioritizedNode, UniqueQueue, MAX_STEPS_PER_NODE,
};
use crate::Id;

use super::graph::{process, Visitor};
use super::hashable_map::HashableHashMap;
use super::simple_set::IndexSet;
use super::types::{ObjectId, ObjectStore, UnionBuilder, UnionStore};
use super::utils::{ReusableState, ReusableStateStack};
use super::{
    function::*, Assignment, Call, CallArgBuilder, CallArgs, CallId, FnId, Func, Lattice, Pointer,
    PropertyAssignments, ResolvedCall, SimpleCFG, StaticFunctionData, Store,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
enum ReturnTypeConstituent {
    Object(ObjectId),
    Invalid,
    NullOrVoid,
}

/// Re-usable state for a [`call resolver`][`Resolver`].
#[derive(Debug, Default)]
pub struct ResolverState<'ast> {
    return_types: FxHashMap<CallId, FxHashSet<ReturnTypeConstituent>>,
    return_states: FxHashMap<CallId, PropertyAssignments>,

    data_flow_state: DataFlowAnalysisState<'ast>,
}

impl ReusableState for ResolverState<'_> {
    fn reset(&mut self) {
        let ResolverState {
            return_types,
            return_states,
            data_flow_state,
        } = self;
        return_types.clear();
        return_states.clear();
        data_flow_state.reset();
    }
}

struct Resolver<'a, 'ast> {
    functions: &'a mut IndexVec<FnId, Func>,
    static_fn_data: &'a IndexVec<FnId, StaticFunctionData<'ast>>,

    call_templates: &'a IndexVec<FnId, CallTemplate>,

    resolved_calls: &'a mut FxHashMap<CallId, ResolvedCall>,

    objects_map: &'a mut FxHashMap<NodeId, ObjectId>,
    call_objects: &'a mut FxHashMap<(CallId, NodeId), ObjectId>,
    object_links: &'a mut FxHashSet<(ObjectId, ObjectId)>,

    objects: &'a mut ObjectStore,

    unions: &'a mut UnionStore,
    invalid_objects: &'a mut GrowableBitSet<ObjectId>,
    calls: &'a mut IndexSet<CallId, Call>,

    fn_assignments: &'a HashableHashMap<Id, Assignment>,

    root_call: CallId,

    state: &'a mut ResolverState<'ast>,
}

impl Visitor<CallId> for Resolver<'_, '_> {
    fn visit_node(&mut self, node: CallId, dependencies: &mut FxHashSet<CallId>) -> bool {
        let mut changed = false;

        debug_assert!(!self.resolved_calls.contains_key(&node));

        let call = &self.calls[node];

        let func = call.func;

        self.state.data_flow_state.reset();

        let initial_lattice = self
            .state
            .data_flow_state
            .lattice_elements
            .insert(Lattice::default());
        let mut entry_lattice = Lattice {
            prop_assignments: call.prop_assignments.clone(),
            ..Default::default()
        };
        entry_lattice
            .var_assignments
            .reserve(self.static_fn_data[func].param_names.len());
        for (i, param_name) in self.static_fn_data[func].param_names.iter().enumerate() {
            let value = call.args.get(i).unwrap_or(Some(Pointer::NullOrVoid));
            entry_lattice
                .var_assignments
                .insert(param_name.clone(), Assignment { rhs: value });
        }
        let entry_lattice = self
            .state
            .data_flow_state
            .lattice_elements
            .insert(entry_lattice);

        let mut analysis = DataFlowAnalysis {
            workQueue: UniqueQueue::reuse_inner(
                std::mem::take(&mut self.state.data_flow_state.work_queue_inner),
                &self.static_fn_data[func].node_priorities,
                true,
            ),
            cfg: &self.static_fn_data[func].cfg,
            entry_lattice,
            initial_lattice,

            call: node,
            path_map: &self.call_templates[func].step_map,
            resolved_calls: self.resolved_calls,
            return_types: &mut self.state.return_types,
            return_states: &mut self.state.return_states,
            objects_map: self.objects_map,
            call_objects: self.call_objects,
            object_links: self.object_links,
            objects: self.objects,
            unions: self.unions,
            invalid_objects: self.invalid_objects,
            calls: self.calls,

            incomplete_dependencies: dependencies,

            changed: &mut changed,
            fn_assignments: self.fn_assignments,

            root_call: self.root_call,

            state: &mut self.state.data_flow_state,
        };
        analysis.analyze();

        changed
    }

    fn finish_node(&mut self, node: CallId) {
        let func = self.calls[node].func;

        // Merge arguments.
        match &self.calls[node].args {
            CallArgs::Heap(args) => {
                for (i, &arg) in args.iter().enumerate() {
                    match self.functions[func].args.get_mut(i) {
                        Some(existing) => {
                            let union = create_union(self.unions, *existing, arg);
                            *existing = union;
                        }
                        None => {
                            self.functions[func].args.push(arg);
                        }
                    }
                }
            }
            CallArgs::Invalid(len) => {
                for i in 0..*len {
                    match self.functions[func].args.get_mut(i) {
                        Some(existing) => {
                            if existing.is_some() {
                                let union = create_union(self.unions, *existing, None);
                                *existing = union;
                            }
                        }
                        None => {
                            self.functions[func].args.push(None);
                        }
                    }
                }
            }
        }

        // TODO: only do this if in_fn?
        {
            // Merge property assignments.
            for ((obj, key), prop) in self.calls[node].prop_assignments.iter() {
                if self.invalid_objects.contains(*obj) {
                    continue;
                }
                match self.functions[func].arg_values.entry((*obj, key.clone())) {
                    Entry::Occupied(mut entry) => {
                        let union = create_union(self.unions, entry.get().rhs, prop.rhs);
                        entry.insert(Assignment { rhs: union });
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(*prop);
                    }
                }
            }
        }

        let return_types = self.state.return_types.remove(&node).unwrap_or_default();

        let mut builder = UnionBuilder::default();

        for ty in return_types {
            match ty {
                ReturnTypeConstituent::Object(obj) => builder.add_object(Some(obj)),
                ReturnTypeConstituent::Invalid => builder.add_object(None),
                ReturnTypeConstituent::NullOrVoid => builder.add_null_or_void(),
            };
        }

        let return_type = self.unions.build_union(builder);

        let prop_assignments = self.state.return_states.remove(&node).unwrap_or_default();

        let resolved = ResolvedCall {
            return_type,
            prop_assignments,
        };

        self.resolved_calls.insert(node, resolved);
    }
}

/// Recursively resolves the return type and side effects of the given call.
pub(super) fn resolve_call(call: CallId, store: &mut Store) {
    if store.resolved_calls.contains_key(&call) {
        return;
    }

    store.call_resolver_state.reset();

    let mut visitor = Resolver {
        call_templates: &store.call_templates,
        functions: &mut store.functions,
        static_fn_data: &store.static_fn_data,
        resolved_calls: &mut store.resolved_calls,
        objects_map: &mut store.objects_map,
        call_objects: &mut store.call_objects,
        object_links: &mut store.object_links,
        objects: &mut store.objects,
        unions: &mut store.unions,
        invalid_objects: &mut store.invalid_objects,
        calls: &mut store.calls,
        fn_assignments: &store.fn_assignments,
        root_call: call,

        state: &mut store.call_resolver_state,
    };

    process(call, &mut visitor);

    debug_assert!(store.resolved_calls.contains_key(&call));
}

fn get_property(
    prop_assignments: &PropertyAssignments,
    unions: &mut UnionStore,
    object: Option<Pointer>,
    key: &JsWord,
    invalid_objects: &mut GrowableBitSet<ObjectId>,
) -> Option<Pointer> {
    // TODO: is this correct (should it return None instead?)
    if let Some(Pointer::NullOrVoid) = object {
        return Some(Pointer::NullOrVoid);
    }
    if let Some(Pointer::Object(ObjectStore::RESOLVING_CALL)) = object {
        return Some(Pointer::Object(ObjectStore::RESOLVING_CALL));
    }
    let invalid = match object {
        Some(Pointer::Object(obj)) => invalid_objects.contains(obj),
        Some(Pointer::Union(union)) => {
            let invalid = { invalidated(Pointer::Union(union), invalid_objects, unions) };
            if invalid {
                invalidate(invalid_objects, unions, object, prop_assignments);
            }
            invalid
        }
        Some(Pointer::Fn(_)) | None => true,
        Some(Pointer::NullOrVoid) => unreachable!(),
    };
    if invalid {
        if cfg!(debug_assertions) {
            match object {
                Some(Pointer::Object(obj)) => {
                    let prop = prop_assignments
                        .get(&(obj, key.clone()))
                        .map(|a| a.rhs)
                        .unwrap_or_default();
                    debug_assert!(
                        matches!(prop, Some(Pointer::Object(o)) if o.is_built_in())
                            || !matches!(prop, Some(Pointer::Object(_) | Pointer::Union(_)))
                            || invalidated(prop.unwrap(), invalid_objects, unions)
                    );
                }
                Some(Pointer::Union(union)) => {
                    for constituent in unions[union].constituents() {
                        let constituent = prop_assignments
                            .get(&(constituent, key.clone()))
                            .map(|a| a.rhs)
                            .unwrap_or_default();
                        debug_assert!(
                            matches!(constituent, Some(Pointer::Object(o)) if o.is_built_in())
                                || !matches!(
                                    constituent,
                                    Some(Pointer::Object(_) | Pointer::Union(_))
                                )
                                || invalidated(constituent.unwrap(), invalid_objects, unions)
                        );
                    }
                }
                Some(Pointer::Fn(_)) | None => {}
                Some(Pointer::NullOrVoid) => unreachable!(),
            }
        }

        return None;
    }

    match object {
        Some(Pointer::Object(obj)) => prop_assignments
            .get(&(obj, key.clone()))
            .map(|a| a.rhs)
            .unwrap_or(Some(Pointer::NullOrVoid)),
        Some(Pointer::Union(union)) => {
            let mut builder = UnionBuilder::default();

            for constituent in unions[union].constituents() {
                let constituent = prop_assignments
                    .get(&(constituent, key.clone()))
                    .and_then(|a| a.rhs)
                    .or(Some(Pointer::NullOrVoid));

                builder.add(constituent, unions);
            }
            unions.build_union(builder)
        }
        Some(Pointer::Fn(_)) | None => None,
        Some(Pointer::NullOrVoid) => unreachable!(),
    }
}

/// Returns a [`Pointer`] to a union of `pointer1`'s type and `pointer2`'s type.
fn create_union(
    unions: &mut UnionStore,
    pointer1: Option<Pointer>,
    pointer2: Option<Pointer>,
) -> Option<Pointer> {
    if pointer1 == pointer2 {
        return pointer1;
    }

    let mut builder = UnionBuilder::default();

    builder.add(pointer1, unions);
    builder.add(pointer2, unions);
    unions.build_union(builder)
}

trait GetPropAssignments: Copy {
    fn prop_assignments(
        &self,
        object: ObjectId,
        queue: &mut Vec<Pointer>,
        done: &FxHashSet<Pointer>,
    );
}

impl<'a> GetPropAssignments for &'a PropertyAssignments {
    fn prop_assignments(
        &self,
        object: ObjectId,
        queue: &mut Vec<Pointer>,
        done: &FxHashSet<Pointer>,
    ) {
        for ((o, _), prop) in self.iter() {
            if *o == object {
                if let Some(value) = prop.rhs {
                    if !done.contains(&value) {
                        queue.push(value);
                    }
                }
            }
        }
    }
}

impl<'a, 'b> GetPropAssignments for (&'a IndexSet<CallId, Call>, CallId) {
    fn prop_assignments(
        &self,
        object: ObjectId,
        queue: &mut Vec<Pointer>,
        done: &FxHashSet<Pointer>,
    ) {
        let props = &self.0[self.1].prop_assignments;
        for ((o, _), prop) in props.iter() {
            if *o == object {
                if let Some(value) = prop.rhs {
                    if !done.contains(&value) {
                        queue.push(value);
                    }
                }
            }
        }
    }
}

/// Recursively invalidates the entity that `pointer` points to.
fn invalidate(
    invalid_objects: &mut GrowableBitSet<ObjectId>,
    unions: &UnionStore,
    pointer: Option<Pointer>,
    prop_assignments: impl GetPropAssignments,
) {
    match pointer {
        Some(Pointer::Object(o)) if o.is_built_in() => return,
        None | Some(Pointer::Fn(_) | Pointer::NullOrVoid) => return,
        Some(Pointer::Object(_) | Pointer::Union(_)) => {}
    }
    if let Some(pointer) = pointer {
        let mut queue = vec![pointer];
        let mut done = FxHashSet::default();

        while let Some(pointer) = queue.pop() {
            done.insert(pointer);

            match pointer {
                Pointer::Object(o) if o.is_built_in() => {}
                Pointer::Object(obj) => {
                    let new_invalidation = invalid_objects.insert(obj);

                    if new_invalidation {
                        prop_assignments.prop_assignments(obj, &mut queue, &done);
                    }
                }
                Pointer::Union(union) => {
                    for constituent in unions[union].constituents() {
                        let pointer = Pointer::Object(constituent);
                        if !done.contains(&pointer) {
                            queue.push(pointer);
                        }
                    }
                }
                Pointer::Fn(_) | Pointer::NullOrVoid => {}
            }
        }
    }
}

/// Returns true if `pointer` points to an invalid object.
fn invalidated(
    pointer: Pointer,
    invalid_objects: &GrowableBitSet<ObjectId>,
    unions: &UnionStore,
) -> bool {
    match pointer {
        Pointer::Object(obj) => invalid_objects.contains(obj),
        Pointer::Union(union) => {
            union.invalid()
                || unions[union]
                    .constituents()
                    .any(|obj| invalid_objects.contains(obj))
        }
        Pointer::Fn(_) => true,
        Pointer::NullOrVoid => false,
    }
}

#[derive(Debug)]
/// A slot where a value can be stored.
enum AssignTarget {
    Var(Id),
    Prop(Option<Pointer>, JsWord),
}

#[derive(Debug)]
/// Info about a call made by a [`Machine`].
enum MachineCall {
    /// Signals that the callee was not a function.
    Invalid,
    /// (fn, args)
    Valid(FnId, CallArgBuilder),
}

#[derive(Debug)]
enum MachineLattice {
    Owned(Lattice),
    Borrowed(LatticeElementId),
}

impl MachineLattice {
    fn get<'a>(&'a self, lattice_elements: &'a IndexSet<LatticeElementId, Lattice>) -> &'a Lattice {
        match self {
            MachineLattice::Owned(l) => l,
            MachineLattice::Borrowed(id) => &lattice_elements[*id],
        }
    }
    fn get_mut<'a>(
        &'a mut self,
        lattice_elements: &IndexSet<LatticeElementId, Lattice>,
    ) -> &'a mut Lattice {
        match self {
            MachineLattice::Owned(l) => l,
            MachineLattice::Borrowed(id) => {
                let id = *id;
                *self = MachineLattice::Owned(lattice_elements[id].clone());
                unwrap_as!(self, MachineLattice::Owned(l), l)
            }
        }
    }
    fn insert_var_assignment(
        &mut self,
        name: Id,
        value: Assignment,
        lattice_elements: &IndexSet<LatticeElementId, Lattice>,
    ) {
        if let Some(existing) = self.get(lattice_elements).var_assignments.get(&name) {
            if *existing == value {
                return;
            }
        }

        self.get_mut(lattice_elements)
            .var_assignments
            .insert(name, value);
    }

    fn insert_prop_assignment(
        &mut self,
        prop: (ObjectId, JsWord),
        value: Assignment,
        lattice_elements: &IndexSet<LatticeElementId, Lattice>,
    ) {
        if let Some(existing) = self.get(lattice_elements).prop_assignments.get(&prop) {
            if *existing == value {
                return;
            }
        } else if let Some(Pointer::NullOrVoid) = value.rhs {
            // Assigning null/void is the same as having nothing assigned.
            return;
        }

        self.get_mut(lattice_elements)
            .prop_assignments
            .insert(prop, value);
    }
}

#[derive(Debug)]
struct MachineState {
    /// Stack of calls (fn and args) that are currently being built.
    calls: Vec<MachineCall>,
    /// Stack of unions (constituents) that are currently being built.
    unions: ReusableStateStack<FxHashSet<Option<Pointer>>>,
    /// Stack of expression values. Using a stack simplifies the [`Step`]
    /// language, allowing us to handle nested expressions by pushing/popping
    /// values on the stack.
    r_value: Vec<Option<Pointer>>,
    /// Register containing the current left-hand-side value.
    l_value: Option<AssignTarget>,
}

impl ReusableState for MachineState {
    fn reset(&mut self) {
        let MachineState {
            calls,
            unions,
            r_value,
            l_value,
        } = self;
        calls.clear();
        unions.reset();
        r_value.clear();
        r_value.push(None);
        *l_value = None;
    }
}

impl Default for MachineState {
    fn default() -> Self {
        Self {
            calls: Vec::new(),
            r_value: vec![None],
            l_value: None,
            unions: Default::default(),
        }
    }
}

#[derive(Debug)]
/// State machine that executes a sequence of [`Steps`][Step] that represent the
/// effects of a node in the control flow graph.
struct Machine<'a> {
    state: &'a mut MachineState,
    lattice: MachineLattice,
    steps: &'a [Step],
    fn_assignments: &'a HashableHashMap<Id, Assignment>,
}

impl Machine<'_> {
    fn get_r_value(&self) -> Option<Pointer> {
        *self.state.r_value.last().unwrap()
    }
    fn set_r_value(&mut self, value: Option<Pointer>) {
        *self.state.r_value.last_mut().unwrap() = value;
    }

    fn get_var(
        &self,
        id: &Id,
        lattice_elements: &IndexSet<LatticeElementId, Lattice>,
    ) -> Option<Assignment> {
        self.lattice
            .get(lattice_elements)
            .var_assignments
            .get(id)
            .or_else(|| self.fn_assignments.get(id))
            .copied()
    }
}

#[derive(Debug, PartialEq, Eq)]
/// Info required to compute the effects of a call.
pub(super) struct CallTemplate {
    /// [`Steps`][Step] that represent the effects of each control flow graph
    /// node required to evaluate the call's effects.
    step_map: FxHashMap<NodeId, Vec<Step>>,
}

impl CallTemplate {
    pub fn new(
        static_fn_data: &IndexVec<FnId, StaticFunctionData>,
        unresolved_ctxt: SyntaxContext,
        func: FnId,
    ) -> CallTemplate {
        let step_map = create_step_map(static_fn_data, unresolved_ctxt, func);
        CallTemplate { step_map }
    }
}

impl<'ast> DataFlowAnalysis<'ast, '_> {
    fn flowThrough(
        &mut self,
        node: Node<'ast>,
        input: LatticeElementId,
    ) -> Option<LatticeElementId> {
        let steps = match self.path_map.get(&node.node_id) {
            Some(steps) => steps,
            None => return Some(input),
        };
        debug_assert!(!steps.is_empty());
        self.state.machine_state.reset();
        let mut machine = Machine {
            lattice: MachineLattice::Borrowed(input),
            steps,
            state: &mut self.state.machine_state,
            fn_assignments: self.fn_assignments,
        };

        for step in machine.steps {
            match step {
                Step::StoreRValue(value) => match value {
                    None => {
                        machine.set_r_value(None);
                    }
                    Some(value) => {
                        let value = match value {
                            RValue::NullOrVoid => Some(Pointer::NullOrVoid),
                            RValue::Var(rhs) => machine
                                .get_var(rhs, &self.state.lattice_elements)
                                .and_then(|a| a.rhs),
                            RValue::Object(o) => get_call_obj(
                                self.objects_map,
                                self.call_objects,
                                self.object_links,
                                self.objects,
                                self.root_call,
                                *o,
                            ),
                            RValue::Prop(prop) => get_property(
                                &machine
                                    .lattice
                                    .get(&self.state.lattice_elements)
                                    .prop_assignments,
                                self.unions,
                                machine.get_r_value(),
                                prop,
                                self.invalid_objects,
                            ),
                            RValue::String => Some(Pointer::Object(ObjectStore::STRING)),
                            RValue::Boolean => Some(Pointer::Object(ObjectStore::BOOL)),
                            RValue::Number => Some(Pointer::Object(ObjectStore::NUMBER)),
                            RValue::BigInt => Some(Pointer::Object(ObjectStore::BIG_INT)),
                        };
                        machine.set_r_value(value);
                    }
                },
                Step::StoreLValue(value) => match value {
                    Some(LValue::Var(id)) => {
                        machine.state.l_value = Some(AssignTarget::Var(id.clone()));
                    }
                    Some(LValue::ObjectProp(obj, prop)) => {
                        machine.state.l_value = Some(AssignTarget::Prop(
                            get_call_obj(
                                self.objects_map,
                                self.call_objects,
                                self.object_links,
                                self.objects,
                                self.root_call,
                                *obj,
                            ),
                            prop.clone(),
                        ));
                    }
                    Some(LValue::RValueProp(prop)) => {
                        machine.state.l_value =
                            Some(AssignTarget::Prop(machine.get_r_value(), prop.clone()));
                    }
                    None => {
                        machine.state.l_value = None;
                    }
                },
                Step::Assign(conditional) => {
                    if let Some(slot) = &machine.state.l_value {
                        let existing = match &slot {
                            AssignTarget::Var(name) => machine
                                .get_var(name, &self.state.lattice_elements)
                                .and_then(|a| a.rhs),
                            AssignTarget::Prop(obj, key) => get_property(
                                &machine
                                    .lattice
                                    .get(&self.state.lattice_elements)
                                    .prop_assignments,
                                self.unions,
                                *obj,
                                key,
                                self.invalid_objects,
                            ),
                        };

                        let rhs = if !conditional {
                            // supersede
                            machine.get_r_value()
                        } else {
                            // union
                            create_union(self.unions, existing, machine.get_r_value())
                        };
                        let new = Assignment { rhs };
                        match slot {
                            AssignTarget::Var(id) => {
                                machine.lattice.insert_var_assignment(
                                    id.clone(),
                                    new,
                                    &self.state.lattice_elements,
                                );
                            }
                            AssignTarget::Prop(obj, prop) => {
                                if let Some(obj) = *obj {
                                    {
                                        if invalidated(obj, self.invalid_objects, self.unions) {
                                            invalidate(
                                                self.invalid_objects,
                                                self.unions,
                                                rhs,
                                                &machine
                                                    .lattice
                                                    .get(&self.state.lattice_elements)
                                                    .prop_assignments,
                                            );
                                            continue;
                                        }
                                    }

                                    match obj {
                                        Pointer::Object(obj) => {
                                            machine.lattice.insert_prop_assignment(
                                                (obj, prop.clone()),
                                                new,
                                                &self.state.lattice_elements,
                                            );
                                        }
                                        Pointer::Union(union) => {
                                            for constituent in self.unions[union].constituents() {
                                                machine.lattice.insert_prop_assignment(
                                                    (constituent, prop.clone()),
                                                    new,
                                                    &self.state.lattice_elements,
                                                );
                                            }
                                        }
                                        Pointer::Fn(_) | Pointer::NullOrVoid => {}
                                    }
                                } else {
                                    // Unknown/invalid assignment target.
                                    invalidate(
                                        self.invalid_objects,
                                        self.unions,
                                        machine.get_r_value(),
                                        &machine
                                            .lattice
                                            .get(&self.state.lattice_elements)
                                            .prop_assignments,
                                    );
                                }
                            }
                        };
                    } else {
                        // Unknown/invalid assignment target.
                        invalidate(
                            self.invalid_objects,
                            self.unions,
                            machine.get_r_value(),
                            &machine
                                .lattice
                                .get(&self.state.lattice_elements)
                                .prop_assignments,
                        );
                    }
                }
                Step::InvalidateRValue => {
                    invalidate(
                        self.invalid_objects,
                        self.unions,
                        machine.get_r_value(),
                        &machine
                            .lattice
                            .get(&self.state.lattice_elements)
                            .prop_assignments,
                    );
                }
                Step::InvalidateLValue => match &machine.state.l_value {
                    Some(AssignTarget::Var(id)) => {
                        if let Some(rhs) = machine
                            .get_var(id, &self.state.lattice_elements)
                            .map(|a| a.rhs)
                        {
                            invalidate(
                                self.invalid_objects,
                                self.unions,
                                rhs,
                                &machine
                                    .lattice
                                    .get(&self.state.lattice_elements)
                                    .prop_assignments,
                            );
                        }
                    }
                    Some(AssignTarget::Prop(obj, prop)) => {
                        let prop = get_property(
                            &machine
                                .lattice
                                .get(&self.state.lattice_elements)
                                .prop_assignments,
                            self.unions,
                            *obj,
                            prop,
                            self.invalid_objects,
                        );

                        invalidate(
                            self.invalid_objects,
                            self.unions,
                            prop,
                            &machine
                                .lattice
                                .get(&self.state.lattice_elements)
                                .prop_assignments,
                        );
                    }
                    None => {}
                },
                Step::StartCall(num_args) => match machine.get_r_value() {
                    Some(Pointer::Fn(f)) => {
                        machine
                            .state
                            .calls
                            .push(MachineCall::Valid(f, CallArgBuilder::new(*num_args)));
                    }
                    _ => {
                        machine.state.calls.push(MachineCall::Invalid);
                    }
                },
                Step::StoreArg => {
                    let mut arg = machine.get_r_value();
                    match arg {
                        Some(Pointer::Object(o)) => {
                            if self.invalid_objects.contains(o) {
                                arg = None;
                            }
                        }
                        Some(Pointer::Union(union)) => {
                            if union.invalid() {
                                arg = None;
                            }
                        }
                        Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) | None => {}
                    }
                    match machine.state.calls.last_mut().unwrap() {
                        MachineCall::Invalid => {
                            invalidate(
                                self.invalid_objects,
                                self.unions,
                                arg,
                                &machine
                                    .lattice
                                    .get(&self.state.lattice_elements)
                                    .prop_assignments,
                            );
                        }
                        MachineCall::Valid(_, args) => args.push(arg),
                    }
                }
                Step::Call => {
                    let (func, mut args) = match machine.state.calls.pop().unwrap() {
                        MachineCall::Invalid => {
                            machine.set_r_value(None);
                            continue;
                        }
                        MachineCall::Valid(f, a) => (f, a),
                    };

                    if let Some(heap) = &args.args {
                        if !heap.is_empty() {
                            let invalid_objects = &*self.invalid_objects;
                            if heap.iter().all(|a| match a {
                                Some(Pointer::Union(union)) => union.invalid(),
                                Some(Pointer::Object(obj)) => invalid_objects.contains(*obj),
                                Some(Pointer::Fn(_)) | None => true,
                                Some(Pointer::NullOrVoid) => false,
                            }) {
                                args.args = None;
                                args.none_count = args.len;
                            }
                        }
                    }
                    let args = args.finish();

                    let mut prop_assignments: PropertyAssignments = HashableHashMap::default();

                    if let CallArgs::Heap(args) = &args {
                        let mut done = FxHashSet::default();
                        let mut queue = Vec::new();

                        {
                            for arg in args.iter() {
                                match arg {
                                    Some(Pointer::Object(o)) => {
                                        queue.push(*o);
                                    }
                                    Some(Pointer::Union(union)) => {
                                        queue.extend(self.unions[*union].constituents());
                                    }
                                    Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) | None => {}
                                }
                            }
                        }

                        done.reserve(queue.len());

                        // TODO: check argument prop assignments for unresolved calls
                        while let Some(o) = queue.pop() {
                            if o == ObjectStore::RESOLVING_CALL {
                                return None;
                            }

                            if done.contains(&o) {
                                continue;
                            }

                            done.insert(o);

                            if self.invalid_objects.contains(o) {
                                continue;
                            }

                            for (key, value) in machine
                                .lattice
                                .get(&self.state.lattice_elements)
                                .prop_assignments
                                .iter()
                            {
                                if key.0 == o {
                                    prop_assignments.insert(key.clone(), *value);
                                    match value.rhs {
                                        Some(Pointer::Object(o)) => {
                                            if o == ObjectStore::RESOLVING_CALL {
                                                return None;
                                            }
                                            if !done.contains(&o) {
                                                queue.push(o);
                                            }
                                        }
                                        Some(Pointer::Union(union)) => {
                                            for constituent in self.unions[union].constituents() {
                                                if o == ObjectStore::RESOLVING_CALL {
                                                    return None;
                                                }
                                                if !done.contains(&constituent) {
                                                    queue.push(constituent);
                                                }
                                            }
                                        }
                                        Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) | None => {
                                        }
                                    }
                                }
                            }
                        }
                    }

                    let inner_call = Call {
                        func,
                        args,
                        prop_assignments,
                    };

                    if cfg!(debug_assertions) {
                        let unions = &*self.unions;
                        let depends_on_unresolved_call = |pointer| match pointer {
                            Some(Pointer::Object(o)) => o == ObjectStore::RESOLVING_CALL,
                            Some(Pointer::Union(union)) => unions[union]
                                .constituents()
                                .any(|c| c == ObjectStore::RESOLVING_CALL),
                            Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) | None => false,
                        };
                        if let CallArgs::Heap(args) = &inner_call.args {
                            for &arg in args.iter() {
                                debug_assert!(!depends_on_unresolved_call(arg));
                            }
                        }

                        debug_assert!(!inner_call
                            .prop_assignments
                            .keys()
                            .any(|(o, _)| *o == ObjectStore::RESOLVING_CALL));
                        debug_assert!(!inner_call
                            .prop_assignments
                            .values()
                            .any(|p| depends_on_unresolved_call(p.rhs)));
                    }

                    let inner_call_id = self.calls.insert(inner_call);

                    let existing = self
                        .resolved_calls
                        .get(&inner_call_id)
                        .map(|resolved| (resolved.return_type, Some(&resolved.prop_assignments)));

                    let (return_type, incoming_assignments) = match existing {
                        Some((t, a)) => (t, a),
                        None => {
                            if inner_call_id != self.call {
                                self.incomplete_dependencies.insert(inner_call_id);
                            }

                            let return_types = match self.return_types.get(&inner_call_id) {
                                Some(t) => t,
                                None => {
                                    machine.set_r_value(Some(Pointer::Object(
                                        ObjectStore::RESOLVING_CALL,
                                    )));
                                    continue;
                                }
                            };
                            if return_types.is_empty() {
                                machine.set_r_value(Some(Pointer::Object(
                                    ObjectStore::RESOLVING_CALL,
                                )));
                                continue;
                            }

                            let mut builder = UnionBuilder::default();

                            for &ty in return_types {
                                match ty {
                                    ReturnTypeConstituent::Object(obj) => {
                                        builder.add_object(Some(obj))
                                    }
                                    ReturnTypeConstituent::Invalid => builder.add_object(None),
                                    ReturnTypeConstituent::NullOrVoid => builder.add_null_or_void(),
                                };
                            }

                            let result = self.unions.build_union(builder);

                            (result, self.return_states.get(&inner_call_id))
                        }
                    };

                    if let Some(incoming_assignments) = incoming_assignments {
                        for ((obj, key), prop) in incoming_assignments.iter() {
                            if self.invalid_objects.contains(*obj) {
                                continue;
                            }
                            let key = (*obj, key.clone());
                            let new = if let Some(existing) = machine
                                .lattice
                                .get(&self.state.lattice_elements)
                                .prop_assignments
                                .get(&key)
                            {
                                let union = create_union(self.unions, existing.rhs, prop.rhs);
                                Assignment { rhs: union }
                            } else {
                                *prop
                            };
                            machine.lattice.insert_prop_assignment(
                                key,
                                new,
                                &self.state.lattice_elements,
                            );
                        }
                    }

                    machine.set_r_value(return_type);
                }
                Step::Return => {
                    let mut changed = false;

                    macro_rules! add_return_ty {
                        ($ty:expr) => {
                            self.return_types.entry(self.call).or_default().insert($ty)
                        };
                    }

                    let r_value = machine.get_r_value();
                    let none_in_return_ty = self
                        .return_types
                        .get(&self.call)
                        .map(|ty| ty.contains(&ReturnTypeConstituent::Invalid))
                        .unwrap_or_default();
                    let mut queue = Vec::new();
                    match r_value {
                        Some(Pointer::Object(o)) => {
                            if o == ObjectStore::RESOLVING_CALL {
                                return None;
                            }
                            if self.invalid_objects.contains(o) {
                                if !none_in_return_ty {
                                    add_return_ty!(ReturnTypeConstituent::Invalid);
                                    changed = true;
                                }
                            } else {
                                changed |= add_return_ty!(ReturnTypeConstituent::Object(o));
                                queue.push(o);
                            }
                        }
                        Some(Pointer::Union(union)) => {
                            if union.invalid() {
                                if !none_in_return_ty {
                                    add_return_ty!(ReturnTypeConstituent::Invalid);
                                    changed = true;
                                }
                            } else {
                                let union = &self.unions[union];
                                if union.contains(ObjectStore::RESOLVING_CALL) && union.len() == 1 {
                                    return None;
                                }
                                queue.extend(union.constituents());
                                for constituent in union.constituents() {
                                    if constituent != ObjectStore::RESOLVING_CALL {
                                        changed |= add_return_ty!(ReturnTypeConstituent::Object(
                                            constituent
                                        ));
                                    }
                                }
                            }
                        }
                        Some(Pointer::Fn(_)) | None => {
                            if !none_in_return_ty {
                                add_return_ty!(ReturnTypeConstituent::Invalid);
                                changed = true;
                            }
                        }
                        Some(Pointer::NullOrVoid) => {
                            changed |= add_return_ty!(ReturnTypeConstituent::NullOrVoid);
                        }
                    }

                    if let CallArgs::Heap(args) = &self.calls[self.call].args {
                        for &arg in args.iter() {
                            match arg {
                                Some(Pointer::Object(o)) => {
                                    queue.push(o);
                                }
                                Some(Pointer::Union(union)) => {
                                    queue.extend(self.unions[union].constituents());
                                }
                                Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) | None => {}
                            }
                        }
                    }

                    fn depends_on_unresolved_call(
                        unions: &UnionStore,
                        pointer: Option<Pointer>,
                    ) -> bool {
                        match pointer {
                            Some(Pointer::Object(o)) => o == ObjectStore::RESOLVING_CALL,
                            Some(Pointer::Union(union)) => {
                                unions[union].contains(ObjectStore::RESOLVING_CALL)
                            }
                            Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) | None => false,
                        }
                    }

                    let mut done = FxHashSet::default();
                    done.reserve(queue.capacity());

                    while let Some(obj) = queue.pop() {
                        if done.contains(&obj) {
                            continue;
                        }
                        done.insert(obj);
                        if obj == ObjectStore::RESOLVING_CALL {
                            changed = true;
                            continue;
                        }
                        if self.invalid_objects.contains(obj) {
                            continue;
                        }
                        for ((o, key), prop) in machine
                            .lattice
                            .get(&self.state.lattice_elements)
                            .prop_assignments
                            .iter()
                        {
                            if *o != obj {
                                continue;
                            }
                            if depends_on_unresolved_call(self.unions, prop.rhs) {
                                changed = true;
                                continue;
                            }

                            match prop.rhs {
                                Some(Pointer::Object(prop)) => {
                                    if !done.contains(&prop) {
                                        queue.push(prop);
                                    }
                                }
                                Some(Pointer::Union(union)) => {
                                    if !union.invalid() {
                                        for constituent in self.unions[union].constituents() {
                                            if !done.contains(&constituent) {
                                                queue.push(constituent);
                                            }
                                        }
                                    }
                                }
                                Some(Pointer::Fn(_) | Pointer::NullOrVoid) | None => {}
                            }
                            let key = (obj, key.clone());
                            let existing = self
                                .return_states
                                .get(&self.call)
                                .and_then(|m| m.get(&key))
                                .copied();
                            match existing {
                                Some(existing) => {
                                    let union = create_union(self.unions, existing.rhs, prop.rhs);
                                    let old = self
                                        .return_states
                                        .entry(self.call)
                                        .or_default()
                                        .insert(key, Assignment { rhs: union })
                                        .unwrap()
                                        .rhs;
                                    changed |= old != union;
                                }
                                None => {
                                    self.return_states
                                        .entry(self.call)
                                        .or_default()
                                        .insert(key, *prop);
                                    changed = true;
                                }
                            }
                        }
                    }

                    *self.changed |= changed;

                    break;
                }
                Step::StartUnion => {
                    machine.state.unions.push();
                }
                Step::PushToUnion => {
                    let v = machine.get_r_value();
                    machine.state.unions.cur().insert(v);
                }
                Step::StoreUnion => {
                    let parts = &*machine.state.unions.cur();

                    let mut builder = UnionBuilder::default();
                    for &ty in parts {
                        builder.add(ty, self.unions);
                    }
                    let result = self.unions.build_union(builder);

                    machine.state.unions.pop();

                    machine.set_r_value(result);
                }
                Step::SaveRValue => {
                    let v = machine.get_r_value();
                    machine.state.r_value.push(v);
                }
                Step::RestoreRValue => {
                    machine.state.r_value.pop();
                }
            }
        }

        Some(match machine.lattice {
            MachineLattice::Owned(state) => self.state.lattice_elements.insert(state),
            MachineLattice::Borrowed(_) => input,
        })
    }
}

/// Derives a unique [`ObjectId`] for the given object literal in the current
/// call. The derived type will be linked to a root type to record the
/// relationship. This approach makes the analysis more accurate by preventing
/// calls from interfering with each other, unless required for correctness.
/// See test `test_calls_do_not_interfere`
fn get_call_obj(
    objects_map: &mut FxHashMap<NodeId, ObjectId>,
    call_objects: &mut FxHashMap<(CallId, NodeId), ObjectId>,
    object_links: &mut FxHashSet<(ObjectId, ObjectId)>,
    objects: &mut ObjectStore,
    root_call: CallId,
    node_id: NodeId,
) -> Option<Pointer> {
    let root = match objects_map.entry(node_id) {
        Entry::Occupied(entry) => *entry.get(),
        Entry::Vacant(entry) => {
            let object_id = objects.next_object_id();
            entry.insert(object_id);
            object_id
        }
    };

    let local_id = match call_objects.entry((root_call, node_id)) {
        Entry::Occupied(entry) => *entry.get(),
        Entry::Vacant(entry) => {
            let object_id = objects.next_object_id();
            entry.insert(object_id);
            object_links.insert((root, object_id));
            object_id
        }
    };

    debug_assert!(root < local_id);

    Some(Pointer::Object(local_id))
}

#[derive(Default)]
struct JoinOp {
    result: Lattice,
}

impl JoinOp {
    fn joinFlow(
        &mut self,
        input: &Lattice,
        unions: &mut UnionStore,
        invalid_objects: &mut GrowableBitSet<ObjectId>,
    ) {
        if input.prop_assignments.len() > self.result.prop_assignments.len() {
            let new = input.prop_assignments.len() - self.result.prop_assignments.len();
            self.result.prop_assignments.reserve(new);
        }
        // Merge property assignments.
        for ((obj, key), prop) in input.prop_assignments.iter() {
            if invalid_objects.contains(*obj) {
                continue;
            }
            match self.result.prop_assignments.entry((*obj, key.clone())) {
                Entry::Occupied(mut entry) => {
                    let union = create_union(unions, entry.get().rhs, prop.rhs);
                    entry.insert(Assignment { rhs: union });
                }
                Entry::Vacant(entry) => {
                    entry.insert(*prop);
                }
            }
        }

        if input.var_assignments.len() > self.result.var_assignments.len() {
            let new = input.var_assignments.len() - self.result.var_assignments.len();
            self.result.var_assignments.reserve(new);
        }
        // Merge variable assignments.
        for (name, assignment) in input.var_assignments.iter() {
            match self.result.var_assignments.entry(name.clone()) {
                Entry::Occupied(mut entry) => {
                    let union = create_union(unions, entry.get().rhs, assignment.rhs);
                    entry.insert(Assignment { rhs: union });
                }
                Entry::Vacant(entry) => {
                    entry.insert(*assignment);
                }
            }
        }
    }

    fn finish(self) -> Lattice {
        self.result
    }
}

/// Re-usable state for a [`DataFlowAnalysis`].
#[derive(Debug, Default)]
struct DataFlowAnalysisState<'ast> {
    work_queue_inner: BTreeSet<PrioritizedNode<Node<'ast>>>,

    lattice_elements: IndexSet<LatticeElementId, Lattice>,
    node_annotations: FxHashMap<NodeId, LinearFlowState>,

    machine_state: MachineState,

    /// Cache of previously processed lattice joins.
    cached_joins: FxHashMap<u64, LatticeElementId>,
    /// Re-usable buffer for storing lattices-to-be-joined.
    join_input_buffer: Vec<LatticeElementId>,
}

impl ReusableState for DataFlowAnalysisState<'_> {
    fn reset(&mut self) {
        let DataFlowAnalysisState {
            work_queue_inner,
            lattice_elements,
            node_annotations,
            machine_state,
            cached_joins,
            // Note: DataFlowAnalysis clears this itself; doing so here would be pointless.
            join_input_buffer: _,
        } = self;
        work_queue_inner.clear();
        lattice_elements.reset();
        node_annotations.clear();
        machine_state.reset();
        cached_joins.clear();
    }
}

#[derive(Debug)]
struct DataFlowAnalysis<'ast, 'a> {
    /// The set of nodes that need to be considered, ordered by their priority
    /// as determined by control flow analysis and data flow direction.
    workQueue: UniqueQueue<'a, Node<'ast>>,

    cfg: &'a SimpleCFG<'ast>,
    entry_lattice: LatticeElementId,
    initial_lattice: LatticeElementId,

    call: CallId,

    path_map: &'a FxHashMap<NodeId, Vec<Step>>,

    resolved_calls: &'a mut FxHashMap<CallId, ResolvedCall>,

    incomplete_dependencies: &'a mut FxHashSet<CallId>,

    return_types: &'a mut FxHashMap<CallId, FxHashSet<ReturnTypeConstituent>>,
    return_states: &'a mut FxHashMap<CallId, PropertyAssignments>,
    objects_map: &'a mut FxHashMap<NodeId, ObjectId>,
    call_objects: &'a mut FxHashMap<(CallId, NodeId), ObjectId>,
    object_links: &'a mut FxHashSet<(ObjectId, ObjectId)>,
    objects: &'a mut ObjectStore,
    unions: &'a mut UnionStore,
    invalid_objects: &'a mut GrowableBitSet<ObjectId>,
    calls: &'a mut IndexSet<CallId, Call>,
    changed: &'a mut bool,

    fn_assignments: &'a HashableHashMap<Id, Assignment>,

    root_call: CallId,

    state: &'a mut DataFlowAnalysisState<'ast>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FlowResult {
    /// Node's state changed. Process successors.
    Change,
    NoChange,
    /// Unable to fully analyse the current node (likely because it depends on
    /// not-yet-resolved calls). Skip analysis of the nodes successors, but
    /// continue analysing other branches.
    Abort,
}

impl<'ast, 'a> DataFlowAnalysis<'ast, 'a> {
    fn analyze(&mut self) {
        self.analyze_inner()
            .expect("Dataflow analysis appears to diverge");
    }

    // TODO: analyze is split into two because tests need to verify that divergence
    // is caught, but all other callers will just panic at the moment (until proper
    // error handling is implemented for the compiler).
    fn analyze_inner(&mut self) -> Result<(), Node<'ast>> {
        {
            let in_ = self.createInitialEstimateLattice();
            let out = self.createInitialEstimateLattice();
            self.state.node_annotations.insert(
                self.cfg.implicit_return.node_id,
                LinearFlowState::new(in_, out),
            );
        }

        self.workQueue.push(self.cfg.entry);
        while let Some(curNode) = self.workQueue.pop() {
            let node_annotations = &mut self.state.node_annotations;
            let initial_lattice = self.initial_lattice;

            // We only add nodes to the work queue once we have processed their
            // predecessors, but we still want to visit the successors at least
            // once, even if their predecessors don't change the state.
            let mut first_visit = false;

            let step_count = &mut node_annotations
                .entry(curNode.node_id)
                .or_insert_with(|| {
                    first_visit = true;
                    LinearFlowState::new(initial_lattice, initial_lattice)
                })
                .stepCount;

            if *step_count > MAX_STEPS_PER_NODE {
                return Err(curNode);
            }
            *step_count += 1;

            self.joinInputs(curNode);
            let r = self.flow(curNode);
            // If there is a change in the current node, we want to grab the list
            // of nodes that this node affects.
            if r == FlowResult::Change || first_visit && r == FlowResult::NoChange {
                // We effectively treat the implicit return as `return undefined`,
                // so we skip processing it when coming from a ReturnStmt as
                // processing the implicit return would incorrectly add `undefined`
                // to the call's return type.
                if !matches!(curNode.kind, NodeKind::ReturnStmt(_)) {
                    let nextNodes = self.cfg.get_successors(curNode);

                    for nextNode in nextNodes {
                        let node = self.cfg.graph[nextNode];
                        self.workQueue.push(node);
                    }
                }
            }
        }

        self.joinInputs(self.cfg.implicit_return);

        Ok(())
    }

    /// Performs a single flow through a node.
    fn flow(&mut self, node: Node<'ast>) -> FlowResult {
        let state = &self.state.node_annotations[&node.node_id];
        let outBefore = state.out;
        if let Some(new_out) = self.flowThrough(node, state.in_) {
            self.get_flow_state_mut(node.node_id).out = new_out;
            if outBefore != new_out {
                FlowResult::Change
            } else {
                FlowResult::NoChange
            }
        } else {
            FlowResult::Abort
        }
    }

    /**
     * Computes the new flow state at a given node's entry by merging the output (input) lattice of
     * the node's predecessor (successor).
     *
     * @param node Node to compute new join.
     */
    fn joinInputs(&mut self, node: Node<'ast>) {
        if self.cfg.entry == node {
            self.get_flow_state_mut(node.node_id).in_ = self.createEntryLattice();
            return;
        }

        let graph = &self.cfg.graph;
        let node_annotations = &self.state.node_annotations;
        let initial_lattice = self.initial_lattice;

        // The only edges coming out of a ReturnStmt go to the implicit return,
        // so we skip these edges (see above comment in `analyze_inner`).
        let inputs = self
            .cfg
            .graph
            .edges_directed(self.cfg.map[&node], Incoming)
            .filter(|e| !matches!(graph[e.source()].kind, NodeKind::ReturnStmt(_)))
            .filter_map(|e| getInputFromEdge(graph, node_annotations, e))
            .filter(|i| *i != initial_lattice);

        self.state.join_input_buffer.clear();
        self.state.join_input_buffer.extend(inputs);

        if self.state.join_input_buffer.is_empty() {
            return;
        }

        if self.state.join_input_buffer.len() == 1 {
            // Only one relevant edge.
            self.get_flow_state_mut(node.node_id).in_ =
                *self.state.join_input_buffer.first().unwrap();
            return;
        }

        self.state.join_input_buffer.sort_unstable();
        self.state.join_input_buffer.dedup();

        // De-duping may have dropped us down to one element, check again.
        if self.state.join_input_buffer.len() == 1 {
            // Only one relevant edge.
            self.get_flow_state_mut(node.node_id).in_ =
                *self.state.join_input_buffer.first().unwrap();
            return;
        }

        let input_hash = {
            let mut h = FxHasher::default();
            self.state.join_input_buffer.hash(&mut h);
            h.finish()
        };

        // Check if we have already joined these inputs.
        let entry = match self.state.cached_joins.entry(input_hash) {
            Entry::Occupied(entry) => {
                self.get_flow_state_mut(node.node_id).in_ = *entry.get(); // Reuse cached join.
                return;
            }
            Entry::Vacant(entry) => entry,
        };

        let mut joiner = JoinOp::default();
        for &id in &self.state.join_input_buffer {
            joiner.joinFlow(
                &self.state.lattice_elements[id],
                self.unions,
                self.invalid_objects,
            );
        }
        let result = self.state.lattice_elements.insert(joiner.finish());
        // Cache result.
        entry.insert(result);
        self.get_flow_state_mut(node.node_id).in_ = result;
    }

    fn get_flow_state_mut(&mut self, node: NodeId) -> &mut LinearFlowState {
        // All nodes should have had their state initialized.
        self.state.node_annotations.get_mut(&node).unwrap()
    }

    fn createEntryLattice(&mut self) -> LatticeElementId {
        self.entry_lattice
    }

    fn createInitialEstimateLattice(&mut self) -> LatticeElementId {
        self.initial_lattice
    }
}

fn getInputFromEdge(
    graph: &DiGraph<Node, Branch>,
    node_annotations: &FxHashMap<NodeId, LinearFlowState>,
    edge: EdgeReference<Branch>,
) -> Option<LatticeElementId> {
    let source = edge.source();
    let node = graph[source];
    node_annotations.get(&node.node_id).map(|state| state.out)
}
