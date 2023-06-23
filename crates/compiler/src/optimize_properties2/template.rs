#![deny(unused_imports)]

use std::collections::hash_map::Entry;

use ast::NodeId;
use index::bit_set::GrowableBitSet;
use index::vec::{Idx, IndexVec};
use petgraph::graph::EdgeReference;
use petgraph::visit::EdgeRef;
use petgraph::Direction::Incoming;
use rustc_hash::{FxHashMap, FxHashSet};
use swc_atoms::JsWord;

use crate::control_flow::node::{Node, NodeKind};
use crate::control_flow::ControlFlowGraph::Branch;
use crate::utils::unwrap_as;
use crate::DataFlowAnalysis::{LatticeElementId, LinearFlowState, UniqueQueue, MAX_STEPS_PER_NODE};
use crate::Id;

use super::graph::{process, Visitor};
use super::hashable_map::HashableHashMap;
use super::simple_set::IndexSet;
use super::unions::{UnionBuilder, UnionStore};
use super::{
    function::*, Assignment, Call, CallArgBuilder, CallArgs, CallId, FnId, Func, Lattice, ObjectId,
    Pointer, PropertyAssignments, ResolvedCall, SimpleCFG, StaticFunctionData, Store,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
enum ReturnTypeConstituent {
    Object(ObjectId),
    Invalid,
    NullOrVoid,
}

struct Resolver<'a, 'ast> {
    functions: &'a mut IndexVec<FnId, Func>,
    static_fn_data: &'a IndexVec<FnId, StaticFunctionData<'ast>>,

    call_templates: &'a FxHashMap<FnId, CallTemplate>,

    resolved_calls: &'a mut FxHashMap<CallId, ResolvedCall>,

    cur_object_id: &'a mut ObjectId,
    objects_map: &'a mut FxHashMap<NodeId, ObjectId>,
    call_objects: &'a mut FxHashMap<(CallId, NodeId), ObjectId>,
    object_links: &'a mut FxHashSet<(ObjectId, ObjectId)>,

    resolving_call_object: ObjectId,
    unions: &'a mut UnionStore,
    invalid_objects: &'a mut GrowableBitSet<ObjectId>,
    calls: &'a mut IndexSet<CallId, Call>,

    fn_assignments: &'a HashableHashMap<Id, Assignment>,

    return_types: FxHashMap<CallId, FxHashSet<ReturnTypeConstituent>>,
    return_states: FxHashMap<CallId, PropertyAssignments>,

    root_call: CallId,
}

impl Visitor<CallId> for Resolver<'_, '_> {
    fn visit_node(&mut self, node: CallId, dependencies: &mut FxHashSet<CallId>) -> bool {
        let mut changed = false;

        debug_assert!(!self.resolved_calls.contains_key(&node));

        let call = &self.calls[node];

        let func = call.func;

        let mut lattice_elements = IndexSet::default();
        let initial_lattice = lattice_elements.insert(Lattice::default());
        let mut entry_lattice = Lattice {
            prop_assignments: call.prop_assignments.clone(),
            var_assignments: HashableHashMap::default(),
        };
        for (i, param_name) in self.static_fn_data[func].param_names.iter().enumerate() {
            let value = call.args.get(i).unwrap_or(Some(Pointer::NullOrVoid));
            entry_lattice
                .var_assignments
                .insert(param_name.clone(), Assignment { rhs: value });
        }
        let entry_lattice = lattice_elements.insert(entry_lattice);

        let mut analysis = DataFlowAnalysis {
            workQueue: UniqueQueue::new(&self.static_fn_data[func].node_priorities, true),
            cfg: &self.static_fn_data[func].cfg,
            lattice_elements,
            entry_lattice,
            initial_lattice,
            node_annotations: FxHashMap::default(),

            call: node,
            path_map: &self.call_templates[&func].step_map,
            resolved_calls: self.resolved_calls,
            return_types: &mut self.return_types,
            return_states: &mut self.return_states,
            cur_object_id: self.cur_object_id,
            objects_map: self.objects_map,
            call_objects: self.call_objects,
            object_links: self.object_links,
            resolving_call_object: self.resolving_call_object,
            unions: self.unions,
            invalid_objects: self.invalid_objects,
            calls: self.calls,

            incomplete_dependencies: dependencies,

            changed: &mut changed,
            fn_assignments: self.fn_assignments,

            root_call: self.root_call,
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

        let return_types = self.return_types.remove(&node).unwrap_or_default();

        let mut builder = UnionBuilder::default();

        for ty in return_types {
            match ty {
                ReturnTypeConstituent::Object(obj) => builder.add_object(Some(obj)),
                ReturnTypeConstituent::Invalid => builder.add_object(None),
                ReturnTypeConstituent::NullOrVoid => builder.add_null_or_void(),
            };
        }

        let return_type = self.unions.build_union(builder);

        let prop_assignments = self.return_states.remove(&node).unwrap_or_default();

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

    let mut visitor = Resolver {
        call_templates: &store.call_templates,
        functions: &mut store.functions,
        static_fn_data: &store.static_fn_data,
        resolved_calls: &mut store.resolved_calls,
        cur_object_id: &mut store.cur_object_id,
        objects_map: &mut store.objects_map,
        call_objects: &mut store.call_objects,
        object_links: &mut store.object_links,
        resolving_call_object: store.resolving_call_object,
        unions: &mut store.unions,
        invalid_objects: &mut store.invalid_objects,
        calls: &mut store.calls,
        fn_assignments: &store.fn_assignments,
        return_types: Default::default(),
        return_states: Default::default(),
        root_call: call,
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
    if let Some(Pointer::NullOrVoid) = object {
        return Some(Pointer::NullOrVoid);
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
                        !matches!(prop, Some(Pointer::Object(_) | Pointer::Union(_)))
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
                            !matches!(constituent, Some(Pointer::Object(_) | Pointer::Union(_)))
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
        Some(Pointer::Object(_) | Pointer::Union(_)) => {}
        None | Some(Pointer::Fn(_) | Pointer::NullOrVoid) => return,
    }
    if let Some(pointer) = pointer {
        let mut queue = vec![pointer];
        let mut done = FxHashSet::default();

        while let Some(pointer) = queue.pop() {
            done.insert(pointer);

            match pointer {
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
enum MachineState {
    Owned(Lattice),
    Borrowed(LatticeElementId),
}

impl MachineState {
    fn get<'a>(&'a self, lattice_elements: &'a IndexSet<LatticeElementId, Lattice>) -> &'a Lattice {
        match self {
            MachineState::Owned(l) => l,
            MachineState::Borrowed(id) => &lattice_elements[*id],
        }
    }
    fn get_mut<'a>(
        &'a mut self,
        lattice_elements: &IndexSet<LatticeElementId, Lattice>,
    ) -> &'a mut Lattice {
        match self {
            MachineState::Owned(l) => l,
            MachineState::Borrowed(id) => {
                let id = *id;
                *self = MachineState::Owned(lattice_elements[id].clone());
                unwrap_as!(self, MachineState::Owned(l), l)
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
/// State machine that executes a sequence of [`Steps`][Step] that represent the
/// effects of a node in the control flow graph.
struct Machine<'a> {
    state: MachineState,
    steps: &'a [Step],
    /// Stack of calls (fn and args) that are currently being built.
    calls: Vec<MachineCall>,
    /// Stack of unions (constituents) that are currently being built.
    unions: Vec<FxHashSet<Option<Pointer>>>,
    /// Stack of expression values. Using a stack simplifies the [`Step`]
    /// language, allowing us to handle nested expressions by pushing/popping
    /// values on the stack.
    r_value: Vec<Option<Pointer>>,
    /// Register containing the current left-hand-side value.
    l_value: Option<AssignTarget>,
    fn_assignments: &'a HashableHashMap<Id, Assignment>,
}

impl Machine<'_> {
    fn get_r_value(&self) -> Option<Pointer> {
        *self.r_value.last().unwrap()
    }
    fn set_r_value(&mut self, value: Option<Pointer>) {
        *self.r_value.last_mut().unwrap() = value;
    }

    fn get_var(
        &self,
        id: &Id,
        lattice_elements: &IndexSet<LatticeElementId, Lattice>,
    ) -> Option<Assignment> {
        self.state
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
    pub fn new(store: &mut Store, func: FnId) -> CallTemplate {
        let step_map = create_step_map(store, func);
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
        let mut machine = Machine {
            state: MachineState::Borrowed(input),
            steps,
            calls: Vec::new(),
            r_value: vec![None],
            l_value: None,
            unions: Vec::new(),
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
                                .get_var(rhs, &self.lattice_elements)
                                .and_then(|a| a.rhs),
                            RValue::Object(o) => self.get_call_obj(*o),
                            RValue::Prop(prop) => get_property(
                                &machine.state.get(&self.lattice_elements).prop_assignments,
                                self.unions,
                                machine.get_r_value(),
                                prop,
                                self.invalid_objects,
                            ),
                        };
                        machine.set_r_value(value);
                    }
                },
                Step::StoreLValue(value) => match value {
                    Some(LValue::Var(id)) => {
                        machine.l_value = Some(AssignTarget::Var(id.clone()));
                    }
                    Some(LValue::ObjectProp(obj, prop)) => {
                        machine.l_value =
                            Some(AssignTarget::Prop(self.get_call_obj(*obj), prop.clone()));
                    }
                    Some(LValue::RValueProp(prop)) => {
                        machine.l_value =
                            Some(AssignTarget::Prop(machine.get_r_value(), prop.clone()));
                    }
                    None => {
                        machine.l_value = None;
                    }
                },
                Step::Assign(conditional) => {
                    if let Some(slot) = &machine.l_value {
                        let existing = match &slot {
                            AssignTarget::Var(name) => machine
                                .get_var(name, &self.lattice_elements)
                                .and_then(|a| a.rhs),
                            AssignTarget::Prop(obj, key) => get_property(
                                &machine.state.get(&self.lattice_elements).prop_assignments,
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
                                machine.state.insert_var_assignment(
                                    id.clone(),
                                    new,
                                    &self.lattice_elements,
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
                                                    .state
                                                    .get(&self.lattice_elements)
                                                    .prop_assignments,
                                            );
                                            continue;
                                        }
                                    }

                                    match obj {
                                        Pointer::Object(obj) => {
                                            machine.state.insert_prop_assignment(
                                                (obj, prop.clone()),
                                                new,
                                                &self.lattice_elements,
                                            );
                                        }
                                        Pointer::Union(union) => {
                                            for constituent in self.unions[union].constituents() {
                                                machine.state.insert_prop_assignment(
                                                    (constituent, prop.clone()),
                                                    new,
                                                    &self.lattice_elements,
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
                                        &machine.state.get(&self.lattice_elements).prop_assignments,
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
                            &machine.state.get(&self.lattice_elements).prop_assignments,
                        );
                    }
                }
                Step::InvalidateRValue => {
                    invalidate(
                        self.invalid_objects,
                        self.unions,
                        machine.get_r_value(),
                        &machine.state.get(&self.lattice_elements).prop_assignments,
                    );
                }
                Step::InvalidateLValue => match &machine.l_value {
                    Some(AssignTarget::Var(id)) => {
                        if let Some(rhs) =
                            machine.get_var(id, &self.lattice_elements).map(|a| a.rhs)
                        {
                            invalidate(
                                self.invalid_objects,
                                self.unions,
                                rhs,
                                &machine.state.get(&self.lattice_elements).prop_assignments,
                            );
                        }
                    }
                    Some(AssignTarget::Prop(obj, prop)) => {
                        let prop = get_property(
                            &machine.state.get(&self.lattice_elements).prop_assignments,
                            self.unions,
                            *obj,
                            prop,
                            self.invalid_objects,
                        );

                        invalidate(
                            self.invalid_objects,
                            self.unions,
                            prop,
                            &machine.state.get(&self.lattice_elements).prop_assignments,
                        );
                    }
                    None => {}
                },
                Step::StartCall(num_args) => match machine.get_r_value() {
                    Some(Pointer::Fn(f)) => {
                        machine
                            .calls
                            .push(MachineCall::Valid(f, CallArgBuilder::new(*num_args)));
                    }
                    _ => {
                        machine.calls.push(MachineCall::Invalid);
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
                    match machine.calls.last_mut().unwrap() {
                        MachineCall::Invalid => {
                            invalidate(
                                self.invalid_objects,
                                self.unions,
                                arg,
                                &machine.state.get(&self.lattice_elements).prop_assignments,
                            );
                        }
                        MachineCall::Valid(_, args) => args.push(arg),
                    }
                }
                Step::Call => {
                    let (func, mut args) = match machine.calls.pop().unwrap() {
                        MachineCall::Invalid => {
                            machine.set_r_value(None);
                            continue;
                        }
                        MachineCall::Valid(f, a) => (f, a),
                    };

                    if let Some(heap) = &args.args {
                        if !heap.is_empty() {
                            if heap.iter().all(|a| match a {
                                Some(Pointer::Union(union)) => union.invalid(),
                                Some(Pointer::Object(obj)) => self.invalid_objects.contains(*obj),
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

                        // TODO: check argument prop assignments for unresolved calls
                        while let Some(o) = queue.pop() {
                            if o == self.resolving_call_object {
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
                                .state
                                .get(&self.lattice_elements)
                                .prop_assignments
                                .iter()
                            {
                                if key.0 == o {
                                    prop_assignments.insert(key.clone(), *value);
                                    match value.rhs {
                                        Some(Pointer::Object(o)) => {
                                            if o == self.resolving_call_object {
                                                return None;
                                            }
                                            if !done.contains(&o) {
                                                queue.push(o);
                                            }
                                        }
                                        Some(Pointer::Union(union)) => {
                                            for constituent in self.unions[union].constituents() {
                                                if o == self.resolving_call_object {
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
                        let depends_on_unresolved_call = |pointer| match pointer {
                            Some(Pointer::Object(o)) => o == self.resolving_call_object,
                            Some(Pointer::Union(union)) => self.unions[union]
                                .constituents()
                                .any(|c| c == self.resolving_call_object),
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
                            .any(|(o, _)| *o == self.resolving_call_object));
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
                                        self.resolving_call_object,
                                    )));
                                    continue;
                                }
                            };
                            if return_types.is_empty() {
                                machine
                                    .set_r_value(Some(Pointer::Object(self.resolving_call_object)));
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
                                .state
                                .get(&self.lattice_elements)
                                .prop_assignments
                                .get(&key)
                            {
                                let union = create_union(self.unions, existing.rhs, prop.rhs);
                                Assignment { rhs: union }
                            } else {
                                *prop
                            };
                            machine
                                .state
                                .insert_prop_assignment(key, new, &self.lattice_elements);
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
                            if o == self.resolving_call_object {
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
                                if union.contains(self.resolving_call_object) && union.len() == 1 {
                                    return None;
                                }
                                queue.extend(union.constituents());
                                for constituent in union.constituents() {
                                    if constituent != self.resolving_call_object {
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
                        resolving_call_object: ObjectId,
                    ) -> bool {
                        match pointer {
                            Some(Pointer::Object(o)) => o == resolving_call_object,
                            Some(Pointer::Union(union)) => {
                                unions[union].contains(resolving_call_object)
                            }
                            Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) | None => false,
                        }
                    }

                    let mut done = FxHashSet::default();

                    while let Some(obj) = queue.pop() {
                        if done.contains(&obj) {
                            continue;
                        }
                        done.insert(obj);
                        if obj == self.resolving_call_object {
                            changed = true;
                            continue;
                        }
                        if self.invalid_objects.contains(obj) {
                            continue;
                        }
                        for ((o, key), prop) in machine
                            .state
                            .get(&self.lattice_elements)
                            .prop_assignments
                            .iter()
                        {
                            if *o != obj {
                                continue;
                            }
                            if depends_on_unresolved_call(
                                self.unions,
                                prop.rhs,
                                self.resolving_call_object,
                            ) {
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
                    machine.unions.push(FxHashSet::default());
                }
                Step::PushToUnion => {
                    let v = machine.get_r_value();
                    machine.unions.first_mut().unwrap().insert(v);
                }
                Step::StoreUnion => {
                    let parts = machine.unions.pop().unwrap();

                    let mut builder = UnionBuilder::default();
                    for ty in parts {
                        builder.add(ty, self.unions);
                    }
                    let result = self.unions.build_union(builder);

                    machine.set_r_value(result);
                }
                Step::SaveRValue => {
                    let v = machine.get_r_value();
                    machine.r_value.push(v);
                }
                Step::RestoreRValue => {
                    machine.r_value.pop();
                }
            }
        }

        Some(match machine.state {
            MachineState::Owned(state) => self.add_lattice_element(state),
            MachineState::Borrowed(_) => input,
        })
    }

    /// Derives a unique [`ObjectId`] for the given object literal in the current
    /// call. The derived type will be linked to a root type to record the
    /// relationship. This approach makes the analysis more accurate by preventing
    /// calls from interfering with each other, unless required for correctness.
    /// See test `test_calls_do_not_interfere`
    fn get_call_obj(&mut self, node_id: NodeId) -> Option<Pointer> {
        let root = match self.objects_map.entry(node_id) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let object_id = *self.cur_object_id;
                self.cur_object_id.increment_by(1);
                entry.insert(object_id);
                object_id
            }
        };

        let local_id = match self.call_objects.entry((self.root_call, node_id)) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let object_id = *self.cur_object_id;
                self.cur_object_id.increment_by(1);
                entry.insert(object_id);
                self.object_links.insert((root, object_id));
                object_id
            }
        };

        debug_assert!(root < local_id);

        Some(Pointer::Object(local_id))
    }
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
        // Merge property assignments.
        for ((obj, key), prop) in input.prop_assignments.iter() {
            if invalid_objects.contains(*obj) {
                continue;
            }
            match self.result.prop_assignments.entry((*obj, key.clone())) {
                Entry::Occupied(entry) => {
                    let union = create_union(unions, entry.get().rhs, prop.rhs);
                    self.result
                        .prop_assignments
                        .insert((*obj, key.clone()), Assignment { rhs: union });
                }
                Entry::Vacant(entry) => {
                    entry.insert(*prop);
                }
            }
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

#[derive(Debug)]
struct DataFlowAnalysis<'ast, 'a> {
    /// The set of nodes that need to be considered, ordered by their priority
    /// as determined by control flow analysis and data flow direction.
    workQueue: UniqueQueue<'a, Node<'ast>>,

    lattice_elements: IndexSet<LatticeElementId, Lattice>,
    cfg: &'a SimpleCFG<'ast>,
    entry_lattice: LatticeElementId,
    initial_lattice: LatticeElementId,
    node_annotations: FxHashMap<NodeId, LinearFlowState>,

    call: CallId,

    path_map: &'a FxHashMap<NodeId, Vec<Step>>,

    resolved_calls: &'ast mut FxHashMap<CallId, ResolvedCall>,

    incomplete_dependencies: &'a mut FxHashSet<CallId>,

    return_types: &'ast mut FxHashMap<CallId, FxHashSet<ReturnTypeConstituent>>,
    return_states: &'ast mut FxHashMap<CallId, PropertyAssignments>,
    cur_object_id: &'ast mut ObjectId,
    objects_map: &'ast mut FxHashMap<NodeId, ObjectId>,
    call_objects: &'ast mut FxHashMap<(CallId, NodeId), ObjectId>,
    object_links: &'ast mut FxHashSet<(ObjectId, ObjectId)>,
    resolving_call_object: ObjectId,
    unions: &'ast mut UnionStore,
    invalid_objects: &'ast mut GrowableBitSet<ObjectId>,
    calls: &'a mut IndexSet<CallId, Call>,
    changed: &'a mut bool,

    fn_assignments: &'a HashableHashMap<Id, Assignment>,

    root_call: CallId,
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
            self.node_annotations.insert(
                self.cfg.implicit_return.node_id,
                LinearFlowState::new(in_, out),
            );
        }

        self.workQueue.push(self.cfg.entry);
        while let Some(curNode) = self.workQueue.pop() {
            let node_annotations = &mut self.node_annotations;
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
        let state = &self.node_annotations[&node.node_id];
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

        // The only edges coming out of a ReturnStmt go to the implicit return,
        // so we skip these edges (see above comment in `analyze_inner`).
        let mut inEdges = self
            .cfg
            .graph
            .edges_directed(self.cfg.map[&node], Incoming)
            .filter(|e| !matches!(graph[e.source()].kind, NodeKind::ReturnStmt(_)));

        if let Some(first) = inEdges.next() {
            if let Some(second) = inEdges.next() {
                let mut joiner = self.createFlowJoiner();
                let mut has_non_empty_input = false;
                if let Some(first) = self.getInputFromEdge(first) {
                    joiner.joinFlow(
                        &self.lattice_elements[first],
                        self.unions,
                        self.invalid_objects,
                    );
                    has_non_empty_input = true;
                }
                if let Some(second) = self.getInputFromEdge(second) {
                    joiner.joinFlow(
                        &self.lattice_elements[second],
                        self.unions,
                        self.invalid_objects,
                    );
                    has_non_empty_input = true;
                }
                for inEdge in inEdges {
                    if let Some(id) = self.getInputFromEdge(inEdge) {
                        joiner.joinFlow(
                            &self.lattice_elements[id],
                            self.unions,
                            self.invalid_objects,
                        );
                        has_non_empty_input = true;
                    }
                }
                if has_non_empty_input {
                    self.get_flow_state_mut(node.node_id).in_ =
                        self.add_lattice_element(joiner.finish());
                }
            } else {
                // Only one relevant edge.
                if let Some(result) = self.getInputFromEdge(first) {
                    self.get_flow_state_mut(node.node_id).in_ = result;
                }
            }
        } else {
            // No relevant edges.
        }
    }

    fn get_flow_state_mut(&mut self, node: NodeId) -> &mut LinearFlowState {
        // All nodes should have had their state initialized.
        self.node_annotations.get_mut(&node).unwrap()
    }

    fn getInputFromEdge(&self, edge: EdgeReference<Branch>) -> Option<LatticeElementId> {
        let source = edge.source();
        let node = self.cfg.graph[source];
        self.node_annotations
            .get(&node.node_id)
            .map(|state| state.out)
    }

    fn add_lattice_element(&mut self, element: Lattice) -> LatticeElementId {
        self.lattice_elements.insert(element)
    }

    fn createEntryLattice(&mut self) -> LatticeElementId {
        self.entry_lattice
    }

    fn createInitialEstimateLattice(&mut self) -> LatticeElementId {
        self.initial_lattice
    }

    fn createFlowJoiner(&self) -> JoinOp {
        JoinOp::default()
    }
}
