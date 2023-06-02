#![deny(unused_imports)]

use std::collections::hash_map::Entry;
use std::sync::{Mutex, RwLock};

use ast::NodeId;
use index::bit_set::GrowableBitSet;
use index::vec::{Idx, IndexVec};
use petgraph::graph::EdgeReference;
use petgraph::visit::EdgeRef;
use petgraph::Direction::Incoming;
use rustc_hash::{FxHashMap, FxHashSet};
use swc_atoms::JsWord;

use crate::control_flow::node::Node;
use crate::control_flow::ControlFlowGraph::Branch;
use crate::utils::unwrap_as;
use crate::DataFlowAnalysis::{LatticeElementId, LinearFlowState, UniqueQueue, MAX_STEPS_PER_NODE};
use crate::Id;

use super::graph::{process, Visitor};
use super::hashable_map::HashableHashMap;
use super::simple_set::IndexSet;
use super::unions::{UnionBuilder, UnionStore};
use super::{function::*, CallArgBuilder, Func, StaticFunctionData};
use super::{
    Assignment, Call, CallArgs, CallId, FnId, Lattice, ObjectId, Pointer, ResolvedCall, SimpleCFG,
    Store,
};

struct Resolver<'a, 'ast> {
    functions: RwLock<&'a mut IndexVec<FnId, Func>>,
    static_fn_data: &'a IndexVec<FnId, StaticFunctionData<'ast>>,

    call_templates: &'a FxHashMap<FnId, CallTemplate<'ast>>,

    resolved_calls: RwLock<&'a mut FxHashMap<CallId, ResolvedCall>>,

    cur_object_id: Mutex<&'a mut ObjectId>,
    objects_map: RwLock<&'a mut FxHashMap<NodeId, ObjectId>>,

    null_or_void: ObjectId,
    null_or_void_p: Option<Pointer>,
    resolving_call_object: ObjectId,
    unions: RwLock<&'a mut UnionStore>,
    invalid_objects: RwLock<&'a mut GrowableBitSet<ObjectId>>,
    calls: RwLock<&'a mut IndexSet<CallId, Call>>,

    fn_assignments: &'a HashableHashMap<Id, Assignment>,

    return_types: RwLock<FxHashMap<CallId, FxHashSet<Option<ObjectId>>>>,
    return_states: RwLock<FxHashMap<CallId, HashableHashMap<(ObjectId, JsWord), Assignment>>>,
}

impl Visitor<CallId> for Resolver<'_, '_> {
    fn visit_node(&self, node: CallId, dependencies: &mut FxHashSet<CallId>) -> bool {
        let mut changed = false;

        let (func, lattice_elements, initial_lattice, entry_lattice) = {
            {
                debug_assert!(!self.resolved_calls.read().unwrap().contains_key(&node));
            }

            let calls = self.calls.read().unwrap();

            let func = calls[node].func;

            let mut lattice_elements = IndexSet::default();
            let initial_lattice = lattice_elements.insert(Lattice::default());
            let mut entry_lattice = Lattice {
                prop_assignments: calls[node].prop_assignments.clone(),
                var_assignments: HashableHashMap::default(),
            };
            for (i, param_name) in self.static_fn_data[func].param_names.iter().enumerate() {
                let value = calls[node].args.get(i).unwrap_or(self.null_or_void_p);
                entry_lattice
                    .var_assignments
                    .insert(param_name.clone(), Assignment { rhs: value });
            }
            let entry_lattice = lattice_elements.insert(entry_lattice);

            (func, lattice_elements, initial_lattice, entry_lattice)
        };

        let mut analysis = DataFlowAnalysis {
            workQueue: UniqueQueue::new(&self.static_fn_data[func].node_priorities, true),
            cfg: &self.static_fn_data[func].cfg,
            lattice_elements,
            entry_lattice,
            initial_lattice,
            node_annotations: FxHashMap::default(),

            call: node,
            path_map: &self.call_templates[&func].step_map,
            resolved_calls: &self.resolved_calls,
            return_types: &self.return_types,
            return_states: &self.return_states,
            cur_object_id: &self.cur_object_id,
            objects_map: &self.objects_map,
            null_or_void: self.null_or_void,
            null_or_void_p: self.null_or_void_p,
            resolving_call_object: self.resolving_call_object,
            unions: &self.unions,
            invalid_objects: &self.invalid_objects,
            calls: &self.calls,

            incomplete_dependencies: dependencies,

            changed: &mut changed,
            fn_assignments: self.fn_assignments,
        };
        analysis.analyze();

        changed
    }

    fn finish_node(&self, node: CallId) {
        let ty = {
            self.return_types
                .write()
                .unwrap()
                .remove(&node)
                .unwrap_or_default()
        };

        let func = { self.calls.read().unwrap()[node].func };

        macro_rules! read_fn {
            () => {
                self.functions.read().unwrap()[func]
            };
        }

        // Merge arguments.
        {
            let (len, all_nones) = {
                match &self.calls.read().unwrap()[node].args {
                    CallArgs::Heap(args) => (args.len(), false),
                    CallArgs::Invalid(len) => (*len, true),
                }
            };

            if all_nones {
                for i in 0..len {
                    let existing = { read_fn!().args.get(i).copied() };
                    match existing {
                        Some(existing) => {
                            if existing.is_some() {
                                let union =
                                    create_union(&self.unions, existing, None, self.null_or_void);
                                self.functions.write().unwrap()[func].args[i] = union;
                            }
                        }
                        None => {
                            self.functions.write().unwrap()[func].args.push(None);
                        }
                    }
                }
            } else {
                let mut i = 0;

                while i < len {
                    let arg = {
                        unwrap_as!(&self.calls.read().unwrap()[node].args, CallArgs::Heap(a), a)[i]
                    };
                    let existing = { read_fn!().args.get(i).copied() };
                    match existing {
                        Some(existing) => {
                            let union =
                                create_union(&self.unions, existing, arg, self.null_or_void);
                            self.functions.write().unwrap()[func].args[i] = union;
                        }
                        None => {
                            self.functions.write().unwrap()[func].args.push(arg);
                        }
                    }
                    i += 1;
                }
            }
        }

        {
            // TODO: bad clone:
            let prop_assignments = { self.calls.read().unwrap()[node].prop_assignments.clone() };
            // Merge property assignments.
            for ((obj, key), prop) in prop_assignments.iter() {
                let obj_invalid = { self.invalid_objects.read().unwrap().contains(*obj) };
                if obj_invalid {
                    debug_assert!(
                        prop.rhs.is_none()
                            || invalidated(prop.rhs.unwrap(), &self.invalid_objects, &self.unions)
                    );
                    continue;
                }
                let key = (*obj, key.clone());
                let existing = { read_fn!().arg_values.get(&key).copied() };
                match existing {
                    Some(existing) => {
                        let union =
                            create_union(&self.unions, existing.rhs, prop.rhs, self.null_or_void);
                        self.functions.write().unwrap()[func]
                            .arg_values
                            .insert(key, Assignment { rhs: union });
                    }
                    None => {
                        self.functions.write().unwrap()[func]
                            .arg_values
                            .insert(key, prop.clone());
                    }
                }
            }
        }

        let return_type = {
            let mut builder = UnionBuilder::new(self.null_or_void);

            for ty in ty {
                builder.add_object(ty);
            }

            self.unions.write().unwrap().build_union(builder)
        };

        let prop_assignments = {
            self.return_states
                .write()
                .unwrap()
                .remove(&node)
                .unwrap_or_default()
        };

        let resolved = ResolvedCall {
            return_type,
            prop_assignments,
        };

        self.resolved_calls.write().unwrap().insert(node, resolved);
    }
}

/// Recursively resolves the return type and side effects of the given call.
pub(super) fn resolve_call(call: CallId, store: &mut Store) {
    if store.resolved_calls.contains_key(&call) {
        return;
    }

    let mut visitor = Resolver {
        call_templates: &store.call_templates,
        functions: RwLock::new(&mut store.functions),
        static_fn_data: &store.static_fn_data,
        resolved_calls: RwLock::new(&mut store.resolved_calls),
        cur_object_id: Mutex::new(&mut store.cur_object_id),
        objects_map: RwLock::new(&mut store.objects_map),
        null_or_void: store.null_or_void,
        null_or_void_p: Some(Pointer::Object(store.null_or_void)),
        resolving_call_object: store.resolving_call_object,
        unions: RwLock::new(&mut store.unions),
        invalid_objects: RwLock::new(&mut store.invalid_objects),
        calls: RwLock::new(&mut store.calls),
        fn_assignments: &store.fn_assignments,
        return_types: RwLock::default(),
        return_states: RwLock::default(),
    };

    process(call, &mut visitor);

    debug_assert!(store.resolved_calls.contains_key(&call));
}

fn get_property(
    prop_assignments: &HashableHashMap<(ObjectId, JsWord), Assignment>,
    unions: &RwLock<&mut UnionStore>,
    object: Option<Pointer>,
    key: &JsWord,
    null_or_void: ObjectId,
    invalid_objects: &RwLock<&mut GrowableBitSet<ObjectId>>,
) -> Option<Pointer> {
    let invalid = match object {
        Some(Pointer::Object(obj)) => invalid_objects.read().unwrap().contains(obj),
        Some(Pointer::Union(union)) => {
            let invalid = { invalidated(Pointer::Union(union), invalid_objects, unions) };
            if invalid {
                invalidate(
                    invalid_objects,
                    unions,
                    object,
                    prop_assignments,
                    Some(Pointer::Object(null_or_void)),
                );
            }
            invalid
        }
        _ => true,
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
                        prop.is_none() || invalidated(prop.unwrap(), invalid_objects, unions)
                    );
                }
                Some(Pointer::Union(union)) => {
                    let constituents = {
                        unions.read().unwrap()[union]
                            .constituents()
                            .collect::<Vec<_>>()
                    };
                    for constituent in constituents {
                        let constituent = prop_assignments
                            .get(&(constituent, key.clone()))
                            .map(|a| a.rhs)
                            .unwrap_or_default();
                        debug_assert!(
                            constituent.is_none()
                                || invalidated(constituent.unwrap(), invalid_objects, unions)
                        );
                    }
                }
                _ => {}
            }
        }

        return None;
    }

    match object {
        Some(Pointer::Object(obj)) => prop_assignments
            .get(&(obj, key.clone()))
            .map(|a| a.rhs)
            .unwrap_or(Some(Pointer::Object(null_or_void))),
        Some(Pointer::Union(union)) => {
            let unions = &mut unions.write().unwrap();
            let mut builder = UnionBuilder::new(null_or_void);

            for constituent in unions[union].constituents() {
                let constituent = prop_assignments
                    .get(&(constituent, key.clone()))
                    .and_then(|a| a.rhs)
                    .or(Some(Pointer::Object(null_or_void)));

                builder.add(constituent, unions);
            }
            unions.build_union(builder)
        }
        Some(Pointer::Fn(_)) | None => None,
    }
}

/// Returns a [`Pointer`] to a union of `pointer1`'s type and `pointer2`'s type.
fn create_union(
    unions: &RwLock<&mut UnionStore>,
    pointer1: Option<Pointer>,
    pointer2: Option<Pointer>,
    null_or_void: ObjectId,
) -> Option<Pointer> {
    if pointer1 == pointer2 {
        return pointer1;
    }

    let unions = &mut unions.write().unwrap();

    let mut builder = UnionBuilder::new(null_or_void);

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

impl<'a> GetPropAssignments for &'a HashableHashMap<(ObjectId, JsWord), Assignment> {
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

impl<'a, 'b> GetPropAssignments for (&'a RwLock<&'b mut IndexVec<FnId, Func>>, FnId) {
    fn prop_assignments(
        &self,
        object: ObjectId,
        queue: &mut Vec<Pointer>,
        done: &FxHashSet<Pointer>,
    ) {
        let lock = self.0.read().unwrap();
        let props = &lock[self.1].arg_values;
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

impl<'a, 'b> GetPropAssignments for (&'a RwLock<&'b mut IndexSet<CallId, Call>>, CallId) {
    fn prop_assignments(
        &self,
        object: ObjectId,
        queue: &mut Vec<Pointer>,
        done: &FxHashSet<Pointer>,
    ) {
        let lock = self.0.read().unwrap();
        let props = &lock[self.1].prop_assignments;
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
    invalid_objects: &RwLock<&mut GrowableBitSet<ObjectId>>,
    unions: &RwLock<&mut UnionStore>,
    pointer: Option<Pointer>,
    prop_assignments: impl GetPropAssignments,
    null_or_void: Option<Pointer>,
) {
    if pointer == null_or_void {
        return;
    }
    if let Some(pointer) = pointer {
        let mut queue = vec![pointer];
        let mut done = FxHashSet::default();

        while let Some(pointer) = queue.pop() {
            done.insert(pointer);

            match pointer {
                Pointer::Object(obj) => {
                    let new_invalidation = { invalid_objects.write().unwrap().insert(obj) };

                    if new_invalidation {
                        prop_assignments.prop_assignments(obj, &mut queue, &done);
                    }
                }
                Pointer::Union(union) => {
                    for constituent in unions.read().unwrap()[union].constituents() {
                        let pointer = Pointer::Object(constituent);
                        if !done.contains(&pointer) {
                            queue.push(pointer);
                        }
                    }
                }
                Pointer::Fn(_) => {}
            }
        }
    }
}

/// Returns true if `pointer` points to an invalid object.
fn invalidated(
    pointer: Pointer,
    invalid_objects: &RwLock<&mut GrowableBitSet<ObjectId>>,
    unions: &RwLock<&mut UnionStore>,
) -> bool {
    match pointer {
        Pointer::Object(obj) => invalid_objects.read().unwrap().contains(obj),
        Pointer::Union(union) => {
            union.invalid() || {
                // TODO: bad clone
                let constituents = {
                    unions.read().unwrap()[union]
                        .constituents()
                        .collect::<Vec<_>>()
                };
                constituents
                    .iter()
                    .any(|&obj| invalid_objects.read().unwrap().contains(obj))
            }
        }
        Pointer::Fn(_) => true,
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
pub(super) struct CallTemplate<'ast> {
    /// [`Steps`][Step] that represent the effects of each control flow graph
    /// node required to evaluate the call's effects.
    step_map: FxHashMap<Node<'ast>, Vec<Step>>,
}

impl<'ast> CallTemplate<'ast> {
    pub fn new(store: &mut Store<'ast>, func: FnId) -> CallTemplate<'ast> {
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
        let steps = match self.path_map.get(&node) {
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
                            RValue::NullOrVoid => self.null_or_void_p,
                            RValue::Var(rhs) => machine
                                .get_var(rhs, &self.lattice_elements)
                                .and_then(|a| a.rhs),
                            RValue::Object(o) => self.get_call_obj(*o),
                            RValue::Prop(prop) => get_property(
                                &machine.state.get(&self.lattice_elements).prop_assignments,
                                &self.unions,
                                machine.get_r_value(),
                                prop,
                                self.null_or_void,
                                &self.invalid_objects,
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
                                .get_var(&name, &self.lattice_elements)
                                .and_then(|a| a.rhs),
                            AssignTarget::Prop(obj, key) => get_property(
                                &machine.state.get(&self.lattice_elements).prop_assignments,
                                &self.unions,
                                *obj,
                                &key,
                                self.null_or_void,
                                &self.invalid_objects,
                            ),
                        };

                        let rhs = if !conditional {
                            // supersede
                            machine.get_r_value()
                        } else {
                            // union
                            create_union(
                                &self.unions,
                                existing,
                                machine.get_r_value(),
                                self.null_or_void,
                            )
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
                                        if invalidated(obj, &self.invalid_objects, &self.unions) {
                                            invalidate(
                                                &self.invalid_objects,
                                                &self.unions,
                                                rhs,
                                                &machine
                                                    .state
                                                    .get(&self.lattice_elements)
                                                    .prop_assignments,
                                                self.null_or_void_p,
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
                                            for constituent in
                                                self.unions.read().unwrap()[union].constituents()
                                            {
                                                machine.state.insert_prop_assignment(
                                                    (constituent, prop.clone()),
                                                    new,
                                                    &self.lattice_elements,
                                                );
                                            }
                                        }
                                        Pointer::Fn(_) => {}
                                    }
                                } else {
                                    // Unknown/invalid assignment target.
                                    invalidate(
                                        &self.invalid_objects,
                                        &self.unions,
                                        machine.get_r_value(),
                                        &machine.state.get(&self.lattice_elements).prop_assignments,
                                        self.null_or_void_p,
                                    );
                                }
                            }
                        };
                    } else {
                        // Unknown/invalid assignment target.
                        invalidate(
                            &self.invalid_objects,
                            &self.unions,
                            machine.get_r_value(),
                            &machine.state.get(&self.lattice_elements).prop_assignments,
                            self.null_or_void_p,
                        );
                    }
                }
                Step::InvalidateRValue => {
                    invalidate(
                        &self.invalid_objects,
                        &self.unions,
                        machine.get_r_value(),
                        &machine.state.get(&self.lattice_elements).prop_assignments,
                        self.null_or_void_p,
                    );
                }
                Step::InvalidateLValue => match &machine.l_value {
                    Some(AssignTarget::Var(id)) => {
                        if let Some(rhs) =
                            machine.get_var(id, &self.lattice_elements).map(|a| a.rhs)
                        {
                            invalidate(
                                &self.invalid_objects,
                                &self.unions,
                                rhs,
                                &machine.state.get(&self.lattice_elements).prop_assignments,
                                self.null_or_void_p,
                            );
                        }
                    }
                    Some(AssignTarget::Prop(obj, prop)) => {
                        let prop = get_property(
                            &machine.state.get(&self.lattice_elements).prop_assignments,
                            self.unions,
                            *obj,
                            prop,
                            self.null_or_void,
                            &self.invalid_objects,
                        );

                        invalidate(
                            &self.invalid_objects,
                            &self.unions,
                            prop,
                            &machine.state.get(&self.lattice_elements).prop_assignments,
                            self.null_or_void_p,
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
                            let invalid_objects = self.invalid_objects.read().unwrap();
                            if invalid_objects.contains(o) {
                                arg = None;
                            }
                        }
                        Some(Pointer::Union(union)) => {
                            if union.invalid() {
                                arg = None;
                            }
                        }
                        _ => {}
                    }
                    match machine.calls.last_mut().unwrap() {
                        MachineCall::Invalid => {
                            invalidate(
                                &self.invalid_objects,
                                &self.unions,
                                arg,
                                &machine.state.get(&self.lattice_elements).prop_assignments,
                                self.null_or_void_p,
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
                            let invalid_objects = self.invalid_objects.read().unwrap();
                            if heap.iter().all(|a| match a {
                                Some(Pointer::Union(union)) => union.invalid(),
                                Some(Pointer::Object(obj)) => invalid_objects.contains(*obj),
                                _ => true,
                            }) {
                                args.args = None;
                                args.none_count = args.len;
                            }
                        }
                    }
                    let args = args.finish();

                    let mut prop_assignments: HashableHashMap<(ObjectId, JsWord), Assignment> =
                        HashableHashMap::default();

                    if let CallArgs::Heap(args) = &args {
                        let mut done = FxHashSet::default();
                        let mut queue = Vec::new();

                        let unions = &self.unions.read().unwrap();

                        for arg in args.iter() {
                            match arg {
                                Some(Pointer::Object(o)) => {
                                    queue.push(*o);
                                }
                                Some(Pointer::Union(union)) => {
                                    queue.extend(unions[*union].constituents());
                                }
                                _ => {}
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

                            {
                                if self.invalid_objects.read().unwrap().contains(o) {
                                    continue;
                                }
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
                                            for constituent in unions[union].constituents() {
                                                if o == self.resolving_call_object {
                                                    return None;
                                                }
                                                if !done.contains(&constituent) {
                                                    queue.push(constituent);
                                                }
                                            }
                                        }
                                        _ => {}
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
                            Some(Pointer::Union(union)) => {
                                let constituents = {
                                    self.unions.read().unwrap()[union]
                                        .constituents()
                                        .collect::<Vec<_>>()
                                };
                                constituents.contains(&self.resolving_call_object)
                            }
                            Some(Pointer::Fn(_)) => false,
                            None => false,
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

                    let inner_call_id = { self.calls.write().unwrap().insert(inner_call) };

                    let existing = {
                        if let Some(resolved) =
                            self.resolved_calls.read().unwrap().get(&inner_call_id)
                        {
                            // TODO: bad clone
                            Some((
                                resolved.return_type,
                                Some(resolved.prop_assignments.clone()),
                            ))
                        } else {
                            None
                        }
                    };

                    let (return_type, incoming_assignments) = match existing {
                        Some((t, a)) => (t, a),
                        None => {
                            if inner_call_id != self.call {
                                self.incomplete_dependencies.insert(inner_call_id);
                            }

                            let builder = {
                                let return_types = self.return_types.read().unwrap();
                                let return_types = {
                                    match return_types.get(&inner_call_id) {
                                        Some(t) => t,
                                        None => {
                                            machine.set_r_value(Some(Pointer::Object(
                                                self.resolving_call_object,
                                            )));
                                            continue;
                                        }
                                    }
                                };
                                if return_types.is_empty() {
                                    machine.set_r_value(Some(Pointer::Object(
                                        self.resolving_call_object,
                                    )));
                                    continue;
                                }

                                let mut builder = UnionBuilder::new(self.null_or_void);

                                for &ty in return_types {
                                    builder.add_object(ty);
                                }

                                builder
                            };

                            let result = { self.unions.write().unwrap().build_union(builder) };

                            // TODO: bad clone
                            (
                                result,
                                self.return_states
                                    .read()
                                    .unwrap()
                                    .get(&inner_call_id)
                                    .cloned(),
                            )
                        }
                    };

                    if let Some(incoming_assignments) = incoming_assignments {
                        for ((obj, key), prop) in incoming_assignments.iter() {
                            let obj_invalid =
                                { self.invalid_objects.read().unwrap().contains(*obj) };
                            if obj_invalid {
                                continue;
                            }
                            let key = (*obj, key.clone());
                            let new = if let Some(existing) = machine
                                .state
                                .get(&self.lattice_elements)
                                .prop_assignments
                                .get(&key)
                            {
                                let union = create_union(
                                    self.unions,
                                    existing.rhs,
                                    prop.rhs,
                                    self.null_or_void,
                                );
                                Assignment { rhs: union }
                            } else {
                                prop.clone()
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
                            self.return_types
                                .write()
                                .unwrap()
                                .entry(self.call)
                                .or_default()
                                .insert($ty)
                        };
                    }

                    let r_value = machine.get_r_value();
                    let none_in_return_ty = {
                        self.return_types
                            .read()
                            .unwrap()
                            .get(&self.call)
                            .map(|ty| ty.contains(&None))
                            .unwrap_or_default()
                    };
                    match r_value {
                        Some(Pointer::Object(o)) => {
                            if o == self.resolving_call_object {
                                return None;
                            }
                            let invalid = { self.invalid_objects.read().unwrap().contains(o) };
                            if invalid {
                                if !none_in_return_ty {
                                    add_return_ty!(None);
                                    changed = true;
                                }
                            } else {
                                changed |= add_return_ty!(Some(o));
                            }
                        }
                        Some(Pointer::Union(union)) => {
                            if union.invalid() {
                                if !none_in_return_ty {
                                    add_return_ty!(None);
                                    changed = true;
                                }
                            } else {
                                let constituents = {
                                    let unions = self.unions.read().unwrap();
                                    let union = &unions[union];
                                    if union.contains(self.resolving_call_object)
                                        && union.len() == 1
                                    {
                                        return None;
                                    }
                                    // TODO: bad clone
                                    union.constituents().collect::<Vec<_>>()
                                };

                                for constituent in constituents {
                                    if constituent != self.resolving_call_object {
                                        changed |= add_return_ty!(Some(constituent));
                                    }
                                }
                            }
                        }
                        _ => {
                            if !none_in_return_ty {
                                add_return_ty!(None);
                                changed = true;
                            }
                        }
                    };

                    fn depends_on_unresolved_call(
                        unions: &RwLock<&mut UnionStore>,
                        pointer: Option<Pointer>,
                        resolving_call_object: ObjectId,
                    ) -> bool {
                        match pointer {
                            Some(Pointer::Object(o)) => o == resolving_call_object,
                            Some(Pointer::Union(union)) => {
                                unions.read().unwrap()[union].contains(resolving_call_object)
                            }
                            Some(Pointer::Fn(_)) => false,
                            None => false,
                        }
                    }

                    for ((obj, key), prop) in machine
                        .state
                        .get(&self.lattice_elements)
                        .prop_assignments
                        .iter()
                    {
                        if *obj == self.resolving_call_object {
                            changed = true;
                            continue;
                        }
                        if depends_on_unresolved_call(
                            &self.unions,
                            prop.rhs,
                            self.resolving_call_object,
                        ) {
                            changed = true;
                            continue;
                        }
                        let obj_invalid = { self.invalid_objects.read().unwrap().contains(*obj) };
                        if obj_invalid {
                            continue;
                        }
                        let key = (*obj, key.clone());
                        let existing = {
                            self.return_states
                                .read()
                                .unwrap()
                                .get(&self.call)
                                .and_then(|m| m.get(&key))
                                .copied()
                        };
                        match existing {
                            Some(existing) => {
                                let union = create_union(
                                    self.unions,
                                    existing.rhs,
                                    prop.rhs,
                                    self.null_or_void,
                                );
                                let old = self
                                    .return_states
                                    .write()
                                    .unwrap()
                                    .entry(self.call)
                                    .or_default()
                                    .insert(key, Assignment { rhs: union })
                                    .unwrap()
                                    .rhs;
                                changed |= old != union;
                            }
                            None => {
                                self.return_states
                                    .write()
                                    .unwrap()
                                    .entry(self.call)
                                    .or_default()
                                    .insert(key, *prop);
                                changed = true;
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

                    let result = {
                        let unions = &mut self.unions.write().unwrap();
                        let mut builder = UnionBuilder::new(self.null_or_void);
                        for ty in parts {
                            builder.add(ty, &unions);
                        }
                        unions.build_union(builder)
                    };

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
        // TODO:
        let existing = { self.objects_map.read().unwrap().get(&node_id).copied() };
        let root = if let Some(existing) = existing {
            existing
        } else {
            let object_id = {
                let mut lock = self.cur_object_id.lock().unwrap();
                let object_id = **lock;
                lock.increment_by(1);
                object_id
            };
            self.objects_map.write().unwrap().insert(node_id, object_id);
            object_id
        };
        Some(Pointer::Object(root))
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
        unions: &RwLock<&mut UnionStore>,
        null_or_void: ObjectId,
        invalid_objects: &RwLock<&mut GrowableBitSet<ObjectId>>,
    ) {
        // Merge property assignments.
        for ((obj, key), prop) in input.prop_assignments.iter() {
            let obj_invalid = { invalid_objects.read().unwrap().contains(*obj) };
            if obj_invalid {
                continue;
            }
            match self.result.prop_assignments.entry((*obj, key.clone())) {
                Entry::Occupied(entry) => {
                    let union = create_union(unions, entry.get().rhs, prop.rhs, null_or_void);
                    self.result
                        .prop_assignments
                        .insert((*obj, key.clone()), Assignment { rhs: union });
                }
                Entry::Vacant(entry) => {
                    entry.insert(prop.clone());
                }
            }
        }

        // Merge variable assignments.
        for (name, assignment) in input.var_assignments.iter() {
            match self.result.var_assignments.entry(name.clone()) {
                Entry::Occupied(mut entry) => {
                    let union = create_union(unions, entry.get().rhs, assignment.rhs, null_or_void);
                    entry.insert(Assignment { rhs: union });
                }
                Entry::Vacant(entry) => {
                    entry.insert(assignment.clone());
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
    node_annotations: FxHashMap<Node<'ast>, LinearFlowState>,

    call: CallId,

    path_map: &'a FxHashMap<Node<'ast>, Vec<Step>>,

    resolved_calls: &'a RwLock<&'ast mut FxHashMap<CallId, ResolvedCall>>,

    incomplete_dependencies: &'a mut FxHashSet<CallId>,

    return_types: &'a RwLock<FxHashMap<CallId, FxHashSet<Option<ObjectId>>>>,
    return_states: &'a RwLock<FxHashMap<CallId, HashableHashMap<(ObjectId, JsWord), Assignment>>>,
    cur_object_id: &'a Mutex<&'ast mut ObjectId>,
    objects_map: &'a RwLock<&'ast mut FxHashMap<NodeId, ObjectId>>,
    null_or_void: ObjectId,
    null_or_void_p: Option<Pointer>,
    resolving_call_object: ObjectId,
    unions: &'a RwLock<&'ast mut UnionStore>,
    invalid_objects: &'a RwLock<&'ast mut GrowableBitSet<ObjectId>>,
    calls: &'a RwLock<&'ast mut IndexSet<CallId, Call>>,
    changed: &'a mut bool,

    fn_assignments: &'a HashableHashMap<Id, Assignment>,
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
            self.node_annotations
                .insert(self.cfg.implicit_return, LinearFlowState::new(in_, out));
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
                .entry(curNode)
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
                if !matches!(curNode, Node::ReturnStmt(_)) {
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
        let state = &self.node_annotations[&node];
        let outBefore = state.out;
        if let Some(new_out) = self.flowThrough(node, state.in_) {
            self.get_flow_state_mut(&node).out = new_out;
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
            self.get_flow_state_mut(&node).in_ = self.createEntryLattice();
            return;
        }

        let graph = &self.cfg.graph;

        // The only edges coming out of a ReturnStmt go to the implicit return,
        // so we skip these edges (see above comment in `analyze_inner`).
        let mut inEdges = self
            .cfg
            .graph
            .edges_directed(self.cfg.map[&node], Incoming)
            .filter(|e| !matches!(graph[e.source()], Node::ReturnStmt(_)));

        if let Some(first) = inEdges.next() {
            if let Some(second) = inEdges.next() {
                let mut joiner = self.createFlowJoiner();
                let mut has_non_empty_input = false;
                if let Some(first) = self.getInputFromEdge(first) {
                    joiner.joinFlow(
                        &self.lattice_elements[first],
                        &self.unions,
                        self.null_or_void,
                        &self.invalid_objects,
                    );
                    has_non_empty_input = true;
                }
                if let Some(second) = self.getInputFromEdge(second) {
                    joiner.joinFlow(
                        &self.lattice_elements[second],
                        &self.unions,
                        self.null_or_void,
                        &self.invalid_objects,
                    );
                    has_non_empty_input = true;
                }
                while let Some(inEdge) = inEdges.next() {
                    if let Some(id) = self.getInputFromEdge(inEdge) {
                        joiner.joinFlow(
                            &self.lattice_elements[id],
                            &self.unions,
                            self.null_or_void,
                            &self.invalid_objects,
                        );
                        has_non_empty_input = true;
                    }
                }
                if has_non_empty_input {
                    self.get_flow_state_mut(&node).in_ = self.add_lattice_element(joiner.finish());
                }
            } else {
                // Only one relevant edge.
                if let Some(result) = self.getInputFromEdge(first) {
                    self.get_flow_state_mut(&node).in_ = result;
                }
            }
        } else {
            // No relevant edges.
        }
    }

    fn get_flow_state_mut(&mut self, node: &Node<'ast>) -> &mut LinearFlowState {
        // All nodes should have had their state initialized.
        self.node_annotations.get_mut(node).unwrap()
    }

    fn getInputFromEdge(&self, edge: EdgeReference<Branch>) -> Option<LatticeElementId> {
        let source = edge.source();
        let node = self.cfg.graph[source];
        self.node_annotations.get(&node).map(|state| state.out)
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
