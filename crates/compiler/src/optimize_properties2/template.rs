use std::collections::hash_map::Entry;
use std::collections::VecDeque;

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
use crate::DataFlowAnalysis::{LatticeElementId, LinearFlowState, UniqueQueue, MAX_STEPS_PER_NODE};
use crate::Id;

use super::function::*;
use super::{
    create_union, invalidate, invalidated, Assignment, Call, CallId, FnId, Lattice, ObjectId,
    Pointer, ResolvedCall, SimpleCFG, Store, Union, UnionId,
};

/// Recursively resolves the return type and side effects of the given call.
pub(super) fn resolve_call(call: CallId, store: &mut Store) {
    let null_or_void_p = Some(Pointer::Object(store.null_or_void));

    let mut call_queue = VecDeque::default();
    let mut call_queue_set = FxHashSet::default();
    call_queue.push_back(call);
    call_queue_set.insert(call);

    let mut dependents: FxHashMap<CallId, FxHashSet<CallId>> = FxHashMap::default();
    let mut return_types: FxHashMap<CallId, FxHashSet<Option<Pointer>>> = FxHashMap::default();
    let mut return_states: FxHashMap<CallId, FxHashMap<ObjectId, FxHashMap<JsWord, Assignment>>> =
        FxHashMap::default();

    let mut iterations = 0;

    while let Some(call) = call_queue.pop_front() {
        iterations += 1;
        if iterations > 1000 {
            todo!("Analysis appear to diverge");
        }
        call_queue_set.remove(&call);

        if store.resolved_calls.contains_key(&call) {
            continue;
        }

        return_types.entry(call).or_default();

        let func = store.calls[call].func;

        let mut lattice_elements = IndexVec::default();
        let initial_lattice = lattice_elements.push(Lattice::default());
        let mut entry_lattice = Lattice {
            prop_assignments: store.calls[call].prop_assignments.clone(),
            var_assignments: store.fn_assignments.clone(),
        };
        for (i, param_name) in store.functions[func].param_names.iter().enumerate() {
            let value = store.calls[call]
                .args
                .get(i)
                .copied()
                .unwrap_or(null_or_void_p);
            entry_lattice
                .var_assignments
                .insert(param_name.clone(), Assignment { rhs: value });
        }
        let entry_lattice = lattice_elements.push(entry_lattice);

        let mut analysis = DataFlowAnalysis {
            workQueue: UniqueQueue::new(&store.functions[func].node_priorities, true),
            cfg: &store.functions[func].cfg,
            lattice_elements,
            entry_lattice,
            initial_lattice,
            node_annotations: FxHashMap::default(),

            call,
            path_map: &store.call_templates[&func].step_map,
            resolved: &mut store.resolved_calls,
            call_queue: &mut call_queue,
            call_queue_set: &mut call_queue_set,
            dependents: &mut dependents,
            return_types: &mut return_types,
            return_states: &mut return_states,
            call_objects: &mut store.call_objects,
            cur_object_id: &mut store.cur_object_id,
            objects_map: &mut store.objects_map,
            object_links: &mut store.object_links,
            null_or_void: store.null_or_void,
            null_or_void_p,
            unions: &mut store.unions,
            invalid_objects: &mut store.invalid_objects,
            calls: &mut store.calls,
        };
        analysis.analyze();
    }

    store.resolved_calls.reserve(return_types.len());

    for (call, ty) in return_types {
        let func = store.calls[call].func;
        let args = &store.calls[call].args;

        // Merge arguments.
        for (i, arg) in args.iter().enumerate() {
            match store.functions[func].args.get_mut(i) {
                Some(existing) => {
                    let union = create_union(&mut store.unions, *existing, *arg, null_or_void_p);
                    *existing = union;
                }
                None => {
                    store.functions[func].args.push(*arg);
                }
            }
        }

        // Merge property assignments.
        for (obj, props) in &store.calls[call].prop_assignments {
            for (key, prop) in props {
                match store.functions[func]
                    .arg_values
                    .prop_assignments
                    .entry(*obj)
                    .or_default()
                    .entry(key.clone())
                {
                    Entry::Occupied(mut entry) => {
                        let union = create_union(
                            &mut store.unions,
                            entry.get().rhs,
                            prop.rhs,
                            null_or_void_p,
                        );
                        entry.insert(Assignment { rhs: union });
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(prop.clone());
                    }
                }
            }
        }

        store.resolved_calls.insert(
            call,
            ResolvedCall {
                return_type: ty,
                prop_assignments: return_states.remove(&call).unwrap_or_default(),
            },
        );
    }
}

fn get_property(
    prop_assignments: &FxHashMap<ObjectId, FxHashMap<JsWord, Assignment>>,
    unions: &mut IndexVec<UnionId, Union>,
    object: Option<Pointer>,
    key: &JsWord,
    null_or_void: ObjectId,
) -> Option<Pointer> {
    match object {
        Some(Pointer::Object(obj)) => prop_assignments
            .get(&obj)
            .and_then(|props| props.get(key))
            .map(|a| a.rhs)
            .unwrap_or(Some(Pointer::Object(null_or_void))),
        Some(Pointer::Union(union)) => {
            let mut constituents = FxHashSet::default();

            for constituent in &unions[union].constituents {
                let constituent = prop_assignments
                    .get(&constituent)
                    .and_then(|props| props.get(key))
                    .and_then(|a| a.rhs)
                    .or(Some(Pointer::Object(null_or_void)));

                match constituent {
                    Some(Pointer::Object(obj)) => {
                        constituents.insert(obj);
                    }
                    Some(Pointer::Union(union)) => {
                        constituents.extend(unions[union].constituents.iter().copied());
                    }
                    _ => todo!(),
                };
            }

            if constituents.len() == 0 {
                return None;
            }

            let result = if constituents.len() == 1 {
                Pointer::Object(constituents.into_iter().next().unwrap())
            } else {
                constituents.remove(&null_or_void);
                let union = Union { constituents };

                if let Some((existing, _)) = unions.iter_enumerated().find(|(_, u)| *u == &union) {
                    Pointer::Union(existing)
                } else {
                    Pointer::Union(unions.push(union))
                }
            };

            Some(result)
        }
        None => None,
        _ => todo!(),
    }
}

#[derive(Debug)]
/// A slot where a value can be stored.
enum AssignTarget {
    Var(Id),
    Prop(Option<Pointer>, JsWord),
}

#[derive(Debug)]
/// State machine that executes a sequence of [`Steps`][Step] that represent the
/// effects of a node in the control flow graph.
struct Machine<'a> {
    state: Lattice,
    steps: &'a [Step],
    /// Stack of calls (fn and args) that are currently being built.
    calls: Vec<(FnId, Vec<Option<Pointer>>)>,
    /// Stack of unions (constituents) that are currently being built.
    unions: Vec<FxHashSet<Option<Pointer>>>,
    /// Stack of expression values. Using a stack simplifies the [`Step`]
    /// language, allowing us to handle nested expressions by pushing/popping
    /// values on the stack.
    r_value: Vec<Option<Pointer>>,
    /// Register containing the current left-hand-side value.
    l_value: Option<AssignTarget>,
}

impl Machine<'_> {
    fn get_r_value(&self) -> Option<Pointer> {
        *self.r_value.last().unwrap()
    }
    fn set_r_value(&mut self, value: Option<Pointer>) {
        *self.r_value.last_mut().unwrap() = value;
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
        let mut machine = Machine {
            state: self.lattice_elements[input].clone(),
            steps: &self.path_map[&node],
            calls: Vec::new(),
            r_value: vec![None],
            l_value: None,
            unions: Vec::new(),
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
                            RValue::Var(rhs) => {
                                machine.state.var_assignments.get(rhs).and_then(|a| a.rhs)
                            }
                            RValue::Object(o) => self.get_call_obj(*o),
                            RValue::Prop(prop) => get_property(
                                &machine.state.prop_assignments,
                                self.unions,
                                machine.get_r_value(),
                                prop,
                                self.null_or_void,
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
                    Some(LValue::Prop(prop)) => {
                        machine.l_value = match &machine.l_value {
                            Some(AssignTarget::Var(id)) => Some(AssignTarget::Prop(
                                machine.state.var_assignments.get(&id).and_then(|a| a.rhs),
                                prop.clone(),
                            )),
                            Some(AssignTarget::Prop(obj, old_prop)) => {
                                let old_value = get_property(
                                    &machine.state.prop_assignments,
                                    self.unions,
                                    *obj,
                                    old_prop,
                                    self.null_or_void,
                                );
                                Some(AssignTarget::Prop(old_value, prop.clone()))
                            }
                            None => None,
                        };
                    }
                    None => {
                        machine.l_value = None;
                    }
                },
                Step::Assign(conditional) => {
                    if let Some(slot) = &machine.l_value {
                        let existing = match &slot {
                            AssignTarget::Var(name) => {
                                machine.state.var_assignments.get(&name).and_then(|a| a.rhs)
                            }
                            AssignTarget::Prop(obj, key) => get_property(
                                &machine.state.prop_assignments,
                                self.unions,
                                *obj,
                                &key,
                                self.null_or_void,
                            ),
                        };

                        let rhs = if !conditional {
                            // supersede
                            machine.get_r_value()
                        } else {
                            // union
                            create_union(
                                self.unions,
                                existing,
                                machine.get_r_value(),
                                self.null_or_void_p,
                            )
                        };
                        let new = Assignment { rhs };
                        match slot {
                            AssignTarget::Var(id) => {
                                machine.state.var_assignments.insert(id.clone(), new);
                            }
                            AssignTarget::Prop(obj, prop) => {
                                if let Some(obj) = *obj {
                                    if invalidated(obj, self.invalid_objects, self.unions) {
                                        invalidate(
                                            self.invalid_objects,
                                            self.unions,
                                            rhs,
                                            &machine.state,
                                            self.null_or_void,
                                        );
                                    }
                                    match obj {
                                        Pointer::Object(obj) => {
                                            machine
                                                .state
                                                .prop_assignments
                                                .entry(obj)
                                                .or_default()
                                                .insert(prop.clone(), new);
                                        }
                                        Pointer::Union(union) => {
                                            for &constituent in &self.unions[union].constituents {
                                                machine
                                                    .state
                                                    .prop_assignments
                                                    .entry(constituent)
                                                    .or_default()
                                                    .insert(prop.clone(), new);
                                            }
                                        }
                                        _ => todo!(),
                                    }
                                } else {
                                    // Unknown/invalid assignment target.
                                    invalidate(
                                        self.invalid_objects,
                                        self.unions,
                                        machine.get_r_value(),
                                        &machine.state,
                                        self.null_or_void,
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
                            &machine.state,
                            self.null_or_void,
                        );
                    }
                }
                Step::InvalidateRValue => {
                    invalidate(
                        self.invalid_objects,
                        self.unions,
                        machine.get_r_value(),
                        &machine.state,
                        self.null_or_void,
                    );
                }
                Step::InvalidateLValue => match &machine.l_value {
                    Some(AssignTarget::Var(id)) => {
                        if let Some(rhs) = machine.state.var_assignments.get(id).map(|a| a.rhs) {
                            invalidate(
                                self.invalid_objects,
                                self.unions,
                                rhs,
                                &machine.state,
                                self.null_or_void,
                            );
                        }
                    }
                    Some(AssignTarget::Prop(obj, prop)) => {
                        let prop = get_property(
                            &machine.state.prop_assignments,
                            self.unions,
                            *obj,
                            prop,
                            self.null_or_void,
                        );

                        invalidate(
                            self.invalid_objects,
                            self.unions,
                            prop,
                            &machine.state,
                            self.null_or_void,
                        );
                    }
                    None => {}
                },
                Step::StartCall => {
                    let func = match machine.get_r_value() {
                        Some(Pointer::Fn(f)) => f,
                        v => todo!("{:#?}", v),
                    };
                    machine.calls.push((func, Vec::new()));
                }
                Step::StoreArg => {
                    let v = machine.get_r_value();
                    machine.calls.last_mut().unwrap().1.push(v);
                }
                Step::Call => {
                    let (func, args) = machine.calls.pop().unwrap();
                    let inner_call = Call {
                        func,
                        args,
                        prop_assignments: machine.state.prop_assignments.clone(),
                    };

                    let inner_call_id = if let Some((existing, _)) = self
                        .calls
                        .iter_enumerated()
                        .find(|(_, c)| *c == &inner_call)
                    {
                        existing
                    } else {
                        self.calls.push(inner_call)
                    };

                    let new_dependent = self
                        .dependents
                        .entry(self.call)
                        .or_default()
                        .insert(inner_call_id);

                    if new_dependent && self.call_queue_set.insert(inner_call_id) {
                        self.call_queue.push_back(inner_call_id);
                    }

                    let (return_types, incoming_assignments) =
                        match self.resolved.get(&inner_call_id) {
                            Some(resolved) => (&resolved.return_type, &resolved.prop_assignments),
                            None => (
                                &*self.return_types.entry(inner_call_id).or_default(),
                                &*self.return_states.entry(inner_call_id).or_default(),
                            ),
                        };

                    if return_types.is_empty() {
                        if self.call_queue_set.insert(self.call) {
                            self.call_queue.push_back(self.call);
                        }
                        return None;
                    }

                    for (obj, props) in incoming_assignments {
                        for (key, prop) in props {
                            match machine
                                .state
                                .prop_assignments
                                .entry(*obj)
                                .or_default()
                                .entry(key.clone())
                            {
                                Entry::Occupied(mut entry) => {
                                    let union = create_union(
                                        self.unions,
                                        entry.get().rhs,
                                        prop.rhs,
                                        self.null_or_void_p,
                                    );
                                    entry.insert(Assignment { rhs: union });
                                }
                                Entry::Vacant(entry) => {
                                    entry.insert(prop.clone());
                                }
                            }
                        }
                    }

                    let mut constituents = FxHashSet::default();

                    let mut invalid = false;

                    for &ty in return_types.iter() {
                        match ty {
                            Some(Pointer::Object(obj)) => {
                                constituents.insert(obj);
                            }
                            Some(Pointer::Union(union)) => {
                                constituents
                                    .extend(self.unions[union].constituents.iter().copied());
                            }
                            None => {
                                invalid = true;
                            }
                            t => todo!("{:#?}", t),
                        };
                    }

                    let result = if constituents.len() == 0 {
                        None
                    } else if constituents.len() == 1 {
                        Some(Pointer::Object(constituents.into_iter().next().unwrap()))
                    } else {
                        constituents.remove(&self.null_or_void);
                        let union = Union { constituents };

                        if let Some((existing, _)) =
                            self.unions.iter_enumerated().find(|(_, u)| *u == &union)
                        {
                            Some(Pointer::Union(existing))
                        } else {
                            Some(Pointer::Union(self.unions.push(union)))
                        }
                    };
                    if invalid && result.is_some() {
                        todo!();
                    }
                    machine.set_r_value(result);
                }
                Step::Return => {
                    let mut changed = self
                        .return_types
                        .entry(self.call)
                        .or_default()
                        .insert(machine.get_r_value());

                    // Join this return state of the function with its existing return state.
                    match self.return_states.entry(self.call) {
                        Entry::Occupied(mut entry) => {
                            for (&obj, props) in &machine.state.prop_assignments {
                                for (key, prop) in props {
                                    match entry.get_mut().entry(obj).or_default().entry(key.clone())
                                    {
                                        Entry::Occupied(mut entry) => {
                                            let union = create_union(
                                                self.unions,
                                                entry.get().rhs,
                                                prop.rhs,
                                                self.null_or_void_p,
                                            );
                                            let old = entry.insert(Assignment { rhs: union }).rhs;
                                            changed |= old != union;
                                        }
                                        Entry::Vacant(entry) => {
                                            entry.insert(*prop);
                                            changed = true;
                                        }
                                    }
                                }
                            }
                        }
                        Entry::Vacant(entry) => {
                            if !machine.state.prop_assignments.is_empty() {
                                entry.insert(machine.state.prop_assignments.clone());
                                changed = true;
                            }
                        }
                    }

                    if changed {
                        if self.call_queue_set.insert(self.call) {
                            self.call_queue.push_back(self.call);
                            if let Some(dependents) = self.dependents.get(&self.call) {
                                for dependent in dependents {
                                    if self.call_queue_set.insert(*dependent) {
                                        self.call_queue.push_back(*dependent);
                                    }
                                }
                            }
                        }
                    }

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
                    let mut constituents = FxHashSet::default();

                    let mut invalid = false;

                    for ty in machine.unions.pop().unwrap() {
                        match ty {
                            Some(Pointer::Object(obj)) => {
                                constituents.insert(obj);
                            }
                            Some(Pointer::Union(union)) => {
                                constituents
                                    .extend(self.unions[union].constituents.iter().copied());
                            }
                            None => {
                                invalid = true;
                            }
                            t => todo!("{:#?}", t),
                        };
                    }

                    let result = if constituents.len() == 0 {
                        None
                    } else if constituents.len() == 1 {
                        Some(Pointer::Object(constituents.into_iter().next().unwrap()))
                    } else {
                        constituents.remove(&self.null_or_void);
                        let union = Union { constituents };

                        if let Some((existing, _)) =
                            self.unions.iter_enumerated().find(|(_, u)| *u == &union)
                        {
                            Some(Pointer::Union(existing))
                        } else {
                            Some(Pointer::Union(self.unions.push(union)))
                        }
                    };
                    if invalid && result.is_some() {
                        todo!();
                    }
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

        Some(if machine.state != self.lattice_elements[input] {
            self.add_lattice_element(machine.state)
        } else {
            // No changes compared to input.
            input
        })
    }

    /// Derives a unique [`ObjectId`] for the given object literal in the current
    /// call. The derived type will be linked to a root type to record the
    /// relationship. This approach makes the analysis more accurate by preventing
    /// calls from interfering with each other, unless required for correctness.
    /// See test `test_calls_do_not_interfere`
    fn get_call_obj(&mut self, node_id: NodeId) -> Option<Pointer> {
        // Explicitly split borrow of self to please the borrow checker.
        let objects_map = &mut self.objects_map;
        let cur_object_id = &mut self.cur_object_id;
        let root = *objects_map.entry(node_id).or_insert_with(|| {
            let object_id = **cur_object_id;
            cur_object_id.increment_by(1);
            object_id
        });
        let local_id = match self.call_objects.entry((self.call, node_id)) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let object_id = *self.cur_object_id;
                self.cur_object_id.increment_by(1);
                entry.insert(object_id);
                self.object_links.entry(root).or_default().push(object_id);
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
        unions: &mut IndexVec<UnionId, Union>,
        null_or_void_p: Option<Pointer>,
    ) {
        // Merge property assignments.
        for (obj, props) in &input.prop_assignments {
            for (key, prop) in props {
                match self
                    .result
                    .prop_assignments
                    .entry(*obj)
                    .or_default()
                    .entry(key.clone())
                {
                    Entry::Occupied(mut entry) => {
                        let union = create_union(unions, entry.get().rhs, prop.rhs, null_or_void_p);
                        entry.insert(Assignment { rhs: union });
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(prop.clone());
                    }
                }
            }
        }

        // Merge variable assignments.
        for (name, assignment) in &input.var_assignments {
            match self.result.var_assignments.entry(name.clone()) {
                Entry::Occupied(mut entry) => {
                    let union =
                        create_union(unions, entry.get().rhs, assignment.rhs, null_or_void_p);
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

    lattice_elements: IndexVec<LatticeElementId, Lattice>,
    cfg: &'a SimpleCFG<'ast>,
    entry_lattice: LatticeElementId,
    initial_lattice: LatticeElementId,
    node_annotations: FxHashMap<Node<'ast>, LinearFlowState>,

    call: CallId,

    path_map: &'a FxHashMap<Node<'ast>, Vec<Step>>,

    resolved: &'a mut FxHashMap<CallId, ResolvedCall>,

    call_queue: &'a mut VecDeque<CallId>,
    call_queue_set: &'a mut FxHashSet<CallId>,
    dependents: &'a mut FxHashMap<CallId, FxHashSet<CallId>>,
    return_types: &'a mut FxHashMap<CallId, FxHashSet<Option<Pointer>>>,
    return_states: &'a mut FxHashMap<CallId, FxHashMap<ObjectId, FxHashMap<JsWord, Assignment>>>,
    call_objects: &'a mut FxHashMap<(CallId, NodeId), ObjectId>,
    cur_object_id: &'a mut ObjectId,
    objects_map: &'a mut FxHashMap<NodeId, ObjectId>,
    object_links: &'a mut FxHashMap<ObjectId, Vec<ObjectId>>,

    null_or_void: ObjectId,
    null_or_void_p: Option<Pointer>,
    unions: &'a mut IndexVec<UnionId, Union>,
    invalid_objects: &'a mut GrowableBitSet<ObjectId>,
    calls: &'a mut IndexVec<CallId, Call>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
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
            if self.lattice_elements[outBefore] != self.lattice_elements[new_out] {
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
                        self.unions,
                        self.null_or_void_p,
                    );
                    has_non_empty_input = true;
                }
                if let Some(second) = self.getInputFromEdge(second) {
                    joiner.joinFlow(
                        &self.lattice_elements[second],
                        self.unions,
                        self.null_or_void_p,
                    );
                    has_non_empty_input = true;
                }
                while let Some(inEdge) = inEdges.next() {
                    if let Some(id) = self.getInputFromEdge(inEdge) {
                        joiner.joinFlow(
                            &self.lattice_elements[id],
                            self.unions,
                            self.null_or_void_p,
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
        // Memoize lattices
        if let Some((existing, _)) = self
            .lattice_elements
            .iter_enumerated()
            .find(|(_, l)| *l == &element)
        {
            existing
        } else {
            self.lattice_elements.push(element)
        }
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
