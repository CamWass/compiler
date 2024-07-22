#![deny(unused_imports)]

use std::collections::hash_map::Entry;
use std::collections::BTreeSet;
use std::hash::{BuildHasherDefault, Hash, Hasher};

use ast::NodeId;
use atoms::JsWord;
use global_common::SyntaxContext;
use index::bit_set::{BitSet, GrowableBitSet};
use index::vec::IndexVec;
use petgraph::graph::{EdgeReference, NodeIndex};
use petgraph::graphmap::GraphMap;
use petgraph::visit::{Dfs, EdgeRef};
use petgraph::Directed;
use petgraph::Direction::{Incoming, Outgoing};
use rustc_hash::{FxHashMap, FxHashSet, FxHasher};

use crate::control_flow::node::{Node, NodeKind};
use crate::control_flow::ControlFlowGraph::Branch;
use crate::find_vars::VarId;
use crate::optimize_properties2::{
    apply_call_side_effects, build_call, create_union, depends_on_unresolved_call,
    fully_invalidated, function::*, invalidate, invalidated, Assignment, Call, CallArgs, CallId,
    FnId, Func, Id, Lattice, NameId, Pointer, PropertyAssignments, ResolvedCall, SimpleCFG,
    StaticFunctionData, Store,
};
use crate::utils::unwrap_as;
use crate::DataFlowAnalysis::{
    LatticeElementId, LinearFlowState, PrioritizedNode, UniqueQueue, MAX_STEPS_PER_NODE,
};

use super::graph::{process, Visitor};
use super::hashable_map::HashableHashMap;
use super::simple_set::IndexSet;
use super::types::{ObjectId, ObjectStore, UnionBuilder, UnionStore};
use super::utils::{ReusableState, ReusableStateStack};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
enum ReturnTypeConstituent {
    Object(ObjectId),
    Invalid,
    NullOrVoid,
}

#[derive(Debug, Default)]
struct ReturnState {
    return_ty: FxHashSet<ObjectId>,
    returns_null_or_void: bool,
    returns_invalid: bool,

    // TODO: use vec?
    prop_assignments: PropertyAssignments,
    captured_vars: HashableHashMap<VarId, Assignment>,
}

/// Re-usable state for a [`call resolver`][`Resolver`].
#[derive(Debug, Default)]
pub struct ResolverState {
    return_states: FxHashMap<CallId, ReturnState>,

    data_flow_state: DataFlowAnalysisState,
}

impl ReusableState for ResolverState {
    fn reset(&mut self) {
        let ResolverState {
            return_states,
            data_flow_state,
        } = self;
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

    fn_assignments: &'a HashableHashMap<VarId, FnId>,

    root_call: CallId,

    state: &'a mut ResolverState,

    accessed_props: &'a FxHashMap<FnId, FxHashSet<NameId>>,

    done_objects: &'a mut GrowableBitSet<ObjectId>,
    done_functions: &'a mut BitSet<FnId>,
    done_vars: &'a mut BitSet<VarId>,

    always_invalid_vars: &'a FxHashSet<VarId>,
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
        let entry_lattice = call.state.clone();
        let entry_lattice = self
            .state
            .data_flow_state
            .lattice_elements
            .insert(entry_lattice);

        let (steps, step_map) = match &self.call_templates[func] {
            CallTemplate::Steps(steps) => (&steps.steps, &steps.map),
            _ => unreachable!(),
        };

        let mut analysis = DataFlowAnalysis {
            work_queue: UniqueQueue::reuse_inner(
                std::mem::take(&mut self.state.data_flow_state.work_queue_inner),
                &self.static_fn_data[func].node_priorities,
                true,
            ),
            cfg: &self.static_fn_data[func].cfg,
            entry_lattice,
            initial_lattice,

            call: node,
            step_map,
            steps,
            call_templates: &self.call_templates,
            resolved_calls: self.resolved_calls,
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

            static_fn_data: &self.static_fn_data,

            done_objects: self.done_objects,
            done_functions: self.done_functions,
            done_vars: self.done_vars,

            accessed_props: self.accessed_props,
            functions: self.functions,

            always_invalid_vars: self.always_invalid_vars,
        };
        analysis.analyze();

        changed
    }

    fn finish_node(&mut self, node: CallId) {
        let func = self.calls[node].func;
        let call = &self.calls[node];

        // Merge property assignments.
        for ((obj, key), prop) in call.state.prop_assignments.iter() {
            if self.invalid_objects.contains(*obj) {
                continue;
            }
            match self.functions[func]
                .entry_state
                .prop_assignments
                .entry((*obj, *key))
            {
                std::collections::btree_map::Entry::Occupied(mut entry) => {
                    let union =
                        create_union(self.unions, entry.get().rhs, prop.rhs, self.invalid_objects);
                    entry.insert(Assignment { rhs: union });
                }
                std::collections::btree_map::Entry::Vacant(entry) => {
                    entry.insert(*prop);
                }
            }
        }

        // Merge variable assignments.
        for (name, assignment) in call.state.var_assignments.iter() {
            match self.functions[func]
                .entry_state
                .var_assignments
                .entry(*name)
            {
                Entry::Occupied(mut entry) => {
                    let union = create_union(
                        self.unions,
                        entry.get().rhs,
                        assignment.rhs,
                        self.invalid_objects,
                    );
                    entry.insert(Assignment { rhs: union });
                }
                Entry::Vacant(entry) => {
                    entry.insert(*assignment);
                }
            }
        }

        let return_state = self.state.return_states.remove(&node).unwrap();

        let mut builder = UnionBuilder::default();

        if return_state.returns_invalid {
            builder.add_object(None, self.invalid_objects);
        }
        if return_state.returns_null_or_void {
            builder.add_null_or_void();
        }

        // TODO: can optimise - we only add ObjectIds and they are added in stored order.
        for &obj in return_state.return_ty.iter() {
            builder.add_object(Some(obj), self.invalid_objects)
        }

        let return_type = self.unions.build_union(builder);

        let resolved = ResolvedCall {
            return_type,
            prop_assignments: return_state.prop_assignments,
            captured_vars: return_state.captured_vars,
        };

        if cfg!(debug_assertions) {
            debug_assert!(!resolved
                .prop_assignments
                .keys()
                .any(|(o, _)| *o == ObjectStore::RESOLVING_CALL));
            debug_assert!(!resolved
                .prop_assignments
                .values()
                .any(|p| depends_on_unresolved_call(p.rhs, self.unions)));
            debug_assert!(!resolved
                .captured_vars
                .values()
                .any(|p| depends_on_unresolved_call(p.rhs, self.unions)));
        }

        self.resolved_calls.insert(node, resolved);
    }
}

/// Recursively resolves the return type and side effects of the given call.
pub(super) fn resolve_call(
    call: CallId,
    store: &mut Store,
    done_objects: &mut GrowableBitSet<ObjectId>,
    done_functions: &mut BitSet<FnId>,
    done_vars: &mut BitSet<VarId>,
) {
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

        done_objects,
        done_functions,
        done_vars,

        accessed_props: &store.accessed_props,

        always_invalid_vars: &store.always_invalid_vars,
    };

    process(call, &mut visitor);

    debug_assert!(store.resolved_calls.contains_key(&call));
}

fn get_property(
    prop_assignments: &PropertyAssignments,
    unions: &mut UnionStore,
    object: Option<Pointer>,
    key: NameId,
    invalid_objects: &mut GrowableBitSet<ObjectId>,
) -> Option<Pointer> {
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
        Some(Pointer::Fn(_) | Pointer::NullOrVoid) | None => true,
    };
    if invalid {
        if cfg!(debug_assertions) {
            match object {
                Some(Pointer::Object(obj)) => {
                    let prop = prop_assignments
                        .get(&(obj, key))
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
                            .get(&(constituent, key))
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
                Some(Pointer::Fn(_) | Pointer::NullOrVoid) | None => {}
            }
        }

        return None;
    }

    match object {
        Some(Pointer::Object(obj)) => prop_assignments
            .get(&(obj, key))
            .map(|a| a.rhs)
            .unwrap_or(Some(Pointer::NullOrVoid)),
        Some(Pointer::Union(union)) => {
            let mut builder = UnionBuilder::default();

            for constituent in unions[union].constituents() {
                let constituent = prop_assignments
                    .get(&(constituent, key))
                    .and_then(|a| a.rhs)
                    .or(Some(Pointer::NullOrVoid));

                builder.add(constituent, unions, invalid_objects);
            }
            unions.build_union(builder)
        }
        Some(Pointer::Fn(_)) | None => None,
        Some(Pointer::NullOrVoid) => unreachable!(),
    }
}

#[derive(Debug, Clone, Copy)]
/// A slot where a value can be stored.
enum AssignTarget {
    Var(VarId),
    Prop(Pointer, NameId),
}

#[derive(Debug)]
/// Info about a call made by a [`Machine`].
enum MachineCall {
    /// Signals that the callee was not a function.
    Invalid,
    /// (fn, args)
    Valid(FnId, CallArgs),
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
        name: VarId,
        value: Assignment,
        lattice_elements: &IndexSet<LatticeElementId, Lattice>,
        invalid_objects: &mut GrowableBitSet<ObjectId>,
        unions: &UnionStore,
        fn_assignments: &HashableHashMap<VarId, FnId>,
        always_invalid_vars: &FxHashSet<VarId>,
    ) {
        if always_invalid_vars.contains(&name) {
            invalidate(
                invalid_objects,
                unions,
                value.rhs,
                &self.get(lattice_elements).prop_assignments,
            );
            return;
        }

        debug_assert!(
            !matches!(
                self.get(lattice_elements)
                    .var_assignments
                    .get(&name)
                    .and_then(|a| a.rhs),
                Some(Pointer::Fn(_))
            ) || self
                .get(lattice_elements)
                .var_assignments
                .get(&name)
                .copied()
                != fn_assignments.get(&name).map(|f| Assignment {
                    rhs: Some(Pointer::Fn(*f)),
                })
        );
        let new = if fully_invalidated(value.rhs, invalid_objects, unions) {
            Assignment { rhs: None }
        } else {
            value
        };

        if let Some(existing) = self.get(lattice_elements).var_assignments.get(&name) {
            if *existing == new {
                return;
            }
            // if new.rhs.is_none() && !fn_assignments.contains_key(&name) {
            //     self.get_mut(lattice_elements).var_assignments.remove(&name);
            //     return;
            // }

            // self.get_mut(lattice_elements)
            //     .var_assignments
            //     .insert(name, new);
        }
        // else if new.rhs.is_some() || fn_assignments.contains_key(&name) {
        //     self.get_mut(lattice_elements)
        //         .var_assignments
        //         .insert(name, new);
        // }

        self.get_mut(lattice_elements)
            .var_assignments
            .insert(name, value);
    }

    fn insert_prop_assignment(
        &mut self,
        prop: (ObjectId, NameId),
        value: Assignment,
        lattice_elements: &IndexSet<LatticeElementId, Lattice>,
        invalid_objects: &GrowableBitSet<ObjectId>,
        unions: &UnionStore,
    ) {
        let new = if fully_invalidated(value.rhs, invalid_objects, unions) {
            Assignment { rhs: None }
        } else {
            value
        };

        if let Some(existing) = self.get(lattice_elements).prop_assignments.get(&prop) {
            if *existing == new {
                return;
            }
        } else if let Some(Pointer::NullOrVoid) = new.rhs {
            // Assigning null/void is the same as having nothing assigned.
            return;
        }

        self.get_mut(lattice_elements)
            .prop_assignments
            .insert(prop, new);
    }
}

#[derive(Debug)]
struct MachineState {
    /// Stack of calls (fn and args) that are currently being built.
    calls: Vec<MachineCall>,
    /// Stack of unions (constituents) that are currently being built.
    unions: Vec<UnionBuilder>,
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
        unions.clear();
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
    fn_assignments: &'a HashableHashMap<VarId, FnId>,
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
        id: &VarId,
        lattice_elements: &IndexSet<LatticeElementId, Lattice>,
        always_invalid_vars: &FxHashSet<VarId>,
    ) -> Option<Assignment> {
        if always_invalid_vars.contains(&id) {
            return Some(Assignment { rhs: None });
        }
        self.lattice
            .get(lattice_elements)
            .var_assignments
            .get(id)
            .copied()
            .or_else(|| {
                self.fn_assignments.get(&id).map(|f| Assignment {
                    rhs: Some(Pointer::Fn(*f)),
                })
            })
    }
}

#[derive(Debug)]
pub(super) struct CallSteps {
    /// [`Steps`][Step] that represent the effects of each control flow graph
    /// node required to evaluate the call's effects.
    pub steps: Vec<Step>,
    /// Map from CFG [`NodeIndex`] to `(start, end)` indices in the step list.
    pub map: Vec<(u32, u32)>,
}

#[derive(Debug)]
/// Info required to compute the effects of a call.
pub(super) enum CallTemplate {
    Steps(CallSteps),
    /// Does not modify/access external state.
    Simple(Option<Pointer>),
    SimpleWithProps(Option<Pointer>, PropertyAssignments),
}

impl CallTemplate {
    pub fn new(
        static_fn_data: &IndexVec<FnId, StaticFunctionData>,
        unresolved_ctxt: SyntaxContext,
        func: FnId,
        function_map: &FxHashMap<NodeId, FnId>,
        names: &mut IndexSet<NameId, JsWord>,
        vars: &mut IndexSet<VarId, Id>,
        builder: &mut StepBuilder,
        unions: &mut UnionStore,
        objects_map: &mut FxHashMap<NodeId, ObjectId>,
        objects: &mut ObjectStore,
        invalid_objects: &mut GrowableBitSet<ObjectId>,
        fn_assignments: &HashableHashMap<VarId, FnId>,
        fn_graph: &mut GraphMap<FnId, (), Directed, BuildHasherDefault<FxHasher>>,
        always_invalid_vars: &FxHashSet<VarId>,
    ) -> CallTemplate {
        let (steps, map, references_env_vars) = create_step_map(
            static_fn_data,
            unresolved_ctxt,
            func,
            function_map,
            names,
            vars,
            builder,
        );

        // Returns true if the function has side effects that cannot be
        // determined/applied ahead of time. For example, calling another function
        // can have unknown side effects, but invalidating a static pointer can
        // safely be done once, ahead of time.
        let contains_dynamic_side_effects = || {
            steps.iter().any(|s| match s {
                Step::StoreRValue(_) => false,
                Step::StoreLValue(_) => false,
                // Invalidating can be a side effect, but we check above that the
                // entity being invalidated is static.
                Step::InvalidateRValue | Step::InvalidateLValue | Step::Assign(_) => false,
                // StartCall has no effect, but is always paired with a Call,
                // which has side effects, so we bail early.
                // Step::StartCall | Step::Call(_) => true,
                _ => false,
            })
        };

        if references_env_vars || contains_dynamic_side_effects() {
            CallTemplate::Steps(CallSteps { steps, map })
        } else {
            let result = compute_call(
                &steps,
                &map,
                static_fn_data[func].cfg.implicit_return_index.index(),
                unions,
                objects_map,
                objects,
                invalid_objects,
                fn_assignments,
                static_fn_data,
                fn_graph,
                func,
                always_invalid_vars,
            );
            let (return_ty, prop_assignments) = match result {
                Ok(t) => t,
                Err(_) => return CallTemplate::Steps(CallSteps { steps, map }),
            };
            if fn_graph.neighbors_directed(func, Outgoing).next().is_none() {
                if prop_assignments.is_empty() {
                    CallTemplate::Simple(return_ty)
                } else {
                    CallTemplate::SimpleWithProps(return_ty, prop_assignments)
                }
            } else {
                CallTemplate::Steps(CallSteps { steps, map })
            }
        }
    }
}

#[derive(Debug)]
struct SimpleMachine<'a> {
    state: &'a mut SimpleMachineState,
    steps: &'a [Step],
    fn_assignments: &'a HashableHashMap<VarId, FnId>,
    always_invalid_vars: &'a FxHashSet<VarId>,
}

impl SimpleMachine<'_> {
    fn get_r_value(&self) -> Option<Pointer> {
        *self.state.r_value.last().unwrap()
    }
    fn set_r_value(&mut self, value: Option<Pointer>) {
        *self.state.r_value.last_mut().unwrap() = value;
    }

    fn get_var(&self, id: &VarId) -> Option<Assignment> {
        if self.always_invalid_vars.contains(&id) {
            return Some(Assignment { rhs: None });
        }
        self.state
            .lattice
            .var_assignments
            .get(id)
            .copied()
            .or_else(|| {
                self.fn_assignments.get(&id).map(|f| Assignment {
                    rhs: Some(Pointer::Fn(*f)),
                })
            })
    }

    fn insert_var_assignment(
        &mut self,
        name: VarId,
        value: Assignment,
        invalid_objects: &mut GrowableBitSet<ObjectId>,
        unions: &UnionStore,
    ) {
        if self.always_invalid_vars.contains(&name) {
            invalidate(
                invalid_objects,
                unions,
                value.rhs,
                &self.state.lattice.prop_assignments,
            );
            return;
        }

        let new = if fully_invalidated(value.rhs, invalid_objects, unions) {
            Assignment { rhs: None }
        } else {
            value
        };

        if let Some(existing) = self.state.lattice.var_assignments.get(&name) {
            if *existing == new {
                return;
            }
            // if new.rhs.is_none() && !fn_assignments.contains_key(&name) {
            //     self.get_mut(lattice_elements).var_assignments.remove(&name);
            //     return;
            // }

            // self.get_mut(lattice_elements)
            //     .var_assignments
            //     .insert(name, new);
        }
        // else if new.rhs.is_some() || fn_assignments.contains_key(&name) {
        //     self.get_mut(lattice_elements)
        //         .var_assignments
        //         .insert(name, new);
        // }

        self.state.lattice.var_assignments.insert(name, value);
    }

    fn insert_prop_assignment(
        &mut self,
        prop: (ObjectId, NameId),
        value: Assignment,
        invalid_objects: &GrowableBitSet<ObjectId>,
        unions: &UnionStore,
    ) {
        let new = if fully_invalidated(value.rhs, invalid_objects, unions) {
            Assignment { rhs: None }
        } else {
            value
        };

        if let Some(existing) = self.state.lattice.prop_assignments.get(&prop) {
            if *existing == new {
                return;
            }
        } else if let Some(Pointer::NullOrVoid) = new.rhs {
            // Assigning null/void is the same as having nothing assigned.
            return;
        }

        self.state.lattice.prop_assignments.insert(prop, new);
    }
}

#[derive(Debug)]
struct SimpleMachineState {
    /// Stack of unions (constituents) that are currently being built.
    unions: ReusableStateStack<FxHashSet<Option<Pointer>>>,
    /// Stack of expression values. Using a stack simplifies the [`Step`]
    /// language, allowing us to handle nested expressions by pushing/popping
    /// values on the stack.
    r_value: Vec<Option<Pointer>>,
    /// Register containing the current left-hand-side value.
    l_value: Option<AssignTarget>,
    lattice: Lattice,
}

impl ReusableState for SimpleMachineState {
    fn reset(&mut self) {
        let SimpleMachineState {
            unions,
            r_value,
            l_value,
            lattice,
        } = self;
        unions.reset();
        r_value.clear();
        r_value.push(None);
        *l_value = None;
        lattice.var_assignments.clear();
    }
}

impl Default for SimpleMachineState {
    fn default() -> Self {
        Self {
            r_value: vec![None],
            unions: Default::default(),
            l_value: None,
            lattice: Lattice::default(),
        }
    }
}

fn compute_call(
    steps: &[Step],
    map: &[(u32, u32)],
    implicit_return_index: usize,
    unions: &mut UnionStore,
    objects_map: &mut FxHashMap<NodeId, ObjectId>,
    objects: &mut ObjectStore,
    invalid_objects: &mut GrowableBitSet<ObjectId>,
    fn_assignments: &HashableHashMap<VarId, FnId>,
    static_fn_data: &IndexVec<FnId, StaticFunctionData>,
    fn_graph: &mut GraphMap<FnId, (), Directed, BuildHasherDefault<FxHasher>>,
    func: FnId,
    always_invalid_vars: &FxHashSet<VarId>,
) -> Result<(Option<Pointer>, PropertyAssignments), ()> {
    let mut state = SimpleMachineState::default();

    let mut return_types = FxHashSet::default();

    let mut prop_assignments = PropertyAssignments::default();

    for (node, (start, end)) in map.iter().enumerate() {
        let steps = {
            if node == implicit_return_index {
                &IMPLICIT_RETURN_STEPS
            } else {
                let (start, end) = (*start as usize, *end as usize);
                if start > steps.len() || end == start {
                    continue;
                }
                &steps[start..end]
            }
        };
        let mut machine = SimpleMachine {
            state: &mut state,
            steps,
            fn_assignments,
            always_invalid_vars,
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
                            RValue::String => Some(Pointer::Object(ObjectStore::STRING)),
                            RValue::Boolean => Some(Pointer::Object(ObjectStore::BOOL)),
                            RValue::Number => Some(Pointer::Object(ObjectStore::NUMBER)),
                            RValue::BigInt => Some(Pointer::Object(ObjectStore::BIG_INT)),
                            RValue::Object(node_id) => {
                                let root = match objects_map.entry(*node_id) {
                                    Entry::Occupied(entry) => *entry.get(),
                                    Entry::Vacant(entry) => {
                                        let object_id = objects.next_object_id();
                                        entry.insert(object_id);
                                        object_id
                                    }
                                };
                                Some(Pointer::Object(root))
                            }
                            RValue::Var(rhs) => machine.get_var(rhs).and_then(|a| a.rhs),
                            RValue::Prop(prop) => get_property(
                                &machine.state.lattice.prop_assignments,
                                unions,
                                machine.get_r_value(),
                                *prop,
                                invalid_objects,
                            ),
                            RValue::Fn(f) => Some(Pointer::Fn(*f)),
                        };
                        machine.set_r_value(value);
                    }
                },
                Step::StoreLValue(value) => match value {
                    Some(LValue::Var(id)) => {
                        machine.state.l_value = Some(AssignTarget::Var(*id));
                    }
                    Some(LValue::ObjectProp(obj, prop)) => {
                        let root = match objects_map.entry(*obj) {
                            Entry::Occupied(entry) => *entry.get(),
                            Entry::Vacant(entry) => {
                                let object_id = objects.next_object_id();
                                entry.insert(object_id);
                                object_id
                            }
                        };
                        machine.state.l_value =
                            Some(AssignTarget::Prop(Pointer::Object(root), *prop));
                    }
                    Some(LValue::RValueProp(prop)) => {
                        machine.state.l_value =
                            machine.get_r_value().map(|o| AssignTarget::Prop(o, *prop));
                    }
                    None => {
                        machine.state.l_value = None;
                    }
                },
                Step::Assign(conditional) => {
                    if let Some(slot) = machine.state.l_value {
                        let existing = match &slot {
                            AssignTarget::Var(name) => machine.get_var(name).and_then(|a| a.rhs),
                            AssignTarget::Prop(obj, key) => get_property(
                                &machine.state.lattice.prop_assignments,
                                unions,
                                Some(*obj),
                                *key,
                                invalid_objects,
                            ),
                        };

                        let rhs = if !conditional {
                            // supersede
                            machine.get_r_value()
                        } else {
                            // union
                            create_union(unions, existing, machine.get_r_value(), invalid_objects)
                        };
                        let new = Assignment { rhs };
                        match slot {
                            AssignTarget::Var(id) => {
                                machine.insert_var_assignment(id, new, invalid_objects, unions);
                            }
                            AssignTarget::Prop(obj, prop) => {
                                if invalidated(obj, invalid_objects, unions) {
                                    invalidate(
                                        invalid_objects,
                                        unions,
                                        rhs,
                                        &machine.state.lattice.prop_assignments,
                                    );
                                    continue;
                                }

                                match obj {
                                    Pointer::Object(obj) => {
                                        machine.insert_prop_assignment(
                                            (obj, prop),
                                            new,
                                            invalid_objects,
                                            unions,
                                        );
                                    }
                                    Pointer::Union(union) => {
                                        for constituent in unions[union].constituents() {
                                            machine.insert_prop_assignment(
                                                (constituent, prop),
                                                new,
                                                invalid_objects,
                                                unions,
                                            );
                                        }
                                    }
                                    Pointer::Fn(_) | Pointer::NullOrVoid => {}
                                }
                            }
                        };
                    } else {
                        // Unknown/invalid assignment target.
                        invalidate(
                            invalid_objects,
                            unions,
                            machine.get_r_value(),
                            &machine.state.lattice.prop_assignments,
                        );
                    }
                }
                Step::InvalidateRValue => {
                    invalidate(
                        invalid_objects,
                        unions,
                        machine.get_r_value(),
                        &machine.state.lattice.prop_assignments,
                    );
                }
                Step::InvalidateLValue => match &machine.state.l_value {
                    Some(AssignTarget::Var(id)) => {
                        if let Some(rhs) = machine.get_var(id).map(|a| a.rhs) {
                            invalidate(
                                invalid_objects,
                                unions,
                                rhs,
                                &machine.state.lattice.prop_assignments,
                            );
                        }
                    }
                    Some(AssignTarget::Prop(obj, prop)) => {
                        let prop = get_property(
                            &machine.state.lattice.prop_assignments,
                            unions,
                            Some(*obj),
                            *prop,
                            invalid_objects,
                        );

                        invalidate(
                            invalid_objects,
                            unions,
                            prop,
                            &machine.state.lattice.prop_assignments,
                        );
                    }
                    None => {}
                },
                Step::StartCall => {
                    match machine.get_r_value() {
                        Some(Pointer::Fn(f)) => {
                            fn_graph.add_edge(func, f, ());
                        }
                        _ => {}
                    }
                    return Err(());
                }
                Step::Return => {
                    let mut queue = Vec::new();
                    let r_value = machine.get_r_value();
                    match r_value {
                        Some(Pointer::Object(o)) => {
                            if invalid_objects.contains(o) {
                                return_types.insert(ReturnTypeConstituent::Invalid);
                            } else {
                                return_types.insert(ReturnTypeConstituent::Object(o));
                                queue.push(Pointer::Object(o));
                            }
                        }
                        Some(Pointer::Union(union)) => {
                            if fully_invalidated(r_value, invalid_objects, unions) {
                                return_types.insert(ReturnTypeConstituent::Invalid);
                            } else {
                                queue.push(Pointer::Union(union));
                                for constituent in unions[union].constituents() {
                                    return_types.insert(ReturnTypeConstituent::Object(constituent));
                                }
                            }
                        }
                        None => {
                            return_types.insert(ReturnTypeConstituent::Invalid);
                        }
                        Some(Pointer::NullOrVoid) => {
                            return_types.insert(ReturnTypeConstituent::NullOrVoid);
                        }
                        Some(Pointer::Fn(f)) => {
                            if static_fn_data[f].captured_vars.is_empty() {
                                return_types.insert(ReturnTypeConstituent::Invalid);
                            } else {
                                fn_graph.add_edge(func, f, ());
                                return_types.insert(ReturnTypeConstituent::Invalid);
                            }
                        }
                    }

                    let mut done = FxHashSet::default();

                    while let Some(p) = queue.pop() {
                        if !done.insert(p) {
                            continue;
                        }
                        match p {
                            Pointer::Object(p) => {
                                if invalid_objects.contains(p) {
                                    continue;
                                }

                                for ((_, key), prop) in machine
                                    .state
                                    .lattice
                                    .prop_assignments
                                    .range((p, NameId::from_u32(0))..(p, NameId::MAX))
                                {
                                    match prop.rhs {
                                        Some(p @ Pointer::Object(_)) => {
                                            if !done.contains(&p) {
                                                queue.push(p);
                                            }
                                        }
                                        Some(Pointer::Union(u)) => {
                                            for c in unions[u].constituents().map(Pointer::Object) {
                                                if !done.contains(&c) {
                                                    queue.push(c);
                                                }
                                            }
                                        }
                                        Some(Pointer::NullOrVoid) | None => {}
                                        Some(Pointer::Fn(f)) => {
                                            if !static_fn_data[f].captured_vars.is_empty() {
                                                fn_graph.add_edge(func, f, ());
                                            }
                                        }
                                    }

                                    let key = (p, *key);
                                    match prop_assignments.entry(key) {
                                        std::collections::btree_map::Entry::Occupied(mut entry) => {
                                            let old = entry.get().rhs;
                                            let union = create_union(
                                                unions,
                                                old,
                                                prop.rhs,
                                                invalid_objects,
                                            );
                                            entry.insert(Assignment { rhs: union });
                                        }
                                        std::collections::btree_map::Entry::Vacant(entry) => {
                                            entry.insert(*prop);
                                        }
                                    }
                                }
                            }
                            Pointer::Union(union) => {
                                for c in unions[union].constituents().map(Pointer::Object) {
                                    if !done.contains(&c) {
                                        queue.push(c);
                                    }
                                }
                            }
                            Pointer::Fn(_) => unreachable!(),
                            Pointer::NullOrVoid => todo!(),
                        }
                    }

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
                        builder.add(ty, unions, invalid_objects);
                    }
                    let result = unions.build_union(builder);

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
                _ => unreachable!(),
            }
        }
    }

    let mut builder = UnionBuilder::default();

    for ty in return_types {
        match ty {
            ReturnTypeConstituent::Object(obj) => builder.add_object(Some(obj), invalid_objects),
            ReturnTypeConstituent::Invalid => builder.add_object(None, invalid_objects),
            ReturnTypeConstituent::NullOrVoid => builder.add_null_or_void(),
        };
    }

    Ok((unions.build_union(builder), prop_assignments))
}

impl<'ast> DataFlowAnalysis<'ast, '_> {
    fn flow_through(
        &mut self,
        node: NodeIndex,
        input: LatticeElementId,
    ) -> Option<LatticeElementId> {
        let steps = {
            if node == self.cfg.implicit_return_index {
                &IMPLICIT_RETURN_STEPS
            } else {
                let (start, end) = self.step_map[node.index()];
                let (start, end) = (start as usize, end as usize);
                if start > self.steps.len() || end == start {
                    return Some(input);
                }
                &self.steps[start..end]
            }
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
                                .get_var(
                                    rhs,
                                    &self.state.lattice_elements,
                                    &self.always_invalid_vars,
                                )
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
                                *prop,
                                self.invalid_objects,
                            ),
                            RValue::String => Some(Pointer::Object(ObjectStore::STRING)),
                            RValue::Boolean => Some(Pointer::Object(ObjectStore::BOOL)),
                            RValue::Number => Some(Pointer::Object(ObjectStore::NUMBER)),
                            RValue::BigInt => Some(Pointer::Object(ObjectStore::BIG_INT)),
                            RValue::Fn(f) => Some(Pointer::Fn(*f)),
                        };
                        machine.set_r_value(value);
                    }
                },
                Step::StoreLValue(value) => match value {
                    Some(LValue::Var(id)) => {
                        machine.state.l_value = Some(AssignTarget::Var(*id));
                    }
                    Some(LValue::ObjectProp(obj, prop)) => {
                        machine.state.l_value = get_call_obj(
                            self.objects_map,
                            self.call_objects,
                            self.object_links,
                            self.objects,
                            self.root_call,
                            *obj,
                        )
                        .map(|o| AssignTarget::Prop(o, *prop));
                    }
                    Some(LValue::RValueProp(prop)) => {
                        machine.state.l_value =
                            machine.get_r_value().map(|o| AssignTarget::Prop(o, *prop));
                    }
                    None => {
                        machine.state.l_value = None;
                    }
                },
                Step::Assign(conditional) => {
                    if let Some(slot) = &machine.state.l_value {
                        let existing = match &slot {
                            AssignTarget::Var(name) => machine
                                .get_var(
                                    name,
                                    &self.state.lattice_elements,
                                    &self.always_invalid_vars,
                                )
                                .and_then(|a| a.rhs),
                            AssignTarget::Prop(obj, key) => get_property(
                                &machine
                                    .lattice
                                    .get(&self.state.lattice_elements)
                                    .prop_assignments,
                                self.unions,
                                Some(*obj),
                                *key,
                                self.invalid_objects,
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
                                self.invalid_objects,
                            )
                        };
                        let new = Assignment { rhs };
                        match slot {
                            AssignTarget::Var(id) => {
                                machine.lattice.insert_var_assignment(
                                    *id,
                                    new,
                                    &self.state.lattice_elements,
                                    self.invalid_objects,
                                    self.unions,
                                    self.fn_assignments,
                                    self.always_invalid_vars,
                                );
                            }
                            AssignTarget::Prop(obj, prop) => {
                                if invalidated(*obj, self.invalid_objects, self.unions) {
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

                                match obj {
                                    Pointer::Object(obj) => {
                                        machine.lattice.insert_prop_assignment(
                                            (*obj, *prop),
                                            new,
                                            &self.state.lattice_elements,
                                            self.invalid_objects,
                                            self.unions,
                                        );
                                    }
                                    Pointer::Union(union) => {
                                        for constituent in self.unions[*union].constituents() {
                                            machine.lattice.insert_prop_assignment(
                                                (constituent, *prop),
                                                new,
                                                &self.state.lattice_elements,
                                                self.invalid_objects,
                                                self.unions,
                                            );
                                        }
                                    }
                                    Pointer::Fn(_) | Pointer::NullOrVoid => {}
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
                            .get_var(id, &self.state.lattice_elements, &self.always_invalid_vars)
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
                            Some(*obj),
                            *prop,
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
                Step::StartCall => match machine.get_r_value() {
                    Some(Pointer::Fn(f)) => {
                        machine
                            .state
                            .calls
                            .push(MachineCall::Valid(f, CallArgs::new()));
                    }
                    _ => {
                        machine.state.calls.push(MachineCall::Invalid);
                    }
                },
                Step::StoreArg => {
                    let mut arg = machine.get_r_value();
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
                        MachineCall::Valid(func, args) => {
                            if fully_invalidated(arg, self.invalid_objects, self.unions) {
                                arg = None;
                            }
                            args.push(
                                arg,
                                &self.static_fn_data[*func],
                                &mut self.invalid_objects,
                                &self.unions,
                                &machine
                                    .lattice
                                    .get(&self.state.lattice_elements)
                                    .prop_assignments,
                            );
                        }
                    }
                }
                Step::Call(conditional) => {
                    let (func, args) = match machine.state.calls.pop().unwrap() {
                        MachineCall::Invalid => {
                            machine.set_r_value(None);
                            continue;
                        }
                        MachineCall::Valid(f, a) => (f, a),
                    };

                    if let CallTemplate::Simple(return_value) = self.call_templates[func] {
                        machine.set_r_value(return_value);
                        continue;
                    }

                    if let CallTemplate::SimpleWithProps(return_value, prop_assignments) =
                        &self.call_templates[func]
                    {
                        for ((obj, key), prop) in prop_assignments.iter() {
                            if self.invalid_objects.contains(*obj) {
                                continue;
                            }
                            let key = (*obj, *key);
                            let existing = machine
                                .lattice
                                .get(&self.state.lattice_elements)
                                .prop_assignments
                                .get(&key)
                                .copied()
                                .and_then(|a| a.rhs);
                            let rhs = if !conditional {
                                // supersede
                                prop.rhs
                            } else {
                                // union
                                create_union(self.unions, existing, prop.rhs, self.invalid_objects)
                            };
                            let new = Assignment { rhs };
                            machine.lattice.insert_prop_assignment(
                                key,
                                new,
                                &self.state.lattice_elements,
                                self.invalid_objects,
                                self.unions,
                            )
                        }

                        machine.set_r_value(*return_value);
                        continue;
                    }

                    let inner_call = build_call(
                        machine.lattice.get(&self.state.lattice_elements),
                        func,
                        args,
                        &self.static_fn_data,
                        self.unions,
                        self.invalid_objects,
                        &self.fn_assignments,
                        &mut self.done_objects,
                        &mut self.done_functions,
                        self.accessed_props,
                        self.functions,
                        self.always_invalid_vars,
                    )
                    .ok()?;

                    let inner_call_id = self.calls.insert(inner_call);

                    let existing = self.resolved_calls.get(&inner_call_id);

                    let (return_type, prop_assignments, var_assignments) = match existing {
                        Some(r) => (r.return_type, &r.prop_assignments, &r.captured_vars),
                        None => {
                            if inner_call_id != self.call {
                                self.incomplete_dependencies.insert(inner_call_id);
                            }

                            let return_state = match self.return_states.get(&inner_call_id) {
                                Some(s) => s,
                                None => {
                                    machine.set_r_value(Some(Pointer::Object(
                                        ObjectStore::RESOLVING_CALL,
                                    )));
                                    continue;
                                }
                            };

                            let return_types = &return_state.return_ty;
                            if return_types.is_empty()
                                && !return_state.returns_invalid
                                && !return_state.returns_null_or_void
                            {
                                machine.set_r_value(Some(Pointer::Object(
                                    ObjectStore::RESOLVING_CALL,
                                )));
                                continue;
                            }

                            let mut builder = UnionBuilder::default();

                            if return_state.returns_invalid {
                                builder.add_object(None, &self.invalid_objects);
                            }
                            if return_state.returns_null_or_void {
                                builder.add_null_or_void()
                            }

                            // TODO: can optimise - we only add ObjectIds and they are added in stored order.
                            for &obj in return_state.return_ty.iter() {
                                builder.add_object(Some(obj), &self.invalid_objects)
                            }

                            let result = self.unions.build_union(builder);

                            (
                                result,
                                &return_state.prop_assignments,
                                &return_state.captured_vars,
                            )
                        }
                    };

                    let lattice_elements = &self.state.lattice_elements;
                    let fn_assignments = self.fn_assignments;
                    let always_invalid_vars = self.always_invalid_vars;

                    apply_call_side_effects(
                        &mut machine,
                        prop_assignments,
                        var_assignments,
                        self.unions,
                        self.invalid_objects,
                        *conditional,
                        |machine, k, v, invalid_objects, unions| {
                            machine.lattice.insert_prop_assignment(
                                k,
                                v,
                                lattice_elements,
                                invalid_objects,
                                unions,
                            );
                        },
                        |machine, k| {
                            machine
                                .lattice
                                .get(lattice_elements)
                                .prop_assignments
                                .get(k)
                                .copied()
                        },
                        |machine, v| machine.get_var(&v, lattice_elements, always_invalid_vars),
                        |machine, name, value, invalid_objects, unions| {
                            machine.lattice.insert_var_assignment(
                                name,
                                value,
                                lattice_elements,
                                invalid_objects,
                                unions,
                                fn_assignments,
                                always_invalid_vars,
                            );
                        },
                    );

                    machine.set_r_value(return_type);
                }
                Step::Return => {
                    let mut changed = false;

                    let return_state = self.return_states.entry(self.call).or_default();

                    macro_rules! add_invalid_return_ty {
                        () => {{
                            changed |= !return_state.returns_invalid;
                            return_state.returns_invalid = true;
                        }};
                    }
                    macro_rules! add_null_or_void_return_ty {
                        () => {{
                            changed |= !return_state.returns_null_or_void;
                            return_state.returns_null_or_void = true;
                        }};
                    }

                    let r_value = machine.get_r_value();
                    let mut queue = vec![Pointer::Fn(self.calls[self.call].func)];
                    match r_value {
                        Some(Pointer::Object(o)) => {
                            if o == ObjectStore::RESOLVING_CALL {
                                return None;
                            }
                            if self.invalid_objects.contains(o) {
                                add_invalid_return_ty!();
                            } else {
                                changed |= return_state.return_ty.insert(o);
                                queue.push(Pointer::Object(o));
                            }
                        }
                        Some(Pointer::Union(u)) => {
                            if fully_invalidated(r_value, self.invalid_objects, self.unions) {
                                add_invalid_return_ty!();
                            } else {
                                let union = &self.unions[u];
                                if union.contains(ObjectStore::RESOLVING_CALL) && union.len() == 1 {
                                    return None;
                                }
                                queue.push(Pointer::Union(u));
                                for constituent in union.constituents() {
                                    if constituent != ObjectStore::RESOLVING_CALL {
                                        changed |= return_state.return_ty.insert(constituent);
                                    }
                                }
                            }
                        }
                        Some(Pointer::Fn(_node)) => {
                            add_invalid_return_ty!();
                            // TODO: allow functions to be returned:
                            // queue.push(Pointer::Fn(f));
                        }
                        None => {
                            add_invalid_return_ty!();
                        }
                        Some(Pointer::NullOrVoid) => {
                            add_null_or_void_return_ty!();
                        }
                    }

                    for param in self.static_fn_data[self.calls[self.call].func].param_indices() {
                        match machine
                            .get_var(
                                &param,
                                &self.state.lattice_elements,
                                &self.always_invalid_vars,
                            )
                            .and_then(|a| a.rhs)
                        {
                            Some(p @ Pointer::Object(_) | p @ Pointer::Union(_)) => {
                                queue.push(p);
                            }
                            Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) | None => {}
                        }
                    }

                    // TODO: eagerly check for RESOLVING_CALL e.g. when pushing, not popping, from queue.
                    // This way we bail sooner.

                    self.done_objects.clear();
                    self.done_functions.clear();
                    self.done_vars.clear();

                    while let Some(p) = queue.pop() {
                        match p {
                            Pointer::Object(p) => {
                                if p == ObjectStore::RESOLVING_CALL {
                                    changed = true;
                                    continue;
                                }
                                if !self.done_objects.insert(p) {
                                    continue;
                                }

                                if self.invalid_objects.contains(p) {
                                    continue;
                                }

                                for ((_, key), prop) in machine
                                    .lattice
                                    .get(&self.state.lattice_elements)
                                    .prop_assignments
                                    .range((p, NameId::from_u32(0))..(p, NameId::MAX))
                                {
                                    if depends_on_unresolved_call(prop.rhs, self.unions) {
                                        changed = true;
                                        continue;
                                    }

                                    match prop.rhs {
                                        Some(Pointer::Fn(f)) => {
                                            if !self.done_functions.contains(f) {
                                                queue.push(Pointer::Fn(f));
                                            }
                                        }
                                        Some(Pointer::Object(o)) => {
                                            if !self.done_objects.contains(o) {
                                                queue.push(Pointer::Object(o));
                                            }
                                        }
                                        Some(Pointer::Union(u)) => {
                                            for constituent in self.unions[u].constituents() {
                                                if !self.done_objects.contains(constituent) {
                                                    queue.push(Pointer::Object(constituent));
                                                }
                                            }
                                        }
                                        Some(Pointer::NullOrVoid) | None => {}
                                    }

                                    let key = (p, *key);
                                    match return_state.prop_assignments.entry(key) {
                                        std::collections::btree_map::Entry::Occupied(mut entry) => {
                                            let old = entry.get().rhs;
                                            let union = create_union(
                                                self.unions,
                                                old,
                                                prop.rhs,
                                                self.invalid_objects,
                                            );
                                            entry.insert(Assignment { rhs: union });
                                            changed |= old != union;
                                        }
                                        std::collections::btree_map::Entry::Vacant(entry) => {
                                            entry.insert(*prop);
                                            changed = true;
                                        }
                                    }
                                }
                            }
                            Pointer::Union(union) => {
                                for constituent in self.unions[union].constituents() {
                                    if !self.done_objects.contains(constituent) {
                                        queue.push(Pointer::Object(constituent));
                                    }
                                }
                            }
                            Pointer::Fn(f) => {
                                if !self.done_functions.insert(f) {
                                    continue;
                                }

                                for var in &self.static_fn_data[f].captured_vars {
                                    if !self.done_vars.insert(*var) {
                                        continue;
                                    }
                                    debug_assert!(!self.always_invalid_vars.contains(var));
                                    let mut from_fn_assignments = false;
                                    let fn_assignments = self.fn_assignments;
                                    let value = match machine
                                        .lattice
                                        .get(&self.state.lattice_elements)
                                        .var_assignments
                                        .get(var)
                                        .copied()
                                        .or_else(|| {
                                            from_fn_assignments = true;
                                            fn_assignments.get(var).map(|f| Assignment {
                                                rhs: Some(Pointer::Fn(*f)),
                                            })
                                        }) {
                                        Some(v) => v,
                                        None => continue,
                                    };
                                    if depends_on_unresolved_call(value.rhs, self.unions) {
                                        changed = true;
                                        continue;
                                    }
                                    let new = if fully_invalidated(
                                        value.rhs,
                                        self.invalid_objects,
                                        self.unions,
                                    ) {
                                        Assignment { rhs: None }
                                    } else {
                                        value
                                    };
                                    match new.rhs {
                                        Some(Pointer::Fn(f)) => {
                                            if !self.done_functions.contains(f) {
                                                queue.push(Pointer::Fn(f));
                                            }
                                        }
                                        Some(Pointer::Object(o)) => {
                                            if !self.done_objects.contains(o) {
                                                queue.push(Pointer::Object(o));
                                            }
                                        }
                                        Some(Pointer::Union(u)) => {
                                            for constituent in self.unions[u].constituents() {
                                                if !self.done_objects.contains(constituent) {
                                                    queue.push(Pointer::Object(constituent));
                                                }
                                            }
                                        }
                                        Some(Pointer::NullOrVoid) | None => {}
                                    }
                                    let new = if from_fn_assignments {
                                        Assignment { rhs: None }
                                    } else {
                                        new
                                    };
                                    match return_state.captured_vars.entry(*var) {
                                        Entry::Occupied(mut entry) => {
                                            let old = entry.get().rhs;
                                            let union = create_union(
                                                self.unions,
                                                old,
                                                new.rhs,
                                                self.invalid_objects,
                                            );
                                            entry.insert(Assignment { rhs: union });
                                            changed |= old != union;
                                        }
                                        Entry::Vacant(entry) => {
                                            entry.insert(new);
                                            changed = true;
                                        }
                                    }
                                }
                            }
                            Pointer::NullOrVoid => todo!(),
                        }
                    }

                    *self.changed |= changed;

                    break;
                }
                Step::StartUnion => {
                    machine.state.unions.push(UnionBuilder::default());
                }
                Step::PushToUnion => {
                    let v = machine.get_r_value();
                    machine.state.unions.last_mut().unwrap().add(
                        v,
                        &self.unions,
                        &self.invalid_objects,
                    );
                }
                Step::StoreUnion => {
                    let builder = machine.state.unions.pop().unwrap();
                    let result = self.unions.build_union(builder);

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
    _call_objects: &mut FxHashMap<(CallId, NodeId), ObjectId>,
    _object_links: &mut FxHashSet<(ObjectId, ObjectId)>,
    objects: &mut ObjectStore,
    _root_call: CallId,
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

    Some(Pointer::Object(root))

    // let local_id = match call_objects.entry((root_call, node_id)) {
    //     Entry::Occupied(entry) => *entry.get(),
    //     Entry::Vacant(entry) => {
    //         let object_id = objects.next_object_id();
    //         entry.insert(object_id);
    //         object_links.insert((root, object_id));
    //         object_id
    //     }
    // };

    // debug_assert!(root < local_id);

    // Some(Pointer::Object(local_id))
}

#[derive(Default)]
struct JoinOp {
    result: Lattice,
}

impl JoinOp {
    fn join_flow(
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
            match self.result.prop_assignments.entry((*obj, *key)) {
                std::collections::btree_map::Entry::Occupied(mut entry) => {
                    let union = create_union(unions, entry.get().rhs, prop.rhs, invalid_objects);
                    entry.insert(Assignment { rhs: union });
                }
                std::collections::btree_map::Entry::Vacant(entry) => {
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
            match self.result.var_assignments.entry(*name) {
                Entry::Occupied(mut entry) => {
                    let union =
                        create_union(unions, entry.get().rhs, assignment.rhs, invalid_objects);
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
struct DataFlowAnalysisState {
    work_queue_inner: BTreeSet<PrioritizedNode>,

    lattice_elements: IndexSet<LatticeElementId, Lattice>,
    /// Indexed by CFG NodeIndex.
    node_annotations: Vec<LinearFlowState>,

    machine_state: MachineState,

    /// Cache of previously processed lattice joins.
    cached_joins: FxHashMap<u64, LatticeElementId>,
    /// Re-usable buffer for storing lattices-to-be-joined.
    join_input_buffer: Vec<LatticeElementId>,

    visited_nodes: GrowableBitSet<usize>,
}

impl ReusableState for DataFlowAnalysisState {
    fn reset(&mut self) {
        let DataFlowAnalysisState {
            work_queue_inner,
            lattice_elements,
            node_annotations,
            machine_state,
            cached_joins,
            visited_nodes,
            // Note: DataFlowAnalysis clears these itself; doing so here would be pointless.
            join_input_buffer: _,
        } = self;
        work_queue_inner.clear();
        lattice_elements.reset();
        node_annotations.clear();
        machine_state.reset();
        cached_joins.clear();
        visited_nodes.clear();
    }
}

#[derive(Debug)]
struct DataFlowAnalysis<'ast, 'a> {
    /// The set of nodes that need to be considered, ordered by their priority
    /// as determined by control flow analysis and data flow direction.
    work_queue: UniqueQueue<'a>,

    cfg: &'a SimpleCFG<'ast>,
    entry_lattice: LatticeElementId,
    initial_lattice: LatticeElementId,

    call: CallId,

    step_map: &'a [(u32, u32)],
    steps: &'a [Step],
    call_templates: &'a IndexVec<FnId, CallTemplate>,

    resolved_calls: &'a mut FxHashMap<CallId, ResolvedCall>,

    incomplete_dependencies: &'a mut FxHashSet<CallId>,

    return_states: &'a mut FxHashMap<CallId, ReturnState>,
    objects_map: &'a mut FxHashMap<NodeId, ObjectId>,
    call_objects: &'a mut FxHashMap<(CallId, NodeId), ObjectId>,
    object_links: &'a mut FxHashSet<(ObjectId, ObjectId)>,
    objects: &'a mut ObjectStore,
    unions: &'a mut UnionStore,
    invalid_objects: &'a mut GrowableBitSet<ObjectId>,
    calls: &'a mut IndexSet<CallId, Call>,
    changed: &'a mut bool,

    fn_assignments: &'a HashableHashMap<VarId, FnId>,

    root_call: CallId,

    state: &'a mut DataFlowAnalysisState,

    static_fn_data: &'a IndexVec<FnId, StaticFunctionData<'ast>>,

    done_objects: &'a mut GrowableBitSet<ObjectId>,
    done_functions: &'a mut BitSet<FnId>,
    done_vars: &'a mut BitSet<VarId>,

    accessed_props: &'a FxHashMap<FnId, FxHashSet<NameId>>,
    functions: &'a mut IndexVec<FnId, Func>,

    always_invalid_vars: &'a FxHashSet<VarId>,
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
            let state = LinearFlowState::new(self.initial_lattice, self.initial_lattice);
            self.state
                .node_annotations
                .extend(std::iter::repeat(state).take(self.cfg.graph.node_count()));
        }

        let mut dfs = Dfs::empty(&self.cfg.graph);

        self.work_queue.push(self.cfg.entry_index);
        while let Some(cur_node_idx) = self.work_queue.pop() {
            if dfs.discovered.contains(cur_node_idx.index()) {
                continue;
            }
            let cur_node = self.cfg.graph[cur_node_idx];

            // We only add nodes to the work queue once we have processed their
            // predecessors, but we still want to visit the successors at least
            // once, even if their predecessors don't change the state.
            let first_visit = self.state.visited_nodes.insert(cur_node_idx.index());

            let step_count = &mut self.state.node_annotations[cur_node_idx.index()].step_count;

            if *step_count > MAX_STEPS_PER_NODE {
                return Err(cur_node);
            }
            *step_count += 1;

            self.join_inputs(cur_node_idx);
            let r = self.flow(cur_node_idx);

            if r == FlowResult::Abort {
                while let Some(_) = dfs.next(&self.cfg.graph) {}
            }

            // If there is a change in the current node, we want to grab the list
            // of nodes that this node affects.
            if r == FlowResult::Change || first_visit && r == FlowResult::NoChange {
                // We effectively treat the implicit return as `return undefined`,
                // so we skip processing it when coming from a ReturnStmt as
                // processing the implicit return would incorrectly add `undefined`
                // to the call's return type.
                if !matches!(cur_node.kind, NodeKind::ReturnStmt(_)) {
                    let next_nodes = self.cfg.graph.neighbors_directed(cur_node_idx, Outgoing);

                    for next_node in next_nodes {
                        debug_assert!(!dfs.discovered.contains(next_node.index()));
                        self.work_queue.push(next_node);
                    }
                }
            }
        }

        self.join_inputs(self.cfg.implicit_return_index);

        Ok(())
    }

    /// Performs a single flow through a node.
    fn flow(&mut self, node_index: NodeIndex) -> FlowResult {
        let state = &self.state.node_annotations[node_index.index()];
        let out_before = state.out;
        if let Some(new_out) = self.flow_through(node_index, state.in_) {
            if out_before != new_out {
                self.state.node_annotations[node_index.index()].out = new_out;
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
    fn join_inputs(&mut self, node_index: NodeIndex) {
        if self.cfg.entry_index == node_index {
            self.state.node_annotations[node_index.index()].in_ = self.entry_lattice;
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
            .edges_directed(node_index, Incoming)
            .filter(|e| !matches!(graph[e.source()].kind, NodeKind::ReturnStmt(_)))
            .map(|e| get_input_from_edge(node_annotations, e))
            .filter(|i| *i != initial_lattice);

        self.state.join_input_buffer.clear();
        self.state.join_input_buffer.extend(inputs);

        if self.state.join_input_buffer.is_empty() {
            return;
        }

        if self.state.join_input_buffer.len() == 1 {
            // Only one relevant edge.
            self.state.node_annotations[node_index.index()].in_ =
                *self.state.join_input_buffer.first().unwrap();
            return;
        }

        self.state.join_input_buffer.sort_unstable();
        self.state.join_input_buffer.dedup();

        // De-duping may have dropped us down to one element, check again.
        if self.state.join_input_buffer.len() == 1 {
            // Only one relevant edge.
            self.state.node_annotations[node_index.index()].in_ =
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
                self.state.node_annotations[node_index.index()].in_ = *entry.get(); // Reuse cached join.
                return;
            }
            Entry::Vacant(entry) => entry,
        };

        let mut joiner = JoinOp::default();
        for &id in &self.state.join_input_buffer {
            joiner.join_flow(
                &self.state.lattice_elements[id],
                self.unions,
                self.invalid_objects,
            );
        }
        let result = self.state.lattice_elements.insert(joiner.finish());
        // Cache result.
        entry.insert(result);
        self.state.node_annotations[node_index.index()].in_ = result;
    }
}

fn get_input_from_edge(
    node_annotations: &[LinearFlowState],
    edge: EdgeReference<Branch>,
) -> LatticeElementId {
    let source = edge.source();
    node_annotations[source.index()].out
}
