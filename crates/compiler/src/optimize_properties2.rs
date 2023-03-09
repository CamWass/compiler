use std::ops::Index;

use ast::*;
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use index::vec::{Idx, IndexVec};
use rustc_hash::{FxHashMap, FxHashSet};
use swc_atoms::JsWord;

use crate::control_flow::node::Node;
use crate::control_flow::ControlFlowAnalysis::*;
use crate::control_flow::ControlFlowGraph::*;
use crate::find_vars::*;
use crate::utils::unwrap_as;
use crate::DefaultNameGenerator::DefaultNameGenerator;
use crate::{DataFlowAnalysis::*, Id, ToId};

#[cfg(test)]
mod tests;

pub fn process(ast: &mut ast::Program) {
    let mut visitor = GlobalVisitor::default();
    ast.visit_with(&mut visitor);

    let mut renamer = Renamer {
        rename_map: visitor.rename_map,
    };
    ast.visit_mut_with(&mut renamer);
}

fn handle_fn<'a, T>(rename_map: &mut FxHashMap<NodeId, JsWord>, node: &'a T)
where
    T: FunctionLike<'a> + VisitWith<'a, DeclFinder>,
    &'a T: Into<ControlFlowRoot<'a>>,
{
    let vars = find_vars_declared_in_fn(node, false);

    let cfa = ControlFlowAnalysis::analyze(node.into(), false);

    let result = Analysis::new(cfa, vars.scopeVariables).analyze();

    // result.1.print_full_with_annotations::<DefaultPrinter>(None);

    // dbg!(result
    //     .0
    //     .lattice_elements
    //     .iter_enumerated()
    //     .collect::<Vec<_>>());

    for (mut id, object) in result.0.objects {
        // Manually resolve object.
        while let Some(mapped) = result.0.merge_map.get(&id) {
            id = *mapped;
        }
        if !result.0.invalid_objects.contains(&id) {
            let mut name_gen = DefaultNameGenerator::default();
            for (_, prop) in &object.properties {
                let new_name = name_gen.generateNextName();
                for &reference in &prop.references {
                    rename_map.insert(reference, new_name.clone());
                }
            }
        }
    }

    node.visit_body_with(&mut FnVisitor { rename_map });
}

struct FnVisitor<'a> {
    rename_map: &'a mut FxHashMap<NodeId, JsWord>,
}

impl Visit<'_> for FnVisitor<'_> {
    fn visit_function(&mut self, node: &Function) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_constructor(&mut self, node: &Constructor) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_arrow_expr(&mut self, node: &ArrowExpr) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_getter_prop(&mut self, node: &GetterProp) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_setter_prop(&mut self, node: &SetterProp) {
        handle_fn(&mut self.rename_map, node);
    }
}

#[derive(Default)]
struct GlobalVisitor {
    rename_map: FxHashMap<NodeId, JsWord>,
}

impl Visit<'_> for GlobalVisitor {
    fn visit_function(&mut self, node: &Function) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_constructor(&mut self, node: &Constructor) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_arrow_expr(&mut self, node: &ArrowExpr) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_getter_prop(&mut self, node: &GetterProp) {
        handle_fn(&mut self.rename_map, node);
    }
    fn visit_setter_prop(&mut self, node: &SetterProp) {
        handle_fn(&mut self.rename_map, node);
    }
}

#[derive(Debug)]
pub struct AnalysisResult {
    pub lattice_elements: IndexVec<LatticeElementId, Lattice>,

    objects: FxHashMap<ObjectId, Object>,
    merge_map: FxHashMap<ObjectId, ObjectId>,
    invalid_objects: FxHashSet<ObjectId>,
}

pub struct Analysis<'ast> {
    data_flow_analysis: DataFlowAnalysis<Node<'ast>, Inner<'ast>, Lattice, JoinOp>,
}

impl<'ast> Analysis<'ast> {
    pub fn new(
        cfa: ControlFlowAnalysisResult<Node<'ast>, LinearFlowState, LatticeElementId>,
        vars: FxHashMap<Id, VarId>,
    ) -> Self {
        let inner = Inner {
            lattice_elements: IndexVec::default(),
            cfg: cfa.cfg,

            vars,

            store: Store {
                objects_map: FxHashMap::default(),
                objects: FxHashMap::default(),
                conflations: FxHashMap::default(),
                merge_map: FxHashMap::default(),
                finalized_conflations: FxHashMap::default(),
                invalid_objects: FxHashSet::default(),

                cur_object_id: ObjectId::from_u32(0),
                cur_conflation_id: ConflationId::from_u32(0),
            },
        };
        let data_flow_analysis = DataFlowAnalysis::new(inner, cfa.nodePriorities);

        Self { data_flow_analysis }
    }

    pub fn analyze(
        mut self,
    ) -> (
        AnalysisResult,
        ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,
    ) {
        self.data_flow_analysis.analyze();

        let result = AnalysisResult {
            lattice_elements: self.data_flow_analysis.inner.lattice_elements,
            objects: self.data_flow_analysis.inner.store.objects,
            merge_map: self.data_flow_analysis.inner.store.merge_map,
            invalid_objects: self.data_flow_analysis.inner.store.invalid_objects,
        };
        (result, self.data_flow_analysis.inner.cfg)
    }
}

#[derive(Debug)]
/// Info collected during analysis of a function.
struct Store {
    objects_map: FxHashMap<NodeId, Pointer>,
    objects: FxHashMap<ObjectId, Object>,
    conflations: FxHashMap<ConflationId, FxHashSet<ObjectId>>,
    merge_map: FxHashMap<ObjectId, ObjectId>,
    finalized_conflations: FxHashMap<ConflationId, ObjectId>,
    invalid_objects: FxHashSet<ObjectId>,

    cur_object_id: ObjectId,
    cur_conflation_id: ConflationId,
}

impl Store {
    fn next_object_id(&mut self) -> ObjectId {
        let id = self.cur_object_id;
        self.cur_object_id.increment_by(1);
        id
    }
    fn next_conflation_id(&mut self) -> ConflationId {
        let id = self.cur_conflation_id;
        self.cur_conflation_id.increment_by(1);
        id
    }

    /// Recursively follows any merge mappings for `obj`, returning the
    /// [`ObjectId`] of the current representation of the object.
    fn resolve_object(&self, mut obj: ObjectId) -> ObjectId {
        while let Some(mapped) = self.merge_map.get(&obj) {
            obj = *mapped;
        }
        obj
    }

    /// Recursively follows any merge mappings for `pointer`, returning the
    /// [`ObjectId`] of the current representation of the object.
    fn resolve_pointer(&self, mut pointer: Pointer) -> Pointer {
        loop {
            match pointer {
                Pointer::Object(obj) => {
                    if let Some(mapped) = self.merge_map.get(&obj) {
                        pointer = Pointer::Object(*mapped);
                    } else {
                        return pointer;
                    }
                }
                Pointer::Conflation(conflation) => {
                    if let Some(mapped) = self.finalized_conflations.get(&conflation) {
                        pointer = Pointer::Object(*mapped);
                    } else {
                        return pointer;
                    }
                }
            }
        }
    }

    /// Conflates the entities that `pointer1` and `pointer2` point to.
    ///
    /// Note that this merely records the conflation - the objects are not merged
    /// until the conflation is used as a single object (e.g. property is accessed).
    fn record_conflation(
        &mut self,
        pointer1: Option<Pointer>,
        pointer2: Option<Pointer>,
        lattice: &Lattice,
    ) -> Option<Pointer> {
        // TODO: invalidate if either is None. This can be avoided if we have a Some() and a None,
        // and the None represents a literal/primitive, in which case we know its properties and
        // the conflated object's props will be the properties of the non-primitive argument (Some)
        // subtract the primitive's props.
        // This is simplified if the primitive is null/undefined, since they have no props.
        let (id1, id2) = match (pointer1, pointer2) {
            (None, None) => return None,
            (None, Some(id2)) => {
                self.invalidate(Some(id2), lattice);
                return Some(id2);
            }
            (Some(id1), None) => {
                self.invalidate(Some(id1), lattice);
                return Some(id1);
            }
            (Some(id1), Some(id2)) => (id1, id2),
        };

        if id1 == id2 {
            return Some(id1);
        }

        let mut conflation = FxHashSet::default();

        match (self.resolve_pointer(id1), self.resolve_pointer(id2)) {
            (Pointer::Object(left), Pointer::Object(right)) => {
                conflation.insert(left);
                conflation.insert(right);
            }
            (Pointer::Object(obj), Pointer::Conflation(conf)) => {
                conflation.insert(obj);
                conflation.extend(self.conflations.get(&conf).unwrap().iter().copied());
            }
            (Pointer::Conflation(conf), Pointer::Object(obj)) => {
                conflation.insert(obj);
                conflation.extend(self.conflations.get(&conf).unwrap().iter().copied());
            }
            (Pointer::Conflation(left), Pointer::Conflation(right)) => {
                conflation.extend(self.conflations.get(&left).unwrap().iter().copied());
                conflation.extend(self.conflations.get(&right).unwrap().iter().copied());
            }
        }

        let conflation_id = self.next_conflation_id();

        self.conflations.insert(conflation_id, conflation);

        Some(Pointer::Conflation(conflation_id))
    }

    /// Recursively invalidates the entity that `pointer` points to.
    fn invalidate(&mut self, pointer: Option<Pointer>, lattice: &Lattice) {
        if let Some(pointer) = pointer {
            let pointer = self.resolve_pointer(pointer);
            let mut queue = vec![pointer];
            let mut done = FxHashSet::default();

            while let Some(pointer) = queue.pop() {
                done.insert(pointer);

                match pointer {
                    Pointer::Object(obj) => {
                        self.invalid_objects.insert(obj);

                        if let Some(props) = lattice.prop_assignments.get(&obj) {
                            for prop in props.values() {
                                if let Some(value) = prop.rhs {
                                    let value = self.resolve_pointer(value);
                                    if !done.contains(&value) {
                                        queue.push(value);
                                    }
                                }
                            }
                        }
                    }
                    Pointer::Conflation(conflation) => {
                        for &constituent in self.conflations.get(&conflation).unwrap() {
                            let constituent = self.resolve_object(constituent);
                            let pointer = Pointer::Object(constituent);
                            if !done.contains(&pointer) {
                                queue.push(pointer);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Returns true if `pointer` points to an invalid object.
    fn invalidated(&self, pointer: Pointer) -> bool {
        let pointer = self.resolve_pointer(pointer);
        match pointer {
            Pointer::Object(obj) => self.invalid_objects.contains(&obj),
            Pointer::Conflation(conflation) => self
                .conflations
                .get(&conflation)
                .unwrap()
                .iter()
                .any(|&obj| {
                    let obj = self.resolve_object(obj);
                    self.invalid_objects.contains(&obj)
                }),
        }
    }
}

#[derive(Debug)]
struct Inner<'ast> {
    lattice_elements: IndexVec<LatticeElementId, Lattice>,
    cfg: ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,

    vars: FxHashMap<Id, VarId>,

    store: Store,
}

impl<'ast> Inner<'ast> {
    fn analyze_node<'a>(&mut self, n: Node<'ast>, lattice: &'a mut Lattice, conditional: bool) {
        let mut v = Analyser {
            analysis: self,
            lattice,
        };

        v.init(n, conditional);
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy)]
/// Points to an abstraction of a javascript object.
///
/// Note: the underlying, object may change during the course of the analysis,
/// leaving the pointer pointing to a non-existant object. For this reason, the
/// pointer should be resolved before trying to access the underlying object.
enum Pointer {
    Object(ObjectId),
    Conflation(ConflationId),
}

index::newtype_index!(struct ObjectId { .. });
index::newtype_index!(struct ConflationId { .. });

#[derive(Debug, Clone, PartialEq, Default)]
struct Property {
    references: FxHashSet<NodeId>,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Object {
    properties: FxHashMap<JsWord, Property>,
}

#[derive(Debug, Clone, PartialEq)]
struct Assignment {
    rhs: Option<Pointer>,
}

/// A place where a variable can be stored.
#[derive(Debug)]
enum Slot<'ast> {
    Var(Id),
    Prop(ObjectId, &'ast Ident),
}

#[derive(Debug)]
struct Analyser<'ast, 'a> {
    analysis: &'a mut Inner<'ast>,
    lattice: &'a mut Lattice,
}

impl<'ast, 'a> Analyser<'ast, 'a> {
    fn get_property(&self, object: ObjectId, key: &JsWord) -> Option<&Assignment> {
        self.lattice
            .prop_assignments
            .get(&object)
            .and_then(|props| props.get(key))
    }

    fn has_property(&self, object: ObjectId, key: &JsWord) -> bool {
        self.get_property(object, key).is_some()
    }

    /// Records `key` as a reference to a property on the object that `pointer`
    /// points to. No reference is recorded if the property has not been assigned
    /// yet.
    ///
    /// Note: this has the side effect of merging any conflation that `pointer`
    /// points to.
    fn reference_prop(&mut self, pointer: Pointer, key: &Ident) {
        let object = self.ensure_conflated(pointer);
        // Only add references to assigned properties.
        if self.has_property(object, &key.sym) {
            self.analysis
                .store
                .objects
                .get_mut(&object)
                .unwrap()
                .properties
                .entry(key.sym.clone())
                .or_default()
                .references
                .insert(key.node_id);
        }
    }

    /// Performs any outstanding conflations for the entity that `pointer` points to.
    /// This recursively merges the constituents into a single object.
    fn ensure_conflated(&mut self, pointer: Pointer) -> ObjectId {
        let mut finalized = FxHashSet::default();
        // Find all objects that will be conflated.
        let pointer = self.analysis.store.resolve_pointer(pointer);
        let mut queue = match pointer {
            Pointer::Object(obj) => vec![obj],
            Pointer::Conflation(conflation) => {
                finalized.insert(conflation);
                self.analysis
                    .store
                    .conflations
                    .remove(&conflation)
                    .unwrap()
                    .into_iter()
                    .collect()
            }
        };
        let mut flattened = FxHashSet::default();
        while let Some(obj) = queue.pop() {
            let obj = self.analysis.store.resolve_object(obj);

            flattened.insert(obj);

            let mut matching_groups = FxHashSet::default();

            for (id, group) in &self.analysis.store.conflations {
                if group.contains(&obj) {
                    matching_groups.insert(*id);
                }
            }

            for id in matching_groups {
                finalized.insert(id);
                let group = self.analysis.store.conflations.remove(&id).unwrap();
                for constituent in group {
                    let constituent = self.analysis.store.resolve_object(constituent);
                    if !flattened.contains(&constituent) {
                        queue.push(constituent);
                    }
                }
            }
        }

        if flattened.len() == 1 {
            // Nothing to conflate.
            return flattened.into_iter().next().unwrap();
        }

        let result_id = self.analysis.store.next_object_id();

        for finalized in finalized {
            self.analysis
                .store
                .finalized_conflations
                .insert(finalized, result_id);
        }

        self.analysis
            .store
            .objects
            .insert(result_id, Object::default());

        let mut invalid = false;

        for constituent in flattened {
            self.analysis.store.merge_map.insert(constituent, result_id);

            let mut incoming = self.analysis.store.objects.remove(&constituent).unwrap();

            if !invalid && self.analysis.store.invalid_objects.contains(&constituent) {
                invalid = true;
            }

            let properties = self
                .lattice
                .prop_assignments
                .remove(&constituent)
                .unwrap_or_default();

            for (key, value) in properties {
                let references = incoming
                    .properties
                    .remove(&key)
                    .unwrap_or_default()
                    .references;
                self.analysis
                    .store
                    .objects
                    .get_mut(&result_id)
                    .unwrap()
                    .properties
                    .entry(key.clone())
                    .or_default()
                    .references
                    .extend(references);

                match self
                    .lattice
                    .prop_assignments
                    .entry(result_id)
                    .or_default()
                    .entry(key.clone())
                {
                    std::collections::hash_map::Entry::Occupied(entry) => {
                        // conflate
                        let existing = entry.get().rhs;
                        let value = self.analysis.store.record_conflation(
                            existing,
                            value.rhs,
                            &self.lattice,
                        );
                        self.lattice
                            .prop_assignments
                            .entry(result_id)
                            .or_default()
                            .get_mut(&key)
                            .unwrap()
                            .rhs = value;
                    }
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        // initialize
                        entry.insert(value);
                    }
                }
            }
        }

        if invalid {
            self.analysis
                .store
                .invalidate(Some(Pointer::Object(result_id)), &self.lattice);
        }

        result_id
    }

    /// Assigns the given value to the [`Slot`] with the given name. Any existing
    /// value is replaced, unless this assignment is conditional, in which case
    /// the slot is assigned a conflation of the old and new values.
    fn assign_to_slot(
        &mut self,
        slot: Option<Slot<'ast>>,
        rhs: Option<Pointer>,
        conditional: bool,
    ) -> Option<Pointer> {
        if let Some(slot) = slot {
            let existing = match &slot {
                Slot::Var(name) => self.lattice.var_assignments.get(&name),
                Slot::Prop(obj, key) => self.get_property(*obj, &key.sym),
            };

            let new = if let Some(rhs_object) = rhs {
                if let Some(existing) = existing {
                    let rhs = if !conditional {
                        // supersede
                        Some(rhs_object)
                    } else {
                        // conflate
                        let existing = existing.rhs;
                        self.analysis.store.record_conflation(
                            existing,
                            Some(rhs_object),
                            &self.lattice,
                        )
                    };
                    Assignment { rhs }
                } else {
                    // initialize
                    Assignment {
                        rhs: Some(rhs_object),
                    }
                }
            } else {
                // Non-object assignment
                Assignment { rhs: None }
            };
            let rhs = new.rhs;
            match slot {
                Slot::Var(name) => {
                    self.lattice.var_assignments.insert(name, new);
                }
                Slot::Prop(obj, key) => {
                    if self.analysis.store.invalid_objects.contains(&obj) {
                        self.analysis.store.invalidate(rhs, &self.lattice);
                    }
                    self.lattice
                        .prop_assignments
                        .entry(obj)
                        .or_default()
                        .insert(key.sym.clone(), new);
                    self.analysis
                        .store
                        .objects
                        .get_mut(&obj)
                        .unwrap()
                        .properties
                        .entry(key.sym.clone())
                        .or_default()
                        .references
                        .insert(key.node_id);
                }
            };
            rhs
        } else {
            // Unknown/invalid assignment target.
            self.analysis.store.invalidate(rhs, &self.lattice);
            rhs
        }
    }

    /// Records an assignment of the form `LHS OP RHS` e.g.
    /// ```js
    /// let a = 1
    /// ```
    /// ```js
    /// a.b = c
    /// ```
    /// ```js
    /// a ||= b
    /// ```
    /// ```js
    /// a().b ||= c
    /// ```
    /// ```js
    /// let { a: b } = c
    /// ```
    ///
    /// `conditional` - Whether the entire assignment (LHS and RHS) is conditionally executed.
    fn record_assignment(
        &mut self,
        lhs: Node<'ast>,
        rhs: &'ast Expr,
        conditional: bool,
        op: AssignOp,
    ) -> Option<Pointer> {
        let conditional_assign = matches!(
            op,
            AssignOp::AndAssign | AssignOp::OrAssign | AssignOp::NullishAssign
        );
        match lhs {
            Node::ArrayPat(_) | Node::ObjectPat(_) => {
                debug_assert!(!conditional_assign, "invalid assignment target");
                self.handle_destructuring(lhs, rhs, conditional)
            }
            _ => {
                let lhs = self.visit_and_get_slot(lhs, conditional);
                let conditional = conditional || conditional_assign;
                let rhs = self.visit_and_get_object(Node::from(rhs), conditional);

                self.assign_to_slot(lhs, rhs, conditional)
            }
        }
    }

    fn handle_destructuring(
        &mut self,
        lhs: Node<'ast>,
        rhs: &'ast Expr,
        conditional: bool,
    ) -> Option<Pointer> {
        let rhs = self.visit_and_get_object(Node::from(rhs), conditional);
        self.visit_destructuring(lhs, rhs, conditional);
        rhs
    }

    fn invalidate_slot(&mut self, node: Node<'ast>, conditional: bool) {
        if let Some(lhs) = self.visit_and_get_slot(node, conditional) {
            let value = match lhs {
                Slot::Var(name) => self.lattice.var_assignments.get(&name).and_then(|a| a.rhs),
                Slot::Prop(obj, key) => self.get_property(obj, &key.sym).and_then(|a| a.rhs),
            };
            self.analysis.store.invalidate(value, &self.lattice);
        }
    }

    fn visit_destructuring(&mut self, lhs: Node<'ast>, rhs: Option<Pointer>, conditional: bool) {
        match lhs {
            Node::ObjectPat(lhs) => {
                let has_non_ident_props = lhs.props.iter().any(|p| match p {
                    ObjectPatProp::KeyValue(p) => !matches!(&p.key, PropName::Ident(_)),
                    ObjectPatProp::Assign(_) | ObjectPatProp::Rest(_) => false,
                });
                if let Some(rhs) = rhs {
                    if has_non_ident_props {
                        self.analysis.store.invalidate(Some(rhs), &self.lattice);
                    }
                    if !self.analysis.store.invalidated(rhs) {
                        for prop in &lhs.props {
                            match prop {
                                ObjectPatProp::KeyValue(prop) => {
                                    let key = unwrap_as!(&prop.key, PropName::Ident(k), k);
                                    self.reference_prop(rhs, key);
                                    let rhs = self.ensure_conflated(rhs);
                                    let rhs_value = self
                                        .get_property(rhs, &key.sym)
                                        .and_then(|assign| assign.rhs);

                                    if matches!(prop.value.as_ref(), Pat::Ident(_) | Pat::Expr(_)) {
                                        let slot = self.visit_and_get_slot(
                                            Node::from(prop.value.as_ref()),
                                            conditional,
                                        );
                                        self.assign_to_slot(slot, rhs_value, conditional);
                                    } else {
                                        self.visit_destructuring(
                                            Node::from(prop.value.as_ref()),
                                            rhs_value,
                                            conditional,
                                        );
                                    }
                                }
                                ObjectPatProp::Assign(_) => {
                                    unreachable!("removed my normalization");
                                }
                                ObjectPatProp::Rest(rest) => {
                                    debug_assert!(lhs.props.last().unwrap() == prop);

                                    // TODO: throw error, don't panic.
                                    // The argument of an object pattern's rest element must be an identifier.
                                    let arg = unwrap_as!(rest.arg.as_ref(), Pat::Ident(i), i);

                                    // TODO: this is imprecise - rest patterns create a new, distinct, object, which has the remaining non-destructured
                                    // properties copied over. These properties must have the same names as those in the original object after renaming.
                                    // But since they are distinct objects, we don't want to conflate them.
                                    let id = arg.to_id();
                                    if self.analysis.vars.contains_key(&id) {
                                        self.assign_to_slot(
                                            Some(Slot::Var(id)),
                                            Some(rhs),
                                            conditional,
                                        );
                                    }
                                }
                            }
                        }
                        return;
                    }
                }
                for prop in &lhs.props {
                    self.visit_destructuring(Node::from(prop), None, conditional);
                }
            }

            Node::BindingIdent(lhs) => {
                let id = lhs.to_id();
                if self.analysis.vars.contains_key(&id) {
                    self.assign_to_slot(Some(Slot::Var(id)), rhs, conditional);
                }
            }
            Node::ArrayPat(lhs) => {
                self.analysis.store.invalidate(rhs, &self.lattice);
                for element in lhs.elems.iter().filter_map(|e| e.as_ref()) {
                    if let Pat::Expr(elem) = element {
                        self.invalidate_slot(Node::from(elem.as_ref()), conditional);
                    } else {
                        self.visit_destructuring(Node::from(element), None, conditional);
                    }
                }
            }
            Node::RestPat(lhs) => {
                self.analysis.store.invalidate(rhs, &self.lattice);
                self.visit_destructuring(Node::from(lhs.arg.as_ref()), None, conditional);
            }
            Node::AssignPat(lhs) => {
                let default_value = self.visit_and_get_object(Node::from(lhs.right.as_ref()), true);

                let result =
                    self.analysis
                        .store
                        .record_conflation(rhs, default_value, &self.lattice);

                self.visit_destructuring(Node::from(lhs.left.as_ref()), result, conditional);
            }
            Node::KeyValuePatProp(lhs) => {
                self.analysis.store.invalidate(rhs, &self.lattice);
                if matches!(lhs.value.as_ref(), Pat::Ident(_) | Pat::Expr(_)) {
                    self.invalidate_slot(Node::from(lhs.value.as_ref()), conditional);
                } else {
                    self.visit_destructuring(Node::from(lhs.value.as_ref()), None, conditional);
                }
            }
            Node::AssignPatProp(_) => {
                unreachable!("removed my normalization");
            }

            _ => {
                dbg!(lhs);
                unreachable!();
            }
        }
    }

    /// Visits the given expression. If the expression resolves to a valid assignment target, a [`Slot`]
    /// representing that target is returned.
    fn visit_and_get_slot(&mut self, node: Node<'ast>, conditional: bool) -> Option<Slot<'ast>> {
        match node {
            Node::Ident(node) => {
                let id = node.to_id();
                if self.analysis.vars.contains_key(&id) {
                    Some(Slot::Var(id))
                } else {
                    None
                }
            }
            Node::BindingIdent(node) => self.visit_and_get_slot(Node::Ident(&node.id), conditional),
            Node::MemberExpr(node) => {
                let obj = self.visit_and_get_object(Node::from(&node.obj), conditional);
                if node.computed {
                    self.analysis.store.invalidate(obj, &self.lattice);
                    self.visit_and_get_slot(Node::from(node.prop.as_ref()), conditional);
                    None
                } else {
                    obj.map(|obj| {
                        let object = self.ensure_conflated(obj);
                        let prop = unwrap_as!(node.prop.as_ref(), Expr::Ident(i), i);
                        self.reference_prop(obj, prop);
                        Slot::Prop(object, prop)
                    })
                }
            }

            // All other nodes cannot evaluate to a reference, and should return None (but we still
            // need to visit their children).
            _ => {
                self.visit_and_get_object(node, conditional);
                None
            }
        }
    }

    fn visit_and_get_object(&mut self, node: Node<'ast>, conditional: bool) -> Option<Pointer> {
        match node {
            // Don't traverse into new control flow nodes.
            Node::FnExpr(_) | Node::ArrowExpr(_) => None,

            Node::AssignExpr(node) => {
                self.record_assignment(Node::from(&node.left), &node.right, conditional, node.op)
            }

            Node::Class(_) => todo!(),
            Node::ExtendsClause(_) => todo!(),
            Node::ClassProp(_) => todo!(),
            Node::PrivateProp(_) => todo!(),
            Node::ClassMethod(_) => todo!(),
            Node::PrivateMethod(_) => todo!(),
            Node::Constructor(_) => todo!(),
            Node::VarDeclarator(node) => {
                let lhs = Node::from(&node.name);
                if let Some(rhs) = &node.init {
                    self.record_assignment(lhs, rhs, conditional, AssignOp::Assign);
                } else {
                    self.visit_and_get_object(lhs, conditional);
                }
                None
            }
            Node::ThisExpr(_) => todo!(),
            Node::ArrayLit(node) => {
                for element in &node.elems {
                    if let Some(element) = element {
                        let obj = self.visit_and_get_object(Node::from(element), conditional);
                        // Can't track once it's in the array.
                        self.analysis.store.invalidate(obj, &self.lattice);
                    }
                }
                None
            }
            Node::ObjectLit(node) => {
                if let Some(existing) = self.analysis.store.objects_map.get(&node.node_id) {
                    Some(*existing)
                } else {
                    let is_simple_obj_lit = node.props.iter().all(|p| match p {
                        Prop::KeyValue(p) => matches!(&p.key, PropName::Ident(_)),
                        _ => false,
                    });

                    if is_simple_obj_lit {
                        let mut obj = Object::default();
                        obj.properties.reserve(node.props.len());

                        let object_id = self.analysis.store.next_object_id();

                        self.analysis.store.objects.insert(object_id, obj);

                        self.analysis
                            .store
                            .objects_map
                            .insert(node.node_id, Pointer::Object(object_id));

                        for prop in &node.props {
                            let prop = unwrap_as!(prop, Prop::KeyValue(p), p);
                            let key = unwrap_as!(&prop.key, PropName::Ident(k), k);

                            let value = self
                                .visit_and_get_object(Node::from(prop.value.as_ref()), conditional);

                            self.assign_to_slot(
                                Some(Slot::Prop(object_id, key)),
                                value,
                                conditional,
                            );

                            self.analysis
                                .store
                                .objects
                                .get_mut(&object_id)
                                .unwrap()
                                .properties
                                .entry(key.sym.clone())
                                .or_default()
                                .references
                                .insert(key.node_id);
                        }

                        Some(Pointer::Object(object_id))
                    } else {
                        dbg!(node);
                        todo!();
                    }
                }
            }
            Node::SpreadElement(node) => {
                let obj = self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional);
                self.analysis.store.invalidate(obj, &self.lattice);
                None
            }
            Node::UnaryExpr(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional);
                None
            }
            Node::UpdateExpr(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional);
                None
            }
            Node::BinExpr(node) => {
                match node.op {
                    BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::NullishCoalescing => {
                        // TODO: if LHS is object, then we know if RHS will execute.
                        let left =
                            self.visit_and_get_object(Node::from(node.left.as_ref()), conditional);
                        let right =
                            self.visit_and_get_object(Node::from(node.right.as_ref()), true);
                        self.analysis
                            .store
                            .record_conflation(left, right, &self.lattice)
                    }
                    _ => {
                        self.visit_and_get_object(Node::from(node.left.as_ref()), conditional);
                        self.visit_and_get_object(Node::from(node.right.as_ref()), conditional);
                        None
                    }
                }
            }
            Node::ClassExpr(_) => todo!(),
            Node::MemberExpr(node) => {
                let obj = self.visit_and_get_object(Node::from(&node.obj), conditional);
                if node.computed {
                    self.analysis.store.invalidate(obj, &self.lattice);
                    self.visit_and_get_object(Node::from(node.prop.as_ref()), conditional);
                    None
                } else {
                    if let Some(obj) = obj {
                        let ident = unwrap_as!(node.prop.as_ref(), Expr::Ident(i), i);
                        let object = self.ensure_conflated(obj);
                        self.reference_prop(obj, ident);
                        if let Some(value) = self.get_property(object, &ident.sym).map(|a| a.rhs) {
                            value
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
            }
            Node::CondExpr(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
                let cons = self.visit_and_get_object(Node::from(node.cons.as_ref()), true);
                let alt = self.visit_and_get_object(Node::from(node.alt.as_ref()), true);
                self.analysis
                    .store
                    .record_conflation(cons, alt, &self.lattice)
            }
            Node::CallExpr(node) => {
                let callee = self.visit_and_get_object(Node::from(&node.callee), conditional);
                self.analysis.store.invalidate(callee, &self.lattice);

                for arg in &node.args {
                    let obj = self.visit_and_get_object(Node::from(arg), conditional);
                    self.analysis.store.invalidate(obj, &self.lattice);
                }
                None
            }
            Node::NewExpr(node) => {
                let callee =
                    self.visit_and_get_object(Node::from(node.callee.as_ref()), conditional);
                self.analysis.store.invalidate(callee, &self.lattice);

                if let Some(args) = &node.args {
                    for arg in args {
                        let obj = self.visit_and_get_object(Node::from(arg), conditional);
                        self.analysis.store.invalidate(obj, &self.lattice);
                    }
                }
                None
            }
            Node::SeqExpr(node) => {
                debug_assert!(node.exprs.len() > 0);

                let mut i = 0;
                while i < node.exprs.len() - 1 {
                    self.visit_and_get_object(Node::from(node.exprs[i].as_ref()), conditional);
                    i += 1;
                }

                self.visit_and_get_object(Node::from(node.exprs[i].as_ref()), conditional)
            }
            Node::YieldExpr(node) => {
                if let Some(arg) = &node.arg {
                    let arg = self.visit_and_get_object(Node::from(arg.as_ref()), conditional);
                    self.analysis.store.invalidate(arg, &self.lattice);
                }
                None
            }
            Node::AwaitExpr(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional)
            }
            Node::Tpl(node) => {
                for expr in &node.exprs {
                    self.visit_and_get_object(Node::from(expr.as_ref()), conditional);
                }
                None
            }
            Node::TaggedTpl(node) => {
                self.visit_and_get_object(Node::from(node.tag.as_ref()), conditional);
                for expr in &node.tpl.exprs {
                    let obj = self.visit_and_get_object(Node::from(expr.as_ref()), conditional);
                    // Expressions in tagged templates can be accessed by the tag function.
                    self.analysis.store.invalidate(obj, &self.lattice);
                }
                None
            }
            Node::ParenExpr(node) => {
                self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional)
            }
            Node::Super(_) => todo!(),
            Node::OptChainExpr(_) => todo!(),
            Node::Function(_) => todo!(),
            Node::Param(_) => todo!(),
            Node::ParamWithoutDecorators(_) => todo!(),
            Node::BindingIdent(node) => {
                self.visit_and_get_object(Node::Ident(&node.id), conditional)
            }
            Node::Ident(node) => {
                let id = node.to_id();
                self.lattice
                    .var_assignments
                    .get(&id)
                    .and_then(|assign| assign.rhs)
            }
            Node::PrivateName(_) => todo!(),

            Node::Str(_)
            | Node::Bool(_)
            | Node::Null(_)
            | Node::Number(_)
            | Node::BigInt(_)
            | Node::Regex(_)
            | Node::TplElement(_)
            | Node::MetaPropExpr(_) => None,

            Node::ImportDefaultSpecifier(_) => todo!(),
            Node::ImportStarAsSpecifier(_) => todo!(),
            Node::ImportNamedSpecifier(_) => todo!(),
            Node::ExportNamespaceSpecifier(_) => todo!(),
            Node::ExportDefaultSpecifier(_) => todo!(),
            Node::ExportNamedSpecifier(_) => todo!(),
            Node::ComputedPropName(node) => {
                self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional);
                None
            }
            Node::SwitchCase(_) => todo!(),
            Node::CatchClause(_) => todo!(),

            // This function is only called on expressions (and their children),
            // so it can't reach e.g. statements. TypeScript and JSX should have
            // been removed by now as well.
            _ => unreachable!(),
        }
    }

    fn init(&mut self, node: Node<'ast>, conditional: bool) {
        match node {
            Node::IfStmt(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
            }
            Node::ExprStmt(node) => {
                self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional);
            }
            Node::BlockStmt(_) => {}
            Node::ForStmt(node) => {
                if let Some(test) = &node.test {
                    self.visit_and_get_object(Node::from(test.as_ref()), conditional);
                }
            }
            Node::Function(_) => {
                // TODO: params:
            }
            Node::VarDecl(node) => {
                for decl in &node.decls {
                    self.visit_and_get_object(Node::VarDeclarator(decl), conditional);
                }
            }
            Node::ThisExpr(_)
            | Node::ArrayLit(_)
            | Node::ObjectLit(_)
            | Node::SpreadElement(_)
            | Node::UnaryExpr(_)
            | Node::UpdateExpr(_)
            | Node::BinExpr(_)
            | Node::FnExpr(_)
            | Node::ClassExpr(_)
            | Node::AssignExpr(_)
            | Node::MemberExpr(_)
            | Node::CondExpr(_)
            | Node::CallExpr(_)
            | Node::NewExpr(_)
            | Node::SeqExpr(_)
            | Node::ArrowExpr(_)
            | Node::YieldExpr(_)
            | Node::MetaPropExpr(_)
            | Node::AwaitExpr(_)
            | Node::Tpl(_)
            | Node::TaggedTpl(_)
            | Node::TplElement(_)
            | Node::ParenExpr(_)
            | Node::Super(_)
            | Node::OptChainExpr(_)
            | Node::BindingIdent(_)
            | Node::Ident(_)
            | Node::PrivateName(_) => {
                self.visit_and_get_object(node, conditional);
            }

            Node::ImplicitReturn => todo!(),
            Node::Class(_) => todo!(),
            Node::ExtendsClause(_) => todo!(),
            Node::ClassProp(_) => todo!(),
            Node::PrivateProp(_) => todo!(),
            Node::ClassMethod(_) => todo!(),
            Node::PrivateMethod(_) => todo!(),
            Node::Constructor(_) => todo!(),
            Node::Decorator(_) => todo!(),
            Node::FnDecl(_) => todo!(),
            Node::ClassDecl(_) => todo!(),
            Node::VarDeclarator(_) => todo!(),
            Node::Param(_) => todo!(),
            Node::ParamWithoutDecorators(_) => todo!(),
            Node::Invalid(_) => todo!(),
            Node::ExportDefaultExpr(_) => todo!(),
            Node::ExportDecl(_) => todo!(),
            Node::ImportDecl(_) => todo!(),
            Node::ExportAll(_) => todo!(),
            Node::NamedExport(_) => todo!(),
            Node::ExportDefaultDecl(_) => todo!(),
            Node::ImportDefaultSpecifier(_) => todo!(),
            Node::ImportStarAsSpecifier(_) => todo!(),
            Node::ImportNamedSpecifier(_) => todo!(),
            Node::ExportNamespaceSpecifier(_) => todo!(),
            Node::ExportDefaultSpecifier(_) => todo!(),
            Node::ExportNamedSpecifier(_) => todo!(),
            Node::Script(_) => todo!(),
            Node::Module(_) => todo!(),
            Node::ArrayPat(_) => todo!(),
            Node::ObjectPat(_) => todo!(),
            Node::AssignPat(_) => todo!(),
            Node::RestPat(_) => todo!(),
            Node::KeyValuePatProp(_) => todo!(),
            Node::AssignPatProp(_) => todo!(),
            Node::KeyValueProp(_) => todo!(),
            Node::AssignProp(_) => todo!(),
            Node::GetterProp(_) => todo!(),
            Node::SetterProp(_) => todo!(),
            Node::MethodProp(_) => todo!(),
            Node::ComputedPropName(_) => todo!(),
            Node::SpreadAssignment(_) => todo!(),
            Node::DebuggerStmt(_) => todo!(),
            Node::WithStmt(_) => todo!(),
            Node::ReturnStmt(node) => {
                if let Some(arg) = &node.arg {
                    let obj = self.visit_and_get_object(Node::from(arg.as_ref()), conditional);
                    self.analysis.store.invalidate(obj, &self.lattice);
                }
            }
            Node::LabeledStmt(_) => todo!(),
            Node::SwitchStmt(_) => todo!(),
            Node::ThrowStmt(_) => todo!(),
            Node::TryStmt(_) => todo!(),
            Node::WhileStmt(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
            }
            Node::DoWhileStmt(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
            }
            Node::ForInStmt(_) => todo!(),
            Node::ForOfStmt(_) => todo!(),
            Node::SwitchCase(_) => todo!(),
            Node::CatchClause(_) => todo!(),

            Node::Str(_)
            | Node::Bool(_)
            | Node::Null(_)
            | Node::Number(_)
            | Node::BigInt(_)
            | Node::Regex(_)
            | Node::EmptyStmt(_)
            | Node::BreakStmt(_)
            | Node::ContinueStmt(_) => {}

            _ => unreachable!("{:#?}", node),
        }
    }
}

impl<'ast> DataFlowAnalysisInner<Node<'ast>, Lattice, JoinOp> for Inner<'ast> {
    fn add_lattice_element(&mut self, element: Lattice) -> LatticeElementId {
        self.lattice_elements.push(element)
    }

    fn isForward(&self) -> bool {
        true
    }

    fn createEntryLattice(&mut self) -> LatticeElementId {
        self.add_lattice_element(Lattice::default())
    }

    fn createInitialEstimateLattice(&mut self) -> LatticeElementId {
        self.createEntryLattice()
    }

    fn createFlowJoiner(&self) -> JoinOp {
        JoinOp::default()
    }

    fn flowThrough(&mut self, node: Node<'ast>, input: LatticeElementId) -> LatticeElementId {
        // Make assignments conditional if the node can end abruptly by an exception.
        let conditional = self
            .cfg
            .graph
            .edges(self.cfg.map[&node])
            .any(|e| *e.weight() == Branch::ON_EX);

        // TODO: use Cow to avoid cloning and doing equality checks for nodes
        // that don't change the state. If the Cow is still Cow::Borrowed after
        // the analysis then there is no need for equality checks.
        let mut new = self.lattice_elements[input].clone();

        self.analyze_node(node, &mut new, conditional);

        if new != self.lattice_elements[input] {
            self.add_lattice_element(new)
        } else {
            // No changes compared to input.
            input
        }
    }

    fn cfg(&self) -> &ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId> {
        &self.cfg
    }
    fn cfg_mut(&mut self) -> &mut ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId> {
        &mut self.cfg
    }
}

impl Index<LatticeElementId> for Inner<'_> {
    type Output = Lattice;

    fn index(&self, index: LatticeElementId) -> &Self::Output {
        &self.lattice_elements[index]
    }
}

#[derive(Default)]
struct JoinOp {
    result: Lattice,
}

impl<'ast> FlowJoiner<Lattice, Inner<'ast>> for JoinOp {
    fn joinFlow(&mut self, inner: &mut Inner<'ast>, input: LatticeElementId) {
        let input = &inner.lattice_elements[input];

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
                    std::collections::hash_map::Entry::Occupied(entry) => {
                        inner
                            .store
                            .record_conflation(entry.get().rhs, prop.rhs, &self.result);
                    }
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        entry.insert(prop.clone());
                    }
                }
            }
        }

        // Merge variable assignments.
        for (name, assignment) in &input.var_assignments {
            match self.result.var_assignments.entry(name.clone()) {
                std::collections::hash_map::Entry::Occupied(entry) => {
                    inner
                        .store
                        .record_conflation(entry.get().rhs, assignment.rhs, &self.result);
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(assignment.clone());
                }
            }
        }
    }

    fn finish(self) -> Lattice {
        self.result
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Lattice {
    var_assignments: FxHashMap<Id, Assignment>,
    prop_assignments: FxHashMap<ObjectId, FxHashMap<JsWord, Assignment>>,
}

impl Annotation for Lattice {}
impl LatticeElement for Lattice {}

struct Renamer {
    rename_map: FxHashMap<NodeId, JsWord>,
}

impl VisitMut<'_> for Renamer {
    fn visit_mut_ident(&mut self, node: &mut Ident) {
        if let Some(new_name) = self.rename_map.get(&node.node_id) {
            node.sym = new_name.clone();
        }
    }
}
