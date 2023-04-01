#![warn(non_snake_case)]
#![warn(non_upper_case_globals)]
#![warn(unused_variables)]
#![deny(non_shorthand_field_patterns)]
#![warn(warnings)]
#![warn(dead_code)]
#![deny(unused_imports)]

use std::ops::Index;

use ast::*;
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use global_common::{SyntaxContext, DUMMY_SP};
use index::bit_set::{BitMatrix, BitSet, GrowableBitSet};
use index::vec::{Idx, IndexVec};
use rustc_hash::{FxHashMap, FxHashSet};
use swc_atoms::{js_word, JsWord};

use crate::control_flow::node::Node;
use crate::control_flow::ControlFlowAnalysis::*;
use crate::control_flow::ControlFlowGraph::*;
use crate::convert::ecma_number_to_string;
use crate::find_vars::*;
use crate::utils::unwrap_as;
use crate::DefaultNameGenerator::DefaultNameGenerator;
use crate::{DataFlowAnalysis::*, Id, ToId};

#[cfg(test)]
mod tests;

fn create_renaming_map(store: &Store) -> FxHashMap<NodeId, JsWord> {
    #[derive(Default, Debug)]
    struct Object {
        invalid: bool,
        properties: FxHashMap<JsWord, FrontId>,
    }

    #[derive(Default, Debug)]
    struct Property {
        name: JsWord,
        references: FxHashSet<NodeId>,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    struct FrontId(usize);
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    struct BackId(usize);

    let mut cur_front_id = FrontId(0);
    let mut cur_back_id = BackId(0);

    let mut objects: FxHashMap<ObjectId, Object> = FxHashMap::default();
    let mut property_map: FxHashMap<FrontId, BackId> = FxHashMap::default();
    let mut properties: FxHashMap<BackId, Property> = FxHashMap::default();

    let mut union_accesses = FxHashMap::<_, FxHashMap<_, FxHashSet<_>>>::default();

    for (PropKey(name, node_id), &pointer) in &store.references {
        if let Some(pointer) = pointer {
            match pointer {
                Pointer::Object(object_id) => {
                    let object = objects.entry(object_id).or_default();
                    object.invalid |= store.invalid_objects.contains(object_id);
                    let id = match object.properties.entry(name.clone()) {
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            let id = entry.get();
                            property_map[id]
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            properties.insert(
                                cur_back_id,
                                Property {
                                    name: name.clone(),
                                    references: FxHashSet::default(),
                                },
                            );
                            property_map.insert(cur_front_id, cur_back_id);
                            let back_id = cur_back_id;
                            entry.insert(cur_front_id);
                            cur_front_id = FrontId(cur_front_id.0 + 1);
                            cur_back_id = BackId(cur_back_id.0 + 1);
                            back_id
                        }
                    };
                    properties.get_mut(&id).unwrap().references.insert(*node_id);
                }
                Pointer::Union(union) => {
                    union_accesses
                        .entry(union)
                        .or_default()
                        .entry(name)
                        .or_default()
                        .insert(*node_id);
                    for &constituent in &store.unions[union].constituents {
                        objects.entry(constituent).or_default();
                    }
                }
            }
        } else {
            todo!();
        }
    }

    for (union, accesses) in union_accesses {
        let invalid = store.invalidated(Pointer::Union(union));
        for (name, references) in accesses {
            let mut new_prop = Property {
                name: name.clone(),
                references,
            };
            for &constituent in &store.unions[union].constituents {
                let constituent = objects.entry(constituent).or_default();
                constituent.invalid |= invalid;
                if let Some(&existing_id) = constituent.properties.get(name) {
                    if let Some(existing_id) = property_map.get(&existing_id) {
                        if let Some(existing) = properties.remove(existing_id) {
                            new_prop.references.extend(existing.references);
                        }
                    }
                    property_map.insert(existing_id, cur_back_id);
                }
                constituent.properties.insert(name.clone(), cur_front_id);
            }
            properties.insert(cur_back_id, new_prop);
            property_map.insert(cur_front_id, cur_back_id);
            cur_front_id = FrontId(cur_front_id.0 + 1);
            cur_back_id = BackId(cur_back_id.0 + 1);
        }
    }

    let mut props = Vec::with_capacity(properties.len());
    for obj in objects.values() {
        if obj.invalid {
            continue;
        }
        for prop in obj.properties.values() {
            let id = property_map[prop];
            props.push(id);
        }
    }
    props.sort_unstable_by(|a, b| {
        let result = properties[b]
            .references
            .len()
            .cmp(&properties[a].references.len());
        if result.is_eq() {
            properties[a].name.cmp(&properties[b].name)
        } else {
            result
        }
    });

    let mut graph_map = FxHashMap::default();

    for (id, prop) in props.iter().enumerate() {
        graph_map.insert(*prop, id);
    }

    let mut remaining_nodes = BitSet::<usize>::new_filled(props.len());

    let mut graph = BitMatrix::new(props.len(), props.len());

    for obj in objects.values() {
        if obj.invalid {
            continue;
        }
        let mut outer = obj.properties.values();
        while let Some(outer_prop) = outer.next() {
            let outer_prop = match property_map.get(outer_prop) {
                Some(p) => p,
                None => continue,
            };
            let outer_node = graph_map[outer_prop];
            for inner_prop in outer.clone() {
                let inner_prop = match property_map.get(inner_prop) {
                    Some(p) => p,
                    None => continue,
                };
                let inner_node = graph_map[&inner_prop];
                graph.insert(outer_node, inner_node);
                graph.insert(inner_node, outer_node);
            }
        }
    }

    let mut colours = vec![0u8; props.len()];

    let mut cur_colour = 0u8;
    let mut subgraph = BitSet::new_empty(props.len());
    loop {
        subgraph.clear();
        let mut cur_word_offset = 0;
        let mut offset = 0;
        while cur_word_offset < remaining_nodes.words().len() {
            let mut word = remaining_nodes.words()[cur_word_offset];

            while word != 0 {
                // Get the position of the next set bit in the current word,
                // then clear the bit.
                let bit_pos = word.trailing_zeros() as usize;
                let bit = 1 << bit_pos;
                word ^= bit;
                let node = bit_pos + offset;
                if !graph.iter(node).any(|n| subgraph.contains(n)) {
                    subgraph.insert(node);
                    colours[node] = cur_colour;
                    remaining_nodes.remove(node);
                }
            }

            cur_word_offset += 1;
            offset += index::bit_set::WORD_BITS;
        }
        cur_colour += 1;

        if remaining_nodes.is_empty() {
            break;
        }
    }

    // Generate new names for the properties that will be renamed.
    let mut name_gen = DefaultNameGenerator::new(FxHashSet::default());
    let mut colour_map = Vec::with_capacity(cur_colour as usize);
    for _ in 0..cur_colour {
        colour_map.push(name_gen.generateNextName());
    }

    let mut rename_map = FxHashMap::default();

    // Translate the colour of each Property instance to a name.
    for id in props {
        let node = graph_map[&id];
        let colour = colours[node];
        let new_name = &colour_map[colour as usize];
        for &reference in &properties[&id].references {
            rename_map.insert(reference, new_name.clone());
        }
    }

    rename_map
}

pub fn process(
    ast: &mut ast::Program,
    node_id_gen: &mut NodeIdGen,
    unresolved_ctxt: SyntaxContext,
) {
    let null_or_void = ObjectId::from_u32(0);
    let mut store = Store {
        objects_map: FxHashMap::default(),
        invalid_objects: GrowableBitSet::new_empty(),
        unions: IndexVec::default(),
        references: FxHashMap::default(),
        null_or_void,
        cur_object_id: ObjectId::from_u32(1),
        unresolved_ctxt,
    };

    // Find all property references and record the types on which they occur.
    let mut visitor = GlobalVisitor { store: &mut store };
    ast.visit_with(&mut visitor);

    let rename_map = create_renaming_map(&store);

    // Actually assign the new names.
    let mut renamer = Renamer {
        node_id_gen,
        rename_map,
    };

    ast.visit_mut_with(&mut renamer);
}

fn handle_fn<'a, T>(store: &mut Store, node: &'a T)
where
    T: FunctionLike<'a> + VisitWith<'a, DeclFinder>,
    &'a T: Into<ControlFlowRoot<'a>>,
{
    let vars = find_vars_declared_in_fn(node, false);

    let cfa = ControlFlowAnalysis::analyze(node.into(), false);

    let mut analysis = Analysis::new(cfa, vars.scopeVariables, store);
    analysis.data_flow_analysis.analyze();

    // result.1.print_full_with_annotations::<DefaultPrinter>(None);

    // dbg!(result
    //     .0
    //     .lattice_elements
    //     .iter_enumerated()
    //     .collect::<Vec<_>>());

    node.visit_body_with(&mut FnVisitor { store });
}

struct FnVisitor<'s> {
    store: &'s mut Store,
}

impl Visit<'_> for FnVisitor<'_> {
    fn visit_function(&mut self, node: &Function) {
        handle_fn(self.store, node);
    }
    fn visit_constructor(&mut self, node: &Constructor) {
        handle_fn(self.store, node);
    }
    fn visit_arrow_expr(&mut self, node: &ArrowExpr) {
        handle_fn(self.store, node);
    }
    fn visit_getter_prop(&mut self, node: &GetterProp) {
        handle_fn(self.store, node);
    }
    fn visit_setter_prop(&mut self, node: &SetterProp) {
        handle_fn(self.store, node);
    }
}

struct GlobalVisitor<'s> {
    store: &'s mut Store,
}

impl Visit<'_> for GlobalVisitor<'_> {
    fn visit_function(&mut self, node: &Function) {
        handle_fn(self.store, node);
    }
    fn visit_constructor(&mut self, node: &Constructor) {
        handle_fn(self.store, node);
    }
    fn visit_arrow_expr(&mut self, node: &ArrowExpr) {
        handle_fn(self.store, node);
    }
    fn visit_getter_prop(&mut self, node: &GetterProp) {
        handle_fn(self.store, node);
    }
    fn visit_setter_prop(&mut self, node: &SetterProp) {
        handle_fn(self.store, node);
    }
}

pub struct Analysis<'ast, 's> {
    data_flow_analysis: DataFlowAnalysis<Node<'ast>, Inner<'ast, 's>, Lattice, JoinOp>,
}

impl<'ast, 's> Analysis<'ast, 's> {
    fn new(
        cfa: ControlFlowAnalysisResult<Node<'ast>, LinearFlowState, LatticeElementId>,
        vars: FxHashMap<Id, VarId>,
        store: &'s mut Store,
    ) -> Self {
        let inner = Inner {
            lattice_elements: IndexVec::default(),
            cfg: cfa.cfg,

            vars,

            store,
        };
        let data_flow_analysis = DataFlowAnalysis::new(inner, cfa.nodePriorities);

        Self { data_flow_analysis }
    }
}

#[derive(Debug)]
/// Info collected during analysis.
struct Store {
    objects_map: FxHashMap<NodeId, ObjectId>,
    invalid_objects: GrowableBitSet<ObjectId>,
    unions: IndexVec<UnionId, Union>,

    references: FxHashMap<PropKey, Option<Pointer>>,

    null_or_void: ObjectId,

    cur_object_id: ObjectId,
    unresolved_ctxt: SyntaxContext,
}

impl Store {
    fn next_object_id(&mut self) -> ObjectId {
        let id = self.cur_object_id;
        self.cur_object_id.increment_by(1);
        id
    }

    /// Returns a [`Pointer`] to a union of `pointer1`'s type and `pointer2`'s type.
    fn create_union(
        &mut self,
        pointer1: Option<Pointer>,
        pointer2: Option<Pointer>,
    ) -> Option<Pointer> {
        // TODO: invalidate if either is None. This can be avoided if we have a Some() and a None,
        // and the None represents a literal/primitive, in which case we know its properties and
        // the conflated object's props will be the properties of the non-primitive argument (Some)
        // subtract the primitive's props.
        // This is simplified if the primitive is null/undefined, since they have no props.
        let (id1, id2) = match (pointer1, pointer2) {
            (None, None) => return None,
            (None, Some(id2)) => {
                return Some(id2);
            }
            (Some(id1), None) => {
                return Some(id1);
            }
            (Some(id1), Some(id2)) => (id1, id2),
        };

        if id1 == id2 {
            return Some(id1);
        }

        let mut constituents = FxHashSet::default();

        match (id1, id2) {
            (Pointer::Object(left), Pointer::Object(right)) => {
                constituents.insert(left);
                constituents.insert(right);
            }
            (Pointer::Object(obj), Pointer::Union(union)) => {
                constituents.insert(obj);
                constituents.extend(self.unions[union].constituents.iter().copied());
            }
            (Pointer::Union(union), Pointer::Object(obj)) => {
                constituents.insert(obj);
                constituents.extend(self.unions[union].constituents.iter().copied());
            }
            (Pointer::Union(left), Pointer::Union(right)) => {
                constituents.extend(self.unions[left].constituents.iter().copied());
                constituents.extend(self.unions[right].constituents.iter().copied());
            }
        }

        let union = Union { constituents };

        if let Some((existing, _)) = self.unions.iter_enumerated().find(|(_, u)| *u == &union) {
            Some(Pointer::Union(existing))
        } else {
            Some(Pointer::Union(self.unions.push(union)))
        }
    }

    /// Recursively invalidates the entity that `pointer` points to.
    fn invalidate(&mut self, pointer: Option<Pointer>, lattice: &Lattice) {
        if let Some(pointer) = pointer {
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
                                    if !done.contains(&value) {
                                        queue.push(value);
                                    }
                                }
                            }
                        }
                    }
                    Pointer::Union(union) => {
                        for &constituent in &self.unions[union].constituents {
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
        match pointer {
            Pointer::Object(obj) => self.invalid_objects.contains(obj),
            Pointer::Union(union) => self.unions[union]
                .constituents
                .iter()
                .any(|&obj| self.invalid_objects.contains(obj)),
        }
    }
}

#[derive(Debug)]
struct Inner<'ast, 's> {
    lattice_elements: IndexVec<LatticeElementId, Lattice>,
    cfg: ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,

    vars: FxHashMap<Id, VarId>,

    store: &'s mut Store,
}

impl<'ast> Inner<'ast, '_> {
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
enum Pointer {
    Object(ObjectId),
    Union(UnionId),
}

index::newtype_index!(struct ObjectId { .. });
index::newtype_index!(struct UnionId { .. });

#[derive(Debug, PartialEq)]
struct Union {
    constituents: FxHashSet<ObjectId>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Assignment {
    rhs: Option<Pointer>,
}

/// A place where a variable can be stored.
#[derive(Debug)]
enum Slot {
    Var(Id),
    /// (Object, Property name)
    Prop(Pointer, JsWord),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
struct PropKey(JsWord, NodeId);

impl PropKey {
    fn from_prop_name(prop: &PropName, unresolved_ctxt: SyntaxContext) -> Option<PropKey> {
        match prop {
            PropName::Ident(p) => Some(PropKey(p.sym.clone(), p.node_id)),
            PropName::Str(p) => Some(PropKey(p.value.clone(), p.node_id)),
            PropName::Num(p) => Some(PropKey(ecma_number_to_string(p.value).into(), p.node_id)),
            PropName::Computed(p) => PropKey::from_expr(&p.expr, unresolved_ctxt, true),
        }
    }

    fn from_expr(expr: &Expr, unresolved_ctxt: SyntaxContext, computed: bool) -> Option<PropKey> {
        match expr {
            Expr::Ident(e) => {
                if !computed
                    || e.span.ctxt == unresolved_ctxt
                        && (e.sym == js_word!("undefined") || e.sym == js_word!("NaN"))
                {
                    Some(PropKey(e.sym.clone(), e.node_id))
                } else {
                    None
                }
            }
            Expr::Lit(e) => match e {
                Lit::Str(e) => Some(PropKey(e.value.clone(), e.node_id)),
                Lit::Bool(e) => {
                    if e.value {
                        Some(PropKey(js_word!("true"), e.node_id))
                    } else {
                        Some(PropKey(js_word!("false"), e.node_id))
                    }
                }
                Lit::Null(e) => Some(PropKey(js_word!("null"), e.node_id)),
                Lit::Num(e) => Some(PropKey(ecma_number_to_string(e.value).into(), e.node_id)),
                Lit::BigInt(e) => Some(PropKey(e.value.to_str_radix(10).into(), e.node_id)),
                Lit::Regex(_) | Lit::JSXText(_) => None,
            },
            _ => None,
        }
    }
}

/// Returns true if the string value of the [`PropName`] is statically determinable.
fn is_simple_prop_name(prop_name: &PropName, unresolved_ctxt: SyntaxContext) -> bool {
    match prop_name {
        PropName::Ident(_) | PropName::Str(_) | PropName::Num(_) => true,
        PropName::Computed(p) => match p.expr.as_ref() {
            Expr::Lit(e) => match e {
                Lit::Str(_) | Lit::Bool(_) | Lit::Null(_) | Lit::Num(_) | Lit::BigInt(_) => true,
                Lit::Regex(_) | Lit::JSXText(_) => false,
            },
            Expr::Ident(e) => {
                e.span.ctxt == unresolved_ctxt
                    && (e.sym == js_word!("undefined") || e.sym == js_word!("NaN"))
            }
            _ => false,
        },
    }
}

#[derive(Debug)]
struct Analyser<'ast, 'a, 's> {
    analysis: &'a mut Inner<'ast, 's>,
    lattice: &'a mut Lattice,
}

impl<'ast> Analyser<'ast, '_, '_> {
    fn get_property(&mut self, pointer: Pointer, key: &JsWord) -> Option<Assignment> {
        match pointer {
            Pointer::Object(obj) => self
                .lattice
                .prop_assignments
                .get(&obj)
                .and_then(|props| props.get(key))
                .copied(),
            Pointer::Union(union) => {
                let mut constituents = FxHashSet::default();

                let mut invalid = false;

                for constituent in &self.analysis.store.unions[union].constituents {
                    let constituent = self
                        .lattice
                        .prop_assignments
                        .get(&constituent)
                        .and_then(|props| props.get(key))
                        .and_then(|assign| assign.rhs);

                    match constituent {
                        Some(Pointer::Object(obj)) => {
                            constituents.insert(obj);
                        }
                        Some(Pointer::Union(union)) => {
                            constituents.extend(
                                self.analysis.store.unions[union]
                                    .constituents
                                    .iter()
                                    .copied(),
                            );
                        }
                        None => {
                            invalid = true;
                        }
                    };
                }

                if constituents.len() == 0 {
                    return None;
                }

                let rhs = if constituents.len() == 1 {
                    Pointer::Object(constituents.into_iter().next().unwrap())
                } else {
                    let union = Union { constituents };
                    Pointer::Union(self.analysis.store.unions.push(union))
                };
                let result = Assignment { rhs: Some(rhs) };

                if invalid {
                    self.analysis.store.invalidate(result.rhs, &self.lattice);
                }

                Some(result)
            }
        }
    }

    fn reference_prop(&mut self, object: Option<Pointer>, key: PropKey) {
        self.analysis.store.references.insert(key, object);
    }

    /// Assigns the given value to the [`Slot`] with the given name. Any existing
    /// value is replaced, unless this assignment is conditional, in which case
    /// the slot is assigned a union of the old and new values.
    fn assign_to_slot(
        &mut self,
        slot: Option<Slot>,
        rhs: Option<Pointer>,
        conditional: bool,
    ) -> Option<Pointer> {
        if let Some(slot) = slot {
            let existing = match &slot {
                Slot::Var(name) => self.lattice.var_assignments.get(&name).cloned(),
                Slot::Prop(obj, key) => self.get_property(*obj, &key),
            };

            let new = if let Some(rhs_object) = rhs {
                if let Some(existing) = existing {
                    let rhs = if !conditional {
                        // supersede
                        Some(rhs_object)
                    } else {
                        // union
                        let existing = existing.rhs;
                        self.analysis.store.create_union(existing, Some(rhs_object))
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
                    if self.analysis.store.invalidated(obj) {
                        self.analysis.store.invalidate(rhs, &self.lattice);
                    }
                    match obj {
                        Pointer::Object(obj) => {
                            self.lattice
                                .prop_assignments
                                .entry(obj)
                                .or_default()
                                .insert(key, new);
                        }
                        Pointer::Union(union) => {
                            for &constituent in &self.analysis.store.unions[union].constituents {
                                self.lattice
                                    .prop_assignments
                                    .entry(constituent)
                                    .or_default()
                                    .insert(key.clone(), new);
                            }
                        }
                    }
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
                Slot::Var(name) => self.lattice.var_assignments.get(&name).copied(),
                Slot::Prop(obj, key) => self.get_property(obj, &key),
            };
            self.analysis
                .store
                .invalidate(value.and_then(|a| a.rhs), &self.lattice);
        }
    }

    fn visit_destructuring(&mut self, lhs: Node<'ast>, rhs: Option<Pointer>, conditional: bool) {
        match lhs {
            Node::ObjectPat(lhs) => {
                let has_complex_props = lhs.props.iter().any(|p| match p {
                    ObjectPatProp::KeyValue(p) => {
                        !is_simple_prop_name(&p.key, self.analysis.store.unresolved_ctxt)
                    }
                    ObjectPatProp::Assign(_) | ObjectPatProp::Rest(_) => false,
                });
                if let Some(rhs) = rhs {
                    if has_complex_props {
                        self.analysis.store.invalidate(Some(rhs), &self.lattice);
                    }
                    if !self.analysis.store.invalidated(rhs) {
                        for prop in &lhs.props {
                            match prop {
                                ObjectPatProp::KeyValue(prop) => {
                                    let key = PropKey::from_prop_name(
                                        &prop.key,
                                        self.analysis.store.unresolved_ctxt,
                                    )
                                    .unwrap();
                                    self.reference_prop(Some(rhs), key.clone());
                                    let rhs_value =
                                        self.get_property(rhs, &key.0).and_then(|a| a.rhs);

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

                let result = self.analysis.store.create_union(rhs, default_value);

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
    fn visit_and_get_slot(&mut self, node: Node<'ast>, conditional: bool) -> Option<Slot> {
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
                if let Some(prop) = PropKey::from_expr(
                    &node.prop,
                    self.analysis.store.unresolved_ctxt,
                    node.computed,
                ) {
                    obj.map(|obj| {
                        self.reference_prop(Some(obj), prop.clone());
                        Slot::Prop(obj, prop.0)
                    })
                } else {
                    self.analysis.store.invalidate(obj, &self.lattice);
                    self.visit_and_get_slot(Node::from(node.prop.as_ref()), conditional);
                    None
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
                    Some(Pointer::Object(*existing))
                } else {
                    let object_id = self.analysis.store.next_object_id();

                    self.analysis
                        .store
                        .objects_map
                        .insert(node.node_id, object_id);

                    let pointer = Pointer::Object(object_id);

                    let is_simple_obj_lit = node.props.iter().all(|p| match p {
                        Prop::KeyValue(p) => {
                            is_simple_prop_name(&p.key, self.analysis.store.unresolved_ctxt)
                        }
                        _ => false,
                    });

                    if is_simple_obj_lit {
                        for prop in &node.props {
                            let prop = unwrap_as!(prop, Prop::KeyValue(p), p);
                            let key = PropKey::from_prop_name(
                                &prop.key,
                                self.analysis.store.unresolved_ctxt,
                            )
                            .unwrap();

                            self.reference_prop(Some(pointer), key.clone());

                            let value = self
                                .visit_and_get_object(Node::from(prop.value.as_ref()), conditional);

                            self.assign_to_slot(
                                Some(Slot::Prop(pointer, key.0)),
                                value,
                                conditional,
                            );
                        }
                    } else {
                        self.analysis.store.invalidate(Some(pointer), &self.lattice);
                    }
                    Some(pointer)
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
                        self.analysis.store.create_union(left, right)
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
                if let Some(prop) = PropKey::from_expr(
                    &node.prop,
                    self.analysis.store.unresolved_ctxt,
                    node.computed,
                ) {
                    if let Some(obj) = obj {
                        self.reference_prop(Some(obj), prop.clone());
                        if let Some(value) = self.get_property(obj, &prop.0).map(|a| a.rhs) {
                            value
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    self.analysis.store.invalidate(obj, &self.lattice);
                    self.visit_and_get_object(Node::from(node.prop.as_ref()), conditional);
                    None
                }
            }
            Node::CondExpr(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
                let cons = self.visit_and_get_object(Node::from(node.cons.as_ref()), true);
                let alt = self.visit_and_get_object(Node::from(node.alt.as_ref()), true);
                self.analysis.store.create_union(cons, alt)
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

            Node::Null(_) => Some(Pointer::Object(self.analysis.store.null_or_void)),

            Node::Str(_)
            | Node::Bool(_)
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

impl<'ast> DataFlowAnalysisInner<Node<'ast>, Lattice, JoinOp> for Inner<'ast, '_> {
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

impl Index<LatticeElementId> for Inner<'_, '_> {
    type Output = Lattice;

    fn index(&self, index: LatticeElementId) -> &Self::Output {
        &self.lattice_elements[index]
    }
}

#[derive(Default)]
struct JoinOp {
    result: Lattice,
}

impl<'ast> FlowJoiner<Lattice, Inner<'ast, '_>> for JoinOp {
    fn joinFlow(&mut self, inner: &mut Inner<'ast, '_>, input: LatticeElementId) {
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
                    std::collections::hash_map::Entry::Occupied(mut entry) => {
                        let union = inner.store.create_union(entry.get().rhs, prop.rhs);
                        entry.insert(Assignment { rhs: union });
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
                std::collections::hash_map::Entry::Occupied(mut entry) => {
                    let union = inner.store.create_union(entry.get().rhs, assignment.rhs);
                    entry.insert(Assignment { rhs: union });
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

struct Renamer<'a> {
    node_id_gen: &'a mut NodeIdGen,
    rename_map: FxHashMap<NodeId, JsWord>,
}

// TODO: node id's are unique. use rename_map.remove to get owned JsWord - no other node will access the entry anyway.
impl VisitMut<'_> for Renamer<'_> {
    fn visit_mut_ident(&mut self, node: &mut Ident) {
        if let Some(new_name) = self.rename_map.get(&node.node_id) {
            node.sym = new_name.clone();
        }
    }

    fn visit_mut_prop_name(&mut self, node: &mut PropName) {
        let node_id_to_rename = match node {
            // Handled by visit_mut_ident
            PropName::Ident(_) => None,
            PropName::Str(p) => Some(p.node_id),
            PropName::Num(p) => Some(p.node_id),
            PropName::Computed(p) => Some(p.expr.node_id()),
        };
        if let Some(node_id_to_rename) = node_id_to_rename {
            if let Some(new_name) = self.rename_map.get(&node_id_to_rename) {
                *node = PropName::Ident(Ident {
                    node_id: self.node_id_gen.next(),
                    span: DUMMY_SP,
                    sym: new_name.clone(),
                    optional: false,
                });
                return;
            }
        }
        node.visit_mut_children_with(self);
    }

    fn visit_mut_member_expr(&mut self, node: &mut MemberExpr) {
        if node.computed {
            if let Some(new_name) = self.rename_map.get(&node.prop.node_id()) {
                *node.prop.as_mut() = Expr::Ident(Ident {
                    node_id: self.node_id_gen.next(),
                    span: DUMMY_SP,
                    sym: new_name.clone(),
                    optional: false,
                });
                node.computed = false;
                return;
            }
        }
        node.visit_mut_children_with(self);
    }
}
