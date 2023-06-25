#![allow(non_snake_case)]
#![warn(non_upper_case_globals)]
#![warn(unused_variables)]
#![deny(non_shorthand_field_patterns)]
#![warn(warnings)]
#![warn(dead_code)]
#![deny(unused_imports)]

mod DataFlowAnalysis2;
mod function;
mod graph;
mod hashable_map;
mod simple_set;
mod template;
mod types;
mod unionfind;

use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::hash::Hash;
use std::ops::Deref;

use ast::*;
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use global_common::{SyntaxContext, DUMMY_SP};
use index::bit_set::{BitMatrix, BitSet, GrowableBitSet};
use index::vec::IndexVec;
use petgraph::prelude::UnGraph;
use petgraph::{
    graph::{DiGraph, Neighbors, NodeIndex},
    EdgeDirection::*,
};
use rustc_hash::{FxHashMap, FxHashSet};
use swc_atoms::{js_word, JsWord};

use crate::control_flow::node::{Node, NodeKind};
use crate::control_flow::ControlFlowAnalysis::*;
use crate::control_flow::ControlFlowGraph::*;
use crate::convert::ecma_number_to_string;
use crate::find_vars::*;
use crate::utils::unwrap_as;
use crate::DataFlowAnalysis::LatticeElementId;
use crate::DefaultNameGenerator::DefaultNameGenerator;
use crate::{Id, ToId};

use simple_set::IndexSet;
use template::*;
use unionfind::UnionFind;
use DataFlowAnalysis2::*;

use self::hashable_map::HashableHashMap;
use self::types::{ObjectId, ObjectStore, UnionBuilder, UnionId, UnionStore};

#[cfg(test)]
mod tests;

fn create_renaming_map(store: &Store) -> FxHashMap<NodeId, JsWord> {
    /*
        Idea:
        Assign properties (from different objects but with the same name) to the same
        bucket (equivalence class) if the must share the same name.

        Each property starts in its own bucket. Then, buckets are merged when:
        - One of their properties is accessed on a union of object types (the buckets
          for each object's version of the property are merged).
        - Their property exists on a 'root' and 'child' object. These are created by
          function calls, where each child object inherits the properties of its root.
          These inherited properties must share the same name (they represent the same
          slot), and so their buckets are merged.

        Once we have created all buckets, a representative `Property` is created for each
        bucket.

        These representatives are then added to a graph, and edges are added between
        representatives if they exist on the same object, and therefore must have distinct
        names. E.g. for `{ prop1: 1, prop2: 2 }; { prop3: 3 }`, the graph would have three
        nodes ('prop1', 'prop2', 'prop3'), and an edge between 'prop1' and 'prop2'.

        This graph is then coloured, assigning each representative a colour (number) such
        that no adjacent nodes have the same colour. Representatives with more references
        to their property name are coloured first, so they get lower colours (and the
        shortest names).

        These colours are then assigned names, with the lowest colours getting the shortest
        names.

        Finally, each representative's references are mapped to their new name based on the
        representative's colour.
    */

    #[derive(Default, Debug)]
    struct Object {
        properties: FxHashMap<JsWord, PropId>,
    }

    #[derive(Debug, Clone)]
    struct Property {
        name: JsWord,
        prop_id: PropId,
        references: FxHashSet<NodeId>,
        invalid: bool,
    }

    index::newtype_index!(struct PropId { .. });

    let mut objects: FxHashMap<ObjectId, Object> = FxHashMap::default();
    let mut properties: IndexVec<PropId, Property> = IndexVec::default();

    let mut union_accesses = FxHashMap::<_, FxHashMap<_, FxHashSet<_>>>::default();

    for (PropKey(name, node_id), &pointer) in &store.references {
        if let Some(pointer) = pointer {
            match pointer {
                Pointer::Object(object_id) => {
                    let object = objects.entry(object_id).or_default();
                    let id = match object.properties.entry(name.clone()) {
                        Entry::Occupied(entry) => *entry.get(),
                        Entry::Vacant(entry) => {
                            let prop_id = properties.push(Property {
                                name: name.clone(),
                                prop_id: properties.next_index(),
                                references: FxHashSet::default(),
                                invalid: store.invalid_objects.contains(object_id),
                            });
                            entry.insert(prop_id);
                            prop_id
                        }
                    };
                    properties[id].references.insert(*node_id);
                }
                Pointer::Union(union) => {
                    // Defer union processing.
                    union_accesses
                        .entry(union)
                        .or_default()
                        .entry(name)
                        .or_default()
                        .insert(*node_id);
                    for constituent in store.unions[union].constituents() {
                        objects.entry(constituent).or_default();
                    }
                }
                Pointer::Fn(_) | Pointer::NullOrVoid => {}
            }
        } else {
            todo!();
        }
    }

    let mut union_find = UnionFind::new(properties.len());

    // Process super/sub type relations.
    for (super_ty, sub_ty) in &store.object_links {
        objects.entry(*super_ty).or_default();
        objects.entry(*sub_ty).or_default();

        let invalid = store.invalid_objects.contains(*sub_ty);
        let [super_ty, sub_ty] = objects.get_many_mut([super_ty, sub_ty]).unwrap();
        for (name, super_prop) in &super_ty.properties {
            match sub_ty.properties.entry(name.clone()) {
                Entry::Occupied(entry) => {
                    union_find.union(*super_prop, *entry.get());
                }
                Entry::Vacant(entry) => {
                    entry.insert(*super_prop);
                    properties[*super_prop].invalid |= invalid;
                }
            }
        }
    }

    // Process unions.
    for (union, accesses) in union_accesses {
        let invalid = store.invalidated(Pointer::Union(union));

        for (name, references) in accesses {
            let mut props = store.unions[union]
                .constituents()
                .filter_map(|c| objects.get(&c).and_then(|c| c.properties.get(name)));

            let representative = if let Some(&representative) = props.next() {
                properties[representative].references.extend(references);
                properties[representative].invalid |= invalid;
                representative
            } else {
                let prop_id = properties.push(Property {
                    name: name.clone(),
                    prop_id: properties.next_index(),
                    references,
                    invalid,
                });
                union_find.add(prop_id);
                prop_id
            };

            for constituent in store.unions[union].constituents() {
                match objects
                    .get_mut(&constituent)
                    .unwrap()
                    .properties
                    .entry(name.clone())
                {
                    Entry::Occupied(entry) => {
                        union_find.union(representative, *entry.get());
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(representative);
                    }
                }
            }
        }
    }

    index::newtype_index!(struct RepId { .. });

    let labeling = union_find.into_labeling();

    // Create representatives for each equivalence class.
    let mut representatives = Vec::new();
    {
        let mut map = FxHashMap::default();
        for obj in objects.values() {
            for &prop in obj.properties.values() {
                let representative = labeling[prop.index()];
                let prop = &properties[prop];
                if prop.invalid {
                    continue;
                }
                let representative = *map.entry(representative).or_insert_with(|| {
                    let index = representatives.len();
                    representatives.push(properties[representative].clone());
                    index
                });
                let representative = &mut representatives[representative];
                representative
                    .references
                    .extend(prop.references.iter().copied());
            }
        }
    }

    // Sort representatives so those with more references have lower IDs.
    representatives.sort_unstable_by(|a, b| {
        let result = b.references.len().cmp(&a.references.len());
        if result.is_eq() {
            a.name.cmp(&b.name)
        } else {
            result
        }
    });

    // Assign representatives their IDs.
    let representatives: IndexVec<RepId, Property> = IndexVec::from_raw(representatives);
    let representatives_map: FxHashMap<PropId, RepId> = representatives
        .iter_enumerated()
        .map(|(id, prop)| (prop.prop_id, id))
        .collect();

    // Create property graph.
    // TODO: only need to store half/triangle since graph is undirected, and therefore reflexivity is implied.
    let mut graph = BitMatrix::new(representatives.len(), representatives.len());
    for obj in objects.values() {
        let mut outer = obj
            .properties
            .values()
            .filter_map(|p| representatives_map.get(&labeling[p.index()]));
        while let Some(&outer_node) = outer.next() {
            for &inner_node in outer.clone() {
                graph.insert(outer_node, inner_node);
                graph.insert(inner_node, outer_node);
            }
        }
    }

    const DEBUG_GRAPH: bool = false;

    if cfg!(debug_assertions) && DEBUG_GRAPH {
        let mut debug_graph: UnGraph<(RepId, JsWord), ()> = UnGraph::default();
        let mut debug_graph_map = FxHashMap::default();

        for obj in objects.values() {
            if obj.properties.len() == 1 {
                let prop = *obj.properties.values().next().unwrap();
                let rep_id = match representatives_map.get(&labeling[prop.index()]) {
                    Some(r) => *r,
                    None => continue,
                };
                debug_graph_map.entry(rep_id).or_insert_with(|| {
                    debug_graph.add_node((rep_id, properties[prop].name.clone()))
                });
                continue;
            }
            let mut outer = obj
                .properties
                .values()
                .filter_map(|p| representatives_map.get(&labeling[p.index()]));
            while let Some(&outer_node) = outer.next() {
                let a = *debug_graph_map.entry(outer_node).or_insert_with(|| {
                    debug_graph.add_node((outer_node, representatives[outer_node].name.clone()))
                });
                for &inner_node in outer.clone() {
                    let b = *debug_graph_map.entry(inner_node).or_insert_with(|| {
                        debug_graph.add_node((inner_node, representatives[inner_node].name.clone()))
                    });
                    debug_graph.update_edge(a, b, ());
                }
            }
        }

        let dot = format!(
            "{:?}",
            petgraph::dot::Dot::with_config(&debug_graph, &[petgraph::dot::Config::EdgeNoLabel])
        );

        std::fs::write("props.dot", dot).expect("Failed to output prop graph");
    }

    // Colour property graph:
    // Try to assign a colour to representatives, from most to least references
    // (lower to higher IDs). A colour can be assigned if none of the
    // representative's neighbours have the colour already. Once no more
    // representatives can be assigned the colour, move on the the next, until
    // no uncoloured representatives remain.
    let mut remaining_nodes = BitSet::<RepId>::new_filled(representatives.len());
    let mut colours = vec![0u16; representatives.len()];
    let mut cur_colour = 0u16;
    let mut subgraph = BitSet::new_empty(representatives.len());
    loop {
        subgraph.clear();
        // This is essentially just `BitSet::iter` inlined, allowing us to modify
        // the set as we go (see `BitIter` for details).
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
                let node = RepId::from_usize(bit_pos + offset);
                if !graph.iter(node).any(|n| subgraph.contains(n)) {
                    subgraph.insert(node);
                    colours[node.index()] = cur_colour;
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

    for (id, representative) in representatives.into_iter_enumerated() {
        let colour = colours[id.index()];
        let new_name = &colour_map[colour as usize];
        for reference in representative.references {
            rename_map.insert(reference, new_name.clone());
        }
    }

    rename_map
}

pub fn analyse(ast: &ast::Program, unresolved_ctxt: SyntaxContext) -> Store<'_> {
    let mut store = Store {
        calls: IndexSet::default(),
        functions: IndexVec::default(),
        static_fn_data: IndexVec::default(),
        function_map: FxHashMap::default(),
        objects_map: FxHashMap::default(),
        invalid_objects: GrowableBitSet::new_empty(),
        unions: UnionStore::default(),
        references: FxHashMap::default(),
        objects: ObjectStore::new(),
        unresolved_ctxt,
        resolved_calls: FxHashMap::default(),
        call_templates: FxHashMap::default(),
        fn_assignments: HashableHashMap::default(),
        object_links: FxHashSet::default(),
        call_objects: FxHashMap::default(),
    };

    let mut visitor = FnVisitor { store: &mut store };
    ast.visit_with(&mut visitor);

    let mut i = 0;
    while i < store.functions.len() {
        let func = FnId::from_usize(i);
        let template = CallTemplate::new(&mut store, func);
        store.call_templates.insert(func, template);
        i += 1;
    }

    let root = match &ast {
        Program::Module(n) => ControlFlowRoot::Module(n),
        Program::Script(n) => ControlFlowRoot::Script(n),
    };
    let cfa = ControlFlowAnalysis::analyze(root, false);

    let data_flow_analysis = DataFlowAnalysis::new(cfa.cfg, &cfa.nodePriorities, false);

    let mut analysis = Analysis { data_flow_analysis };
    analysis.data_flow_analysis.analyze(&mut store);

    let mut i = 0;
    while i < store.functions.len() {
        let static_data = &store.static_fn_data[i.into()];

        let cfa = ControlFlowAnalysisResult {
            cfg: ControlFlowGraph {
                map: static_data.cfg.map.clone(),
                implicit_return: static_data.cfg.implicit_return,
                entry: static_data.cfg.entry,
                graph: static_data.cfg.graph.clone(),
                node_annotations: FxHashMap::default(),
                edge_annotations: FxHashMap::default(),
            },
            nodePriorities: static_data.node_priorities.clone(),
        };

        let data_flow_analysis = DataFlowAnalysis::new(cfa.cfg, &cfa.nodePriorities, true);

        let mut analysis = Analysis { data_flow_analysis };
        analysis.data_flow_analysis.analyze(&mut store);
        i += 1;
    }

    debug_assert!(!store.invalid_objects.contains(ObjectStore::RESOLVING_CALL));
    debug_assert!(!store.invalid_objects.contains(ObjectStore::NUMBER));
    debug_assert!(!store.invalid_objects.contains(ObjectStore::STRING));
    debug_assert!(!store.invalid_objects.contains(ObjectStore::BOOL));
    debug_assert!(!store.invalid_objects.contains(ObjectStore::BIG_INT));
    store
}

pub fn process(
    ast: &mut ast::Program,
    node_id_gen: &mut NodeIdGen,
    unresolved_ctxt: SyntaxContext,
) {
    let store = analyse(ast, unresolved_ctxt);

    let rename_map = create_renaming_map(&store);

    dbg!(&store, &rename_map);

    // Actually assign the new names.
    let mut renamer = Renamer {
        node_id_gen,
        rename_map,
    };

    ast.visit_mut_with(&mut renamer);
}

struct FnVisitor<'ast, 's> {
    store: &'s mut Store<'ast>,
}

impl<'ast> FnVisitor<'ast, '_> {
    fn handle_fn<T>(&mut self, node: &'ast T) -> FnId
    where
        T: FunctionLike<'ast> + GetNodeId,
        &'ast T: Into<ControlFlowRoot<'ast>>,
        Node<'ast>: From<&'ast T>,
    {
        debug_assert!(!self.store.function_map.contains_key(&node.node_id()));

        let cfa =
            ControlFlowAnalysis::<DummyAnnotation, DummyAnnotation>::analyze(node.into(), false);

        // cfa.cfg.print_full();
        let cfg = SimpleCFG {
            map: cfa.cfg.map,
            implicit_return: cfa.cfg.implicit_return,
            entry: cfa.cfg.entry,
            graph: cfa.cfg.graph,
        };

        let param_names = node
            .params()
            .map(|p| {
                if let Pat::Ident(param) = &p {
                    param.to_id()
                } else {
                    todo!("non-ident param");
                }
            })
            .collect();

        let static_data = StaticFunctionData {
            cfg,
            node_priorities: cfa.nodePriorities,
            param_names,
        };

        let func = Func {
            args: Vec::new(),
            arg_values: HashableHashMap::default(),
        };

        let id = self.store.functions.push(func);
        self.store.function_map.insert(node.node_id(), id);

        let static_id = self.store.static_fn_data.push(static_data);
        debug_assert_eq!(static_id, id);

        node.visit_body_with(self);
        id
    }
}

impl<'ast> Visit<'ast> for FnVisitor<'ast, '_> {
    fn visit_fn_decl(&mut self, node: &'ast FnDecl) {
        let id = self.handle_fn(&node.function);
        self.store.fn_assignments.insert(
            node.ident.to_id(),
            Assignment {
                rhs: Some(Pointer::Fn(id)),
            },
        );
    }
    fn visit_function(&mut self, node: &'ast Function) {
        self.handle_fn(node);
    }
    fn visit_constructor(&mut self, node: &'ast Constructor) {
        self.handle_fn(node);
    }
    fn visit_arrow_expr(&mut self, node: &'ast ArrowExpr) {
        self.handle_fn(node);
    }
    fn visit_getter_prop(&mut self, node: &'ast GetterProp) {
        self.handle_fn(node);
    }
    fn visit_setter_prop(&mut self, node: &'ast SetterProp) {
        self.handle_fn(node);
    }
}

#[derive(Debug)]
struct Analysis<'ast, 'p> {
    data_flow_analysis: DataFlowAnalysis<'ast, 'p>,
}

#[derive(Debug)]
/// Info collected during analysis.
pub struct Store<'ast> {
    calls: IndexSet<CallId, Call>,
    pub(super) functions: IndexVec<FnId, Func>,
    static_fn_data: IndexVec<FnId, StaticFunctionData<'ast>>,
    function_map: FxHashMap<NodeId, FnId>,
    objects_map: FxHashMap<NodeId, ObjectId>,
    invalid_objects: GrowableBitSet<ObjectId>,
    unions: UnionStore,

    references: FxHashMap<PropKey, Option<Pointer>>,

    objects: ObjectStore,

    pub(super) unresolved_ctxt: SyntaxContext,

    resolved_calls: FxHashMap<CallId, ResolvedCall>,
    call_templates: FxHashMap<FnId, CallTemplate>,
    fn_assignments: HashableHashMap<Id, Assignment>,

    object_links: FxHashSet<(ObjectId, ObjectId)>,
    call_objects: FxHashMap<(CallId, NodeId), ObjectId>,
}

impl Store<'_> {
    /// Returns a [`Pointer`] to a union of `pointer1`'s type and `pointer2`'s type.
    fn create_union(
        &mut self,
        pointer1: Option<Pointer>,
        pointer2: Option<Pointer>,
    ) -> Option<Pointer> {
        create_union(&mut self.unions, pointer1, pointer2)
    }

    /// Recursively invalidates the entity that `pointer` points to.
    fn invalidate(&mut self, pointer: Option<Pointer>, lattice: &Lattice) {
        invalidate(
            &mut self.invalid_objects,
            &self.unions,
            pointer,
            &lattice.prop_assignments,
        );
    }

    /// Returns true if `pointer` points to an invalid object.
    fn invalidated(&self, pointer: Pointer) -> bool {
        invalidated(pointer, &self.invalid_objects, &self.unions)
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

/// Recursively invalidates the entity that `pointer` points to.
fn invalidate(
    invalid_objects: &mut GrowableBitSet<ObjectId>,
    unions: &UnionStore,
    pointer: Option<Pointer>,
    prop_assignments: &PropertyAssignments,
) {
    if let Some(pointer) = pointer {
        match pointer {
            Pointer::Object(o) if o.is_built_in() => return,
            Pointer::Object(obj) => {
                if invalid_objects.contains(obj) {
                    return;
                }
            }
            Pointer::Union(_) => {}
            Pointer::Fn(_) | Pointer::NullOrVoid => return,
        }

        let mut queue = vec![pointer];
        let mut done = FxHashSet::default();

        while let Some(pointer) = queue.pop() {
            done.insert(pointer);

            match pointer {
                Pointer::Object(o) if o.is_built_in() => {}
                Pointer::Object(obj) => {
                    let new_invalidation = invalid_objects.insert(obj);

                    if new_invalidation {
                        for ((o, _), prop) in prop_assignments.iter() {
                            if *o == obj {
                                if let Some(value) = prop.rhs {
                                    if !done.contains(&value) {
                                        queue.push(value);
                                    }
                                }
                            }
                        }
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

#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy)]
/// Points to an abstraction of a javascript object.
pub(self) enum Pointer {
    Object(ObjectId),
    Union(UnionId),
    Fn(FnId),
    NullOrVoid,
}

index::newtype_index!(pub(super) struct FnId { .. });
index::newtype_index!(pub(super) struct CallId { .. });

#[derive(Debug)]
pub(super) struct SimpleCFG<'ast> {
    map: FxHashMap<Node<'ast>, NodeIndex>,
    implicit_return: Node<'ast>,
    entry: Node<'ast>,
    graph: DiGraph<Node<'ast>, Branch>,
}

impl<'ast> SimpleCFG<'ast> {
    pub fn get_successors(&self, node: Node<'ast>) -> Neighbors<'_, Branch> {
        self.graph
            .neighbors_directed(*self.map.get(&node).unwrap(), Outgoing)
    }
}

#[derive(Debug)]
pub(super) struct Func {
    args: Vec<Option<Pointer>>,
    arg_values: PropertyAssignments,
}

#[derive(Debug)]
pub(super) struct StaticFunctionData<'ast> {
    cfg: SimpleCFG<'ast>,
    node_priorities: FxHashMap<Node<'ast>, usize>,
    param_names: Vec<Id>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(super) struct Call {
    func: FnId,
    args: CallArgs,
    prop_assignments: PropertyAssignments,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum CallArgs {
    Heap(Box<[Option<Pointer>]>),
    /// Number of consecutive `None`s
    Invalid(usize),
}

impl CallArgs {
    fn get(&self, index: usize) -> Option<Option<Pointer>> {
        match self {
            CallArgs::Heap(args) => args.get(index).copied(),
            CallArgs::Invalid(len) => {
                if index >= *len {
                    None
                } else {
                    Some(None)
                }
            }
        }
    }
}

#[derive(Debug)]
struct CallArgBuilder {
    len: usize,
    args: Option<Vec<Option<Pointer>>>,
    none_count: usize,
}

impl CallArgBuilder {
    fn new(len: usize) -> Self {
        Self {
            len,
            args: None,
            none_count: 0,
        }
    }

    fn push(&mut self, arg: Option<Pointer>) {
        if let Some(args) = &mut self.args {
            args.push(arg);
        } else if arg.is_none() {
            self.none_count += 1;
        } else {
            let mut new = Vec::new();
            new.reserve_exact(self.len);
            new.extend(std::iter::repeat(None).take(self.none_count));
            new.push(arg);
            self.args = Some(new);
        }
    }

    fn finish(self) -> CallArgs {
        match self.args {
            Some(args) => {
                debug_assert_eq!(self.len, args.len());
                debug_assert!(self.len > 0);
                CallArgs::Heap(args.into())
            }
            None => {
                debug_assert_eq!(self.len, self.none_count);
                CallArgs::Invalid(self.none_count)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub(super) struct ResolvedCall {
    return_type: Option<Pointer>,
    prop_assignments: PropertyAssignments,
}

#[derive(Debug, PartialEq, Hash, Eq)]
pub(super) struct Union {
    constituents: Vec<ObjectId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct Assignment {
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
pub(super) struct PropKey(JsWord, NodeId);

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

fn get_property(
    lattice: &Lattice,
    unions: &mut UnionStore,
    pointer: Pointer,
    key: &JsWord,
    invalid_objects: &mut GrowableBitSet<ObjectId>,
) -> Option<Pointer> {
    // TODO: is this correct (should it return None instead?)
    if let Pointer::NullOrVoid = pointer {
        return Some(Pointer::NullOrVoid);
    }
    let invalid = match pointer {
        Pointer::Object(obj) => invalid_objects.contains(obj),
        Pointer::Union(_) => {
            if invalidated(pointer, invalid_objects, unions) {
                invalidate(
                    invalid_objects,
                    unions,
                    Some(pointer),
                    &lattice.prop_assignments,
                );
                true
            } else {
                false
            }
        }
        Pointer::Fn(_) => true,
        Pointer::NullOrVoid => unreachable!(),
    };
    if invalid {
        if cfg!(debug_assertions) {
            match pointer {
                Pointer::Object(obj) => {
                    let prop = lattice
                        .prop_assignments
                        .get(&(obj, key.clone()))
                        .map(|a| a.rhs)
                        .unwrap_or_default();
                    debug_assert!(
                        matches!(prop, Some(Pointer::Object(o)) if o.is_built_in())
                            || !matches!(prop, Some(Pointer::Object(_) | Pointer::Union(_)))
                            || invalidated(prop.unwrap(), invalid_objects, unions)
                    );
                }
                Pointer::Union(union) => {
                    for constituent in unions[union].constituents() {
                        let constituent = lattice
                            .prop_assignments
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
                Pointer::Fn(_) => {}
                Pointer::NullOrVoid => unreachable!(),
            }
        }

        return None;
    }
    match pointer {
        Pointer::Object(obj) => lattice
            .prop_assignments
            .get(&(obj, key.clone()))
            .map(|a| a.rhs)
            .unwrap_or(Some(Pointer::NullOrVoid)),
        Pointer::Union(union) => {
            let mut builder = UnionBuilder::default();

            for constituent in unions[union].constituents() {
                let constituent = lattice
                    .prop_assignments
                    .get(&(constituent, key.clone()))
                    .map(|a| a.rhs)
                    .unwrap_or(Some(Pointer::NullOrVoid));

                builder.add(constituent, unions);
            }

            unions.build_union(builder)
        }
        Pointer::Fn(_) => None,
        Pointer::NullOrVoid => unreachable!(),
    }
}

#[derive(Debug)]
struct CowLattice<'a>(Cow<'a, Lattice>);

impl Deref for CowLattice<'_> {
    type Target = Lattice;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl CowLattice<'_> {
    fn insert_var_assignment(&mut self, name: Id, value: Assignment) {
        if let Some(existing) = self.0.var_assignments.get(&name) {
            if *existing == value {
                return;
            }
        }

        self.0.to_mut().var_assignments.insert(name, value);
    }

    fn insert_prop_assignment(&mut self, prop: (ObjectId, JsWord), value: Assignment) {
        if let Some(existing) = self.0.prop_assignments.get(&prop) {
            if *existing == value {
                return;
            }
        } else if let Some(Pointer::NullOrVoid) = value.rhs {
            // Assigning null/void is the same as having nothing assigned.
            return;
        }

        self.0.to_mut().prop_assignments.insert(prop, value);
    }

    fn get_var(
        &self,
        id: &Id,
        fn_assignments: &HashableHashMap<Id, Assignment>,
    ) -> Option<Assignment> {
        self.0
            .var_assignments
            .get(id)
            .or_else(|| fn_assignments.get(id))
            .copied()
    }
}

#[derive(Debug)]
struct Analyser<'ast, 'a> {
    lattice: CowLattice<'a>,
    store: &'a mut Store<'ast>,
    in_fn: bool,
}

impl<'ast> Analyser<'ast, '_> {
    fn get_property(&mut self, pointer: Pointer, key: &JsWord) -> Option<Pointer> {
        get_property(
            &self.lattice,
            &mut self.store.unions,
            pointer,
            key,
            &mut self.store.invalid_objects,
        )
    }

    fn reference_prop(&mut self, object: Option<Pointer>, key: PropKey) {
        self.store.references.insert(key, object);
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
                Slot::Var(name) => self
                    .lattice
                    .get_var(name, &self.store.fn_assignments)
                    .and_then(|a| a.rhs),
                Slot::Prop(obj, key) => self.get_property(*obj, key),
            };

            let rhs = if !conditional {
                // supersede
                rhs
            } else {
                // union
                self.store.create_union(existing, rhs)
            };
            let new = Assignment { rhs };
            match slot {
                Slot::Var(name) => {
                    self.lattice.insert_var_assignment(name, new);
                }
                Slot::Prop(obj, key) => {
                    if self.store.invalidated(obj) {
                        self.store.invalidate(rhs, &self.lattice);
                    } else {
                        match obj {
                            Pointer::Object(obj) => {
                                self.lattice.insert_prop_assignment((obj, key), new);
                            }
                            Pointer::Union(union) => {
                                for constituent in self.store.unions[union].constituents() {
                                    self.lattice
                                        .insert_prop_assignment((constituent, key.clone()), new);
                                }
                            }
                            Pointer::Fn(_) => {
                                unreachable!();
                            }
                            Pointer::NullOrVoid => {}
                        }
                    }
                }
            };
            rhs
        } else {
            // Unknown/invalid assignment target.
            self.store.invalidate(rhs, &self.lattice);
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
        match lhs.kind {
            NodeKind::ArrayPat(_) | NodeKind::ObjectPat(_) => {
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
                Slot::Var(name) => self
                    .lattice
                    .get_var(&name, &self.store.fn_assignments)
                    .and_then(|a| a.rhs),
                Slot::Prop(obj, key) => self.get_property(obj, &key),
            };
            self.store.invalidate(value, &self.lattice);
        }
    }

    fn visit_destructuring(&mut self, lhs: Node<'ast>, rhs: Option<Pointer>, conditional: bool) {
        match lhs.kind {
            NodeKind::ObjectPat(lhs) => {
                let has_complex_props = lhs.props.iter().any(|p| match p {
                    ObjectPatProp::KeyValue(p) => {
                        !is_simple_prop_name(&p.key, self.store.unresolved_ctxt)
                    }
                    ObjectPatProp::Assign(_) | ObjectPatProp::Rest(_) => false,
                });
                if let Some(rhs) = rhs {
                    if has_complex_props {
                        self.store.invalidate(Some(rhs), &self.lattice);
                    }
                    if !self.store.invalidated(rhs) {
                        for prop in &lhs.props {
                            match prop {
                                ObjectPatProp::KeyValue(prop) => {
                                    let key = PropKey::from_prop_name(
                                        &prop.key,
                                        self.store.unresolved_ctxt,
                                    )
                                    .unwrap();
                                    self.reference_prop(Some(rhs), key.clone());
                                    let rhs_value = self.get_property(rhs, &key.0);

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
                                    unreachable!("removed by normalization");
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
                                    // if self.analysis.vars.contains_key(&id) {
                                    self.assign_to_slot(
                                        Some(Slot::Var(id)),
                                        Some(rhs),
                                        conditional,
                                    );
                                    // }
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

            NodeKind::BindingIdent(lhs) => {
                let id = lhs.to_id();
                // if self.analysis.vars.contains_key(&id) {
                self.assign_to_slot(Some(Slot::Var(id)), rhs, conditional);
                // }
            }
            NodeKind::ArrayPat(lhs) => {
                self.store.invalidate(rhs, &self.lattice);
                for element in lhs.elems.iter().filter_map(|e| e.as_ref()) {
                    if let Pat::Expr(elem) = element {
                        self.invalidate_slot(Node::from(elem.as_ref()), conditional);
                    } else {
                        self.visit_destructuring(Node::from(element), None, conditional);
                    }
                }
            }
            NodeKind::RestPat(lhs) => {
                self.store.invalidate(rhs, &self.lattice);
                self.visit_destructuring(Node::from(lhs.arg.as_ref()), None, conditional);
            }
            NodeKind::AssignPat(lhs) => {
                let default_value = self.visit_and_get_object(Node::from(lhs.right.as_ref()), true);

                let result = self.store.create_union(rhs, default_value);

                self.visit_destructuring(Node::from(lhs.left.as_ref()), result, conditional);
            }
            NodeKind::KeyValuePatProp(lhs) => {
                self.store.invalidate(rhs, &self.lattice);
                if matches!(lhs.value.as_ref(), Pat::Ident(_) | Pat::Expr(_)) {
                    self.invalidate_slot(Node::from(lhs.value.as_ref()), conditional);
                } else {
                    self.visit_destructuring(Node::from(lhs.value.as_ref()), None, conditional);
                }
            }
            NodeKind::AssignPatProp(_) => {
                unreachable!("removed by normalization");
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
        match node.kind {
            NodeKind::Ident(node) => {
                let id = node.to_id();
                // if self.analysis.vars.contains_key(&id) {
                Some(Slot::Var(id))
                // } else {
                //     None
                // }
            }
            NodeKind::BindingIdent(node) => {
                self.visit_and_get_slot(Node::from(&node.id), conditional)
            }
            NodeKind::MemberExpr(node) => {
                let obj = self.visit_and_get_object(Node::from(&node.obj), conditional);
                if let Some(prop) =
                    PropKey::from_expr(&node.prop, self.store.unresolved_ctxt, node.computed)
                {
                    obj.map(|obj| {
                        self.reference_prop(Some(obj), prop.clone());
                        Slot::Prop(obj, prop.0)
                    })
                } else {
                    self.store.invalidate(obj, &self.lattice);
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

    fn call_fn(&mut self, func: FnId, args: CallArgs) -> Option<Pointer> {
        let mut prop_assignments: PropertyAssignments = HashableHashMap::default();

        if let CallArgs::Heap(args) = &args {
            let mut queue = Vec::new();
            for arg in args.iter() {
                match arg {
                    Some(Pointer::Object(o)) => {
                        queue.push(*o);
                    }
                    Some(Pointer::Union(union)) => {
                        queue.extend(self.store.unions[*union].constituents());
                    }
                    Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) | None => {}
                }
            }

            let mut done = FxHashSet::default();

            while let Some(o) = queue.pop() {
                if self.store.invalid_objects.contains(o) || done.contains(&o) {
                    continue;
                }

                done.insert(o);

                for (key, value) in self.lattice.prop_assignments.iter() {
                    if key.0 == o {
                        prop_assignments.insert(key.clone(), *value);
                        match value.rhs {
                            Some(Pointer::Object(o)) => {
                                if !done.contains(&o) {
                                    queue.push(o);
                                }
                            }
                            Some(Pointer::Union(union)) => {
                                for constituent in self.store.unions[union].constituents() {
                                    if !done.contains(&constituent) {
                                        queue.push(constituent);
                                    }
                                }
                            }
                            Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) | None => {}
                        }
                    }
                }
            }
        }

        let call = Call {
            func,
            args,
            prop_assignments,
        };

        let depends_on_unresolved_call = |pointer| match pointer {
            Some(Pointer::Object(o)) => o == ObjectStore::RESOLVING_CALL,
            Some(Pointer::Union(union)) => {
                self.store.unions[union].contains(ObjectStore::RESOLVING_CALL)
            }
            Some(Pointer::Fn(_)) | Some(Pointer::NullOrVoid) | None => false,
        };

        if cfg!(debug_assertions) {
            if let CallArgs::Heap(args) = &call.args {
                for &arg in args.iter() {
                    debug_assert!(!depends_on_unresolved_call(arg));
                }
            }

            debug_assert!(!call
                .prop_assignments
                .keys()
                .any(|(o, _)| *o == ObjectStore::RESOLVING_CALL));
            debug_assert!(!call
                .prop_assignments
                .values()
                .any(|p| depends_on_unresolved_call(p.rhs)));
        }

        let call = self.store.calls.insert(call);

        resolve_call(call, self.store);

        // Merge property assignments.
        for ((obj, key), prop) in self.store.resolved_calls[&call].prop_assignments.iter() {
            if self.store.invalid_objects.contains(*obj) {
                continue;
            }
            let key = (*obj, key.clone());
            let new = if let Some(existing) = self.lattice.prop_assignments.get(&key) {
                let union = create_union(&mut self.store.unions, existing.rhs, prop.rhs);
                Assignment { rhs: union }
            } else {
                *prop
            };
            self.lattice.insert_prop_assignment(key, new);
        }

        self.store.resolved_calls[&call].return_type
    }

    fn visit_and_get_object(&mut self, node: Node<'ast>, conditional: bool) -> Option<Pointer> {
        match node.kind {
            // Don't traverse into new control flow nodes.
            NodeKind::FnExpr(_) | NodeKind::ArrowExpr(_) => None,

            NodeKind::AssignExpr(node) => {
                self.record_assignment(Node::from(&node.left), &node.right, conditional, node.op)
            }

            NodeKind::Class(_) => todo!(),
            NodeKind::ExtendsClause(_) => todo!(),
            NodeKind::ClassProp(_) => todo!(),
            NodeKind::PrivateProp(_) => todo!(),
            NodeKind::ClassMethod(_) => todo!(),
            NodeKind::PrivateMethod(_) => todo!(),
            NodeKind::Constructor(_) => todo!(),
            NodeKind::VarDeclarator(node) => {
                let lhs = Node::from(&node.name);
                if let Some(rhs) = &node.init {
                    self.record_assignment(lhs, rhs, conditional, AssignOp::Assign);
                } else {
                    self.visit_and_get_object(lhs, conditional);
                }
                None
            }
            NodeKind::ThisExpr(_) => None,
            NodeKind::ArrayLit(node) => {
                for element in &node.elems {
                    if let Some(element) = element {
                        let obj = self.visit_and_get_object(Node::from(element), conditional);
                        // Can't track once it's in the array.
                        self.store.invalidate(obj, &self.lattice);
                    }
                }
                None
            }
            NodeKind::ObjectLit(node) => {
                let object_id = if let Some(existing) = self.store.objects_map.get(&node.node_id) {
                    *existing
                } else {
                    let object_id = self.store.objects.next_object_id();
                    self.store.objects_map.insert(node.node_id, object_id);
                    object_id
                };

                let pointer = Pointer::Object(object_id);

                let is_simple_obj_lit = node.props.iter().all(|p| match p {
                    Prop::KeyValue(p) => is_simple_prop_name(&p.key, self.store.unresolved_ctxt),
                    _ => false,
                });

                if is_simple_obj_lit {
                    for prop in &node.props {
                        let prop = unwrap_as!(prop, Prop::KeyValue(p), p);
                        let key =
                            PropKey::from_prop_name(&prop.key, self.store.unresolved_ctxt).unwrap();

                        self.reference_prop(Some(pointer), key.clone());

                        let value =
                            self.visit_and_get_object(Node::from(prop.value.as_ref()), conditional);

                        self.assign_to_slot(Some(Slot::Prop(pointer, key.0)), value, conditional);
                    }
                } else {
                    self.store.invalidate(Some(pointer), &self.lattice);
                }
                Some(pointer)
            }
            NodeKind::SpreadElement(node) => {
                let obj = self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional);
                self.store.invalidate(obj, &self.lattice);
                None
            }
            NodeKind::UnaryExpr(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional);
                None
            }
            NodeKind::UpdateExpr(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional);
                None
            }
            NodeKind::BinExpr(node) => {
                match node.op {
                    BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::NullishCoalescing => {
                        // TODO: if LHS is object, then we know if RHS will execute.
                        let left =
                            self.visit_and_get_object(Node::from(node.left.as_ref()), conditional);
                        let right =
                            self.visit_and_get_object(Node::from(node.right.as_ref()), true);
                        self.store.create_union(left, right)
                    }
                    _ => {
                        self.visit_and_get_object(Node::from(node.left.as_ref()), conditional);
                        self.visit_and_get_object(Node::from(node.right.as_ref()), conditional);
                        None
                    }
                }
            }
            NodeKind::ClassExpr(_) => todo!(),
            NodeKind::MemberExpr(node) => {
                let obj = self.visit_and_get_object(Node::from(&node.obj), conditional);
                if let Some(prop) =
                    PropKey::from_expr(&node.prop, self.store.unresolved_ctxt, node.computed)
                {
                    if let Some(obj) = obj {
                        self.reference_prop(Some(obj), prop.clone());
                        self.get_property(obj, &prop.0)
                    } else {
                        None
                    }
                } else {
                    self.store.invalidate(obj, &self.lattice);
                    self.visit_and_get_object(Node::from(node.prop.as_ref()), conditional);
                    None
                }
            }
            NodeKind::CondExpr(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
                let cons = self.visit_and_get_object(Node::from(node.cons.as_ref()), true);
                let alt = self.visit_and_get_object(Node::from(node.alt.as_ref()), true);
                self.store.create_union(cons, alt)
            }
            NodeKind::CallExpr(node) => {
                let callee = self.visit_and_get_object(Node::from(&node.callee), conditional);
                if let Some(Pointer::Fn(func)) = callee {
                    let mut args = CallArgBuilder::new(node.args.len());
                    for arg in &node.args {
                        let mut arg = self.visit_and_get_object(Node::from(arg), conditional);
                        if let Some(Pointer::Object(o)) = arg {
                            if self.store.invalid_objects.contains(o) {
                                arg = None;
                            }
                        }
                        if let Some(Pointer::Union(union)) = arg {
                            if union.invalid() {
                                arg = None;
                            }
                        }
                        args.push(arg);
                    }
                    // TODO: only call whe in_fn is true?
                    self.call_fn(func, args.finish())
                } else {
                    self.store.invalidate(callee, &self.lattice);

                    for arg in &node.args {
                        let obj = self.visit_and_get_object(Node::from(arg), conditional);
                        self.store.invalidate(obj, &self.lattice);
                    }
                    None
                }
            }
            NodeKind::NewExpr(node) => {
                let callee =
                    self.visit_and_get_object(Node::from(node.callee.as_ref()), conditional);
                self.store.invalidate(callee, &self.lattice);

                if let Some(args) = &node.args {
                    for arg in args {
                        let obj = self.visit_and_get_object(Node::from(arg), conditional);
                        self.store.invalidate(obj, &self.lattice);
                    }
                }
                None
            }
            NodeKind::SeqExpr(node) => {
                debug_assert!(!node.exprs.is_empty());

                let mut i = 0;
                while i < node.exprs.len() - 1 {
                    self.visit_and_get_object(Node::from(node.exprs[i].as_ref()), conditional);
                    i += 1;
                }

                self.visit_and_get_object(Node::from(node.exprs[i].as_ref()), conditional)
            }
            NodeKind::YieldExpr(node) => {
                if let Some(arg) = &node.arg {
                    let arg = self.visit_and_get_object(Node::from(arg.as_ref()), conditional);
                    self.store.invalidate(arg, &self.lattice);
                }
                None
            }
            NodeKind::AwaitExpr(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional)
            }
            NodeKind::Tpl(node) => {
                for expr in &node.exprs {
                    self.visit_and_get_object(Node::from(expr.as_ref()), conditional);
                }
                None
            }
            NodeKind::TaggedTpl(node) => {
                self.visit_and_get_object(Node::from(node.tag.as_ref()), conditional);
                for expr in &node.tpl.exprs {
                    let obj = self.visit_and_get_object(Node::from(expr.as_ref()), conditional);
                    // Expressions in tagged templates can be accessed by the tag function.
                    self.store.invalidate(obj, &self.lattice);
                }
                None
            }
            NodeKind::ParenExpr(node) => {
                self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional)
            }
            NodeKind::Super(_) => todo!(),
            NodeKind::OptChainExpr(_) => todo!(),
            NodeKind::Function(_) => todo!(),
            NodeKind::Param(_) => todo!(),
            NodeKind::ParamWithoutDecorators(_) => todo!(),
            NodeKind::BindingIdent(node) => {
                self.visit_and_get_object(Node::from(&node.id), conditional)
            }
            NodeKind::Ident(node) => {
                if node.span.ctxt == self.store.unresolved_ctxt && node.sym == js_word!("undefined")
                {
                    Some(Pointer::NullOrVoid)
                } else {
                    let id = node.to_id();
                    self.lattice
                        .get_var(&id, &self.store.fn_assignments)
                        .and_then(|assign| assign.rhs)
                }
            }
            NodeKind::PrivateName(_) => todo!(),

            NodeKind::Null(_) => Some(Pointer::NullOrVoid),

            NodeKind::Str(_) => Some(Pointer::Object(ObjectStore::STRING)),
            NodeKind::Bool(_) => Some(Pointer::Object(ObjectStore::BOOL)),
            NodeKind::Number(_) => Some(Pointer::Object(ObjectStore::NUMBER)),
            NodeKind::BigInt(_) => Some(Pointer::Object(ObjectStore::BIG_INT)),

            NodeKind::Regex(_) | NodeKind::TplElement(_) | NodeKind::MetaPropExpr(_) => None,

            NodeKind::ImportDefaultSpecifier(_) => todo!(),
            NodeKind::ImportStarAsSpecifier(_) => todo!(),
            NodeKind::ImportNamedSpecifier(_) => todo!(),
            NodeKind::ExportNamespaceSpecifier(_) => todo!(),
            NodeKind::ExportDefaultSpecifier(_) => todo!(),
            NodeKind::ExportNamedSpecifier(_) => todo!(),
            NodeKind::ComputedPropName(node) => {
                self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional);
                None
            }
            NodeKind::CatchClause(_) => todo!(),

            // This function is only called on expressions (and their children),
            // so it can't reach e.g. statements. TypeScript and JSX should have
            // been removed by now as well.
            _ => unreachable!(),
        }
    }

    fn init(&mut self, node: Node<'ast>, conditional: bool) {
        match node.kind {
            NodeKind::IfStmt(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
            }
            NodeKind::ExprStmt(node) => {
                self.visit_and_get_object(Node::from(node.expr.as_ref()), conditional);
            }
            NodeKind::BlockStmt(_) => {}
            NodeKind::ForStmt(node) => {
                if let Some(test) = &node.test {
                    self.visit_and_get_object(Node::from(test.as_ref()), conditional);
                }
            }
            NodeKind::Function(node) => {
                let func = *self.store.function_map.get(&node.node_id).unwrap();

                for (i, param_name) in self.store.static_fn_data[func]
                    .param_names
                    .iter()
                    .enumerate()
                {
                    let value = self.store.functions[func]
                        .args
                        .get(i)
                        .copied()
                        .unwrap_or(Some(Pointer::NullOrVoid));

                    self.lattice
                        .insert_var_assignment(param_name.clone(), Assignment { rhs: value });
                }

                for ((obj, key), prop) in self.store.functions[func].arg_values.iter() {
                    if self.store.invalid_objects.contains(*obj) {
                        continue;
                    }
                    let key = (*obj, key.clone());
                    let new = if let Some(existing) = self.lattice.prop_assignments.get(&key) {
                        let union = create_union(&mut self.store.unions, existing.rhs, prop.rhs);
                        Assignment { rhs: union }
                    } else {
                        *prop
                    };
                    self.lattice.insert_prop_assignment(key, new);
                }
            }
            NodeKind::VarDecl(node) => {
                for decl in &node.decls {
                    self.visit_and_get_object(Node::from(decl), conditional);
                }
            }
            NodeKind::ThisExpr(_)
            | NodeKind::ArrayLit(_)
            | NodeKind::ObjectLit(_)
            | NodeKind::SpreadElement(_)
            | NodeKind::UnaryExpr(_)
            | NodeKind::UpdateExpr(_)
            | NodeKind::BinExpr(_)
            | NodeKind::FnExpr(_)
            | NodeKind::ClassExpr(_)
            | NodeKind::AssignExpr(_)
            | NodeKind::MemberExpr(_)
            | NodeKind::CondExpr(_)
            | NodeKind::CallExpr(_)
            | NodeKind::NewExpr(_)
            | NodeKind::SeqExpr(_)
            | NodeKind::ArrowExpr(_)
            | NodeKind::YieldExpr(_)
            | NodeKind::MetaPropExpr(_)
            | NodeKind::AwaitExpr(_)
            | NodeKind::Tpl(_)
            | NodeKind::TaggedTpl(_)
            | NodeKind::TplElement(_)
            | NodeKind::ParenExpr(_)
            | NodeKind::Super(_)
            | NodeKind::OptChainExpr(_)
            | NodeKind::BindingIdent(_)
            | NodeKind::Ident(_)
            | NodeKind::PrivateName(_) => {
                self.visit_and_get_object(node, conditional);
            }

            NodeKind::ImplicitReturn => {}
            NodeKind::Class(_) => todo!(),
            NodeKind::ExtendsClause(_) => todo!(),
            NodeKind::ClassProp(_) => todo!(),
            NodeKind::PrivateProp(_) => todo!(),
            NodeKind::ClassMethod(_) => todo!(),
            NodeKind::PrivateMethod(_) => todo!(),
            NodeKind::Constructor(_) => todo!(),
            NodeKind::Decorator(_) => todo!(),
            NodeKind::FnDecl(_) => {}
            NodeKind::ClassDecl(_) => todo!(),
            NodeKind::VarDeclarator(_) => todo!(),
            NodeKind::Param(_) => todo!(),
            NodeKind::ParamWithoutDecorators(_) => todo!(),
            NodeKind::ExportDefaultExpr(_) => todo!(),
            NodeKind::ExportDecl(_) => todo!(),
            NodeKind::ImportDecl(_) => todo!(),
            NodeKind::ExportAll(_) => todo!(),
            NodeKind::NamedExport(_) => todo!(),
            NodeKind::ExportDefaultDecl(_) => todo!(),
            NodeKind::ImportDefaultSpecifier(_) => todo!(),
            NodeKind::ImportStarAsSpecifier(_) => todo!(),
            NodeKind::ImportNamedSpecifier(_) => todo!(),
            NodeKind::ExportNamespaceSpecifier(_) => todo!(),
            NodeKind::ExportDefaultSpecifier(_) => todo!(),
            NodeKind::ExportNamedSpecifier(_) => todo!(),
            NodeKind::Script(_) => {}
            NodeKind::Module(_) => {}
            NodeKind::ArrayPat(_) => todo!(),
            NodeKind::ObjectPat(_) => todo!(),
            NodeKind::AssignPat(_) => todo!(),
            NodeKind::RestPat(_) => todo!(),
            NodeKind::KeyValuePatProp(_) => todo!(),
            NodeKind::AssignPatProp(_) => todo!(),
            NodeKind::KeyValueProp(_) => todo!(),
            NodeKind::AssignProp(_) => todo!(),
            NodeKind::GetterProp(_) => {}
            NodeKind::SetterProp(_) => {}
            NodeKind::MethodProp(_) => todo!(),
            NodeKind::ComputedPropName(_) => todo!(),
            NodeKind::SpreadAssignment(_) => todo!(),
            NodeKind::DebuggerStmt(_) => {}
            NodeKind::WithStmt(_) => todo!(),
            NodeKind::ReturnStmt(node) => {
                if let Some(arg) = &node.arg {
                    let obj = self.visit_and_get_object(Node::from(arg.as_ref()), conditional);
                    if !self.in_fn {
                        self.store.invalidate(obj, &self.lattice);
                    }
                }
            }
            NodeKind::LabeledStmt(_) => {}
            NodeKind::SwitchStmt(node) => {
                self.visit_and_get_object(Node::from(node.discriminant.as_ref()), conditional);
            }
            NodeKind::ThrowStmt(node) => {
                self.visit_and_get_object(Node::from(node.arg.as_ref()), conditional);
            }
            NodeKind::TryStmt(_) => {}
            NodeKind::WhileStmt(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
            }
            NodeKind::DoWhileStmt(node) => {
                self.visit_and_get_object(Node::from(node.test.as_ref()), conditional);
            }
            NodeKind::ForInStmt(node) => {
                let right = self.visit_and_get_object(Node::from(node.right.as_ref()), conditional);
                self.store.invalidate(right, &self.lattice);
            }
            NodeKind::ForOfStmt(_) => todo!(),
            NodeKind::SwitchCase(node) => {
                if let Some(test) = &node.test {
                    self.visit_and_get_object(Node::from(test.as_ref()), conditional);
                }
            }
            NodeKind::CatchClause(node) => {
                if let Some(param) = &node.param {
                    match param {
                        Pat::Array(_) | Pat::Object(_) => {
                            self.visit_destructuring(Node::from(param), None, conditional);
                        }
                        _ => {
                            self.invalidate_slot(Node::from(param), conditional);
                        }
                    }
                }
            }

            NodeKind::Str(_)
            | NodeKind::Bool(_)
            | NodeKind::Null(_)
            | NodeKind::Number(_)
            | NodeKind::BigInt(_)
            | NodeKind::Regex(_)
            | NodeKind::EmptyStmt(_)
            | NodeKind::BreakStmt(_)
            | NodeKind::ContinueStmt(_) => {}

            _ => unreachable!("{:#?}", node),
        }
    }
}

impl<'ast> DataFlowAnalysis<'ast, '_> {
    fn flowThrough(
        &mut self,
        node: Node<'ast>,
        store: &mut Store<'ast>,
        input: LatticeElementId,
        in_fn: bool,
    ) -> LatticeElementId {
        // Make assignments conditional if the node can end abruptly by an exception.
        let conditional = self
            .cfg
            .graph
            .edges(self.cfg.map[&node])
            .any(|e| *e.weight() == Branch::ON_EX);

        let mut v = Analyser {
            lattice: CowLattice(Cow::Borrowed(&self.lattice_elements[input])),
            store,
            in_fn,
        };

        v.init(node, conditional);

        match v.lattice.0 {
            Cow::Borrowed(_) => {
                // No changes compared to input.
                input
            }
            Cow::Owned(new) => self.add_lattice_element(new),
        }
    }
}

#[derive(Default)]
struct JoinOp {
    result: Lattice,
}

impl<'ast> JoinOp {
    fn joinFlow(
        &mut self,
        analysis: &mut DataFlowAnalysis<'ast, '_>,
        store: &mut Store,
        input: LatticeElementId,
    ) {
        let input = &analysis[input];

        // Merge property assignments.
        for ((obj, key), prop) in input.prop_assignments.iter() {
            if store.invalid_objects.contains(*obj) {
                continue;
            }
            match self.result.prop_assignments.entry((*obj, key.clone())) {
                Entry::Occupied(entry) => {
                    let union = create_union(&mut store.unions, entry.get().rhs, prop.rhs);
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
                    let union = create_union(&mut store.unions, entry.get().rhs, assignment.rhs);
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

type PropertyAssignments = HashableHashMap<(ObjectId, JsWord), Assignment>;

#[derive(Clone, Debug, PartialEq, Default, Hash, Eq)]
pub(super) struct Lattice {
    var_assignments: HashableHashMap<Id, Assignment>,
    prop_assignments: PropertyAssignments,
}

impl Annotation for Lattice {}

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
