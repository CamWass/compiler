// #![allow(non_snake_case)]
#![warn(non_upper_case_globals)]
#![warn(unused_variables)]
#![deny(non_shorthand_field_patterns)]
#![warn(warnings)]
#![warn(dead_code)]
#![deny(unused_imports)]

#[cfg(test)]
mod tests;

use std::collections::hash_map::Entry;
use std::convert::TryInto;
use std::fmt::Display;

use crate::find_vars::*;
use crate::optimize_properties2::simple_set::IndexSet;
use crate::optimize_properties2::unionfind::UnionFind;
use crate::optimize_properties2::{is_simple_prop_name, Id, NameId, PropKey, Renamer};
use crate::utils::unwrap_as;
use crate::DefaultNameGenerator::DefaultNameGenerator;
use ast::*;
use atoms::{js_word, JsWord};
use ecma_visit::{Visit, VisitMutWith, VisitWith};
use global_common::SyntaxContext;
use index::bit_set::{BitMatrix, BitSet};
use index::vec::IndexVec;
use petgraph::graph::UnGraph;
use petgraph::graphmap::DiGraphMap;
use rustc_hash::{FxHashMap, FxHashSet};

/// When true, the property interference relations are outputted as a graph in
/// graphviz dot format (debug builds only).
const OUTPUT_PROP_GRAPH: bool = false;
/// When true, the data flow relations are outputted as a graph in graphviz dot
/// format (debug builds only).
const OUTPUT_RELO_GRAPH: bool = false;

/*
TODO:
visit all nested expressions, e.g. those in complex prop names.
invalidate:
    params that can't be statically bound? e.g. rest params
*/

pub fn process(
    ast: &mut ast::Program,
    program_data: &mut ProgramData,
    unresolved_ctxt: SyntaxContext,
) {
    let (mut store, points_to) = analyse(ast, unresolved_ctxt);

    let rename_map = create_renaming_map(&mut store, points_to);

    // Actually assign the new names.
    let mut renamer = Renamer {
        program_data,
        rename_map,
    };

    ast.visit_mut_with(&mut renamer);
}

fn create_renaming_map(
    store: &mut Store,
    points_to: FxHashMap<PointerId, FxHashSet<ConcretePointer>>,
) -> FxHashMap<NodeId, JsWord> {
    /*
        Idea:
        Assign properties (from different objects but with the same name) to the same
        bucket (equivalence class) if the must share the same name.

        Each property starts in its own bucket. Then, buckets are merged when one of their
        properties is accessed on a union of object types (the buckets for each object's
        version of the property are merged).

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
        properties: FxHashMap<NameId, PropId>,
    }

    #[derive(Debug, Clone)]
    struct Property {
        name: NameId,
        prop_id: PropId,
        references: FxHashSet<NodeId>,
        invalid: bool,
    }

    index::newtype_index!(struct PropId { .. });

    let mut objects: FxHashMap<ConcretePointer, Object> = FxHashMap::default();
    let mut properties: IndexVec<PropId, Property> = IndexVec::default();

    let mut prop_map: FxHashMap<PropKey, Vec<ConcretePointer>> = FxHashMap::default();

    for (key, pointer) in &store.references {
        let objs = points_to.get(pointer).unwrap();
        prop_map.entry(*key).or_default().extend(objs);
        for &obj in objs {
            let object = objects.entry(obj).or_default();
            let id = match object.properties.entry(key.0) {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(entry) => {
                    let prop_id = properties.push(Property {
                        name: key.0,
                        prop_id: properties.next_index(),
                        references: FxHashSet::default(),
                        invalid: store
                            .invalid_pointers
                            .contains(&store.pointers.insert(obj.into())),
                    });
                    entry.insert(prop_id);
                    prop_id
                }
            };
            properties[id].references.insert(key.1);
        }
    }

    let mut union_find = UnionFind::new(properties.len());

    // Process unions.
    for (PropKey(name, _), objs) in prop_map {
        if objs.is_empty() {
            continue;
        }
        let representative = *objects
            .get(&objs[0])
            .and_then(|c| c.properties.get(&name))
            .unwrap();

        for constituent in objs {
            let prop = *objects
                .get(&constituent)
                .and_then(|c| c.properties.get(&name))
                .unwrap();
            union_find.union(representative, prop);
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
                let representative = *map.entry(representative).or_insert_with(|| {
                    let index = representatives.len();
                    representatives.push(properties[representative].clone());
                    index
                });
                let representative = &mut representatives[representative];
                let prop = &properties[prop];
                representative
                    .references
                    .extend(prop.references.iter().copied());
                representative.invalid |= prop.invalid;
            }
        }
    }

    representatives.retain(|r| !r.invalid);

    // Sort representatives so those with more references have lower IDs.
    representatives.sort_unstable_by(|a, b| {
        let result = b.references.len().cmp(&a.references.len());
        if result.is_eq() {
            store.names[a.name].cmp(&store.names[b.name])
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

    if cfg!(debug_assertions) && OUTPUT_PROP_GRAPH {
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
                    let name = store.names[properties[prop].name].clone();
                    debug_graph.add_node((rep_id, name))
                });
                continue;
            }
            let mut outer = obj
                .properties
                .values()
                .filter_map(|p| representatives_map.get(&labeling[p.index()]));
            while let Some(&outer_node) = outer.next() {
                let a = *debug_graph_map.entry(outer_node).or_insert_with(|| {
                    let name = store.names[representatives[outer_node].name].clone();
                    debug_graph.add_node((outer_node, name))
                });
                for &inner_node in outer.clone() {
                    let b = *debug_graph_map.entry(inner_node).or_insert_with(|| {
                        let name = store.names[representatives[inner_node].name].clone();
                        debug_graph.add_node((inner_node, name))
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

fn compute_points_to_map(
    graph: &mut DiGraphMap<PointerId, GraphEdge>,
    store: &mut Store,
) -> FxHashMap<PointerId, FxHashSet<ConcretePointer>> {
    let mut points_to: FxHashMap<_, FxHashSet<_>> = FxHashMap::default();

    // Invalidate parameters for functions that access the arguments array.
    for func in store.functions.values() {
        if func.accesses_arguments_array {
            for param in func.param_indices() {
                let pointer = store.pointers.insert(Pointer::Var(param));
                store.invalid_pointers.insert(pointer);
            }
        }
    }

    store
        .invalid_pointers
        .insert(store.pointers.insert(Pointer::Unknown));

    for pointer in 0..store.pointers.len() {
        let pointer = PointerId::from_usize(pointer);
        let concrete = match store.pointers[pointer] {
            Pointer::Object(id) => Some(ConcretePointer::Object(id)),
            Pointer::Fn(id) => Some(ConcretePointer::Fn(id)),
            Pointer::Unknown => Some(ConcretePointer::Unknown),
            Pointer::NullOrVoid => Some(ConcretePointer::NullOrVoid),
            Pointer::Bool => Some(ConcretePointer::Bool),
            Pointer::Num => Some(ConcretePointer::Num),
            Pointer::String => Some(ConcretePointer::String),
            Pointer::BigInt => Some(ConcretePointer::BigInt),
            Pointer::Regex => Some(ConcretePointer::Regex),
            Pointer::ReturnValue(_)
            | Pointer::Arg(_, _)
            | Pointer::Prop(_, _)
            | Pointer::Var(_) => None,
        };
        if let Some(concrete) = concrete {
            points_to.entry(pointer).or_default().insert(concrete);
        }
    }

    loop {
        flow_edges(graph, store, &mut points_to);
        // After we've reached a fixedpoint above, if we couldn't infer the values
        // of a pointer, set them to Unknown and run fixedpoint again to propagate
        // the Unknowns.
        let mut changed = false;
        for pointer in 0..store.pointers.len() {
            let pointer = PointerId::from_usize(pointer);
            // Functions implicitly return undefined sometimes.
            if matches!(store.pointers[pointer], Pointer::ReturnValue(_)) {
                continue;
            }
            if !points_to.contains_key(&pointer) || points_to.get(&pointer).unwrap().is_empty() {
                points_to
                    .entry(pointer)
                    .or_default()
                    .insert(ConcretePointer::Unknown);
                changed = true;
            }
        }
        // Propagate 'invalid-ness'.
        for pointer in 0..store.pointers.len() {
            let pointer = PointerId::from_usize(pointer);
            if store.invalid_pointers.contains(&pointer) {
                let values = match points_to.get(&pointer) {
                    Some(v) => v,
                    None => continue,
                };
                for &value in values {
                    changed |= store
                        .invalid_pointers
                        .insert(store.pointers.insert(value.into()));
                }
            }
        }
        if !changed {
            break;
        }
    }

    store
        .invalid_pointers
        .remove(&store.pointers.insert(Pointer::NullOrVoid));

    points_to
}

fn flow_edges(
    graph: &mut DiGraphMap<PointerId, GraphEdge>,
    store: &mut Store,
    points_to: &mut FxHashMap<PointerId, FxHashSet<ConcretePointer>>,
) {
    loop {
        let mut changed = false;
        for (src, dest, kind) in graph.all_edges() {
            match kind {
                GraphEdge::Subset => {
                    if src == dest {
                        continue;
                    }
                    points_to.entry(src).or_default();
                    points_to.entry(dest).or_default();
                    let [src, dest] = points_to.get_many_mut([&src, &dest]).unwrap();
                    for v in src.iter() {
                        changed |= dest.insert(*v);
                    }
                }
                GraphEdge::Return => {
                    if matches!(store.pointers[src], Pointer::Fn(_)) {
                        continue;
                    }
                    let callees = match points_to.get(&src) {
                        Some(c) => c.clone(),
                        None => continue,
                    };
                    for callee in callees {
                        let callee = store.pointers.insert(callee.into());
                        let return_node = store.pointers.insert(Pointer::ReturnValue(callee));
                        let return_types = match points_to.get(&return_node) {
                            Some(r) => r.clone(),
                            None => continue,
                        };
                        for return_ty in return_types {
                            changed |= points_to.entry(dest).or_default().insert(return_ty);
                        }
                    }
                }
                GraphEdge::Prop => {
                    let (obj, name) = match store.pointers[dest] {
                        Pointer::Prop(obj, name) => (obj, name),
                        _ => unreachable!(),
                    };

                    let concrete_objects = match points_to.get(&obj) {
                        Some(o) => o.clone(),
                        None => continue,
                    };

                    for concrete_object in concrete_objects {
                        let concrete_object = store.pointers.insert(concrete_object.into());
                        let prop = store.pointers.insert(Pointer::Prop(concrete_object, name));

                        // Values stored in a property of an invalid pointer must be invalidated.
                        if store.invalid_pointers.contains(&concrete_object) {
                            changed |= points_to
                                .entry(dest)
                                .or_default()
                                .insert(ConcretePointer::Unknown);
                            changed |= store.invalid_pointers.insert(dest);
                            changed |= store.invalid_pointers.insert(prop);
                        }

                        let props = match points_to.get(&prop) {
                            Some(p) => p.clone(),
                            None => continue,
                        };

                        for prop in props {
                            changed |= points_to.entry(dest).or_default().insert(prop);
                        }
                    }
                }
                GraphEdge::Arg => {
                    let (callee, index) = match store.pointers[src] {
                        Pointer::Arg(callee, index) => (callee, index),
                        _ => unreachable!(),
                    };

                    let concrete_callees = match points_to.get(&callee) {
                        Some(c) => c.clone(),
                        None => continue,
                    };

                    let concrete_values = match points_to.get(&src) {
                        Some(c) => c.clone(),
                        None => continue,
                    };

                    for concrete_callee in concrete_callees {
                        match concrete_callee {
                            ConcretePointer::Fn(callee) => {
                                match store
                                    .functions
                                    .get(&callee)
                                    .unwrap()
                                    .param_indices()
                                    .nth(index)
                                {
                                    Some(param) => {
                                        let param = store.pointers.insert(Pointer::Var(param));
                                        for value in &concrete_values {
                                            changed |=
                                                points_to.entry(param).or_default().insert(*value);
                                        }
                                    }
                                    None => {
                                        // Don't invalidate extra arguments. The function cannot access them
                                        // unless it uses e.g. arguments array, which is detected else where.
                                    }
                                }
                            }

                            ConcretePointer::Unknown => {
                                // Invalidate arguments passed to unknown callers.
                                for &value in &concrete_values {
                                    changed |= store
                                        .invalid_pointers
                                        .insert(store.pointers.insert(value.into()));
                                }
                            }

                            ConcretePointer::Object(_)
                            | ConcretePointer::NullOrVoid
                            | ConcretePointer::Bool
                            | ConcretePointer::Num
                            | ConcretePointer::String
                            | ConcretePointer::BigInt
                            | ConcretePointer::Regex => {}
                        }
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
}

fn analyse(
    ast: &ast::Program,
    unresolved_ctxt: SyntaxContext,
) -> (Store, FxHashMap<PointerId, FxHashSet<ConcretePointer>>) {
    let mut store = Store {
        unresolved_ctxt,
        functions: FxHashMap::default(),
        names: IndexSet::default(),
        vars: IndexSet::default(),
        pointers: IndexSet::default(),
        references: FxHashSet::default(),
        invalid_pointers: FxHashSet::default(),
    };

    {
        let mut v = DeclFinder {
            names: &mut store.names,
            vars: &mut store.vars,
            var_start: VarId::from_u32(0),
        };
        ast.visit_with(&mut v);
    }

    {
        let mut visitor = FnVisitor {
            store: &mut store,
            cur_var_start: VarId::from_u32(0),
            cur_fn: None,
        };
        ast.visit_with(&mut visitor);
    }

    let mut graph = compute_relations(ast, &mut store);
    let points_to = compute_points_to_map(&mut graph, &mut store);

    (store, points_to)
}

fn compute_relations(ast: &ast::Program, store: &mut Store) -> DiGraphMap<PointerId, GraphEdge> {
    let mut graph = DiGraphMap::new();

    let mut visitor = GraphVisitor {
        store,
        graph: &mut graph,
        cur_fn: None,
    };
    ast.visit_with(&mut visitor);

    if cfg!(debug_assertions) && OUTPUT_RELO_GRAPH {
        for id in 0..store.pointers.len() {
            let id = PointerId::from_usize(id);
            graph.add_node(id);
        }

        let map = |store: &Store, n: PointerId| {
            let value: String = match &store.pointers[n] {
                Pointer::Prop(obj, prop) => {
                    format!(
                        "Prop(Object PointerId:{}, prop:(NameId:{}, '{}'))",
                        obj.as_u32(),
                        prop.as_u32(),
                        store.names[*prop]
                    )
                }
                Pointer::Var(id) => format!(
                    "Var(VarId:{}, '{}')",
                    id.as_u32(),
                    store.names[store.vars[*id].0]
                ),
                Pointer::Object(id) => format!("Object(NodeId:{})", id.as_u32()),
                Pointer::Fn(id) => format!("Fn(NodeId:{})", id.as_u32()),
                Pointer::Unknown => "Unknown".into(),
                Pointer::NullOrVoid => "NullOrVoid".into(),
                Pointer::Bool => "Bool".into(),
                Pointer::Num => "Num".into(),
                Pointer::String => "String".into(),
                Pointer::BigInt => "BigInt".into(),
                Pointer::Regex => "Regex".into(),
                Pointer::ReturnValue(id) => {
                    format!("ReturnValue(Callee PointerId:{})", id.as_u32())
                }
                Pointer::Arg(func, index) => {
                    format!("Arg(func PointerId:{}, index:{})", func.as_u32(), index)
                }
            };
            format!("id:{}, {}", n.as_u32(), value)
        };

        let print_graph: petgraph::prelude::DiGraph<String, GraphEdge> = graph
            .clone()
            .into_graph()
            .map(|_, n| map(store, *n), |_, e| *e);
        let dot = format!("{}", petgraph::dot::Dot::with_config(&print_graph, &[]));

        std::fs::write("./relo.dot", dot).expect("Failed to output fn graph");
    }

    graph
}

struct GraphVisitor<'a> {
    store: &'a mut Store,
    graph: &'a mut DiGraphMap<PointerId, GraphEdge>,
    cur_fn: Option<NodeId>,
}

impl GraphVisitor<'_> {
    fn get_rhs(&mut self, expr: &Expr) -> Vec<PointerId> {
        match expr {
            Expr::This(_) => vec![self.store.pointers.insert(Pointer::Unknown)],
            Expr::Array(n) => {
                for element in &n.elems {
                    match element {
                        Some(ExprOrSpread::Spread(_)) => todo!(),
                        Some(ExprOrSpread::Expr(element)) => {
                            let value = self.get_rhs(element);
                            self.invalidate(&value);
                        }
                        None => {}
                    }
                }
                vec![self.store.pointers.insert(Pointer::Unknown)]
            }
            Expr::Object(n) => {
                let obj = self.store.pointers.insert(Pointer::Object(n.node_id));

                let is_simple_obj_lit = n.props.iter().all(|p| match p {
                    Prop::KeyValue(p) => is_simple_prop_name(&p.key, self.store.unresolved_ctxt),
                    _ => false,
                });

                if is_simple_obj_lit {
                    for prop in &n.props {
                        match prop {
                            Prop::KeyValue(prop) => {
                                let key = PropKey::from_prop_name(
                                    &prop.key,
                                    self.store.unresolved_ctxt,
                                    &mut self.store.names,
                                )
                                .expect("checked above");
                                let value = self.get_rhs(&prop.value);
                                let prop = self.get_prop_value(obj, key.0);
                                self.reference_prop(obj, key);
                                for value in value {
                                    self.make_subset_of(value, prop);
                                }
                            }
                            Prop::Getter(_)
                            | Prop::Setter(_)
                            | Prop::Method(_)
                            | Prop::Spread(_) => {
                                unreachable!("checked above")
                            }

                            Prop::Shorthand(_) => unreachable!("normalised away"),
                            Prop::Assign(_) => unreachable!("invalid for obj lit"),
                        }
                    }
                } else {
                    self.invalidate(&[obj]);
                }
                vec![obj]
            }
            Expr::Fn(n) => {
                n.function.visit_with(self);
                let func = self.store.pointers.insert(Pointer::Fn(n.function.node_id));
                if let Some(name) = &n.ident {
                    let name = Id::new(name, &mut self.store.names);
                    let var = self.store.vars.get_index(&name).unwrap();
                    let var = self.store.pointers.insert(Pointer::Var(var));
                    self.make_subset_of(func, var);
                }
                vec![func]
            }
            Expr::Unary(n) => {
                n.visit_children_with(self);
                // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-unary-operators
                match n.op {
                    UnaryOp::Plus => vec![self.store.pointers.insert(Pointer::Num)],
                    UnaryOp::TypeOf => vec![self.store.pointers.insert(Pointer::String)],
                    UnaryOp::Void => vec![self.store.pointers.insert(Pointer::NullOrVoid)],
                    UnaryOp::Bang | UnaryOp::Delete => {
                        vec![self.store.pointers.insert(Pointer::Bool)]
                    }
                    // Output type depends in input type.
                    UnaryOp::Minus | UnaryOp::Tilde => {
                        vec![self.store.pointers.insert(Pointer::Unknown)]
                    }
                }
            }
            Expr::Update(n) => {
                n.visit_children_with(self);
                vec![self.store.pointers.insert(Pointer::Unknown)]
            }
            Expr::Bin(n) => {
                match n.op {
                    BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::NullishCoalescing => {
                        // TODO: if LHS is object, then we know if RHS will execute.
                        let mut left = self.get_rhs(&n.left);
                        let mut right = self.get_rhs(&n.right);
                        left.append(&mut right);
                        left
                    }
                    _ => {
                        n.visit_children_with(self);
                        match n.op {
                            // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-equality-operators
                            BinaryOp::EqEq
                            | BinaryOp::NotEq
                            | BinaryOp::EqEqEq
                            | BinaryOp::NotEqEq => vec![self.store.pointers.insert(Pointer::Bool)],
                            // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-relational-operators
                            BinaryOp::Lt
                            | BinaryOp::LtEq
                            | BinaryOp::Gt
                            | BinaryOp::GtEq
                            | BinaryOp::In
                            | BinaryOp::InstanceOf => {
                                vec![self.store.pointers.insert(Pointer::Bool)]
                            }
                            // TODO: we can infer the output type based on the input types.
                            // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-applystringornumericbinaryoperator
                            BinaryOp::LShift
                            | BinaryOp::RShift
                            | BinaryOp::ZeroFillRShift
                            | BinaryOp::Add
                            | BinaryOp::Sub
                            | BinaryOp::Mul
                            | BinaryOp::Div
                            | BinaryOp::Mod
                            | BinaryOp::BitOr
                            | BinaryOp::BitXor
                            | BinaryOp::BitAnd
                            | BinaryOp::Exp => vec![self.store.pointers.insert(Pointer::Unknown)],

                            BinaryOp::LogicalOr
                            | BinaryOp::LogicalAnd
                            | BinaryOp::NullishCoalescing => unreachable!("handled above"),
                        }
                    }
                }
            }
            Expr::Assign(n) => {
                let lhs = match &n.left {
                    ast::PatOrExpr::Expr(n) => PatOrExpr::Expr(n),
                    ast::PatOrExpr::Pat(n) => PatOrExpr::Pat(n),
                };
                self.record_assignment(lhs, &n.right, n.op)
            }
            Expr::Member(n) => match &n.obj {
                ExprOrSuper::Super(_) => todo!(),
                ExprOrSuper::Expr(obj) => {
                    let mut obj = self.get_rhs(obj);

                    if let Some(prop) = PropKey::from_expr(
                        &n.prop,
                        self.store.unresolved_ctxt,
                        n.computed,
                        &mut self.store.names,
                    ) {
                        for obj in &mut obj {
                            self.reference_prop(*obj, prop);
                            *obj = self.get_prop_value(*obj, prop.0);
                        }
                        obj
                    } else {
                        self.invalidate(&obj);
                        n.prop.visit_with(self);
                        vec![self.store.pointers.insert(Pointer::Unknown)]
                    }
                }
            },
            Expr::Cond(n) => {
                n.test.visit_with(self);
                let mut cons = self.get_rhs(&n.cons);
                let mut alt = self.get_rhs(&n.alt);
                cons.append(&mut alt);
                cons
            }
            Expr::Call(n) => match &n.callee {
                ExprOrSuper::Super(_) => todo!(),
                ExprOrSuper::Expr(callee) => {
                    let mut callee = self.get_rhs(callee);
                    let args = n
                        .args
                        .iter()
                        .map(|arg| match arg {
                            ExprOrSpread::Spread(_) => todo!(),
                            ExprOrSpread::Expr(arg) => self.get_rhs(arg),
                        })
                        .collect::<Vec<_>>();
                    for callee in &callee {
                        for (i, arg_values) in args.iter().enumerate() {
                            for value in arg_values {
                                let arg_pointer =
                                    self.store.pointers.insert(Pointer::Arg(*callee, i));
                                self.make_subset_of(*value, arg_pointer);
                                self.graph.add_edge(arg_pointer, *callee, GraphEdge::Arg);
                            }
                        }
                    }
                    for callee in &mut callee {
                        *callee = self.get_return_value(*callee);
                    }
                    callee
                }
            },
            Expr::New(n) => {
                let callee = self.get_rhs(&n.callee);
                self.invalidate(&callee);
                if let Some(args) = &n.args {
                    for arg in args {
                        match arg {
                            ExprOrSpread::Spread(_) => todo!(),
                            ExprOrSpread::Expr(arg) => {
                                let value = self.get_rhs(arg);
                                self.invalidate(&value);
                            }
                        }
                    }
                }
                vec![self.store.pointers.insert(Pointer::Unknown)]
            }
            Expr::Seq(n) => {
                debug_assert!(!n.exprs.is_empty());

                let mut i = 0;
                while i < n.exprs.len() - 1 {
                    n.exprs[i].visit_with(self);
                    i += 1;
                }

                self.get_rhs(&n.exprs[i])
            }
            Expr::Ident(n) => {
                if n.ctxt == self.store.unresolved_ctxt {
                    if n.sym == js_word!("undefined") {
                        vec![self.store.pointers.insert(Pointer::NullOrVoid)]
                    } else {
                        vec![self.store.pointers.insert(Pointer::Unknown)]
                    }
                } else {
                    let name = Id::new(n, &mut self.store.names);
                    let var = self.store.vars.get_index(&name).unwrap();
                    vec![self.store.pointers.insert(Pointer::Var(var))]
                }
            }
            Expr::Lit(n) => match n {
                Lit::Str(_) => vec![self.store.pointers.insert(Pointer::String)],
                Lit::Bool(_) => vec![self.store.pointers.insert(Pointer::Bool)],
                Lit::Null(_) => vec![self.store.pointers.insert(Pointer::NullOrVoid)],
                Lit::Num(_) => vec![self.store.pointers.insert(Pointer::Num)],
                Lit::BigInt(_) => vec![self.store.pointers.insert(Pointer::BigInt)],
                Lit::Regex(_) => vec![self.store.pointers.insert(Pointer::Regex)],
                Lit::JSXText(_) => unreachable!(),
            },
            Expr::Tpl(n) => {
                n.visit_children_with(self);
                vec![self.store.pointers.insert(Pointer::Unknown)]
            }
            Expr::TaggedTpl(n) => {
                n.tag.visit_with(self);
                for expr in &n.tpl.exprs {
                    let obj = self.get_rhs(expr);
                    // Expressions in tagged templates can be accessed by the tag function.
                    self.invalidate(&obj);
                }
                vec![self.store.pointers.insert(Pointer::Unknown)]
            }
            Expr::Arrow(n) => {
                n.params.visit_with(self);
                let old = self.cur_fn;
                self.cur_fn = Some(n.node_id);
                n.body.visit_with(self);
                self.cur_fn = old;
                vec![self.store.pointers.insert(Pointer::Fn(n.node_id))]
            }
            Expr::Class(_) => todo!(),
            Expr::Yield(n) => {
                if let Some(arg) = &n.arg {
                    let value = self.get_rhs(arg);
                    self.invalidate(&value);
                }
                vec![self.store.pointers.insert(Pointer::Unknown)]
            }
            Expr::MetaProp(n) => {
                n.visit_children_with(self);
                vec![self.store.pointers.insert(Pointer::Unknown)]
            }
            Expr::Await(n) => {
                n.visit_children_with(self);
                vec![self.store.pointers.insert(Pointer::Unknown)]
            }
            Expr::Paren(n) => self.get_rhs(&n.expr),
            Expr::PrivateName(_) => todo!(),
            Expr::OptChain(_) => todo!(),

            Expr::JSXMember(_)
            | Expr::JSXNamespacedName(_)
            | Expr::JSXEmpty(_)
            | Expr::JSXElement(_)
            | Expr::JSXFragment(_)
            | Expr::Invalid(_) => unreachable!(),
        }
    }

    /// Assigns the given value to the [`Slot`].
    fn assign_to_slot(&mut self, slot: Option<Vec<Slot>>, rhs: &[PointerId]) {
        if let Some(slot) = slot {
            for slot in slot {
                let lhs = match slot {
                    Slot::Var(name) => self.store.pointers.insert(Pointer::Var(name)),
                    Slot::Prop(obj, key) => self.get_prop_value(obj, key),
                };
                for rhs in rhs {
                    self.make_subset_of(*rhs, lhs);
                }
            }
        } else {
            // Unknown/invalid assignment target.
            self.invalidate(rhs);
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
    fn record_assignment(&mut self, lhs: PatOrExpr, rhs: &Expr, op: AssignOp) -> Vec<PointerId> {
        let conditional_assign = matches!(
            op,
            AssignOp::AndAssign | AssignOp::OrAssign | AssignOp::NullishAssign
        );
        match lhs {
            PatOrExpr::Pat(pat @ (Pat::Array(_) | Pat::Object(_))) => {
                debug_assert!(!conditional_assign, "invalid assignment target");
                self.handle_destructuring(pat, rhs)
            }
            _ => {
                let lhs = self.visit_and_get_slot(lhs);
                let rhs = self.get_rhs(rhs);

                self.assign_to_slot(lhs, &rhs);
                rhs
            }
        }
    }

    fn handle_destructuring(&mut self, lhs: &Pat, rhs: &Expr) -> Vec<PointerId> {
        let rhs = self.get_rhs(rhs);
        self.visit_destructuring(lhs, &rhs);
        rhs
    }

    fn visit_destructuring(&mut self, lhs: &Pat, rhs: &[PointerId]) {
        match lhs {
            Pat::Object(lhs) => {
                let has_complex_props = lhs.props.iter().any(|p| match p {
                    ObjectPatProp::KeyValue(p) => {
                        !is_simple_prop_name(&p.key, self.store.unresolved_ctxt)
                    }
                    ObjectPatProp::Assign(_) | ObjectPatProp::Rest(_) => false,
                });
                if has_complex_props {
                    self.invalidate(rhs);
                }

                for prop in &lhs.props {
                    match prop {
                        ObjectPatProp::KeyValue(prop) => {
                            let key = PropKey::from_prop_name(
                                &prop.key,
                                self.store.unresolved_ctxt,
                                &mut self.store.names,
                            )
                            .unwrap();
                            for rhs in rhs {
                                self.reference_prop(*rhs, key);
                            }

                            let new_rhs = rhs
                                .iter()
                                .map(|v| self.get_prop_value(*v, key.0))
                                .collect::<Vec<_>>();

                            self.visit_destructuring(&prop.value, &new_rhs);
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
                            let slot = self
                                .get_var_id_from_ident(&arg.id)
                                .map(|v| vec![Slot::Var(v)]);
                            self.assign_to_slot(slot, rhs);
                        }
                    }
                }
            }

            Pat::Ident(lhs) => {
                let slot = self
                    .get_var_id_from_ident(&lhs.id)
                    .map(|v| vec![Slot::Var(v)]);
                self.assign_to_slot(slot, rhs);
            }
            Pat::Array(lhs) => {
                self.invalidate(rhs);
                for element in lhs.elems.iter().filter_map(|e| e.as_ref()) {
                    if let Pat::Expr(elem) = element {
                        todo!();
                        // self.invalidate_slot(Node::from(elem.as_ref()));
                    } else {
                        let rhs = vec![self.store.pointers.insert(Pointer::Unknown)];
                        self.visit_destructuring(element, &rhs);
                    }
                }
            }
            Pat::Rest(lhs) => {
                self.invalidate(rhs);
                let rhs = vec![self.store.pointers.insert(Pointer::Unknown)];
                self.visit_destructuring(&lhs.arg, &rhs);
            }
            Pat::Assign(lhs) => {
                let mut default_value = self.get_rhs(&lhs.right);
                default_value.extend_from_slice(rhs);

                self.visit_destructuring(&lhs.left, &default_value);
            }
            Pat::Invalid(_) | Pat::Expr(_) => unreachable!(),
        }
    }

    fn get_var_id_from_ident(&mut self, ident: &Ident) -> Option<VarId> {
        if ident.ctxt == self.store.unresolved_ctxt {
            None
        } else {
            let name = self.store.names.get_index(&ident.sym).unwrap();
            let id = Id(name, ident.ctxt);
            let id = self.store.vars.get_index(&id).unwrap();

            Some(id)
        }
    }

    /// Visits the given expression. If the expression resolves to a valid assignment target, a [`Slot`]
    /// representing that target is returned.
    fn visit_and_get_slot(&mut self, node: PatOrExpr) -> Option<Vec<Slot>> {
        let lhs_expr = match node {
            PatOrExpr::Expr(node) => node,
            PatOrExpr::Pat(node) => match node {
                Pat::Ident(node) => {
                    return self
                        .get_var_id_from_ident(&node.id)
                        .map(|v| vec![Slot::Var(v)])
                }
                Pat::Expr(e) => e.as_ref(),
                _ => {
                    dbg!(node);
                    unreachable!();
                }
            },
        };
        match lhs_expr {
            Expr::Ident(node) => self.get_var_id_from_ident(node).map(|v| vec![Slot::Var(v)]),
            Expr::Member(node) => {
                let obj = match &node.obj {
                    ExprOrSuper::Super(_) => todo!(),
                    ExprOrSuper::Expr(obj) => self.get_rhs(obj),
                };

                if let Some(prop) = PropKey::from_expr(
                    &node.prop,
                    self.store.unresolved_ctxt,
                    node.computed,
                    &mut self.store.names,
                ) {
                    Some(
                        obj.into_iter()
                            .map(|obj| {
                                self.reference_prop(obj, prop);
                                Slot::Prop(obj, prop.0)
                            })
                            .collect(),
                    )
                } else {
                    self.invalidate(&obj);
                    node.prop.visit_with(self);
                    None
                }
            }
            // All other nodes cannot evaluate to a reference, and should return None (but we still
            // need to visit their children).
            _ => {
                lhs_expr.visit_with(self);
                None
            }
        }
    }

    fn reference_prop(&mut self, object: PointerId, key: PropKey) {
        debug_assert!(
            !self.store.references.contains(&(key, object)),
            "should not reference same prop twice"
        );
        self.store.references.insert((key, object));
    }

    fn get_prop_value(&mut self, obj: PointerId, prop: NameId) -> PointerId {
        let access = self.store.pointers.insert(Pointer::Prop(obj, prop));
        self.graph.add_edge(obj, access, GraphEdge::Prop);
        access
    }

    fn get_return_value(&mut self, callee: PointerId) -> PointerId {
        let ret = self.store.pointers.insert(Pointer::ReturnValue(callee));
        self.graph.add_edge(callee, ret, GraphEdge::Return);
        ret
    }

    fn make_subset_of(&mut self, sub: PointerId, sup: PointerId) {
        self.graph.add_edge(sub, sup, GraphEdge::Subset);
    }

    fn invalidate(&mut self, value: &[PointerId]) {
        for value in value {
            self.store.invalid_pointers.insert(*value);
        }
    }
}

impl Visit<'_> for GraphVisitor<'_> {
    fn visit_stmt(&mut self, n: &Stmt) {
        match n {
            Stmt::With(n) => {
                let obj = self.get_rhs(&n.obj);
                self.invalidate(&obj);
                n.body.visit_with(self);
            }
            Stmt::Return(n) => {
                let cur_fn = match self.cur_fn {
                    Some(cur_fn) => cur_fn,
                    None => {
                        if let Some(value) = &n.arg {
                            let rhs = self.get_rhs(value);
                            self.invalidate(&rhs);
                        }
                        return;
                    }
                };
                let cur_fn = self.store.pointers.insert(Pointer::Fn(cur_fn));
                let lhs = self.get_return_value(cur_fn);
                if let Some(value) = &n.arg {
                    let rhs = self.get_rhs(value);
                    for rhs in rhs {
                        self.make_subset_of(rhs, lhs);
                    }
                } else {
                    let rhs = self.store.pointers.insert(Pointer::NullOrVoid);
                    self.make_subset_of(rhs, lhs);
                }
            }
            Stmt::Throw(n) => {
                let value = self.get_rhs(&n.arg);
                self.invalidate(&value);
            }
            Stmt::ForIn(ForInStmt {
                left, right, body, ..
            })
            | Stmt::ForOf(ForOfStmt {
                left, right, body, ..
            }) => {
                left.visit_with(self);
                let rhs = self.get_rhs(right);
                self.invalidate(&rhs);
                body.visit_with(self);
            }
            _ => {
                n.visit_children_with(self);
            }
        }
    }

    fn visit_decl(&mut self, n: &Decl) {
        match n {
            Decl::Class(_) => todo!(),
            Decl::Fn(func) => {
                let name = Id::new(&func.ident, &mut self.store.names);
                let var = self.store.vars.get_index(&name).unwrap();
                let f = self
                    .store
                    .pointers
                    .insert(Pointer::Fn(func.function.node_id));
                let var = self.store.pointers.insert(Pointer::Var(var));
                self.make_subset_of(f, var);
                func.visit_children_with(self);
            }
            Decl::Var(var) => {
                var.visit_children_with(self);
            }
        }
    }

    fn visit_function(&mut self, n: &Function) {
        n.params.visit_with(self);
        n.decorators.visit_with(self);
        let old = self.cur_fn;
        self.cur_fn = Some(n.node_id);
        n.body.visit_with(self);
        self.cur_fn = old;
    }

    fn visit_arrow_expr(&mut self, n: &ArrowExpr) {
        n.params.visit_with(self);
        let old = self.cur_fn;
        self.cur_fn = Some(n.node_id);
        n.body.visit_with(self);
        self.cur_fn = old;
    }

    fn visit_var_declarator(&mut self, n: &VarDeclarator) {
        if let Some(rhs) = &n.init {
            self.record_assignment(PatOrExpr::Pat(&n.name), rhs, AssignOp::Assign);
        } else {
            n.name.visit_with(self);
        }
    }

    fn visit_expr(&mut self, n: &Expr) {
        self.get_rhs(n);
    }

    fn visit_module_decl(&mut self, _: &ModuleDecl) {
        todo!();
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum GraphEdge {
    Subset,
    Return,
    Prop,
    Arg,
}

impl Display for GraphEdge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum Pointer {
    Prop(PointerId, NameId),
    Var(VarId),
    Object(NodeId),
    Fn(NodeId),
    Unknown,
    NullOrVoid,
    Bool,
    Num,
    String,
    BigInt,
    Regex,
    ReturnValue(PointerId),
    Arg(PointerId, usize),
}

impl From<ConcretePointer> for Pointer {
    fn from(value: ConcretePointer) -> Self {
        match value {
            ConcretePointer::Object(id) => Pointer::Object(id),
            ConcretePointer::Fn(id) => Pointer::Fn(id),
            ConcretePointer::Unknown => Pointer::Unknown,
            ConcretePointer::NullOrVoid => Pointer::NullOrVoid,
            ConcretePointer::Bool => Pointer::Bool,
            ConcretePointer::Num => Pointer::Num,
            ConcretePointer::String => Pointer::String,
            ConcretePointer::BigInt => Pointer::BigInt,
            ConcretePointer::Regex => Pointer::Regex,
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum ConcretePointer {
    Object(NodeId),
    Fn(NodeId),
    Unknown,
    NullOrVoid,
    Bool,
    Num,
    String,
    BigInt,
    Regex,
}

/// A place where a variable can be stored.
#[derive(Debug, Clone, Copy)]
enum Slot {
    Var(VarId),
    /// (Object, Property name)
    Prop(PointerId, NameId),
}

#[derive(Debug)]
struct StaticFunctionData {
    var_start: u32,
    param_end: u32,
    accesses_arguments_array: bool,
}

impl StaticFunctionData {
    fn param_indices(&self) -> impl Iterator<Item = VarId> {
        VarId::from_u32(self.var_start)..VarId::from_u32(self.param_end)
    }
}

index::newtype_index!(struct PointerId { .. });

#[derive(Debug)]
struct Store {
    unresolved_ctxt: SyntaxContext,
    functions: FxHashMap<NodeId, StaticFunctionData>,
    names: IndexSet<NameId, JsWord>,
    vars: IndexSet<VarId, Id>,
    pointers: IndexSet<PointerId, Pointer>,
    references: FxHashSet<(PropKey, PointerId)>,
    invalid_pointers: FxHashSet<PointerId>,
}
struct DeclFinder<'a> {
    names: &'a mut IndexSet<NameId, JsWord>,
    vars: &'a mut IndexSet<VarId, Id>,

    var_start: VarId,
}

impl DeclFinder<'_> {
    fn record_var(&mut self, ident: &Ident) {
        let id = Id::new(ident, self.names);
        // Var should not have been previously defined (unless it was within the
        // current function).
        debug_assert!(
            !self.vars.contains(&id) || self.vars.get_index(&id).unwrap() >= self.var_start
        );
        self.vars.insert(id);
    }
}

impl<'ast> Visit<'ast> for DeclFinder<'_> {
    // Don't visit nested functions.
    fn visit_function(&mut self, _node: &Function) {}
    fn visit_constructor(&mut self, _node: &Constructor) {}
    fn visit_arrow_expr(&mut self, _node: &ArrowExpr) {}
    fn visit_getter_prop(&mut self, _node: &GetterProp) {}
    fn visit_setter_prop(&mut self, _node: &SetterProp) {}

    // Function names are in scope.
    fn visit_fn_decl(&mut self, node: &FnDecl) {
        self.record_var(&node.ident);
    }

    // Expressions can't declare new vars.
    fn visit_expr(&mut self, _node: &Expr) {}
    fn visit_prop_name(&mut self, _node: &PropName) {}

    fn visit_binding_ident(&mut self, node: &BindingIdent) {
        self.record_var(&node.id);
    }

    // The key of AssignPatProp is LHS but is an Ident, so won't be caught by
    // the BindingIdent visitor above.
    fn visit_assign_pat_prop(&mut self, p: &AssignPatProp) {
        self.record_var(&p.key);
    }
}

struct FnVisitor<'s> {
    store: &'s mut Store,
    cur_var_start: VarId,
    cur_fn: Option<NodeId>,
}

impl<'ast> FnVisitor<'_> {
    fn handle_fn<T>(&mut self, node: &'ast T, fn_expr_name: Option<&'ast Ident>)
    where
        T: FunctionLike<'ast> + GetNodeId,
    {
        let var_start = self.store.vars.len().try_into().unwrap();

        let mut v = DeclFinder {
            names: &mut self.store.names,
            vars: &mut self.store.vars,
            var_start: VarId::from_u32(var_start),
        };

        for param in node.params() {
            if let Pat::Ident(param) = param {
                v.record_var(&param.id);
            } else {
                todo!("non-ident param");
            }
        }

        let param_end = v.vars.len().try_into().unwrap();

        // FnExpr's name is local to it. Although the name comes before the
        // params, we record it afterwards to simplify tracking of where
        // vars/params start/end.
        if let Some(fn_expr_name) = fn_expr_name {
            v.record_var(fn_expr_name);
        }

        node.visit_body_with(&mut v);

        let static_data = StaticFunctionData {
            var_start,
            param_end,
            accesses_arguments_array: false,
        };

        self.store.functions.insert(node.node_id(), static_data);

        let old_var_start = self.cur_var_start;
        self.cur_var_start = VarId::from_u32(var_start);
        let old_cur_fn = self.cur_fn;
        self.cur_fn = Some(node.node_id());
        node.visit_body_with(self);
        self.cur_fn = old_cur_fn;
        self.cur_var_start = old_var_start;
    }
}

impl<'ast> Visit<'ast> for FnVisitor<'_> {
    fn visit_ident(&mut self, node: &'ast Ident) {
        if let Some(func) = self.cur_fn {
            if node.sym == js_word!("arguments") && node.ctxt == self.store.unresolved_ctxt {
                self.store
                    .functions
                    .get_mut(&func)
                    .unwrap()
                    .accesses_arguments_array = true;
            }
        }
    }

    fn visit_fn_decl(&mut self, node: &'ast FnDecl) {
        self.handle_fn(&node.function, None);
    }
    fn visit_fn_expr(&mut self, node: &'ast FnExpr) {
        self.handle_fn(&node.function, node.ident.as_ref());
    }
    fn visit_function(&mut self, node: &'ast Function) {
        self.handle_fn(node, None);
    }
    fn visit_constructor(&mut self, node: &'ast Constructor) {
        self.handle_fn(node, None);
    }
    fn visit_arrow_expr(&mut self, node: &'ast ArrowExpr) {
        self.handle_fn(node, None);
    }
    fn visit_getter_prop(&mut self, node: &'ast GetterProp) {
        self.handle_fn(node, None);
    }
    fn visit_setter_prop(&mut self, node: &'ast SetterProp) {
        self.handle_fn(node, None);
    }
}

enum PatOrExpr<'a> {
    Pat(&'a Pat),
    Expr(&'a Expr),
}