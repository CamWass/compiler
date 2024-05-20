// #![allow(non_snake_case)]
#![warn(non_upper_case_globals)]
#![warn(unused_variables)]
#![deny(non_shorthand_field_patterns)]
#![warn(warnings)]
#![warn(dead_code)]
#![deny(unused_imports)]

use std::collections::hash_map::Entry;
use std::convert::TryInto;
use std::fmt::Display;

use crate::find_vars::*;
use crate::optimize_properties2::simple_set::IndexSet;
use crate::optimize_properties2::unionfind::UnionFind;
use crate::optimize_properties2::{Id, NameId, PropKey, Renamer};
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

/*
TODO:
invalidate:
    params/args when arguments array is used
    params that can't be statically bound? e.g. rest params
    Unknown/invalid assignment target
    things stored in unknown/invalid vars
    things stored in properties of unknown/invalid things
    things that are index using computed props (either read/write)
    complex destructuring targets or spread targets
    things stored in arrays
    arguments passed to unknown callees
    X in `new X()`
    X in `yield X`
    X in `yield X`
    X in `throw X`
    X in `return X` when we aren't in a function
    expressions in tagged templates
    X in `for (... in X)`
    X in `for (... of X)`
    X in `try {} catch (X) {}`
    X in `with (X) {}` ??? but we should probably bail early on with statements
*/

pub fn process(
    ast: &mut ast::Program,
    program_data: &mut ProgramData,
    unresolved_ctxt: SyntaxContext,
) {
    let (mut store, points_to) = analyse(ast, unresolved_ctxt);

    dbg!(&store, &points_to);

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

    // TODO:
    // These could be reserve space for built-ins using a loop to calculate the capacities, which
    // the compiler may be able to optimise away, since it's an iteration over a static slice.
    let mut objects: FxHashMap<ConcretePointer, Object> = FxHashMap::default();
    let mut properties: IndexVec<PropId, Property> = IndexVec::default();

    for (PropKey(name, node_id), pointer) in &store.references {
        let objs = points_to.get(pointer).unwrap();
        if objs.len() == 1 {
            let obj = *objs.iter().next().unwrap();
            let object = objects.entry(obj).or_default();
            let id = match object.properties.entry(*name) {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(entry) => {
                    let prop_id = properties.push(Property {
                        name: *name,
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
            properties[id].references.insert(*node_id);
        } else if objs.len() > 1 {
            // Defer union processing.
            for obj in objs {
                objects.entry(*obj).or_default();
            }
        } else {
            todo!("unreachable?");
        }
    }

    let mut union_find = UnionFind::new(properties.len());

    // Process unions.
    for (PropKey(name, node_id), pointer) in &store.references {
        let objs = points_to.get(pointer).unwrap();
        if objs.len() > 1 {
            let mut props = objs
                .iter()
                .filter_map(|c| objects.get(c).and_then(|c| c.properties.get(name)));

            let representative = if let Some(&representative) = props.next() {
                properties[representative].references.insert(*node_id);
                representative
            } else {
                let mut references = FxHashSet::default();
                references.insert(*node_id);
                let prop_id = properties.push(Property {
                    name: *name,
                    prop_id: properties.next_index(),
                    references,
                    invalid: false,
                });
                union_find.add(prop_id);
                prop_id
            };

            for &constituent in objs {
                properties[representative].invalid |= store
                    .invalid_pointers
                    .contains(&store.pointers.insert(constituent.into()));
                match objects
                    .get_mut(&constituent)
                    .unwrap()
                    .properties
                    .entry(*name)
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

    const DEBUG_GRAPH: bool = true;

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
        let mut changed = false;
        for (src, dest, kind) in graph.all_edges() {
            match kind {
                GraphEdge::Subset => {
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
                    if matches!(store.pointers[src], Pointer::Object(_)) {
                        continue;
                    }

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
                                    None => todo!("e.g. rest params, args array"),
                                }
                            }
                            ConcretePointer::Object(_)
                            | ConcretePointer::Unknown
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

    // Propagate 'invalid-ness'.
    for pointer in 0..store.pointers.len() {
        let pointer = PointerId::from_usize(pointer);
        if store.invalid_pointers.contains(&pointer) {
            let values = match points_to.get(&pointer) {
                Some(v) => v,
                None => continue,
            };
            for &value in values {
                store
                    .invalid_pointers
                    .insert(store.pointers.insert(value.into()));
            }
        }
    }

    dbg!(&points_to);

    points_to
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
        references: FxHashMap::default(),
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

    // TODO temp
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

    {
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
                n.visit_children_with(self);
                vec![self.store.pointers.insert(Pointer::Unknown)]
            }
            Expr::Object(n) => {
                let obj = self.store.pointers.insert(Pointer::Object(n.node_id));
                for prop in &n.props {
                    match prop {
                        Prop::KeyValue(prop) => {
                            if let Some(key) = PropKey::from_prop_name(
                                &prop.key,
                                self.store.unresolved_ctxt,
                                &mut self.store.names,
                            ) {
                                let value = self.get_rhs(&prop.value);
                                let prop = self.get_prop_value(obj, key.0);
                                self.reference_prop(obj, key);
                                for value in value {
                                    self.make_subset_of(value, prop);
                                }
                            } else {
                                todo!();
                            }
                        }
                        Prop::Getter(_) | Prop::Setter(_) | Prop::Method(_) | Prop::Spread(_) => {
                            todo!()
                        }

                        Prop::Shorthand(_) => unreachable!("normalised away"),
                        Prop::Assign(_) => unreachable!("invalid for obj lit"),
                    }
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
            Expr::Assign(n) => match &n.left {
                PatOrExpr::Expr(left) => {
                    let lhs = match left.as_ref() {
                        Expr::Ident(node) => {
                            let name = Id::new(node, &mut self.store.names);
                            let var = self.store.vars.get_index(&name).unwrap();
                            vec![self.store.pointers.insert(Pointer::Var(var))]
                        }
                        Expr::Member(node) => match &node.obj {
                            ExprOrSuper::Super(_) => todo!(),
                            ExprOrSuper::Expr(obj) => {
                                let mut obj = self.get_rhs(obj);

                                if let Some(prop) = PropKey::from_expr(
                                    &node.prop,
                                    self.store.unresolved_ctxt,
                                    node.computed,
                                    &mut self.store.names,
                                ) {
                                    for obj in &mut obj {
                                        self.reference_prop(*obj, prop);
                                        *obj = self.get_prop_value(*obj, prop.0);
                                    }
                                    obj
                                } else {
                                    todo!();
                                }
                            }
                        },

                        _ => {
                            left.visit_with(self);
                            vec![self.store.pointers.insert(Pointer::Unknown)]
                        }
                    };

                    let rhs = self.get_rhs(&n.right);
                    for lhs in lhs {
                        for rhs in &rhs {
                            self.make_subset_of(*rhs, lhs);
                        }
                    }
                    rhs
                }
                PatOrExpr::Pat(_) => todo!(),
            },
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
                        for obj in obj {
                            self.invalidate(obj);
                        }
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
                n.visit_children_with(self);
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
                n.visit_children_with(self);
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
                n.visit_children_with(self);
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

    fn reference_prop(&mut self, object: PointerId, key: PropKey) {
        self.store.references.insert(key, object);
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

    fn invalidate(&mut self, pointer: PointerId) {
        self.store.invalid_pointers.insert(pointer);
    }
}

impl Visit<'_> for GraphVisitor<'_> {
    fn visit_stmt(&mut self, n: &Stmt) {
        match n {
            Stmt::With(_) => {
                // todo!()
            }
            Stmt::Return(n) => {
                let cur_fn = self.cur_fn.expect("no top level return");
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
            Stmt::Throw(_) => {
                // todo!()
            }
            _ => {}
        }

        n.visit_children_with(self);
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
        match &n.name {
            Pat::Ident(name) => {
                if let Some(init) = &n.init {
                    let rhs = self.get_rhs(init);
                    let name = Id::new(&name.id, &mut self.store.names);
                    let var = self.store.vars.get_index(&name).unwrap();
                    let var = self.store.pointers.insert(Pointer::Var(var));
                    for rhs in rhs {
                        self.make_subset_of(rhs, var);
                    }
                }
            }
            Pat::Array(_) => todo!(),
            Pat::Rest(_) => todo!(),
            Pat::Object(_) => todo!(),
            Pat::Assign(_) => todo!(),
            Pat::Invalid(_) => todo!(),
            Pat::Expr(_) => todo!(),
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

#[derive(Debug)]
struct StaticFunctionData {
    var_start: u32,
    param_end: u32,
}

impl StaticFunctionData {
    fn param_indices(&self) -> impl Iterator<Item = VarId> {
        VarId::from_u32(self.var_start)..VarId::from_u32(self.param_end)
    }

    fn param_count(&self) -> usize {
        (self.param_end - self.var_start) as usize
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
    references: FxHashMap<PropKey, PointerId>,
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
        };

        self.store.functions.insert(node.node_id(), static_data);

        let old_var_start = self.cur_var_start;
        self.cur_var_start = VarId::from_u32(var_start);
        node.visit_body_with(self);
        self.cur_var_start = old_var_start;
    }
}

impl<'ast> Visit<'ast> for FnVisitor<'_> {
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
