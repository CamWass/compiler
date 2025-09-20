#![warn(non_upper_case_globals)]
#![warn(unused_variables)]
#![deny(non_shorthand_field_patterns)]
#![warn(warnings)]
#![warn(dead_code)]
#![deny(unused_imports)]

#[cfg(test)]
mod tests;

mod graph;
mod growable_unionfind;
mod simple_set;
mod unionfind;

use std::collections::hash_map::Entry;
use std::convert::TryInto;

use crate::convert::ecma_number_to_string;
use crate::find_vars::{FunctionLike, VarId};
use crate::utils::unwrap_as;
use crate::DefaultNameGenerator::DefaultNameGenerator;
use ast::*;
use atoms::{js_word, JsWord};
use ecma_visit::{Visit, VisitMut, VisitMutWith, VisitWith};
use global_common::SyntaxContext;
use graph::{Graph, GraphEdge, SmallSet};
use index::bit_set::{BitMatrix, BitSet, GrowableBitSet};
use index::vec::IndexVec;
use petgraph::graph::UnGraph;
use rustc_hash::{FxHashMap, FxHashSet};
use simple_set::IndexSet;
use unionfind::UnionFind;

/// When true, the property interference relations are outputted as a graph in
/// graphviz dot format (debug builds only).
const OUTPUT_PROP_GRAPH: bool = false;
/// When true, the data flow relations are outputted as a graph in graphviz dot
/// format (debug builds only).
const OUTPUT_RELO_GRAPH: bool = false;

/*
TODO:
invalidate:
    params that can't be statically bound? e.g. rest params
*/

pub fn process(
    ast: &mut ast::Program,
    program_data: &mut ProgramData,
    unresolved_ctxt: SyntaxContext,
) {
    let (mut store, points_to) = analyse(ast, unresolved_ctxt);

    let rename_map = create_renaming_map(&mut store, &points_to);

    // Actually assign the new names.
    let mut renamer = Renamer {
        program_data,
        rename_map,
    };

    ast.visit_mut_with(&mut renamer);
}

fn create_renaming_map(store: &mut Store, points_to: &Graph) -> FxHashMap<NodeId, JsWord> {
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

    let mut objects: FxHashMap<PointerId, Object> = FxHashMap::default();
    let mut properties: IndexVec<PropId, Property> = IndexVec::default();

    let mut prop_map: FxHashMap<PropKey, Vec<PointerId>> = FxHashMap::default();

    let unknown_set = SmallSet::single(PointerId::UNKNOWN);

    for (key, pointer) in &store.references {
        let objs = points_to.get_immutable(*pointer).unwrap_or(&unknown_set);
        let prop_map_entry = prop_map.entry(*key).or_default();
        for obj in objs {
            if obj.is_primitive() && !is_built_in_property(obj, &store.names[key.0]) {
                // non-built in prop on primitive - ignore.
                // Note: we don't skip Unknown as we need to track when a prop is accessed
                // on a union with Unknown.
                continue;
            }

            prop_map_entry.push(obj);

            let object = objects.entry(obj).or_default();
            let id = match object.properties.entry(key.0) {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(entry) => {
                    let built_in = is_built_in_property(obj, &store.names[key.0]);
                    let prop_id = properties.push(Property {
                        name: key.0,
                        prop_id: properties.next_index(),
                        references: FxHashSet::default(),
                        invalid: built_in || store.invalid_pointers.contains(obj),
                    });
                    entry.insert(prop_id);
                    prop_id
                }
            };
            if !properties[id].invalid {
                properties[id].references.insert(key.1);
            }
        }
    }

    let mut union_find = UnionFind::new(properties.len());

    if cfg!(debug_assertions) {
        for (pointer, _) in STATIC_POINTERS {
            if pointer == PointerId::UNKNOWN {
                continue;
            }
            if let Some(obj) = &objects.get(&pointer) {
                for prop in &obj.properties {
                    debug_assert!(
                        is_built_in_property(pointer, &store.names[*prop.0]),
                        "primitives should only have static props"
                    );
                }
            }
        }
    }

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
                representative.invalid |= prop.invalid;
                if !representative.invalid {
                    representative
                        .references
                        .extend(prop.references.iter().copied());
                }
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
    while !remaining_nodes.is_empty() {
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
    }

    // Generate new names for the properties that will be renamed.
    let mut name_gen = DefaultNameGenerator::default();
    let mut colour_map = Vec::with_capacity(cur_colour as usize);
    for _ in 0..cur_colour {
        colour_map.push(name_gen.generate_next_name());
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

pub fn analyse(ast: &ast::Program, unresolved_ctxt: SyntaxContext) -> (Store, Graph) {
    let mut pointers = IndexSet::default();

    for (id, pointer) in STATIC_POINTERS {
        let i = pointers.insert(pointer);
        assert_eq!(i, id, "static pointers have been inserted in wrong order");
    }

    let mut store = Store {
        unresolved_ctxt,
        functions: FxHashMap::default(),
        names: IndexSet::default(),
        vars: IndexSet::default(),
        pointers,
        references: FxHashSet::default(),
        invalid_pointers: GrowableBitSet::default(),
        concrete_pointer_bound: PointerId::MAX,
        calls: FxHashSet::default(),
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
        let mut params_to_invalidate = Vec::new();

        let mut visitor = FnVisitor {
            store: &mut store,
            accesses_arguments_array: false,
            params_to_invalidate: &mut params_to_invalidate,
        };
        ast.visit_with(&mut visitor);

        store.concrete_pointer_bound = PointerId::from_usize(store.pointers.len());

        for param in params_to_invalidate {
            let pointer = store.pointers.insert(Pointer::Var(param));
            store.invalid_pointers.insert(pointer);
        }
    }

    let mut graph = compute_relations(ast, &mut store);
    graph.compute_points_to_map(&mut store);

    if cfg!(debug_assertions) {
        for p in &store.pointers {
            match p {
                Pointer::ReturnValue(p) | Pointer::Arg(p, _) => {
                    debug_assert_ne!(*p, PointerId::NULL_OR_VOID);
                    debug_assert_ne!(*p, PointerId::BOOL);
                    debug_assert_ne!(*p, PointerId::NUM);
                    debug_assert_ne!(*p, PointerId::STRING);
                    debug_assert_ne!(*p, PointerId::BIG_INT);
                    debug_assert_ne!(*p, PointerId::REGEX);
                    debug_assert_ne!(*p, PointerId::UNKNOWN);
                }

                Pointer::Prop(obj, name) => {
                    debug_assert_ne!(*obj, PointerId::NULL_OR_VOID);
                    // debug_assert_ne!(*obj, PointerId::UNKNOWN);

                    debug_assert!(
                        !obj.is_primitive() || is_built_in_property(*obj, &store.names[*name])
                    );
                }

                Pointer::Var(_)
                | Pointer::Object(_)
                | Pointer::Fn(_)
                | Pointer::Unknown
                | Pointer::NullOrVoid
                | Pointer::Bool
                | Pointer::Num
                | Pointer::String
                | Pointer::BigInt
                | Pointer::Regex => {}
            }
        }

        for p in store.concrete_pointers() {
            debug_assert!(store.pointers[p].is_concrete());
        }
        for p in store.non_concrete_pointers() {
            debug_assert!(!store.pointers[p].is_concrete());
        }
    }

    if cfg!(debug_assertions) && OUTPUT_RELO_GRAPH {
        let dot = graph.get_dot(&store);

        std::fs::write("./relo.dot", dot).expect("Failed to output fn graph");
    }

    (store, graph)
}

fn compute_relations(ast: &ast::Program, store: &mut Store) -> Graph {
    let mut graph = Graph::default();

    let mut visitor = GraphVisitor {
        store,
        graph: &mut graph,
        cur_fn: None,
    };
    ast.visit_with(&mut visitor);

    graph
}

struct GraphVisitor<'a> {
    store: &'a mut Store,
    graph: &'a mut Graph,
    cur_fn: Option<NodeId>,
}

impl GraphVisitor<'_> {
    fn get_rhs(&mut self, expr: &Expr, used: bool, expr_ctxt: ExprContext) -> Vec<PointerId> {
        macro_rules! ret {
            ($val:expr) => {
                if used {
                    $val
                } else {
                    Vec::new()
                }
            };
        }
        match expr {
            Expr::This(_) => ret!(vec![PointerId::UNKNOWN]),
            Expr::Array(n) => {
                for element in &n.elems {
                    match element {
                        Some(ExprOrSpread::Spread(SpreadElement { expr: element, .. }))
                        | Some(ExprOrSpread::Expr(element)) => {
                            let value = self.get_rhs(element, true, ExprContext::Expression);
                            self.invalidate(&value);
                        }
                        None => {}
                    }
                }
                ret!(vec![PointerId::UNKNOWN])
            }
            Expr::Object(n) => {
                let obj = self
                    .store
                    .pointers
                    .get_index(&Pointer::Object(n.node_id))
                    .unwrap();

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
                                let value =
                                    self.get_rhs(&prop.value, true, ExprContext::Expression);
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

                            Prop::Assign(_) => unreachable!("invalid for obj lit"),
                        }
                    }
                } else {
                    n.props.visit_with(self);
                    self.invalidate(&[obj]);
                }
                ret!(vec![obj])
            }
            Expr::Fn(n) => {
                n.function.visit_with(self);
                let func = self
                    .store
                    .pointers
                    .get_index(&Pointer::Fn(n.function.node_id))
                    .unwrap();
                if let Some(name) = &n.ident {
                    let name = Id::new(name, &mut self.store.names);
                    let var = self.store.vars.get_index(&name).unwrap();
                    let var = self.store.pointers.insert(Pointer::Var(var));
                    self.make_subset_of(func, var);
                }
                ret!(vec![func])
            }
            Expr::Unary(n) => {
                n.visit_children_with(self);
                // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-unary-operators
                match n.op {
                    UnaryOp::Plus => ret!(vec![PointerId::NUM]),
                    UnaryOp::TypeOf => ret!(vec![PointerId::STRING]),
                    UnaryOp::Void => ret!(vec![PointerId::NULL_OR_VOID]),
                    UnaryOp::Bang | UnaryOp::Delete => {
                        ret!(vec![PointerId::BOOL])
                    }
                    // Output type depends in input type, but is always Number or BigInt.
                    UnaryOp::Minus | UnaryOp::Tilde => {
                        ret!(vec![PointerId::NUM, PointerId::BIG_INT])
                    }
                }
            }
            Expr::Update(n) => {
                n.visit_children_with(self);
                // Output type depends in input type, but is always Number or BigInt.
                ret!(vec![PointerId::NUM, PointerId::BIG_INT])
            }
            Expr::Bin(n) => {
                match n.op {
                    BinaryOp::LogicalOr | BinaryOp::LogicalAnd | BinaryOp::NullishCoalescing => {
                        let mut left = self.get_rhs(&n.left, used, ExprContext::Expression);
                        let mut right = self.get_rhs(&n.right, used, ExprContext::Expression);
                        left.append(&mut right);
                        ret!(left)
                    }
                    BinaryOp::In => {
                        self.get_rhs(&n.left, false, ExprContext::Expression);
                        let obj = self.get_rhs(&n.right, true, ExprContext::Expression);

                        if let Some(prop) = PropKey::from_expr(
                            &n.left,
                            self.store.unresolved_ctxt,
                            true,
                            &mut self.store.names,
                        ) {
                            for obj in &obj {
                                self.reference_prop(*obj, prop);
                            }
                        } else {
                            self.invalidate(&obj);
                        }
                        ret!(vec![PointerId::BOOL])
                    }
                    _ => {
                        n.visit_children_with(self);
                        match n.op {
                            // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-equality-operators
                            BinaryOp::EqEq
                            | BinaryOp::NotEq
                            | BinaryOp::EqEqEq
                            | BinaryOp::NotEqEq => ret!(vec![PointerId::BOOL]),
                            // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-relational-operators
                            BinaryOp::Lt
                            | BinaryOp::LtEq
                            | BinaryOp::Gt
                            | BinaryOp::GtEq
                            | BinaryOp::InstanceOf => {
                                ret!(vec![PointerId::BOOL])
                            }
                            // https://tc39.es/ecma262/multipage/ecmascript-language-expressions.html#sec-applystringornumericbinaryoperator
                            BinaryOp::Add => {
                                ret!(vec![PointerId::NUM, PointerId::BIG_INT, PointerId::STRING])
                            }
                            BinaryOp::LShift
                            | BinaryOp::RShift
                            | BinaryOp::ZeroFillRShift
                            | BinaryOp::Sub
                            | BinaryOp::Mul
                            | BinaryOp::Div
                            | BinaryOp::Mod
                            | BinaryOp::BitOr
                            | BinaryOp::BitXor
                            | BinaryOp::BitAnd
                            | BinaryOp::Exp => ret!(vec![PointerId::NUM, PointerId::BIG_INT]),

                            BinaryOp::LogicalOr
                            | BinaryOp::LogicalAnd
                            | BinaryOp::NullishCoalescing
                            | BinaryOp::In => unreachable!("handled above"),
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
                    let mut obj = self.get_rhs(obj, true, ExprContext::Expression);

                    if n.computed {
                        n.prop.visit_with(self);
                    }

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
                        ret!(obj)
                    } else {
                        self.invalidate(&obj);
                        ret!(vec![PointerId::UNKNOWN])
                    }
                }
            },
            Expr::Cond(n) => {
                n.test.visit_with(self);
                let mut cons = self.get_rhs(&n.cons, used, ExprContext::Expression);
                let mut alt = self.get_rhs(&n.alt, used, ExprContext::Expression);
                cons.append(&mut alt);
                ret!(cons)
            }
            Expr::Call(n) => match &n.callee {
                ExprOrSuper::Super(_) => todo!(),
                ExprOrSuper::Expr(callee) => {
                    let mut callee = self.get_rhs(callee, true, ExprContext::Expression);

                    if callee.len() == 1 {
                        self.record_single_callee_call(callee[0], n.node_id, expr_ctxt);
                    }

                    let args = n
                        .args
                        .iter()
                        .map(|arg| match arg {
                            ExprOrSpread::Spread(arg) => {
                                self.get_rhs(&arg.expr, true, ExprContext::Expression)
                            }
                            ExprOrSpread::Expr(arg) => {
                                self.get_rhs(arg, true, ExprContext::Expression)
                            }
                        })
                        .collect::<Vec<_>>();

                    if n.args.iter().any(|a| matches!(a, ExprOrSpread::Spread(_))) {
                        for arg in args {
                            self.invalidate(&arg);
                        }
                        // TODO: this is too conservative; ideally we'd just record that each
                        // callee is called with unknown parameters and let that info propagate.
                        // However, we don't yet know what we're calling, so we just invalidate
                        // the callees, which will cause their params to be invalidated too.
                        self.invalidate(&callee);
                        return ret!(vec![PointerId::UNKNOWN]);
                    }

                    for &callee in &callee {
                        if callee == PointerId::UNKNOWN {
                            for arg in &args {
                                self.invalidate(arg);
                            }
                            continue;
                        }
                        if !self.store.is_callable_pointer(callee) {
                            continue;
                        }
                        for (i, arg_values) in args.iter().enumerate() {
                            let index = i.try_into().expect("< u16::MAX params");
                            let arg_pointer =
                                self.store.pointers.insert(Pointer::Arg(callee, index));
                            for value in arg_values {
                                self.make_subset_of(*value, arg_pointer);
                                self.graph.add_initial_edge(
                                    callee,
                                    arg_pointer,
                                    GraphEdge::Arg(index),
                                    self.store,
                                );
                            }
                        }
                    }
                    for callee in &mut callee {
                        if *callee == PointerId::UNKNOWN {
                            continue;
                        }
                        if self.store.is_callable_pointer(*callee) {
                            *callee = self.get_return_value(*callee);
                        } else {
                            *callee = PointerId::NULL_OR_VOID;
                        }
                    }
                    ret!(callee)
                }
            },
            Expr::New(n) => {
                let callee = self.get_rhs(&n.callee, true, ExprContext::Expression);
                self.invalidate(&callee);
                if let Some(args) = &n.args {
                    for arg in args {
                        let arg = match arg {
                            ExprOrSpread::Spread(arg) => &arg.expr,
                            ExprOrSpread::Expr(arg) => arg,
                        };
                        let value = self.get_rhs(arg, true, ExprContext::Expression);
                        self.invalidate(&value);
                    }
                }
                ret!(vec![PointerId::UNKNOWN])
            }
            Expr::Seq(n) => {
                debug_assert!(!n.exprs.is_empty());

                let mut i = 0;
                while i < n.exprs.len() - 1 {
                    n.exprs[i].visit_with(self);
                    i += 1;
                }

                self.get_rhs(&n.exprs[i], used, ExprContext::Expression)
            }
            Expr::Ident(n) => {
                if n.ctxt == self.store.unresolved_ctxt {
                    if n.sym == js_word!("undefined") {
                        ret!(vec![PointerId::NULL_OR_VOID])
                    } else {
                        ret!(vec![PointerId::UNKNOWN])
                    }
                } else {
                    let name = Id::new(n, &mut self.store.names);
                    let var = self.store.vars.get_index(&name).unwrap();
                    ret!(vec![self.store.pointers.insert(Pointer::Var(var))])
                }
            }
            Expr::Lit(n) => match n {
                Lit::Str(_) => ret!(vec![PointerId::STRING]),
                Lit::Bool(_) => ret!(vec![PointerId::BOOL]),
                Lit::Null(_) => ret!(vec![PointerId::NULL_OR_VOID]),
                Lit::Num(_) => ret!(vec![PointerId::NUM]),
                Lit::BigInt(_) => ret!(vec![PointerId::BIG_INT]),
                Lit::Regex(_) => ret!(vec![PointerId::REGEX]),
            },
            Expr::Tpl(n) => {
                n.visit_children_with(self);
                ret!(vec![PointerId::UNKNOWN])
            }
            Expr::TaggedTpl(n) => {
                n.tag.visit_with(self);
                for expr in &n.tpl.exprs {
                    let obj = self.get_rhs(expr, true, ExprContext::Expression);
                    // Expressions in tagged templates can be accessed by the tag function.
                    self.invalidate(&obj);
                }
                ret!(vec![PointerId::UNKNOWN])
            }
            Expr::Arrow(n) => {
                let old = self.cur_fn;
                self.cur_fn = Some(n.node_id);
                n.params.visit_with(self);
                n.body.visit_with(self);
                self.cur_fn = old;
                ret!(vec![self
                    .store
                    .pointers
                    .get_index(&Pointer::Fn(n.node_id))
                    .unwrap()])
            }
            Expr::Class(_) => todo!(),
            Expr::Yield(n) => {
                if let Some(arg) = &n.arg {
                    let value = self.get_rhs(arg, true, ExprContext::Expression);
                    self.invalidate(&value);
                }
                ret!(vec![PointerId::UNKNOWN])
            }
            Expr::MetaProp(n) => {
                n.visit_children_with(self);
                ret!(vec![PointerId::UNKNOWN])
            }
            Expr::Await(n) => {
                let value = self.get_rhs(&n.arg, true, ExprContext::Expression);
                self.invalidate(&value);
                ret!(vec![PointerId::UNKNOWN])
            }
            Expr::PrivateName(_) => todo!(),
            Expr::OptChain(opt_chain) => match opt_chain.expr.as_ref() {
                Expr::Member(_) | Expr::Call(_) => {
                    // Note: optional chaining can short circuit and also return undefined,
                    // but that does not impact our analysis, so we ignore it.
                    self.get_rhs(&opt_chain.expr, used, ExprContext::Expression)
                }
                _ => unreachable!("invalid optional chain expr"),
            },

            Expr::Invalid(_) => unreachable!(),
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
        if let PatOrExpr::Pat(pat @ (Pat::Array(_) | Pat::Object(_))) = lhs {
            debug_assert!(!conditional_assign, "invalid assignment target");
            self.handle_destructuring(pat, rhs)
        } else {
            let lhs = self.visit_and_get_slot(lhs);
            let rhs = self.get_rhs(rhs, true, ExprContext::Expression);

            self.assign_to_slot(lhs, &rhs);
            rhs
        }
    }

    fn handle_destructuring(&mut self, lhs: &Pat, rhs: &Expr) -> Vec<PointerId> {
        let rhs = self.get_rhs(rhs, true, ExprContext::Expression);
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
                    ObjectPatProp::Rest(_) => false,
                });
                if has_complex_props {
                    self.invalidate(rhs);
                }

                for prop in &lhs.props {
                    match prop {
                        ObjectPatProp::KeyValue(prop) => {
                            prop.key.visit_with(self);
                            let Some(key) = PropKey::from_prop_name(
                                &prop.key,
                                self.store.unresolved_ctxt,
                                &mut self.store.names,
                            ) else {
                                continue;
                            };
                            for rhs in rhs {
                                self.reference_prop(*rhs, key);
                            }

                            let new_rhs = rhs
                                .iter()
                                .map(|v| self.get_prop_value(*v, key.0))
                                .collect::<Vec<_>>();

                            self.visit_destructuring(&prop.value, &new_rhs);
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
                    if let Pat::Expr(_elem) = element {
                        todo!();
                        // self.invalidate_slot(Node::from(elem.as_ref()));
                    } else {
                        let rhs = &[PointerId::UNKNOWN];
                        self.visit_destructuring(element, rhs);
                    }
                }
            }
            Pat::Rest(lhs) => {
                self.invalidate(rhs);
                let rhs = &[PointerId::UNKNOWN];
                self.visit_destructuring(&lhs.arg, rhs);
            }
            Pat::Assign(lhs) => {
                let mut default_value = self.get_rhs(&lhs.right, true, ExprContext::Expression);
                default_value.extend_from_slice(rhs);

                self.visit_destructuring(&lhs.left, &default_value);
            }
            Pat::Expr(lhs) => {
                let lhs = self.visit_and_get_slot(PatOrExpr::Expr(lhs));
                self.assign_to_slot(lhs, rhs);
            }
            Pat::Invalid(_) => unreachable!(),
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
                    ExprOrSuper::Expr(obj) => self.get_rhs(obj, true, ExprContext::Expression),
                };

                if node.computed {
                    node.prop.visit_with(self);
                }

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
        self.store.references.insert((key, object));
    }

    fn get_prop_value(&mut self, obj: PointerId, prop: NameId) -> PointerId {
        // if obj == PointerId::UNKNOWN {
        //     return PointerId::UNKNOWN;
        // }
        if obj == PointerId::NULL_OR_VOID {
            return PointerId::NULL_OR_VOID;
        }
        if obj.is_primitive() && !is_built_in_property(obj, &self.store.names[prop]) {
            // non-built in prop on primitive - ignore.
            return PointerId::NULL_OR_VOID;
        }

        let access = self.store.pointers.insert(Pointer::Prop(obj, prop));
        self.graph
            .add_initial_edge(obj, access, GraphEdge::Prop(prop), self.store);
        access
    }

    fn get_return_value(&mut self, callee: PointerId) -> PointerId {
        let ret = self.store.pointers.insert(Pointer::ReturnValue(callee));
        self.graph
            .add_initial_edge(callee, ret, GraphEdge::Return, self.store);
        ret
    }

    fn make_subset_of(&mut self, sub: PointerId, sup: PointerId) {
        if sub == sup {
            return;
        }
        self.graph
            .add_initial_edge(sub, sup, GraphEdge::Subset, self.store);
    }

    fn invalidate(&mut self, value: &[PointerId]) {
        for value in value {
            self.graph.invalidate(*value, self.store);
        }
    }

    fn record_single_callee_call(&mut self, callee: PointerId, node: NodeId, context: ExprContext) {
        self.store.calls.insert((node, callee, context));
    }
}

impl Visit<'_> for GraphVisitor<'_> {
    fn visit_stmt(&mut self, n: &Stmt) {
        match n {
            Stmt::With(n) => {
                let obj = self.get_rhs(&n.obj, true, ExprContext::Expression);
                self.invalidate(&obj);
                n.body.visit_with(self);
            }
            Stmt::Return(n) => {
                let Some(cur_fn) = self.cur_fn else {
                    if let Some(value) = &n.arg {
                        let rhs = self.get_rhs(value, true, ExprContext::Expression);
                        self.invalidate(&rhs);
                    }
                    return;
                };
                let cur_fn = self.store.pointers.get_index(&Pointer::Fn(cur_fn)).unwrap();
                let lhs = self.get_return_value(cur_fn);
                if let Some(value) = &n.arg {
                    let rhs = self.get_rhs(value, true, ExprContext::Expression);
                    for rhs in rhs {
                        self.make_subset_of(rhs, lhs);
                    }
                } else {
                    let rhs = PointerId::NULL_OR_VOID;
                    self.make_subset_of(rhs, lhs);
                }
            }
            Stmt::Throw(n) => {
                let value = self.get_rhs(&n.arg, true, ExprContext::Expression);
                self.invalidate(&value);
            }
            Stmt::ForOf(ForOfStmt {
                left, right, body, ..
            }) => {
                left.visit_with(self);
                let rhs = self.get_rhs(right, true, ExprContext::Expression);
                self.invalidate(&rhs);
                body.visit_with(self);
            }
            Stmt::ForIn(ForInStmt {
                left, right, body, ..
            }) => {
                left.visit_with(self);

                let lhs = match left {
                    VarDeclOrPat::VarDecl(lhs) => {
                        assert!(lhs.decls.len() == 1);
                        &lhs.decls[0].name
                    }
                    VarDeclOrPat::Pat(lhs) => lhs,
                };
                self.visit_destructuring(lhs, &[PointerId::STRING]);

                let rhs = self.get_rhs(right, true, ExprContext::Expression);
                self.invalidate(&rhs);
                body.visit_with(self);
            }
            Stmt::Expr(n) => {
                self.get_rhs(&n.expr, false, ExprContext::Statement);
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
                    .get_index(&Pointer::Fn(func.function.node_id))
                    .unwrap();
                let var = self.store.pointers.insert(Pointer::Var(var));
                self.make_subset_of(f, var);
                func.visit_children_with(self);
            }
            Decl::Var(var) => {
                var.visit_children_with(self);
            }
        }
    }

    // TODO: we don't yet handle getters/setters

    fn visit_function(&mut self, n: &Function) {
        let old = self.cur_fn;
        self.cur_fn = Some(n.node_id);
        n.params.visit_with(self);
        n.body.visit_with(self);
        self.cur_fn = old;
    }

    fn visit_arrow_expr(&mut self, n: &ArrowExpr) {
        let old = self.cur_fn;
        self.cur_fn = Some(n.node_id);
        n.params.visit_with(self);
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
        self.get_rhs(n, false, ExprContext::Expression);
    }

    fn visit_module_decl(&mut self, _: &ModuleDecl) {
        todo!();
    }

    fn visit_params(&mut self, params: &[Param]) {
        let cur_fn = self.cur_fn.unwrap();
        let cur_fn = self.store.pointers.insert(Pointer::Fn(cur_fn));
        for (i, param) in params.iter().enumerate() {
            let index = i.try_into().expect("< u16::MAX params");
            let rhs = self.store.pointers.insert(Pointer::Arg(cur_fn, index));
            self.visit_destructuring(&param.pat, &[rhs]);
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Pointer {
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
    Arg(PointerId, u16),
}

impl Pointer {
    fn is_concrete(&self) -> bool {
        match self {
            Pointer::ReturnValue(_)
            | Pointer::Arg(_, _)
            | Pointer::Prop(_, _)
            | Pointer::Var(_) => false,

            Pointer::Object(_)
            | Pointer::Fn(_)
            | Pointer::Unknown
            | Pointer::NullOrVoid
            | Pointer::Bool
            | Pointer::Num
            | Pointer::String
            | Pointer::BigInt
            | Pointer::Regex => true,
        }
    }
}

/// A place where a variable can be stored.
#[derive(Debug, Clone, Copy)]
enum Slot {
    Var(VarId),
    /// (Object, Property name)
    Prop(PointerId, NameId),
}

#[derive(Debug, Clone, Copy)]
struct StaticFunctionData {
    tracked_param_count: u16,
}

impl StaticFunctionData {
    fn is_valid_arg_index(&self, arg_index: u16) -> bool {
        arg_index < self.tracked_param_count
    }
}

index::newtype_index!(pub struct PointerId { .. });

/// Where an expression occurs - either in an expression statement or in another
/// expression.
/// ```js
/// foo(); // <- Call expression is in an expression statement, context is Statement.
/// ```
/// ```js
/// const a = foo(); // <- Call expression is in an expression, context is Expression.
/// ```
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum ExprContext {
    Expression,
    Statement,
}

#[derive(Debug)]
pub struct Store {
    unresolved_ctxt: SyntaxContext,
    functions: FxHashMap<NodeId, StaticFunctionData>,
    names: IndexSet<NameId, JsWord>,
    vars: IndexSet<VarId, Id>,
    pub pointers: IndexSet<PointerId, Pointer>,
    references: FxHashSet<(PropKey, PointerId)>,
    pub invalid_pointers: GrowableBitSet<PointerId>,
    concrete_pointer_bound: PointerId,
    pub calls: FxHashSet<(NodeId, PointerId, ExprContext)>,
}

impl Store {
    // Invalidated the given pointer. Returns true if it was not previously invalid.
    fn invalidate(&mut self, pointer: PointerId) -> bool {
        if pointer.is_built_in() {
            false
        } else {
            self.invalid_pointers.insert(pointer)
        }
    }

    fn is_callable_pointer(&self, pointer: PointerId) -> bool {
        if pointer.is_built_in() {
            false
        } else {
            match self.pointers[pointer] {
                Pointer::Prop(_, _)
                | Pointer::Var(_)
                | Pointer::ReturnValue(_)
                | Pointer::Arg(_, _)
                | Pointer::Fn(_) => true,

                Pointer::Object(_) => false,

                Pointer::Unknown
                | Pointer::NullOrVoid
                | Pointer::Bool
                | Pointer::Num
                | Pointer::String
                | Pointer::BigInt
                | Pointer::Regex => unreachable!("primitives checked above"),
            }
        }
    }

    fn is_concrete(&self, pointer: PointerId) -> bool {
        pointer < self.concrete_pointer_bound
    }

    fn concrete_pointers(&self) -> impl Iterator<Item = PointerId> {
        PointerId::from_u32(0)..self.concrete_pointer_bound
    }

    fn non_concrete_pointers(&self) -> impl Iterator<Item = PointerId> {
        self.concrete_pointer_bound..PointerId::from_usize(self.pointers.len())
    }
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

impl Visit<'_> for DeclFinder<'_> {
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
}

struct FnVisitor<'s> {
    store: &'s mut Store,
    /// Whether the current function accesses the `arguments` array.
    accesses_arguments_array: bool,
    params_to_invalidate: &'s mut Vec<VarId>,
}

impl FnVisitor<'_> {
    fn handle_fn<T>(&mut self, node: &T, fn_expr_name: Option<&Ident>, is_arrow: bool)
    where
        T: FunctionLike + GetNodeId,
    {
        let var_start = self.store.vars.len().try_into().unwrap();

        let mut v = DeclFinder {
            names: &mut self.store.names,
            vars: &mut self.store.vars,
            var_start: VarId::from_u32(var_start),
        };

        let mut has_rest = false;
        for param in node.params() {
            v.visit_pat(param);
            if matches!(param, Pat::Rest(_)) {
                has_rest = true;
            }
        }

        let param_binding_count = v.vars.len() - var_start as usize;

        // FnExpr's name is local to it. Although the name comes before the
        // params, we record it afterwards to simplify tracking of where
        // vars/params start/end.
        if let Some(fn_expr_name) = fn_expr_name {
            v.record_var(fn_expr_name);
        }

        node.body().visit_with(&mut v);

        let tracked_param_count = if has_rest {
            node.param_count() - 1
        } else {
            node.param_count()
        };

        let static_data = StaticFunctionData {
            tracked_param_count: tracked_param_count.try_into().expect("> u16::MAX params"),
        };

        let node_id = node.node_id();

        self.store.pointers.insert(Pointer::Fn(node_id));

        self.store.functions.insert(node_id, static_data);

        let old_accesses_arguments_array = self.accesses_arguments_array;

        // Arrow functions don't have their own `arguments` array, so we want to
        // continue tracking for the parent function.
        if !is_arrow {
            self.accesses_arguments_array = false;
        }

        for param in node.params() {
            self.visit_pat(param);
        }
        node.body().visit_with(self);

        if self.accesses_arguments_array && !is_arrow {
            // Invalidate parameters for functions that access the arguments array.
            self.params_to_invalidate.extend(
                VarId::from_u32(var_start)..VarId::from_u32(var_start + param_binding_count as u32),
            );
        } else if has_rest {
            // Invalidate the rest binding.
            self.params_to_invalidate
                .push(VarId::from_u32(var_start + param_binding_count as u32 - 1));
        }

        if !is_arrow {
            self.accesses_arguments_array = old_accesses_arguments_array;
        }
    }
}

impl<'ast> Visit<'ast> for FnVisitor<'_> {
    fn visit_object_lit(&mut self, n: &'ast ObjectLit) {
        self.store.pointers.insert(Pointer::Object(n.node_id));
        n.visit_children_with(self);
    }

    fn visit_ident(&mut self, node: &'ast Ident) {
        if !self.accesses_arguments_array
            && node.sym == js_word!("arguments")
            && node.ctxt == self.store.unresolved_ctxt
        {
            self.accesses_arguments_array = true;
        }
    }

    fn visit_fn_decl(&mut self, node: &'ast FnDecl) {
        self.handle_fn(&node.function, None, false);
    }
    fn visit_fn_expr(&mut self, node: &'ast FnExpr) {
        self.handle_fn(node.function.as_ref(), node.ident.as_ref(), false);
    }
    fn visit_function(&mut self, node: &'ast Function) {
        self.handle_fn(node, None, false);
    }
    fn visit_constructor(&mut self, node: &'ast Constructor) {
        self.handle_fn(node, None, false);
    }
    fn visit_arrow_expr(&mut self, node: &'ast ArrowExpr) {
        self.handle_fn(node, None, true);
    }
    fn visit_getter_prop(&mut self, node: &'ast GetterProp) {
        self.handle_fn(node, None, false);
    }
    fn visit_setter_prop(&mut self, node: &'ast SetterProp) {
        self.handle_fn(node, None, false);
    }
}

enum PatOrExpr<'a> {
    Pat(&'a Pat),
    Expr(&'a Expr),
}

const STATIC_POINTERS: [(PointerId, Pointer); 7] = [
    (PointerId::BOOL, Pointer::Bool),
    (PointerId::NUM, Pointer::Num),
    (PointerId::STRING, Pointer::String),
    (PointerId::BIG_INT, Pointer::BigInt),
    (PointerId::REGEX, Pointer::Regex),
    (PointerId::NULL_OR_VOID, Pointer::NullOrVoid),
    (PointerId::UNKNOWN, Pointer::Unknown),
];

impl PointerId {
    const BOOL: Self = Self::from_u32(0);
    const NUM: Self = Self::from_u32(1);
    const STRING: Self = Self::from_u32(2);
    const BIG_INT: Self = Self::from_u32(3);
    const REGEX: Self = Self::from_u32(4);
    const NULL_OR_VOID: Self = Self::from_u32(5);

    const UNKNOWN: Self = Self::from_u32(6);

    fn is_primitive(self) -> bool {
        self.as_u32() <= Self::NULL_OR_VOID.as_u32()
    }

    fn is_built_in(self) -> bool {
        self.as_u32() <= Self::UNKNOWN.as_u32()
    }
}

/// Properties that are accessible on all objects.
pub static OBJECT_PROPERTIES: &[JsWord] = &[
    js_word!("prototype"),
    // From Object.prototype
    // https://tc39.es/ecma262/#sec-properties-of-the-object-prototype-object
    js_word!("constructor"),
    js_word!("hasOwnProperty"),
    js_word!("isPrototypeOf"),
    js_word!("propertyIsEnumerable"),
    js_word!("toLocaleString"),
    js_word!("toString"),
    js_word!("valueOf"),
    js_word!("__proto__"),
    js_word!("__defineGetter__"),
    js_word!("__defineSetter__"),
    js_word!("__lookupGetter__"),
    js_word!("__lookupSetter__"),
];

pub static NUM_PROPERTIES: &[JsWord] = &[
    js_word!("toExponential"),
    js_word!("toFixed"),
    js_word!("toPrecision"),
];

pub static STRING_PROPERTIES: &[JsWord] = &[
    js_word!("length"),
    js_word!("at"),
    js_word!("charAt"),
    js_word!("charCodeAt"),
    js_word!("codePointAt"),
    js_word!("concat"),
    js_word!("endsWith"),
    js_word!("includes"),
    js_word!("indexOf"),
    js_word!("isWellFormed"),
    js_word!("lastIndexOf"),
    js_word!("localeCompare"),
    js_word!("match"),
    js_word!("matchAll"),
    js_word!("normalize"),
    js_word!("padEnd"),
    js_word!("padStart"),
    js_word!("repeat"),
    js_word!("replace"),
    js_word!("replaceAll"),
    js_word!("search"),
    js_word!("slice"),
    js_word!("split"),
    js_word!("startsWith"),
    js_word!("substr"),
    js_word!("substring"),
    js_word!("toLocaleLowerCase"),
    js_word!("toLocaleUpperCase"),
    js_word!("toLowerCase"),
    js_word!("toUpperCase"),
    js_word!("toWellFormed"),
    js_word!("trim"),
    js_word!("trimEnd"),
    js_word!("trimStart"),
];

pub static REGEX_PROPERTIES: &[JsWord] = &[
    // https://tc39.es/ecma262/#sec-properties-of-the-regexp-prototype-object
    js_word!("exec"),
    js_word!("dotAll"),
    js_word!("flags"),
    js_word!("global"),
    js_word!("hasIndices"),
    js_word!("ignoreCase"),
    js_word!("multiline"),
    js_word!("source"),
    js_word!("sticky"),
    js_word!("test"),
    js_word!("unicode"),
    js_word!("unicodeSets"),
    // https://tc39.es/ecma262/#sec-properties-of-regexp-instances
    js_word!("lastIndex"),
];

static BUILT_INS: &[(PointerId, &[JsWord])] = &[
    (PointerId::BOOL, &[]),
    (PointerId::NUM, NUM_PROPERTIES),
    (PointerId::STRING, STRING_PROPERTIES),
    (PointerId::BIG_INT, &[]),
    (PointerId::REGEX, REGEX_PROPERTIES),
    (PointerId::NULL_OR_VOID, &[]),
    (PointerId::UNKNOWN, &[]),
];

fn is_built_in_property(obj: PointerId, name: &JsWord) -> bool {
    if obj == PointerId::UNKNOWN || obj == PointerId::NULL_OR_VOID {
        false
    } else {
        OBJECT_PROPERTIES.contains(name)
            || obj.is_built_in() && BUILT_INS[obj.as_usize()].1.contains(name)
    }
}

#[test]
fn test_built_in_pointer_order() {
    for (i, p) in STATIC_POINTERS.iter().enumerate() {
        assert_eq!(i, p.0.as_usize());
        assert_eq!(BUILT_INS[i].0, p.0);
    }
}

struct Renamer<'a> {
    program_data: &'a mut ProgramData,
    rename_map: FxHashMap<NodeId, JsWord>,
}

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
            PropName::BigInt(p) => Some(p.node_id),
            PropName::Computed(p) => Some(p.expr.node_id()),
        };
        if let Some(node_id_to_rename) = node_id_to_rename {
            if let Some(new_name) = self.rename_map.get(&node_id_to_rename) {
                *node = PropName::Ident(Ident {
                    node_id: self.program_data.new_id_from(node.node_id()),
                    sym: new_name.clone(),
                    ctxt: SyntaxContext::empty(),
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
                    node_id: self.program_data.new_id_from(node.prop.node_id()),
                    sym: new_name.clone(),
                    ctxt: SyntaxContext::empty(),
                });
                node.computed = false;
                return;
            }
        }
        node.visit_mut_children_with(self);
    }

    fn visit_mut_str(&mut self, node: &mut Str) {
        if let Some(new_name) = self.rename_map.get(&node.node_id) {
            node.value = new_name.clone();
            node.has_escape = false;
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct PropKey(NameId, NodeId);

impl PropKey {
    fn from_prop_name(
        prop: &PropName,
        unresolved_ctxt: SyntaxContext,
        names: &mut IndexSet<NameId, JsWord>,
    ) -> Option<PropKey> {
        match prop {
            PropName::Ident(p) => Some(PropKey(names.insert(p.sym.clone()), p.node_id)),
            PropName::Str(p) => Some(PropKey(names.insert(p.value.clone()), p.node_id)),
            PropName::Num(p) => Some(PropKey(
                names.insert(ecma_number_to_string(p.value).into()),
                p.node_id,
            )),
            PropName::Computed(p) => PropKey::from_expr(&p.expr, unresolved_ctxt, true, names),
            PropName::BigInt(p) => {
                Some(PropKey(names.insert(p.value.to_string().into()), p.node_id))
            }
        }
    }

    fn from_expr(
        expr: &Expr,
        unresolved_ctxt: SyntaxContext,
        computed: bool,
        names: &mut IndexSet<NameId, JsWord>,
    ) -> Option<PropKey> {
        match expr {
            Expr::Ident(e) => {
                if !computed
                    || e.ctxt == unresolved_ctxt
                        && (e.sym == js_word!("undefined") || e.sym == js_word!("NaN"))
                {
                    Some(PropKey(names.insert(e.sym.clone()), e.node_id))
                } else {
                    None
                }
            }
            Expr::Lit(e) => match e {
                Lit::Str(e) => Some(PropKey(names.insert(e.value.clone()), e.node_id)),
                Lit::Bool(e) => {
                    if e.value {
                        Some(PropKey(names.insert(js_word!("true")), e.node_id))
                    } else {
                        Some(PropKey(names.insert(js_word!("false")), e.node_id))
                    }
                }
                Lit::Null(e) => Some(PropKey(names.insert(js_word!("null")), e.node_id)),
                Lit::Num(e) => Some(PropKey(
                    names.insert(ecma_number_to_string(e.value).into()),
                    e.node_id,
                )),
                Lit::BigInt(e) => Some(PropKey(
                    names.insert(e.value.to_str_radix(10).into()),
                    e.node_id,
                )),
                Lit::Regex(_) => None,
            },
            _ => None,
        }
    }
}

/// Returns true if the string value of the [`PropName`] is statically determinable.
fn is_simple_prop_name(prop_name: &PropName, unresolved_ctxt: SyntaxContext) -> bool {
    match prop_name {
        PropName::Ident(_) | PropName::Str(_) | PropName::Num(_) | PropName::BigInt(_) => true,
        PropName::Computed(p) => match p.expr.as_ref() {
            Expr::Lit(e) => match e {
                Lit::Str(_) | Lit::Bool(_) | Lit::Null(_) | Lit::Num(_) | Lit::BigInt(_) => true,
                Lit::Regex(_) => false,
            },
            Expr::Ident(e) => {
                e.ctxt == unresolved_ctxt
                    && (e.sym == js_word!("undefined") || e.sym == js_word!("NaN"))
            }
            _ => false,
        },
    }
}

index::newtype_index!(pub struct NameId { .. });

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Id(NameId, SyntaxContext);

impl Id {
    fn new(ident: &Ident, names: &mut IndexSet<NameId, JsWord>) -> Self {
        Self(names.insert(ident.sym.clone()), ident.ctxt)
    }
}
