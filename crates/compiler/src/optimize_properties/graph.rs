#![allow(clippy::collapsible_if)]

use std::collections::hash_map::Entry;
use std::collections::BinaryHeap;
use std::fmt::{Display, Write};
use std::rc::Rc;

use arrayvec::ArrayVec;
use index::bit_set::{BitIter, GrowableBitSet};
use petgraph::algo::TarjanScc;
use petgraph::graph::{EdgeIndex, NodeIndex};
use petgraph::visit::EdgeRef;
use petgraph::Directed;
use petgraph::Direction::{Incoming, Outgoing};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::utils::unwrap_as;

use super::growable_unionfind::GrowableUnionFind;
use super::{is_built_in_property, NameId, Pointer, PointerId, Store};

#[derive(Default)]
pub struct Graph {
    nodes: GrowableUnionFind<PointerId>,
    points_to: FxHashMap<PointerId, SmallSet>,
    queue: UniqueQueue,

    graph: petgraph::Graph<PointerId, GraphEdge, Directed>,
    graph_map: Vec<NodeIndex>,

    invalidation_queue: Vec<RepId>,
}

/// Returns true if the given node has no out edges.
fn is_sink_node(graph: &petgraph::Graph<PointerId, GraphEdge, Directed>, node: NodeIndex) -> bool {
    let node = &graph.raw_nodes()[node.index()];
    node.next_edge(Outgoing) == EdgeIndex::end()
}

/// Returns the graph node that represents the given pointer.
fn pointer_to_node(graph_map: &[NodeIndex], pointer: PointerId) -> Option<NodeIndex> {
    match graph_map.get(pointer.index()) {
        Some(slot) => {
            if *slot == NodeIndex::end() {
                None
            } else {
                Some(*slot)
            }
        }
        None => None,
    }
}

impl Graph {
    fn get_node(&mut self, pointer: PointerId) -> NodeIndex {
        let idx = pointer.as_usize();
        match self.graph_map.get_mut(idx) {
            Some(slot) => {
                if *slot == NodeIndex::end() {
                    *slot = self.graph.add_node(pointer);
                }
                *slot
            }
            None => {
                self.graph_map.resize_with(idx + 1, NodeIndex::end);
                self.graph_map[idx] = self.graph.add_node(pointer);
                self.graph_map[idx]
            }
        }
    }

    fn add_edge(&mut self, src: PointerId, dest: PointerId, kind: GraphEdge) -> bool {
        let src = self.get_node(src);
        let dest = self.get_node(dest);

        if kind == GraphEdge::Subset {
            debug_assert_ne!(src, dest);
        }

        let mut existing = self.graph.edges_connecting(src, dest);

        if !existing.any(|e| *e.weight() == kind) {
            self.graph.add_edge(src, dest, kind);
            true
        } else {
            false
        }
    }

    pub(super) fn add_initial_edge(
        &mut self,
        src: PointerId,
        dest: PointerId,
        kind: GraphEdge,
        store: &mut Store,
    ) {
        if matches!(kind, GraphEdge::Subset) && store.is_concrete(src) {
            if store.invalid_pointers.contains(dest) {
                store.invalidate(src);
            }
            self.points_to.entry(dest).or_default().insert(src);
            return;
        }

        self.add_edge(src, dest, kind);

        if matches!(kind, GraphEdge::Subset) {
            debug_assert_ne!(src, dest);

            if store.invalid_pointers.contains(dest) {
                self.invalidate(src, store);
            }
        }
    }

    fn make_subset_of(&mut self, src: RepId, dest: RepId, store: &mut Store) -> bool {
        if src.0 == dest.0 {
            return false;
        }

        if store.is_concrete(src.0) {
            if store.invalid_pointers.contains(dest.0) {
                store.invalidate(src.0);
            }
            return self.points_to.entry(dest.0).or_default().insert(src.0);
        }

        self.prioritise(src);
        self.prioritise(dest);

        let mut changed = self.add_edge(src.0, dest.0, GraphEdge::Subset);

        if store.invalid_pointers.contains(dest.0) {
            self.invalidate(src.0, store);
        }

        changed |= self.add_all(src, dest, store);
        changed
    }

    pub fn invalidate(&mut self, pointer: PointerId, store: &mut Store) -> bool {
        let pointer = RepId(self.nodes.find_mut(pointer));

        debug_assert!(self.invalidation_queue.is_empty());
        self.invalidation_queue.clear();
        self.invalidation_queue.push(pointer);

        let mut changed = false;

        while let Some(pointer) = self.invalidation_queue.pop() {
            if pointer.0.is_built_in() || store.invalid_pointers.contains(pointer.0) {
                continue;
            }

            changed = true;
            store.invalidate(pointer.0);

            if let Some(node) = pointer_to_node(&self.graph_map, pointer.0) {
                for incoming in self.graph.edges_directed(node, Incoming) {
                    if matches!(incoming.weight(), GraphEdge::Subset) {
                        let source = incoming.source();
                        if !self.graph[source].is_built_in()
                            && !store.invalid_pointers.contains(self.graph[source])
                        {
                            let p = RepId(self.nodes.find_mut(self.graph[source]));
                            self.invalidation_queue.push(p);
                        }
                    }
                }
            }

            if let Some(values) = self.points_to.get(&pointer.0).cloned() {
                let valid_values = values
                    .iter()
                    .filter(|v| !v.is_built_in() && !store.invalid_pointers.contains(*v))
                    .map(RepId);
                self.invalidation_queue.extend(valid_values);
            }
        }

        changed
    }

    pub fn compute_points_to_map(&mut self, store: &mut Store) {
        store.invalid_pointers.insert(PointerId::UNKNOWN);

        for pointer in store.concrete_pointers() {
            self.points_to.insert(pointer, SmallSet::single(pointer));
        }

        let mut tarjan = TarjanScc::default();

        self.queue.priorities.reserve(self.graph.node_count());

        let priorities = &mut self.queue.priorities;
        let graph = &self.graph;

        let mut priority = 0_u32;
        tarjan.run(&self.graph, |scc| {
            for n in scc {
                priorities.insert(graph[*n], priority);
            }
            priority += 1;
        });

        debug_assert_eq!(self.queue.priorities.len(), self.graph.node_count());

        let internal_nodes = self
            .graph
            .node_indices()
            .filter(|n| !is_sink_node(graph, *n))
            .map(|n| graph[n]);

        self.queue.extend(internal_nodes);
        let mut edges = Vec::new();
        let mut prop_merge_queue = Vec::new();

        let mut first = true;

        loop {
            self.flow_edges(store, &mut edges, &mut prop_merge_queue);
            // After we've reached a fixedpoint above, if we couldn't infer the values
            // of a pointer, set them to Unknown and run fixedpoint again to propagate
            // the Unknowns.
            loop {
                let mut invalidated = false;

                for pointer in store.non_concrete_pointers() {
                    let node = self.get_graph_node_id(pointer);

                    if let Pointer::Arg(callee, _) = store.pointers[pointer] {
                        let unknown_callee = self
                            .get(callee)
                            .map(|concrete_callees| concrete_callees.contains(&PointerId::UNKNOWN))
                            .unwrap_or(false);

                        if unknown_callee {
                            if let Some(concrete_values) = self.get(node).cloned() {
                                // Invalidate arguments passed to unknown callers.
                                for value in &concrete_values {
                                    invalidated |= self.invalidate(value, store);
                                }
                            }
                        }

                        // If the callee is invalid, then it could be called with unknown arguments.
                        if store.invalid_pointers.contains(callee) {
                            let changed = self.insert(node, PointerId::UNKNOWN, store);
                            if changed {
                                if let Some(n) = pointer_to_node(&self.graph_map, node.0) {
                                    if !is_sink_node(&self.graph, n) {
                                        // TODO: unnecessary? Is the node guaranteed to be prioritised?
                                        self.prioritise(node);
                                        self.queue.push(node.0);
                                    }
                                }
                            }
                        }
                    }

                    if let Pointer::ReturnValue(callee) = store.pointers[pointer] {
                        // If the callee is invalid, then we can't know how the return type is used.
                        if store.invalid_pointers.contains(callee) {
                            invalidated |= self.invalidate(pointer, store);
                        }
                    }

                    if let Pointer::Prop(obj, name) = store.pointers[pointer] {
                        let obj_invalid = store.invalid_pointers.contains(obj);
                        let is_built_in = is_built_in_property(obj, &store.names[name]);
                        if obj_invalid || is_built_in {
                            invalidated |= self.invalidate(pointer, store);
                            let changed = self.insert(node, PointerId::UNKNOWN, store);
                            if changed {
                                if let Some(n) = pointer_to_node(&self.graph_map, node.0) {
                                    if !is_sink_node(&self.graph, n) {
                                        // TODO: unnecessary? Is the node guaranteed to be prioritised?
                                        self.prioritise(node);
                                        self.queue.push(node.0);
                                    }
                                }
                            }
                            continue;
                        }
                    }

                    if first {
                        if self.points_to_nothing(node) {
                            // Functions implicitly return undefined sometimes.
                            if matches!(store.pointers[pointer], Pointer::ReturnValue(_)) {
                                let changed = self.insert(node, PointerId::NULL_OR_VOID, store);
                                if changed {
                                    if let Some(n) = pointer_to_node(&self.graph_map, node.0) {
                                        if !is_sink_node(&self.graph, n) {
                                            // TODO: unnecessary? Is the node guaranteed to be prioritised?
                                            self.prioritise(node);
                                            self.queue.push(node.0);
                                        }
                                    }
                                }
                                continue;
                            }

                            // Undefined properties on valid objects are undefined. We know the
                            // obj is valid as props on invalid objects are checked above.
                            if matches!(store.pointers[pointer], Pointer::Prop(_, _)) {
                                let changed = self.insert(node, PointerId::NULL_OR_VOID, store);
                                if changed {
                                    if let Some(n) = pointer_to_node(&self.graph_map, node.0) {
                                        if !is_sink_node(&self.graph, n) {
                                            self.queue.push(node.0);
                                        }
                                    }
                                }
                                continue;
                            }
                            invalidated |= self.invalidate(pointer, store);
                            self.insert(node, PointerId::UNKNOWN, store);
                            if let Some(n) = pointer_to_node(&self.graph_map, node.0) {
                                if !is_sink_node(&self.graph, n) {
                                    // TODO: unnecessary? Is the node guaranteed to be prioritised?
                                    self.prioritise(node);
                                    self.queue.push(node.0);
                                }
                            }
                        }
                    }
                }

                first = false;

                if !invalidated {
                    break;
                }
            }

            if self.queue.inner.is_empty() {
                break;
            }
        }

        if cfg!(debug_assertions) {
            for set in self.points_to.values() {
                if let SmallSet::Heap(set) = set {
                    debug_assert!(!set.is_empty(), "Heap variant should not be empty");
                }
            }

            let edges = self
                .graph
                .raw_edges()
                .iter()
                .map(|e| (e.source(), e.weight, e.target()))
                .collect::<FxHashSet<_>>();

            debug_assert_eq!(
                edges.len(),
                self.graph.edge_count(),
                "Graph should not contain duplicate edges"
            );

            // Check that concrete pointers don't have any outgoing subset edges.
            for p in store.concrete_pointers() {
                if let Some(node) = pointer_to_node(&self.graph_map, p) {
                    debug_assert!(self.graph.edges_directed(node, Incoming).count() == 0);
                    let subset_edges = self
                        .graph
                        .edges_directed(node, Outgoing)
                        .filter(|e| *e.weight() == GraphEdge::Subset)
                        .count();
                    debug_assert_eq!(subset_edges, 0);
                }
            }
        }
    }

    fn flow_edges(
        &mut self,
        store: &mut Store,
        edges: &mut Vec<(NodeIndex, GraphEdge)>,
        prop_merge_queue: &mut Vec<RepId>,
    ) {
        while let Some(node) = self.queue.pop() {
            // Skip nodes that aren't their representative. The representative should
            // always be in the queue if the node was, since we add the rep to the queue
            // when we merge it with node.
            let rep = self.get_graph_node_id(node);
            if node != rep.0 {
                continue;
            }
            let node = rep.0;

            edges.clear();
            let n = self.get_node(node);
            edges.extend(
                self.graph
                    .edges_directed(n, Outgoing)
                    .map(|e| (e.target(), *e.weight())),
            );

            for (dest, kind) in edges.iter().copied() {
                debug_assert_ne!(dest, n, "Graph should not have loops");
                let dest = self.get_graph_node_id(self.graph[dest]);
                let dest_node = self.graph_map[dest.0.index()];

                match kind {
                    GraphEdge::Subset => {
                        let changed = self.add_all(node, dest, store);
                        if changed {
                            if !is_sink_node(&self.graph, dest_node) {
                                self.queue.push(dest.0);
                            }
                        }
                    }
                    GraphEdge::Return => {
                        let callees = match self.get(node) {
                            Some(c) => c.clone(),
                            None => continue,
                        };
                        let mut changed = false;
                        for callee in &callees {
                            if callee == PointerId::UNKNOWN {
                                if self.make_subset_of(RepId(PointerId::UNKNOWN), dest, store) {
                                    changed = true;
                                }
                                continue;
                            }
                            if !store.is_callable_pointer(callee) {
                                continue;
                            }
                            let return_node = store.pointers.insert(Pointer::ReturnValue(callee));
                            let return_node = self.get_graph_node_id(return_node);
                            if self.make_subset_of(return_node, dest, store) {
                                changed = true;
                            }
                        }
                        if changed {
                            if !is_sink_node(&self.graph, dest_node) {
                                self.queue.push(dest.0);
                            }
                        }
                    }
                    GraphEdge::Prop(name) => {
                        let concrete_objects = match self.get(node) {
                            Some(o) => o.clone(),
                            None => continue,
                        };

                        prop_merge_queue.clear();

                        let mut changed = false;
                        let mut rep = dest;
                        let mut invalid = store.invalid_pointers.contains(dest.0);
                        for concrete_object in &concrete_objects {
                            if concrete_object == PointerId::UNKNOWN {
                                changed |= self.insert(rep, PointerId::UNKNOWN, store);
                                continue;
                            }
                            if concrete_object == PointerId::NULL_OR_VOID {
                                changed |= self.insert(rep, PointerId::NULL_OR_VOID, store);
                                continue;
                            }
                            if concrete_object.is_primitive()
                                && !is_built_in_property(concrete_object, &store.names[name])
                            {
                                // non-built in prop on primitive - ignore.
                                continue;
                            }
                            let prop_pointer =
                                store.pointers.insert(Pointer::Prop(concrete_object, name));
                            let prop_pointer = self.get_graph_node_id(prop_pointer);

                            if rep.0 == prop_pointer.0 {
                                continue;
                            }

                            invalid |= store.invalid_pointers.contains(prop_pointer.0);

                            self.nodes.union(prop_pointer.0, rep.0);

                            let new_rep = self.get_graph_node_id(rep.0);

                            let src;
                            if new_rep.0 != rep.0 {
                                // prop_pointer became representative
                                src = rep;
                                rep = new_rep;
                            } else {
                                // rep is still the representative
                                src = prop_pointer;
                            }
                            prop_merge_queue.push(src);
                        }

                        if prop_merge_queue.is_empty() {
                            debug_assert_eq!(rep.0, dest.0);
                            if changed {
                                if !is_sink_node(&self.graph, dest_node) {
                                    self.queue.push(dest.0);
                                }
                            }

                            continue;
                        }

                        let rep_node = self.get_node(rep.0);

                        for &src in prop_merge_queue.iter() {
                            debug_assert_ne!(src.0, rep.0);

                            if invalid {
                                store.invalid_pointers.insert(src.0);
                            }

                            if let Some(src_values) = self.points_to.remove(&src.0) {
                                match self.points_to.entry(rep.0) {
                                    Entry::Occupied(mut entry) => {
                                        changed |= entry.get_mut().extend(src_values);
                                    }
                                    Entry::Vacant(entry) => {
                                        changed = true;
                                        entry.insert(src_values);
                                    }
                                }
                            }

                            let Some(src_node) = pointer_to_node(&self.graph_map, src.0) else {
                                continue;
                            };

                            // Move all of src_node's in edges so they point to rep_node instead.
                            while let Some(edge) =
                                self.graph.edges_directed(src_node, Incoming).next()
                            {
                                let id = edge.id();
                                let source = edge.source();
                                let weight = *edge.weight();

                                if edge.source() != rep_node {
                                    let mut existing =
                                        self.graph.edges_connecting(source, rep_node);
                                    if !existing.any(|e| *e.weight() == weight) {
                                        self.graph.add_edge(source, rep_node, weight);
                                    }
                                }

                                changed = true;

                                self.graph.remove_edge(id);
                            }
                            // Move all of src_node's out edges so they come from rep_node instead.
                            while let Some(edge) =
                                self.graph.edges_directed(src_node, Outgoing).next()
                            {
                                let id = edge.id();
                                let target = edge.target();
                                let weight = *edge.weight();

                                if rep_node != target {
                                    let mut existing =
                                        self.graph.edges_connecting(rep_node, target);
                                    if !existing.any(|e| *e.weight() == weight) {
                                        self.graph.add_edge(rep_node, target, weight);
                                    }
                                }

                                changed = true;

                                self.graph.remove_edge(id);
                            }
                        }

                        if invalid {
                            self.invalidate(rep.0, store);
                        }

                        if changed {
                            if !is_sink_node(&self.graph, rep_node) {
                                self.prioritise(rep);
                                self.queue.push(rep.0);
                            }
                        }
                    }
                    GraphEdge::Arg(index) => {
                        let concrete_callees = match self.get(node) {
                            Some(c) => c.clone(),
                            None => continue,
                        };

                        for concrete_callee in &concrete_callees {
                            match store.pointers[concrete_callee] {
                                Pointer::Fn(callee) => {
                                    let func = store.functions.get(&callee).unwrap();
                                    if func.is_valid_arg_index(index) {
                                        let concrete_arg = store
                                            .pointers
                                            .insert(Pointer::Arg(concrete_callee, index));
                                        let concrete_arg = self.get_graph_node_id(concrete_arg);
                                        if self.make_subset_of(dest, concrete_arg, store) {
                                            if !is_sink_node(
                                                &self.graph,
                                                self.graph_map[concrete_arg.0.index()],
                                            ) {
                                                self.queue.push(concrete_arg.0);
                                            }
                                        }
                                    } else {
                                        self.invalidate(dest.0, store);
                                    }
                                }

                                Pointer::Unknown
                                | Pointer::Object(_)
                                | Pointer::NullOrVoid
                                | Pointer::Bool
                                | Pointer::Num
                                | Pointer::String
                                | Pointer::BigInt
                                | Pointer::Regex => {}

                                Pointer::Prop(_, _)
                                | Pointer::Var(_)
                                | Pointer::ReturnValue(_)
                                | Pointer::Arg(_, _) => unreachable!("non-concrete"),
                            }
                        }
                    }
                }
            }
        }
    }

    fn prioritise(&mut self, pointer: RepId) {
        if !self.queue.priorities.contains_key(&pointer.0) {
            let node = self.get_node(pointer.0);
            let priority = self
                .graph
                .neighbors_directed(node, Incoming)
                .map(|n| self.queue.priorities[&self.graph[n]])
                .min()
                .unwrap_or(u32::MAX);
            self.queue.priorities.insert(pointer.0, priority);
        }
    }

    fn get_graph_node_id(&mut self, pointer: PointerId) -> RepId {
        RepId(self.nodes.find_mut(pointer))
    }

    fn insert(&mut self, pointer: RepId, value: PointerId, store: &Store) -> bool {
        debug_assert!(store.is_concrete(value));

        debug_assert!(!store.is_concrete(pointer.0));

        self.points_to.entry(pointer.0).or_default().insert(value)
    }

    fn points_to_nothing(&mut self, pointer: RepId) -> bool {
        !self.points_to.contains_key(&pointer.0)
    }

    fn add_all<T: GetRepId, U: GetRepId>(&mut self, src: T, dest: U, store: &mut Store) -> bool {
        let src = src.get_rep_id(self).0;
        let dest = dest.get_rep_id(self).0;

        debug_assert!(!store.is_concrete(dest));

        if src == dest {
            return false;
        }

        let src_set = self.points_to.get(&src).cloned();

        let Some(src_set) = src_set else {
            return false;
        };

        if store.invalid_pointers.contains(dest) & !store.invalid_pointers.contains(src) {
            for p in src_set.iter() {
                self.invalidate(p, store);
            }
        }

        let dest_set = self.points_to.entry(dest).or_default();

        if dest_set.is_empty() {
            *dest_set = src_set.clone();
            !src_set.is_empty()
        } else {
            dest_set.extend_ref(&src_set)
        }
    }

    fn get<T: GetRepId>(&mut self, pointer: T) -> Option<&SmallSet> {
        let representative = { pointer.get_rep_id(self).0 };
        self.points_to.get(&representative)
    }

    pub(super) fn get_immutable(&self, pointer: PointerId) -> Option<&SmallSet> {
        let representative = self.nodes.find(pointer);
        self.points_to.get(&representative)
    }

    pub(super) fn get_dot(&mut self, store: &Store) -> String {
        let mut print_graph = self.graph.clone();

        let mut map = |store: &Store, n: PointerId| {
            let n = self.nodes.find(n);
            let pointers = (0..store.pointers.len())
                .filter_map(|p| {
                    let p = PointerId::from_usize(p);
                    let id = self.get_graph_node_id(p).0;
                    if id == n {
                        Some(p)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            let mut res = format!("{:?}: ", n);

            for &p in &pointers {
                let value: String = match &store.pointers[p] {
                    Pointer::Prop(obj, prop) => {
                        format!(
                            "Prop({}, ({}, '{}'))",
                            obj.as_u32(),
                            prop.as_u32(),
                            store.names[*prop]
                        )
                    }
                    Pointer::Var(id) => {
                        format!("Var({}, '{}')", id.as_u32(), store.names[store.vars[*id].0])
                    }
                    Pointer::Object(id) => format!("Object({})", id.as_u32()),
                    Pointer::Fn(id) => format!("Fn({})", id.as_u32()),
                    Pointer::Unknown => "Unknown".into(),
                    Pointer::NullOrVoid => "NullOrVoid".into(),
                    Pointer::Bool => "Bool".into(),
                    Pointer::Num => "Num".into(),
                    Pointer::String => "String".into(),
                    Pointer::BigInt => "BigInt".into(),
                    Pointer::Regex => "Regex".into(),
                    Pointer::ReturnValue(id) => {
                        format!("ReturnValue({})", id.as_u32())
                    }
                    Pointer::Arg(func, index) => {
                        format!("Arg({}, {})", func.as_u32(), index)
                    }
                };
                if pointers.len() > 1 {
                    write!(&mut res, "{}|{} ", p.as_u32(), value).unwrap();
                } else {
                    write!(&mut res, "{} ", value).unwrap();
                }
            }

            res
        };

        print_graph.retain_nodes(|g, n| {
            g.edges_directed(n, Outgoing).next().is_some()
                || g.edges_directed(n, Incoming).next().is_some()
        });
        let print_graph: petgraph::prelude::DiGraph<String, GraphEdge> =
            print_graph.map(|_, n| map(store, *n), |_, e| *e);
        format!("{}", petgraph::dot::Dot::with_config(&print_graph, &[]))
    }
}

#[derive(Clone, Copy)]
struct RepId(PointerId);

trait GetRepId {
    fn get_rep_id(self, points_to: &mut Graph) -> RepId;
}

impl GetRepId for PointerId {
    fn get_rep_id(self, points_to: &mut Graph) -> RepId {
        RepId(points_to.nodes.find_mut(self))
    }
}

impl GetRepId for RepId {
    fn get_rep_id(self, _: &mut Graph) -> RepId {
        self
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub(super) enum GraphEdge {
    Subset,
    Return,
    Prop(NameId),
    Arg(u16),
}

impl Display for GraphEdge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

#[derive(Debug)]
struct PrioritizedNode(u32, PointerId);

impl PartialEq for PrioritizedNode {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for PrioritizedNode {}

impl std::cmp::Ord for PrioritizedNode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl std::cmp::PartialOrd for PrioritizedNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Default)]
struct UniqueQueue {
    inner: BinaryHeap<PrioritizedNode>,
    priorities: FxHashMap<PointerId, u32>,
}

impl UniqueQueue {
    fn pop(&mut self) -> Option<PointerId> {
        self.inner.pop().map(|p| p.1)
    }

    fn push(&mut self, node: PointerId) {
        self.inner
            .push(PrioritizedNode(self.priorities[&node], node));
    }

    fn extend(&mut self, iter: impl Iterator<Item = PointerId>) {
        let priorities = &self.priorities;
        self.inner
            .extend(iter.map(|n| PrioritizedNode(priorities[&n], n)));
    }
}

#[derive(Debug, Clone)]
pub(super) enum SmallSet {
    Inline(ArrayVec<PointerId, 2>),
    Heap(Rc<GrowableBitSet<PointerId>>),
}

impl Default for SmallSet {
    fn default() -> Self {
        Self::Inline(ArrayVec::default())
    }
}

impl SmallSet {
    pub(super) fn single(pointer: PointerId) -> Self {
        let mut s = ArrayVec::new();
        s.push(pointer);
        Self::Inline(s)
    }

    fn insert(&mut self, value: PointerId) -> bool {
        match self {
            SmallSet::Inline(set) => {
                if !set.contains(&value) {
                    if set.len() == set.capacity() {
                        let mut heap = GrowableBitSet::default();
                        for v in set {
                            heap.insert(*v);
                        }
                        heap.insert(value);
                        *self = Self::Heap(Rc::new(heap));
                    } else {
                        set.push(value);
                    }
                    true
                } else {
                    false
                }
            }
            SmallSet::Heap(set) => Rc::make_mut(set).insert(value),
        }
    }

    fn contains(&self, value: &PointerId) -> bool {
        match self {
            SmallSet::Inline(set) => set.contains(value),
            SmallSet::Heap(set) => set.contains(*value),
        }
    }

    pub(super) fn iter(&self) -> SmallSetIter<'_> {
        match self {
            SmallSet::Inline(set) => SmallSetIter::Slice(set.iter()),
            SmallSet::Heap(set) => SmallSetIter::Set(set.iter()),
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            SmallSet::Inline(set) => set.is_empty(),
            SmallSet::Heap(set) => {
                debug_assert!(!set.is_empty());
                false
            }
        }
    }

    fn extend(&mut self, mut other: SmallSet) -> bool {
        match (&self, &other) {
            (SmallSet::Inline(_), SmallSet::Heap(_)) => {
                other = std::mem::replace(self, other);
                let mut changed = false;
                for value in &other {
                    changed |= self.insert(value);
                }
                changed
            }
            (SmallSet::Heap(a), SmallSet::Heap(b)) => {
                if Rc::ptr_eq(a, b) {
                    return false;
                }

                let (this, mut other) = unwrap_as!(
                    (self, other),
                    (SmallSet::Heap(a), SmallSet::Heap(b)),
                    (a, b)
                );

                if other.capacity() > this.capacity() {
                    other = std::mem::replace(this, other);
                }
                Rc::make_mut(this).union(&other)
            }
            _ => {
                let mut changed = false;
                for value in &other {
                    changed |= self.insert(value);
                }
                changed
            }
        }
    }

    fn extend_ref(&mut self, other: &SmallSet) -> bool {
        match self {
            SmallSet::Inline(_) => {
                let mut changed = false;
                for value in other {
                    changed |= self.insert(value);
                }
                changed
            }
            SmallSet::Heap(set) => match other {
                SmallSet::Inline(other) => {
                    let mut changed = false;
                    for value in other {
                        changed |= self.insert(*value);
                    }
                    changed
                }
                SmallSet::Heap(other) => {
                    if Rc::ptr_eq(set, other) {
                        return false;
                    }
                    Rc::make_mut(set).union(other)
                }
            },
        }
    }
}

impl<'a> IntoIterator for &'a SmallSet {
    type IntoIter = SmallSetIter<'a>;
    type Item = PointerId;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub(super) enum SmallSetIter<'a> {
    Slice(std::slice::Iter<'a, PointerId>),
    Set(BitIter<'a, PointerId>),
}

impl Iterator for SmallSetIter<'_> {
    type Item = PointerId;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SmallSetIter::Slice(iter) => iter.next().copied(),
            SmallSetIter::Set(iter) => iter.next(),
        }
    }
}
