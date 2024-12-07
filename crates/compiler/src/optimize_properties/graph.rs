use arrayvec::ArrayVec;
use index::bit_set::{BitIter, GrowableBitSet};
use petgraph::algo::TarjanScc;
use petgraph::graph::{EdgeIndex, NodeIndex};
use petgraph::visit::EdgeRef;
use petgraph::Directed;
use petgraph::Direction::{Incoming, Outgoing};
use priority_queue::PriorityQueue;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;
use std::fmt::{Display, Write};
use std::rc::Rc;

use crate::utils::unwrap_as;

use super::growable_unionfind::GrowableUnionFind;
use super::{is_built_in_property, NameId, Pointer, PointerId, Store};

type GraphType = petgraph::Graph<PointerId, GraphEdge, Directed>;

#[derive(Default)]
pub struct Graph {
    pub nodes: GrowableUnionFind<PointerId>,
    pub points_to: FxHashMap<PointerId, SmallSet>,
    pub queue: UniqueQueue,
    edges: FxHashSet<Edge>,
    subset_edges: FxHashSet<(NodeIndex, NodeIndex)>,

    pub graph: GraphType,
    graph_map: Vec<NodeIndex>,
}

/// Returns true if the given node has no out edges.
fn is_sink_node(graph: &GraphType, node: NodeIndex) -> bool {
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

fn get_node(
    graph: &mut GraphType,
    graph_map: &mut Vec<NodeIndex>,

    pointer: PointerId,
) -> NodeIndex {
    let idx = pointer.as_usize();
    match graph_map.get_mut(idx) {
        Some(slot) => {
            if *slot == NodeIndex::end() {
                *slot = graph.add_node(pointer);
            }
            *slot
        }
        None => {
            graph_map.resize_with(idx + 1, || NodeIndex::end());
            graph_map[idx] = graph.add_node(pointer);
            graph_map[idx]
        }
    }
}

// force_visit_neighbours should be true when invalidating after merging nodes.
fn invalidate(
    points_to: &FxHashMap<PointerId, SmallSet>,
    nodes: &mut GrowableUnionFind<PointerId>,

    graph: &GraphType,
    graph_map: &[NodeIndex],

    pointer: PointerId,
    store: &mut Store,

    force_visit_neighbours: bool,
) -> bool {
    let initial = RepId(nodes.find_mut(pointer));
    let Some(node) = pointer_to_node(graph_map, initial.0) else {
        return store.invalidate(initial.0);
    };

    if initial.0.is_built_in() {
        return false;
    }
    if !force_visit_neighbours && store.invalid_pointers.contains(initial.0) {
        return false;
    }

    let mut queue = vec![node];

    let mut changed = false;

    while let Some(node) = queue.pop() {
        let pointer = graph[node];
        if pointer.is_built_in() {
            continue;
        }
        if !force_visit_neighbours
            && pointer == initial.0
            && store.invalid_pointers.contains(pointer)
        {
            continue;
        }

        changed = true;
        store.invalidate(pointer);

        for incoming in graph.edges_directed(node, Incoming) {
            if matches!(incoming.weight(), GraphEdge::Subset(_)) {
                let source = incoming.source();
                if !graph[source].is_built_in() && !store.invalid_pointers.contains(graph[source]) {
                    queue.push(source);
                }
            }
        }

        if let Some(values) = points_to.get(&pointer).cloned() {
            let valid_values = values
                .iter()
                .filter(|v| !v.is_built_in() && !store.invalid_pointers.contains(*v))
                .map(|v| graph_map[v.index()]);
            queue.extend(valid_values);
        }
    }

    changed
}

// force_visit_neighbours should be true when invalidating after merging nodes.
fn record_computed_access(
    points_to: &mut FxHashMap<PointerId, SmallSet>,
    nodes: &mut GrowableUnionFind<PointerId>,

    graph: &mut GraphType,
    graph_map: &mut Vec<NodeIndex>,

    edges: &mut FxHashSet<Edge>,
    subset_edges: &mut FxHashSet<(NodeIndex, NodeIndex)>,

    pointer: PointerId,
    store: &mut Store,

    force_visit_neighbours: bool,
) -> bool {
    let initial = RepId(nodes.find_mut(pointer));
    let Some(node) = pointer_to_node(graph_map, initial.0) else {
        return store.record_computed_access(initial.0);
    };

    if initial.0.is_built_in() {
        return false;
    }
    if !force_visit_neighbours && store.accessed_dynamically.contains(initial.0) {
        return false;
    }

    let mut queue = Vec::new();

    queue.push(node);

    let mut changed = false;

    while let Some(node) = queue.pop() {
        let pointer = graph[node];
        if pointer.is_built_in() {
            continue;
        }
        if !force_visit_neighbours
            && pointer == initial.0
            && store.accessed_dynamically.contains(pointer)
        {
            continue;
        }

        changed = true;
        store.record_computed_access(pointer);

        for incoming in graph.edges_directed(node, Incoming) {
            if matches!(incoming.weight(), GraphEdge::Subset(_)) {
                let source = incoming.source();
                if !graph[source].is_built_in()
                    && !store.accessed_dynamically.contains(graph[source])
                {
                    queue.push(source);
                }
            }
        }

        if let Some(values) = points_to.get(&pointer).cloned() {
            let valid_values = values
                .iter()
                .filter(|v| !v.is_built_in() && !store.accessed_dynamically.contains(*v))
                .map(|v| graph_map[v.index()]);
            queue.extend(valid_values);
        }

        if let Some((rep, rep_node)) = merge_prop_accesses(
            points_to,
            nodes,
            graph,
            graph_map,
            edges,
            subset_edges,
            pointer,
            node,
            store,
            &mut Vec::new(),
        ) {
            // todo
        }
    }

    changed
}

fn merge_prop_accesses(
    points_to: &mut FxHashMap<PointerId, SmallSet>,
    nodes: &mut GrowableUnionFind<PointerId>,

    graph: &mut GraphType,
    graph_map: &mut Vec<NodeIndex>,

    edges: &mut FxHashSet<Edge>,
    subset_edges: &mut FxHashSet<(NodeIndex, NodeIndex)>,

    pointer: PointerId,
    node: NodeIndex,
    store: &mut Store,
    props: &mut Vec<NodeIndex>,
) -> Option<(RepId, NodeIndex)> {
    props.clear();
    props.extend(
        graph
            .edges_directed(node, Outgoing)
            .filter(|e| matches!(e.weight(), GraphEdge::Prop(_)))
            .map(|e| e.target()),
    );

    if !props.is_empty() {
        return None;
    }

    let mut to_merge = Vec::new();

    let dest = store.pointers.insert(Pointer::Prop(pointer, NameId::MAX));
    let mut rep = RepId(nodes.find_mut(dest));
    let mut invalid = store.invalid_pointers.contains(rep.0);
    let mut accessed_dynamically = store.accessed_dynamically.contains(rep.0);
    for prop_node in props {
        let prop_pointer = graph[*prop_node];
        let prop_pointer = RepId(nodes.find_mut(prop_pointer));

        if prop_pointer.0 == rep.0 {
            continue;
        }

        invalid |= store.invalid_pointers.contains(prop_pointer.0);
        accessed_dynamically |= store.accessed_dynamically.contains(prop_pointer.0);

        nodes.union(prop_pointer.0, rep.0);

        let new_rep = RepId(nodes.find_mut(rep.0));

        let src;
        if new_rep.0 != rep.0 {
            // prop_pointer became representative
            src = rep;
            rep = new_rep;
        } else {
            // rep is still the representative
            src = prop_pointer;
        }
        to_merge.push(src);
    }

    if to_merge.is_empty() {
        return None;
    }

    let (changed, rep, rep_node) = merge_nodes(
        points_to,
        nodes,
        graph,
        graph_map,
        edges,
        subset_edges,
        rep,
        invalid,
        accessed_dynamically,
        &to_merge,
        store,
    );

    if changed {
        Some((rep, rep_node))
    } else {
        None
    }
}

fn merge_nodes(
    points_to: &mut FxHashMap<PointerId, SmallSet>,
    nodes: &mut GrowableUnionFind<PointerId>,

    graph: &mut GraphType,
    graph_map: &mut Vec<NodeIndex>,

    edges: &mut FxHashSet<Edge>,
    subset_edges: &mut FxHashSet<(NodeIndex, NodeIndex)>,

    rep: RepId,
    invalid: bool,
    accessed_dynamically: bool,
    queue: &[RepId],
    store: &mut Store,
) -> (bool, RepId, NodeIndex) {
    let rep_node = get_node(graph, graph_map, rep.0);

    let mut changed = false;
    for &src in queue {
        debug_assert_ne!(src.0, rep.0);

        if invalid {
            store.invalid_pointers.insert(src.0);
        }

        if accessed_dynamically {
            store.accessed_dynamically.insert(src.0);
        }

        if let Some(src_values) = points_to.remove(&src.0) {
            match points_to.entry(rep.0) {
                Entry::Occupied(mut entry) => {
                    changed |= entry.get_mut().extend(src_values);
                }
                Entry::Vacant(entry) => {
                    changed = true;
                    entry.insert(src_values);
                }
            }
        }

        let Some(src_node) = pointer_to_node(graph_map, src.0) else {
            continue;
        };

        // Move all of src_node's in edges so they point to rep_node instead.
        while let Some(edge) = graph.edges_directed(src_node, Incoming).next() {
            let id = edge.id();
            let source = edge.source();
            let target = edge.target();
            let weight = *edge.weight();

            if matches!(weight, GraphEdge::Subset(_)) {
                subset_edges.remove(&(source, target));

                if nodes.find_mut(graph[source]) != rep.0 {
                    if subset_edges.insert((source, rep_node)) {
                        changed = true;
                        graph.add_edge(source, rep_node, weight);
                    }
                }
            } else {
                edges.remove(&Edge::new(source, target, weight));

                if nodes.find_mut(graph[source]) != rep.0 {
                    if edges.insert(Edge::new(source, rep_node, weight)) {
                        changed = true;
                        graph.add_edge(source, rep_node, weight);
                    }
                }
            }

            graph.remove_edge(id);
        }
        // Move all of src_node's out edges so they come from rep_node instead.
        while let Some(edge) = graph.edges_directed(src_node, Outgoing).next() {
            let id = edge.id();
            let source = edge.source();
            let target = edge.target();
            let weight = *edge.weight();

            if matches!(weight, GraphEdge::Subset(_)) {
                subset_edges.remove(&(source, target));

                if nodes.find_mut(graph[target]) != rep.0 {
                    if subset_edges.insert((rep_node, target)) {
                        changed = true;
                        graph.add_edge(rep_node, target, weight);
                    }
                }
            } else {
                edges.remove(&Edge::new(source, target, weight));

                if nodes.find_mut(graph[target]) != rep.0 {
                    if edges.insert(Edge::new(rep_node, target, weight)) {
                        changed = true;
                        graph.add_edge(rep_node, target, weight);
                    }
                }
            }

            graph.remove_edge(id);
        }
    }

    if invalid {
        invalidate(points_to, nodes, graph, graph_map, rep.0, store, true);
    }

    if accessed_dynamically {
        record_computed_access(
            points_to,
            nodes,
            graph,
            graph_map,
            edges,
            subset_edges,
            rep.0,
            store,
            true,
        );
    }

    (changed, rep, rep_node)
}

impl Graph {
    fn get_node(&mut self, pointer: PointerId) -> NodeIndex {
        get_node(&mut self.graph, &mut self.graph_map, pointer)
    }

    fn add_edge(
        &mut self,
        src: NodeIndex,
        dest: NodeIndex,
        kind: GraphEdge,
        initial: bool,
    ) -> bool {
        debug_assert_ne!(src, dest);

        if matches!(kind, GraphEdge::Subset(_)) {
            if !self.subset_edges.contains(&(src, dest)) {
                if initial {
                    self.graph.add_edge(src, dest, kind);
                    self.subset_edges.insert((src, dest));
                } else {
                    let first_edge = !self.graph.contains_edge(src, dest);
                    self.graph.add_edge(src, dest, kind);
                    self.subset_edges.insert((src, dest));
                    if first_edge {
                        self.prioritise(dest);
                        self.prioritise(src);
                    }
                }

                true
            } else {
                false
            }
        } else {
            let edge = Edge::new(src, dest, kind);

            if !self.edges.contains(&edge) {
                if initial {
                    self.graph.add_edge(src, dest, kind);
                    self.edges.insert(edge);
                } else {
                    let first_edge = !self.graph.contains_edge(src, dest);
                    self.graph.add_edge(src, dest, kind);
                    self.edges.insert(edge);
                    if first_edge {
                        self.prioritise(dest);
                        self.prioritise(src);
                    }
                }

                true
            } else {
                false
            }
        }
    }

    pub(super) fn add_initial_edge(
        &mut self,
        src: PointerId,
        dest: PointerId,
        kind: GraphEdge,
        store: &mut Store,
    ) {
        let src_node = self.get_node(src);
        let dest_node = self.get_node(dest);

        if matches!(kind, GraphEdge::Subset(_)) {
            debug_assert_ne!(src, dest);

            if store.invalid_pointers.contains(dest) {
                self.invalidate(src, store, false);
            }
            if store.accessed_dynamically.contains(dest) {
                self.record_computed_access(src, store, false);
            }
        }

        self.add_edge(src_node, dest_node, kind, true);
    }

    fn make_subset_of<T: GetRepId, U: GetRepId>(
        &mut self,
        src: T,
        dest: U,
        store: &mut Store,
    ) -> bool {
        let src = src.get_rep_id(self);
        let dest = dest.get_rep_id(self);
        if src.0 == dest.0 {
            return false;
        }

        let src_node = self.get_node(src.0);
        let dest_node = self.get_node(dest.0);

        if store.invalid_pointers.contains(dest.0) {
            self.invalidate(src.0, store, false);
        }
        if store.accessed_dynamically.contains(dest.0) {
            self.record_computed_access(src.0, store, false);
        }

        let mut changed = self.add_edge(src_node, dest_node, GraphEdge::Subset(0), false);
        if changed {
            self.prioritise(dest_node);
            self.prioritise(src_node);
        }
        changed |= self.add_all(src, dest, store);
        changed
    }

    // TODO: accept slice of pointers
    // force_visit_neighbours should be true when invalidating after merging nodes.
    pub fn invalidate(
        &mut self,
        pointer: PointerId,
        store: &mut Store,
        force_visit_neighbours: bool,
    ) -> bool {
        invalidate(
            &self.points_to,
            &mut self.nodes,
            &self.graph,
            &mut self.graph_map,
            pointer,
            store,
            force_visit_neighbours,
        )
    }

    // TODO: accept slice of pointers
    // force_visit_neighbours should be true when invalidating after merging nodes.
    pub fn record_computed_access(
        &mut self,
        pointer: PointerId,
        store: &mut Store,
        force_visit_neighbours: bool,
    ) -> bool {
        record_computed_access(
            &mut self.points_to,
            &mut self.nodes,
            &mut self.graph,
            &mut self.graph_map,
            &mut self.edges,
            &mut self.subset_edges,
            pointer,
            store,
            force_visit_neighbours,
        )
    }

    fn merge_prop_accesses(
        &mut self,
        pointer: PointerId,
        node: NodeIndex,
        store: &mut Store,
    ) -> Option<(RepId, NodeIndex)> {
        let mut queue = Vec::new();
        queue.extend(
            self.graph
                .edges_directed(node, Outgoing)
                .filter(|e| matches!(e.weight(), GraphEdge::Prop(_)))
                .map(|e| e.target()),
        );

        if !queue.is_empty() {
            return None;
        }

        let mut to_merge = Vec::new();

        let dest = store.pointers.insert(Pointer::Prop(pointer, NameId::MAX));
        let mut rep = self.get_graph_node_id(dest);
        let mut invalid = store.invalid_pointers.contains(rep.0);
        let mut accessed_dynamically = store.accessed_dynamically.contains(rep.0);
        for prop_node in queue {
            let prop_pointer = self.graph[prop_node];
            let prop_pointer = self.get_graph_node_id(prop_pointer);

            if prop_pointer.0 == rep.0 {
                continue;
            }

            invalid |= store.invalid_pointers.contains(prop_pointer.0);
            accessed_dynamically |= store.accessed_dynamically.contains(prop_pointer.0);

            self.nodes.union(prop_pointer.0, rep.0);

            let new_rep = RepId(self.nodes.find_mut(rep.0));

            let src;
            if new_rep.0 != rep.0 {
                // prop_pointer became representative
                src = rep;
                rep = new_rep;
            } else {
                // rep is still the representative
                src = prop_pointer;
            }
            to_merge.push(src);
        }

        if to_merge.is_empty() {
            return None;
        }

        let (changed, rep, rep_node) =
            self.merge_nodes(rep, invalid, accessed_dynamically, &to_merge, store);

        if changed {
            Some((rep, rep_node))
        } else {
            None
        }
    }

    fn merge_nodes(
        &mut self,
        rep: RepId,
        invalid: bool,
        accessed_dynamically: bool,
        queue: &[RepId],
        store: &mut Store,
    ) -> (bool, RepId, NodeIndex) {
        merge_nodes(
            &mut self.points_to,
            &mut self.nodes,
            &mut self.graph,
            &mut self.graph_map,
            &mut self.edges,
            &mut self.subset_edges,
            rep,
            invalid,
            accessed_dynamically,
            queue,
            store,
        )
    }

    pub fn compute_points_to_map(&mut self, store: &mut Store) {
        for pointer in store.concrete_pointers() {
            self.points_to.insert(pointer, SmallSet::single(pointer));
        }

        // let mut count = 0;

        // for node in self.graph.node_indices() {
        //     if self
        //         .graph
        //         .edges_directed(node, Outgoing)
        //         .any(|e| *e.weight() == GraphEdge::Prop(NameId::MAX))
        //         || store.accessed_dynamically.contains(self.graph[node])
        //     {
        //         let prop_count = self
        //             .graph
        //             .edges_directed(node, Outgoing)
        //             .filter(|e| matches!(e.weight(),GraphEdge::Prop(n) if *n != NameId::MAX))
        //             .count();

        //         count += prop_count;
        //     }
        // }
        // dbg!(count);

        // todo!();

        {
            let mut to_merge = Vec::new();

            let mut tarjan = TarjanSubsetScc::new();

            let self_points_to = &mut self.points_to;
            let self_graph_map = &mut self.graph_map;
            let self_nodes = &mut self.nodes;
            let self_edges = &mut self.edges;
            let self_subset_edges = &mut self.subset_edges;

            tarjan.run(&mut self.graph, |scc, g| {
                if scc.len() > 1 {
                    debug_assert!(scc.iter().all(|n| !store.is_concrete(g[*n])));

                    to_merge.clear();

                    let mut rep = RepId(self_nodes.find_mut(g[scc[0]]));
                    let mut invalid = store.invalid_pointers.contains(rep.0);
                    let mut accessed_dynamically = store.accessed_dynamically.contains(rep.0);

                    for &node in scc.iter().skip(1) {
                        let pointer = RepId(self_nodes.find_mut(g[node]));

                        invalid |= store.invalid_pointers.contains(pointer.0);
                        accessed_dynamically |= store.accessed_dynamically.contains(pointer.0);

                        self_nodes.union(pointer.0, rep.0);

                        let new_rep = RepId(self_nodes.find_mut(rep.0));

                        let src;
                        if new_rep.0 != rep.0 {
                            // prop_pointer became representative
                            src = rep;
                            rep = new_rep;
                        } else {
                            // rep is still the representative
                            src = pointer;
                        }
                        to_merge.push(src);
                    }

                    let (_, rep, _) = merge_nodes(
                        self_points_to,
                        self_nodes,
                        g,
                        self_graph_map,
                        self_edges,
                        self_subset_edges,
                        rep,
                        invalid,
                        accessed_dynamically,
                        &to_merge,
                        store,
                    );

                    if invalid {
                        invalidate(
                            self_points_to,
                            self_nodes,
                            g,
                            self_graph_map,
                            rep.0,
                            store,
                            true,
                        );
                    }
                    if accessed_dynamically {
                        record_computed_access(
                            self_points_to,
                            self_nodes,
                            g,
                            self_graph_map,
                            self_edges,
                            self_subset_edges,
                            rep.0,
                            store,
                            true,
                        );
                    }
                }
            });
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
            priority += 50;
        });

        debug_assert_eq!(self.queue.priorities.len(), self.graph.node_count());

        let internal_nodes = self
            .graph
            .node_indices()
            .filter(|n| !is_sink_node(graph, *n))
            .map(|n| graph[n]);

        self.queue.extend(internal_nodes);
        let mut edges = Vec::new();

        let mut first = true;

        loop {
            self.flow_edges(store, &mut edges);
            // After we've reached a fixedpoint above, if we couldn't infer the values
            // of a pointer, set them to Unknown and run fixedpoint again to propagate
            // the Unknowns.
            loop {
                let mut changed = false;

                for pointer in store.concrete_pointers() {
                    if store.accessed_dynamically.contains(pointer) {
                        let node = self.get_graph_node_id(pointer);
                        if let Some(node) = pointer_to_node(&self.graph_map, node.0) {
                            if let Some((rep, rep_node)) =
                                self.merge_prop_accesses(pointer, node, store)
                            {
                                if !is_sink_node(&self.graph, rep_node) {
                                    self.prioritise(rep_node);
                                    self.queue.push(rep.0);
                                }
                            }
                        }
                    }
                }

                for pointer in store.non_concrete_pointers() {
                    let node = self.get_graph_node_id(pointer);

                    if let Pointer::Arg(callee, _) = store.pointers[pointer] {
                        let callee = self.get_graph_node_id(callee);
                        let unknown_callee = self
                            .get(callee)
                            .map(|concrete_callees| concrete_callees.contains(&PointerId::UNKNOWN))
                            .unwrap_or(false);

                        if unknown_callee {
                            if let Some(concrete_values) = self.get(node).cloned() {
                                // Invalidate arguments passed to unknown callers.
                                for value in &concrete_values {
                                    changed |= self.invalidate(value, store, false);
                                }
                            }
                        }

                        // If the callee is invalid, then it could be called with unknown arguments.
                        if store.invalid_pointers.contains(callee.0) {
                            let changed = self.insert(node, PointerId::UNKNOWN, store);
                            if changed {
                                if let Some(n) = pointer_to_node(&self.graph_map, node.0) {
                                    if !is_sink_node(&self.graph, n) {
                                        // TODO: unnecessary? Is the node guaranteed to be prioritised?
                                        self.prioritise(n);
                                        self.queue.push(node.0);
                                    }
                                }
                            }
                        }
                    }

                    if let Pointer::ReturnValue(callee) = store.pointers[pointer] {
                        let callee = self.get_graph_node_id(callee);
                        // If the callee is invalid, then we can't know how the return type is used.
                        if store.invalid_pointers.contains(callee.0) {
                            changed |= self.invalidate(pointer, store, false);
                        }
                    }

                    if let Pointer::Prop(obj, name) = store.pointers[pointer] {
                        let obj_invalid = store.invalid_pointers.contains(obj);
                        let is_built_in =
                            name != NameId::MAX && is_built_in_property(obj, &store.names[name]);
                        if obj_invalid || is_built_in {
                            changed |= self.invalidate(pointer, store, false);
                            let changed = self.insert(node, PointerId::UNKNOWN, store);
                            if changed {
                                if let Some(n) = pointer_to_node(&self.graph_map, node.0) {
                                    if !is_sink_node(&self.graph, n) {
                                        // TODO: unnecessary? Is the node guaranteed to be prioritised?
                                        self.prioritise(n);
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
                                            self.prioritise(n);
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
                                            // TODO: unnecessary? Is the node guaranteed to be prioritised?
                                            self.prioritise(n);
                                            self.queue.push(node.0);
                                        }
                                    }
                                }
                                continue;
                            }
                            changed |= self.invalidate(pointer, store, false);
                            self.insert(node, PointerId::UNKNOWN, store);
                            if let Some(n) = pointer_to_node(&self.graph_map, node.0) {
                                if !is_sink_node(&self.graph, n) {
                                    // TODO: unnecessary? Is the node guaranteed to be prioritised?
                                    self.prioritise(n);
                                    self.queue.push(node.0);
                                }
                            }
                        }
                    }
                }

                first = false;

                if !changed {
                    break;
                }
            }

            if self.queue.inner.is_empty() {
                break;
            }
        }

        // {
        //     let mut excess_edges = 0;
        //     let mut excess_count: FxHashMap<&str, usize> = FxHashMap::default();

        //     for node in self.graph.node_indices() {
        //         let mut seen_return = false;
        //         let mut edges = FxHashSet::default();
        //         for edge in self.graph.edges_directed(node, Outgoing) {
        //             match edge.weight() {
        //                 GraphEdge::Subset(_) => {}
        //                 GraphEdge::Return(_) => {
        //                     if !seen_return {
        //                         seen_return = true;
        //                     } else {
        //                         excess_edges += 1;
        //                         *excess_count.entry("Return").or_default() += 1;
        //                     }
        //                 }
        //                 GraphEdge::Prop(_) | GraphEdge::Arg(_) => {
        //                     if !edges.insert(*edge.weight()) {
        //                         excess_edges += 1;
        //                         let key = match edge.weight() {
        //                             GraphEdge::Subset(_) => "Subset",
        //                             GraphEdge::Return(_) => "Return",
        //                             GraphEdge::Prop(_) => "Prop",
        //                             GraphEdge::Arg(_) => "Arg",
        //                         };
        //                         *excess_count.entry(key).or_default() += 1;
        //                     }
        //                 }
        //             }
        //         }
        //     }

        //     dbg!(excess_edges, excess_count);
        // }

        // {
        //     let mut counts: std::collections::BTreeMap<usize, usize> =
        //         std::collections::BTreeMap::default();
        //     for (_, set) in self.points_to.iter() {
        //         *counts.entry(set.len()).or_default() += 1;
        //     }
        //     dbg!(counts);

        //     let mut unknown_only = 0;
        //     let mut primitive_only = 0;

        //     let mut primitive_counts: std::collections::BTreeMap<usize, usize> =
        //         std::collections::BTreeMap::default();

        //     let is_primitive = |p: PointerId| {
        //         matches!(
        //             store.pointers[p],
        //             Pointer::Unknown
        //                 | Pointer::NullOrVoid
        //                 | Pointer::Bool
        //                 | Pointer::Num
        //                 | Pointer::String
        //                 | Pointer::BigInt
        //                 | Pointer::Regex
        //         )
        //     };
        //     for (_, set) in self.points_to.iter() {
        //         if set.len() == 1 && set.iter().next().unwrap() == PointerId::UNKNOWN {
        //             unknown_only += 1;
        //         }

        //         if set.iter().all(is_primitive) {
        //             primitive_only += 1;
        //         }

        //         let primitive_count = set.iter().filter(|p| is_primitive(*p)).count();
        //         *primitive_counts.entry(primitive_count).or_default() += 1;
        //     }
        //     dbg!(primitive_counts);
        //     dbg!(unknown_only);
        //     dbg!(primitive_only);

        //     dbg!(self.points_to.len());
        // }

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
        }
    }

    fn merge_scc(
        nodes: &mut GrowableUnionFind<PointerId>,
        points_to: &mut FxHashMap<PointerId, SmallSet>,
        edges: &mut FxHashSet<Edge>,
        subset_edges: &mut FxHashSet<(NodeIndex, NodeIndex)>,

        graph: &mut GraphType,
        graph_map: &mut Vec<NodeIndex>,

        scc: &[NodeIndex],
        store: &mut Store,
    ) {
        let mut to_merge = Vec::new();

        let mut rep = RepId(nodes.find_mut(graph[scc[0]]));
        let mut invalid = store.invalid_pointers.contains(rep.0);
        let mut accessed_dynamically = store.accessed_dynamically.contains(rep.0);
        for &node in scc.iter().skip(1) {
            let pointer = RepId(nodes.find_mut(graph[node]));

            invalid |= store.invalid_pointers.contains(pointer.0);
            accessed_dynamically |= store.accessed_dynamically.contains(pointer.0);

            nodes.union(pointer.0, rep.0);

            let new_rep = RepId(nodes.find_mut(rep.0));

            let src;
            if new_rep.0 != rep.0 {
                // prop_pointer became representative
                src = rep;
                rep = new_rep;
            } else {
                // rep is still the representative
                src = pointer;
            }
            to_merge.push(src);
        }

        let (_, rep, _) = merge_nodes(
            points_to,
            nodes,
            graph,
            graph_map,
            edges,
            subset_edges,
            rep,
            invalid,
            accessed_dynamically,
            &to_merge,
            store,
        );

        if invalid {
            invalidate(points_to, nodes, graph, graph_map, rep.0, store, true);
        }

        if accessed_dynamically {
            record_computed_access(
                points_to,
                nodes,
                graph,
                graph_map,
                edges,
                subset_edges,
                rep.0,
                store,
                true,
            );
        }
    }

    fn flow_edges(&mut self, store: &mut Store, edges: &mut Vec<(NodeIndex, GraphEdge)>) {
        let mut iter = 0;
        while let Some(node) = self.queue.pop() {
            // Skip nodes that aren't their representative. The representative should
            // always be in the queue if the node was, since we add the rep to the queue
            // when we merge it with node.
            let rep = self.get_graph_node_id(node);
            if node != rep.0 {
                continue;
            }
            let node = rep.0;

            if iter % 100_000 == 0 {
                // println!("Iter: {}, queue size: {}", iter, self.queue.inner.len());

                // {
                //     let mut redundant_prop_nodes = 0;
                //     let mut redundant_arg_nodes = 0;
                //     let mut redundant_return_nodes = 0;
                //     for node in self.graph.node_indices() {
                //         let mut set = FxHashSet::default();
                //         for edge in self.graph.edges_directed(node, Outgoing) {
                //             match edge.weight() {
                //                 GraphEdge::Subset(_) => {}
                //                 GraphEdge::Return(_) => {
                //                     if set.contains(edge.weight()) {
                //                         redundant_return_nodes += 1;
                //                     } else {
                //                         set.insert(*edge.weight());
                //                     }
                //                 }
                //                 GraphEdge::Prop(_) => {
                //                     if set.contains(edge.weight()) {
                //                         redundant_prop_nodes += 1;
                //                     } else {
                //                         set.insert(*edge.weight());
                //                     }
                //                 }
                //                 GraphEdge::Arg(_) => {
                //                     if set.contains(edge.weight()) {
                //                         redundant_arg_nodes += 1;
                //                     } else {
                //                         set.insert(*edge.weight());
                //                     }
                //                 }
                //             }
                //         }
                //     }

                //     dbg!(
                //         redundant_prop_nodes,
                //         redundant_arg_nodes,
                //         redundant_return_nodes,
                //         self.graph.node_count(),
                //         store.pointers.len()
                //     );
                // }

                // {
                //     let mut tarjan = TarjanScc::default();

                //     let mut size_counts: std::collections::BTreeMap<usize, usize> =
                //         std::collections::BTreeMap::new();

                //     tarjan.run(&self.graph, |scc| {
                //         *size_counts.entry(scc.len()).or_default() += 1;
                //     });

                //     dbg!(size_counts);
                // }
                {
                    let mut tarjan = TarjanSubsetScc::new();

                    let self_nodes = &mut self.nodes;
                    let self_points_to = &mut self.points_to;
                    let self_queue = &mut self.queue;
                    let self_edges = &mut self.edges;
                    let self_subset_edges = &mut self.subset_edges;
                    let self_graph_map = &mut self.graph_map;

                    tarjan.run(&mut self.graph, |scc, g| {
                        if scc.len() > 1 {
                            Self::merge_scc(
                                self_nodes,
                                self_points_to,
                                self_edges,
                                self_subset_edges,
                                g,
                                self_graph_map,
                                scc,
                                store,
                            );
                        }
                    });

                    let mut tarjan = TarjanScc::new();

                    let graph = &self.graph;

                    let mut priority = 0_u32;
                    tarjan.run(&self.graph, |scc| {
                        let node = graph[scc[0]];
                        self_queue.priorities.insert(node, priority);
                        self_queue.inner.change_priority(&node, priority);
                        priority += 1;
                    });

                    // let mut tarjan = TarjanScc::new();

                    // let mut subset_scc_size_counts: std::collections::BTreeMap<usize, usize> =
                    //     std::collections::BTreeMap::new();

                    // // let mut nodes_in_sccs = FxHashSet::default();

                    // tarjan.run(&mut self.graph, |scc, _, _, _, _| {
                    //     if scc.len() > 1 {
                    //         *subset_scc_size_counts.entry(scc.len()).or_default() += 1;
                    //         // nodes_in_sccs += scc.len();
                    //     }
                    // });

                    // // dbg!(nodes_in_sccs);
                    // // dbg!(nodes_in_sccs as f64 / self.graph.node_count() as f64 * 100.0);

                    // dbg!(subset_scc_size_counts);
                }
            }

            edges.clear();

            let n = self.get_node(node);

            edges.extend(
                self.graph
                    .edges_directed(n, Outgoing)
                    .map(|e| (e.target(), *e.weight())),
            );

            iter += 1;

            for (dest, kind) in edges.iter().copied() {
                debug_assert_ne!(dest, n, "Graph should not have loops");
                let dest = self.get_graph_node_id(self.graph[dest]);
                let dest_node = self.graph_map[dest.0.index()];

                match kind {
                    GraphEdge::Subset(_) => {
                        let changed = self.add_all(node, dest, store);
                        if changed {
                            if !is_sink_node(&self.graph, dest_node) {
                                self.queue.push(dest.0);
                            }
                        }
                    }
                    GraphEdge::Return(_) => {
                        let callees = match self.get(node) {
                            Some(c) => c.clone(),
                            None => continue,
                        };
                        let mut changed = false;
                        for callee in &callees {
                            if !store.is_callable_pointer(callee) && callee != PointerId::UNKNOWN {
                                continue;
                            }
                            let return_node = store.pointers.insert(Pointer::ReturnValue(callee));
                            let return_node = self.get_graph_node_id(return_node);
                            if return_node.0 == dest.0 {
                                continue;
                            }
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

                        let mut to_merge = Vec::new();

                        // if name == NameId::MAX {
                        //     debug_assert!(concrete_objects.iter().all(|c| !self
                        //         .graph
                        //         .edges_directed(self.graph_map[c.index()], Outgoing)
                        //         .any(
                        //             |e| matches!(e.weight(), GraphEdge::Prop(n) if *n != NameId::MAX)
                        //         )), "{:#?}", concrete_objects.iter().map(|c| self
                        //             .graph
                        //             .edges_directed(self.graph_map[c.index()], Outgoing)
                        //             .filter_map(
                        //                 |e| match e.weight() {
                        //                     GraphEdge::Prop(n) if *n == NameId::MAX => Some("MAX".to_string()),
                        //                     GraphEdge::Prop(n) => Some(store.names[*n].to_string()),
                        //                     _ => None
                        //                 }
                        //             ).collect::<Vec<_>>()).collect::<Vec<_>>());
                        // }

                        let mut rep = dest;
                        let mut invalid = store.invalid_pointers.contains(dest.0);
                        let mut accessed_dynamically = store.accessed_dynamically.contains(dest.0);
                        let mut has_unknown = false;
                        let mut has_null_or_void = false;
                        for concrete_object in &concrete_objects {
                            if concrete_object == PointerId::UNKNOWN {
                                has_unknown = true;
                                continue;
                            }
                            if concrete_object == PointerId::NULL_OR_VOID {
                                has_null_or_void = true;
                                continue;
                            }
                            if name == NameId::MAX {
                                // todo!();
                                // // Dynamic
                                // if let Some(node) =
                                //     pointer_to_node(&self.graph_map, concrete_object)
                                // {
                                //     if let Some(changed) = self.merge_prop_accesses(
                                //         concrete_object,
                                //         node,
                                //         store,
                                //         prop_merge_buffer,
                                //         prop_merge_queue
                                //     ) {
                                //         if !is_sink_node(
                                //             &self.graph,
                                //             self.graph_map[changed.0.index()],
                                //         ) {
                                //             // self.prioritise(changed);
                                //             self.queue.push(changed.0);
                                //         }
                                //     }
                                // }
                            } else {
                                if concrete_object.is_primitive()
                                    && !is_built_in_property(concrete_object, &store.names[name])
                                {
                                    // non-built in prop on primitive - ignore.
                                    continue;
                                }
                            }
                            let prop_pointer =
                                store.pointers.insert(Pointer::Prop(concrete_object, name));
                            let prop_pointer = self.get_graph_node_id(prop_pointer);

                            if rep.0 == prop_pointer.0 {
                                continue;
                            }

                            invalid |= store.invalid_pointers.contains(prop_pointer.0);
                            accessed_dynamically |=
                                store.accessed_dynamically.contains(prop_pointer.0);

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
                            to_merge.push(src);
                        }

                        let mut changed = false;

                        if has_unknown {
                            changed |= self.insert(rep.0, PointerId::UNKNOWN, store);
                        }
                        if has_null_or_void {
                            changed |= self.insert(rep.0, PointerId::NULL_OR_VOID, store);
                        }

                        if to_merge.is_empty() {
                            debug_assert_eq!(rep.0, dest.0);
                            if changed {
                                if !is_sink_node(&self.graph, dest_node) {
                                    self.queue.push(dest.0);
                                }
                            }

                            continue;
                        }

                        let (ch, rep, rep_node) =
                            self.merge_nodes(rep, invalid, accessed_dynamically, &to_merge, store);

                        if ch || changed {
                            if !is_sink_node(&self.graph, rep_node) {
                                self.prioritise(rep_node);
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
                                        self.invalidate(dest.0, store, false);
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

    fn prioritise(&mut self, node: NodeIndex) {
        prioritise(&mut self.queue, &mut self.graph, &mut self.graph_map, node);
    }

    // fn prioritise2(&mut self, node: NodeIndex, store: &Store) {
    //     // We choose the smallest x such that: out < x < in
    //     let in_priority = self
    //         .graph
    //         .neighbors_directed(node, Incoming)
    //         .filter_map(|n| self.queue.priorities.get(&self.graph[n]).copied())
    //         .min()
    //         .unwrap_or(u32::MAX);
    //     let out_priority = self
    //         .graph
    //         .neighbors_directed(node, Outgoing)
    //         .filter_map(|n| self.queue.priorities.get(&self.graph[n]).copied())
    //         .max()
    //         .unwrap_or(0);

    //     if let Some(existing) = self.queue.priorities.get(&self.graph[node]) {
    //         if out_priority < *existing && *existing < in_priority {
    //             // Priority is already suitable.
    //             return;
    //         }
    //     }

    //     if in_priority == out_priority || in_priority < out_priority {
    //         let mut tarjan = TarjanScc::default();

    //         let priorities = &mut self.queue.priorities;
    //         let inner = &mut self.queue.inner;
    //         let graph = &self.graph;

    //         let mut priority = 0_u32;
    //         tarjan.run(&self.graph, |scc| {
    //             for n in scc {
    //                 priorities.insert(graph[*n], priority);
    //                 inner.change_priority(&graph[*n], priority);
    //             }
    //             priority += 1;
    //         });
    //     } else {
    //         self.queue
    //             .priorities
    //             .insert(self.graph[node], out_priority + 1);
    //         self.queue
    //             .inner
    //             .change_priority(&self.graph[node], out_priority + 1);
    //     }
    //     // if in_priority == out_priority {
    //     //     println!("Full re-prioritisation 1");
    //     //     let mut tarjan = TarjanScc::default();

    //     //     let priorities = &mut self.queue.priorities;
    //     //     let inner = &mut self.queue.inner;
    //     //     let graph = &self.graph;

    //     //     let mut priority = 0_u32;
    //     //     tarjan.run(&self.graph, |scc| {
    //     //         for n in scc {
    //     //             priorities.insert(graph[*n], priority);
    //     //             inner.change_priority(&graph[*n], priority);
    //     //         }
    //     //         priority += 1;
    //     //     });
    //     // } else if in_priority < out_priority {
    //     //     println!("Full re-prioritisation 2");
    //     //     // let dot = self.get_dot(&store);

    //     //     // std::fs::write("./relo.dot", dot).expect("Failed to output fn graph");
    //     //     // dbg!(in_priority, out_priority);
    //     //     // let in_priorities = self
    //     //     //     .graph
    //     //     //     .neighbors_directed(node, Incoming)
    //     //     //     .map(|n| self.queue.priorities.get(&self.graph[n]).copied())
    //     //     //     .collect::<Vec<_>>();
    //     //     // let out_priorities = self
    //     //     //     .graph
    //     //     //     .neighbors_directed(node, Outgoing)
    //     //     //     .map(|n| self.queue.priorities.get(&self.graph[n]).copied())
    //     //     //     .collect::<Vec<_>>();
    //     //     // dbg!(in_priorities, out_priorities);
    //     //     // dbg!(node, self.graph[node]);
    //     //     // unreachable!("order has been violated");
    //     //     let mut tarjan = TarjanScc::default();

    //     //     let priorities = &mut self.queue.priorities;
    //     //     let inner = &mut self.queue.inner;
    //     //     let graph = &self.graph;

    //     //     let mut priority = 0_u32;
    //     //     tarjan.run(&self.graph, |scc| {
    //     //         for n in scc {
    //     //             priorities.insert(graph[*n], priority);
    //     //             inner.change_priority(&graph[*n], priority);
    //     //         }
    //     //         priority += 1;
    //     //     });
    //     // } else {
    //     //     self.queue
    //     //         .priorities
    //     //         .insert(self.graph[node], out_priority + 1);
    //     //     self.queue
    //     //         .inner
    //     //         .change_priority(&self.graph[node], out_priority + 1);
    //     // }
    // }

    fn get_graph_node_id(&mut self, pointer: PointerId) -> RepId {
        RepId(self.nodes.find_mut(pointer))
    }

    fn insert<T: GetRepId>(&mut self, pointer: T, value: PointerId, store: &Store) -> bool {
        debug_assert!(store.is_concrete(value));

        let node = pointer.get_rep_id(self).0;
        debug_assert!(!store.is_concrete(node));

        self.points_to.entry(node).or_default().insert(value)
    }

    fn points_to_nothing<T: GetRepId>(&mut self, pointer: T) -> bool {
        let node = pointer.get_rep_id(self).0;
        !self.points_to.contains_key(&node)
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
                self.invalidate(p, store, false);
            }
        }
        if store.accessed_dynamically.contains(dest) & !store.accessed_dynamically.contains(src) {
            for p in src_set.iter() {
                self.record_computed_access(p, store, false);
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
                        if *prop == NameId::MAX {
                            format!("Prop({}, Dynamic)", obj.as_u32())
                        } else {
                            format!(
                                "Prop({}, ({}, '{}'))",
                                obj.as_u32(),
                                prop.as_u32(),
                                store.names[*prop]
                            )
                        }
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

fn prioritise(
    queue: &mut UniqueQueue,

    graph: &GraphType,
    graph_map: &[NodeIndex],

    node: NodeIndex,
) {
    let pointer = graph[node];
    if !queue.priorities.contains_key(&pointer) {
        let node = graph_map[pointer.index()];
        let priority = graph
            .neighbors_directed(node, Incoming)
            .map(|n| queue.priorities[&graph[n]])
            .min()
            .unwrap_or(u32::MAX);
        queue.priorities.insert(pointer, priority.saturating_sub(1));
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

#[derive(Debug, Copy, Clone, Hash, Eq)]
struct Edge(NodeIndex, NodeIndex, GraphEdge);

impl Edge {
    pub fn new(src: NodeIndex, dest: NodeIndex, kind: GraphEdge) -> Self {
        Self(src, dest, kind)
    }
}

impl PartialEq for Edge {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0) & self.1.eq(&other.1) & self.2.eq(&other.2)
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub(super) enum GraphEdge {
    // u32 is dummy/unused
    Subset(u32),
    // u32 is dummy/unused
    Return(u32),
    Prop(NameId),
    Arg(u16),
}

// impl PartialEq for GraphEdge {
//     fn eq(&self, other: &Self) -> bool {
//         let a = match *self {
//             Self::Arg(a) => a,
//             Self::Prop(a) => a.as_u32(),
//             Self::Subset(a) => a,
//             Self::Return(a) => a,
//         };
//         let b = match *other {
//             Self::Arg(a) => a,
//             Self::Prop(a) => a.as_u32(),
//             Self::Subset(a) => a,
//             Self::Return(a) => a,
//         };
//         std::mem::discriminant(self) == std::mem::discriminant(other) && a == b
//     }
// }

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
    inner: PriorityQueue<PointerId, u32>,
    priorities: FxHashMap<PointerId, u32>,
}

impl UniqueQueue {
    fn pop(&mut self) -> Option<PointerId> {
        self.inner.pop().map(|p| p.0)
    }

    fn push(&mut self, node: PointerId) {
        self.inner.push(node, self.priorities[&node]);
    }

    fn extend(&mut self, iter: impl Iterator<Item = PointerId>) {
        let priorities = &self.priorities;
        self.inner.extend(iter.map(|n| (n, priorities[&n])));
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

    pub fn TODO_TEMP_LEN(&self) -> usize {
        match self {
            SmallSet::Inline(set) => set.len(),
            SmallSet::Heap(set) => set.count(),
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

#[derive(Copy, Clone, Debug)]
struct NodeData {
    rootindex: Option<std::num::NonZeroUsize>,
}

/// A reusable state for computing the *strongly connected components* using [Tarjan's algorithm][1].
///
/// [1]: https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
#[derive(Debug)]
struct TarjanSubsetScc {
    index: usize,
    componentcount: usize,
    nodes: Vec<NodeData>,
    stack: Vec<NodeIndex>,
}

impl TarjanSubsetScc {
    /// Creates a new `TarjanScc`
    pub fn new() -> Self {
        TarjanSubsetScc {
            index: 1,                        // Invariant: index < componentcount at all times.
            componentcount: std::usize::MAX, // Will hold if componentcount is initialized to number of nodes - 1 or higher.
            nodes: Vec::new(),
            stack: Vec::new(),
        }
    }

    /// \[Generic\] Compute the *strongly connected components* using Algorithm 3 in
    /// [A Space-Efficient Algorithm for Finding Strongly Connected Components][1] by David J. Pierce,
    /// which is a memory-efficient variation of [Tarjan's algorithm][2].
    ///
    ///
    /// [1]: https://homepages.ecs.vuw.ac.nz/~djp/files/P05.pdf
    /// [2]: https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
    ///
    /// Calls `f` for each strongly strongly connected component (scc).
    /// The order of node ids within each scc is arbitrary, but the order of
    /// the sccs is their postorder (reverse topological sort).
    ///
    /// For an undirected graph, the sccs are simply the connected components.
    ///
    /// This implementation is recursive and does one pass over the nodes.
    fn run<F>(&mut self, g: &mut GraphType, mut f: F)
    where
        F: FnMut(&[NodeIndex], &mut GraphType),
    {
        self.nodes.clear();
        self.nodes
            .resize(g.raw_nodes().len(), NodeData { rootindex: None });

        for n in g.node_indices() {
            let visited = self.nodes[n.index()].rootindex.is_some();
            if !visited {
                self.visit(n, g, &mut f);
            }
        }

        debug_assert!(self.stack.is_empty());
    }

    fn visit<F>(&mut self, v: NodeIndex, g: &mut GraphType, f: &mut F)
    where
        F: FnMut(&[NodeIndex], &mut GraphType),
    {
        macro_rules! node {
            ($node:expr) => {
                self.nodes[$node.index()]
            };
        }

        let node_v = &mut node![v];
        debug_assert!(node_v.rootindex.is_none());

        let mut v_is_local_root = true;
        let v_index = self.index;
        node_v.rootindex = std::num::NonZeroUsize::new(v_index);
        self.index += 1;

        let mut neighbours = g.neighbors_directed(v, Outgoing).detach();
        while let Some(w) = neighbours.next(g) {
            let weight = g[w.0];
            let target = w.1;
            if !matches!(weight, GraphEdge::Subset(_)) {
                continue;
            }
            if node![target].rootindex.is_none() {
                self.visit(target, g, f);
            }
            if node![target].rootindex < node![v].rootindex {
                node![v].rootindex = node![target].rootindex;
                v_is_local_root = false
            }
        }

        if v_is_local_root {
            // Pop the stack and generate an SCC.
            let mut indexadjustment = 1;
            let c = std::num::NonZeroUsize::new(self.componentcount);
            let nodes = &mut self.nodes;
            let start = self
                .stack
                .iter()
                .rposition(|&w| {
                    if nodes[v.index()].rootindex > nodes[w.index()].rootindex {
                        true
                    } else {
                        nodes[w.index()].rootindex = c;
                        indexadjustment += 1;
                        false
                    }
                })
                .map(|x| x + 1)
                .unwrap_or_default();
            nodes[v.index()].rootindex = c;
            self.stack.push(v); // Pushing the component root to the back right before getting rid of it is somewhat ugly, but it lets it be included in f.
            f(&self.stack[start..], g);
            self.stack.truncate(start);
            self.index -= indexadjustment; // Backtrack index back to where it was before we ever encountered the component.
            self.componentcount -= 1;
        } else {
            self.stack.push(v); // Stack is filled up when backtracking, unlike in Tarjans original algorithm.
        }
    }
}
