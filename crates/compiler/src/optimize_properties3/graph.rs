use std::collections::BinaryHeap;
use std::fmt::{Display, Write};

use index::vec::Idx;
use petgraph::algo::TarjanScc;
use petgraph::graphmap::DiGraphMap;
use petgraph::Direction::{Incoming, Outgoing};

use super::*;

pub struct Graph {
    graph: DiGraphMap<GraphNodeId, GraphEdge>,
    node_map: FxHashMap<PointerId, GraphNodeId>,
    nodes: UnionFind<GraphNodeId>,
    points_to: FxHashMap<GraphNodeId, SmallSet<PointerId>>,
    cur_node_id: GraphNodeId,
}

impl Graph {
    pub fn new() -> Self {
        Self {
            graph: DiGraphMap::default(),
            node_map: FxHashMap::default(),
            nodes: UnionFind::new(0),
            points_to: FxHashMap::default(),
            cur_node_id: GraphNodeId::from_u32(0),
        }
    }

    pub(super) fn add_edge(&mut self, src: PointerId, dest: PointerId, kind: GraphEdge) -> bool {
        let src = self.get_graph_node_id(src).0;
        let dest = self.get_graph_node_id(dest).0;

        if kind == GraphEdge::Subset {
            debug_assert_ne!(src, dest);
        }

        self.graph.add_edge(src, dest, kind).is_none()
    }

    fn make_subset_of<T: GetRepId, U: GetRepId>(&mut self, src: T, dest: U) -> bool {
        let src = src.get_rep_id(self).0;
        let dest = dest.get_rep_id(self).0;
        if src == dest {
            return false;
        }

        let mut changed = self.graph.add_edge(src, dest, GraphEdge::Subset).is_none();
        changed |= self.add_all(src, dest);
        changed
    }

    pub fn compute_points_to_map(&mut self, store: &mut Store) {
        // Invalidate parameters for functions that access the arguments array.
        for func in store.functions.values() {
            if func.accesses_arguments_array {
                for param in func.param_indices() {
                    let pointer = store.pointers.insert(Pointer::Var(param));
                    store.invalid_pointers.insert(pointer);
                }
            }
        }

        store.invalid_pointers.insert(store.unknown_pointer);

        for pointer in 0..store.pointers.len() {
            let pointer = PointerId::from_usize(pointer);
            let concrete = match store.pointers[pointer] {
                Pointer::Object(_)
                | Pointer::Fn(_)
                | Pointer::Unknown
                | Pointer::NullOrVoid
                | Pointer::Bool
                | Pointer::Num
                | Pointer::String
                | Pointer::BigInt
                | Pointer::Regex => Some(pointer),
                Pointer::ReturnValue(_)
                | Pointer::Arg(_, _)
                | Pointer::Prop(_, _)
                | Pointer::Var(_) => None,
            };
            if let Some(concrete) = concrete {
                self.insert(pointer, concrete, store);
            }
        }

        let mut tarjan = TarjanScc::default();

        let mut priorities =
            FxHashMap::with_capacity_and_hasher(self.graph.node_count(), Default::default());

        let mut priority = 0_u32;
        tarjan.run(&self.graph, |scc| {
            for n in scc {
                priorities.insert(*n, priority);
            }
            priority += 1;
        });

        debug_assert_eq!(priorities.len(), self.graph.node_count());

        let mut queue = UniqueQueue::new(priorities);
        queue.extend(self.graph.nodes());
        let mut edges = Vec::new();

        loop {
            self.flow_edges(store, &mut queue, &mut edges);
            // After we've reached a fixedpoint above, if we couldn't infer the values
            // of a pointer, set them to Unknown and run fixedpoint again to propagate
            // the Unknowns.
            loop {
                let mut invalidated = false;

                for pointer in 0..store.pointers.len() {
                    let pointer = PointerId::from_usize(pointer);
                    let node = self.get_graph_node_id(pointer);

                    if let Pointer::Arg(callee, _) = store.pointers[pointer] {
                        let unknown_callee = self
                            .get(callee)
                            .map(|concrete_callees| {
                                concrete_callees.contains(&store.unknown_pointer)
                            })
                            .unwrap_or(false);

                        if unknown_callee {
                            if let Some(concrete_values) = self.get(node) {
                                // Invalidate arguments passed to unknown callers.
                                for &value in concrete_values {
                                    invalidated |= store.invalidate(value);
                                }
                            }
                        }
                    }

                    if store.invalid_pointers.contains(&pointer) {
                        if let Some(values) = self.get(node) {
                            for &value in values {
                                invalidated |= store.invalidate(value);
                            }
                        }
                    }
                    if matches!(store.pointers[pointer], Pointer::Prop(obj, _) if store.invalid_pointers.contains(&obj))
                    {
                        if let Some(values) = self.get(node) {
                            for &value in values {
                                invalidated |= store.invalidate(value);
                            }
                        }
                        invalidated |= store.invalidate(pointer);
                        let changed = self.insert(node, store.unknown_pointer, store);
                        if changed {
                            prioritise(&self.graph, &mut queue, node.0);
                            queue.push(node.0);
                        }
                        continue;
                    }

                    // Functions implicitly return undefined sometimes.
                    if matches!(store.pointers[pointer], Pointer::ReturnValue(_)) {
                        let changed = self.insert(node, store.null_or_void_pointer, store);
                        if changed {
                            prioritise(&self.graph, &mut queue, node.0);
                            queue.push(node.0);
                        }
                        continue;
                    }
                    if self.points_to_nothing(node) {
                        // Undefined properties on valid objects are undefined. We know the obj must be valid,
                        // otherwise flow edges would have flowed Unknown into this prop.
                        if matches!(store.pointers[pointer], Pointer::Prop(_, _)) {
                            let changed = self.insert(node, store.null_or_void_pointer, store);
                            if changed {
                                prioritise(&self.graph, &mut queue, node.0);
                                queue.push(node.0);
                            }
                            continue;
                        }
                        invalidated |= store.invalidate(pointer);
                        self.insert(node, store.unknown_pointer, store);
                        prioritise(&self.graph, &mut queue, node.0);
                        queue.push(node.0);
                    }
                }

                if !invalidated {
                    break;
                }
            }

            if queue.inner.is_empty() {
                break;
            }
        }
    }

    fn flow_edges(
        &mut self,
        store: &mut Store,
        queue: &mut UniqueQueue,
        edges: &mut Vec<(GraphNodeId, GraphNodeId, GraphEdge)>,
    ) {
        while let Some(node) = queue.pop() {
            edges.clear();
            edges.extend(
                self.graph
                    .edges_directed(node, Outgoing)
                    .map(|e| (e.0, e.1, *e.2)),
            );

            for (_, dest, kind) in edges.iter().copied() {
                let node = RepId(self.nodes.find_mut(node));
                let dest = RepId(self.nodes.find_mut(dest));

                match kind {
                    GraphEdge::Subset => {
                        let changed = self.add_all(node, dest);
                        if changed {
                            prioritise(&self.graph, queue, self.nodes.find_mut(dest.0));
                            queue.push(dest.0);
                        }
                    }
                    GraphEdge::Return => {
                        let callees = match self.get(node) {
                            Some(c) => c.clone(),
                            None => continue,
                        };
                        let mut changed = false;
                        let mut dest = dest;
                        for callee in callees {
                            let return_node = store.pointers.insert(Pointer::ReturnValue(callee));
                            let return_node = self.get_graph_node_id(return_node);
                            if return_node.0 == dest.0 {
                                continue;
                            }
                            if self.make_subset_of(return_node, dest) {
                                prioritise(&self.graph, queue, self.nodes.find_mut(return_node.0));
                                changed = true;
                                dest = RepId(self.nodes.find_mut(dest.0));
                            }
                        }
                        if changed {
                            prioritise(&self.graph, queue, self.nodes.find_mut(dest.0));
                            queue.push(dest.0);
                        }
                    }
                    GraphEdge::Prop(name) => {
                        let concrete_objects = match self.get(node) {
                            Some(o) => o.clone(),
                            None => continue,
                        };

                        let mut changed = false;
                        let mut dest = dest;
                        for concrete_object in concrete_objects {
                            let prop_pointer =
                                store.pointers.insert(Pointer::Prop(concrete_object, name));
                            let prop_pointer = self.get_graph_node_id(prop_pointer);

                            if dest.0 == prop_pointer.0 {
                                continue;
                            }

                            self.nodes.union(prop_pointer.0, dest.0);

                            let rep = RepId(self.nodes.find_mut(dest.0));

                            let src;
                            if rep.0 != dest.0 {
                                // prop_pointer became representative
                                src = dest;
                                dest = rep;
                            } else {
                                // dest became representative
                                src = prop_pointer;
                            }

                            debug_assert_ne!(src.0, dest.0);

                            if let Some(src_values) = self.points_to.remove(&src.0) {
                                match self.points_to.entry(rep.0) {
                                    Entry::Occupied(mut entry) => {
                                        entry.get_mut().extend(src_values);
                                    }
                                    Entry::Vacant(entry) => {
                                        entry.insert(src_values);
                                    }
                                }
                            }

                            let in_edges = self
                                .graph
                                .edges_directed(src.0, Incoming)
                                .map(|e| (e.0, e.1, *e.2))
                                .collect::<Vec<_>>();
                            let out_edges = self
                                .graph
                                .edges_directed(src.0, Outgoing)
                                .map(|e| (e.0, e.1, *e.2))
                                .collect::<Vec<_>>();

                            self.graph.remove_node(src.0);
                            for in_edge in in_edges {
                                self.graph.add_edge(in_edge.0, rep.0, in_edge.2);
                            }
                            for out_edge in out_edges {
                                self.graph.add_edge(rep.0, out_edge.1, out_edge.2);
                            }

                            changed = true;
                        }

                        if changed {
                            prioritise(&self.graph, queue, self.nodes.find_mut(dest.0));
                            queue.push(dest.0);
                        }
                    }
                    GraphEdge::Arg(index) => {
                        let concrete_callees = match self.get(node) {
                            Some(c) => c.clone(),
                            None => continue,
                        };

                        let mut dest = dest;
                        for concrete_callee in concrete_callees {
                            match store.pointers[concrete_callee] {
                                Pointer::Fn(callee) => {
                                    let func = store.functions.get(&callee).unwrap();
                                    match func.param_indices().nth(index) {
                                        Some(param) => {
                                            let param = store.pointers.insert(Pointer::Var(param));
                                            let param = self.get_graph_node_id(param);
                                            if self.make_subset_of(dest, param) {
                                                dest = RepId(self.nodes.find_mut(dest.0));
                                                let param = RepId(self.nodes.find_mut(param.0));
                                                prioritise(&self.graph, queue, param.0);
                                                queue.push(param.0);
                                            }
                                        }
                                        None => {
                                            // Don't invalidate extra arguments. The function cannot access them
                                            // unless it uses e.g. arguments array, which is detected else where.
                                        }
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

    fn get_graph_node_id(&mut self, pointer: PointerId) -> RepId {
        get_graph_node_id(
            pointer,
            &mut self.cur_node_id,
            &mut self.nodes,
            &mut self.node_map,
        )
    }

    fn insert<T: GetRepId>(&mut self, pointer: T, value: PointerId, store: &Store) -> bool {
        debug_assert!(store.pointers[value].is_concrete());

        let node = pointer.get_rep_id(self).0;
        self.points_to.entry(node).or_default().insert(value)
    }

    fn points_to_nothing<T: GetRepId>(&mut self, pointer: T) -> bool {
        let node = pointer.get_rep_id(self).0;
        self.points_to.get(&node).is_none()
    }

    fn add_all<T: GetRepId, U: GetRepId>(&mut self, src: T, dest: U) -> bool {
        let src = src.get_rep_id(self).0;
        let dest = dest.get_rep_id(self).0;

        if src == dest {
            return false;
        }

        if self.points_to.get(&src).is_none() {
            return false;
        }

        self.points_to.entry(dest).or_default();

        let [src_set, dest_set] = self.points_to.get_many_mut([&src, &dest]).unwrap();

        let before = dest_set.len();

        if dest_set.is_empty() {
            *dest_set = src_set.clone();
        } else {
            dest_set.extend_ref(src_set);
        }

        let changed = dest_set.len() != before;

        changed
    }

    fn get<T: GetRepId>(&mut self, pointer: T) -> Option<&SmallSet<PointerId>> {
        let representative = { pointer.get_rep_id(self).0 };
        self.points_to.get(&representative)
    }

    pub(super) fn get_immutable(&self, pointer: PointerId) -> Option<&SmallSet<PointerId>> {
        let representative = self.nodes.find(self.node_map[&pointer]);
        self.points_to.get(&representative)
    }

    pub(super) fn get_dot(&mut self, store: &Store) -> String {
        let print_graph = self.graph.clone();

        let mut map = |store: &Store, n: GraphNodeId| {
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
            let mut res = format!("({:?})", n);

            for p in pointers {
                let value: String = match &store.pointers[p] {
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
                write!(&mut res, "id:{}, {}", p.as_u32(), value).unwrap();
            }
            res
        };

        let print_graph: petgraph::prelude::DiGraph<String, GraphEdge> = print_graph
            .into_graph()
            .map(|_, n| map(&store, *n), |_, e| *e);
        format!("{}", petgraph::dot::Dot::with_config(&print_graph, &[]))
    }
}

fn get_graph_node_id(
    pointer: PointerId,
    cur_node_id: &mut GraphNodeId,
    nodes: &mut UnionFind<GraphNodeId>,
    node_map: &mut FxHashMap<PointerId, GraphNodeId>,
) -> RepId {
    match node_map.entry(pointer) {
        Entry::Occupied(entry) => RepId(nodes.find_mut(*entry.get())),
        Entry::Vacant(entry) => {
            let id = *cur_node_id;
            cur_node_id.increment_by(1);
            nodes.add(id);
            entry.insert(id);
            RepId(id)
        }
    }
}

#[derive(Clone, Copy)]
struct RepId(GraphNodeId);

index::newtype_index!(struct GraphNodeId { .. });

trait GetRepId {
    fn get_rep_id(self, points_to: &mut Graph) -> RepId;
}

impl GetRepId for PointerId {
    fn get_rep_id(self, points_to: &mut Graph) -> RepId {
        points_to.get_graph_node_id(self)
    }
}

impl GetRepId for RepId {
    fn get_rep_id(self, _: &mut Graph) -> RepId {
        self
    }
}

impl GetRepId for GraphNodeId {
    fn get_rep_id(self, points_to: &mut Graph) -> RepId {
        RepId(points_to.nodes.find_mut(self))
    }
}

fn prioritise(
    graph: &DiGraphMap<GraphNodeId, GraphEdge>,
    queue: &mut UniqueQueue,
    node: GraphNodeId,
) {
    if !queue.priorities.contains_key(&node) {
        let priority = graph
            .neighbors_directed(node, Incoming)
            .map(|n| queue.priorities[&n])
            .min()
            .unwrap_or(u32::MAX);
        queue.priorities.insert(node, priority);
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum GraphEdge {
    Subset,
    Return,
    Prop(NameId),
    Arg(usize),
}

impl Display for GraphEdge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

#[derive(Debug)]
struct PrioritizedNode(u32, GraphNodeId);

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

#[derive(Debug)]
struct UniqueQueue {
    inner: BinaryHeap<PrioritizedNode>,
    priorities: FxHashMap<GraphNodeId, u32>,
}

impl UniqueQueue {
    fn new(priorities: FxHashMap<GraphNodeId, u32>) -> Self {
        Self {
            inner: BinaryHeap::new(),
            priorities,
        }
    }

    fn pop(&mut self) -> Option<GraphNodeId> {
        self.inner.pop().map(|p| p.1)
    }

    fn push(&mut self, node: GraphNodeId) {
        self.inner
            .push(PrioritizedNode(self.priorities[&node], node));
    }

    fn extend(&mut self, iter: impl Iterator<Item = GraphNodeId>) {
        let priorities = &self.priorities;
        self.inner
            .extend(iter.map(|n| PrioritizedNode(priorities[&n], n)))
    }
}
