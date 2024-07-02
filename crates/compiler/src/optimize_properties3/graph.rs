use std::collections::BinaryHeap;
use std::fmt::{Display, Write};
use std::hash::BuildHasherDefault;
use std::rc::Rc;

use arrayvec::ArrayVec;
use petgraph::algo::TarjanScc;
use petgraph::graphmap::GraphMap;
use petgraph::Directed;
use petgraph::Direction::{Incoming, Outgoing};
use rustc_hash::FxHasher;
use unionfind::GrowableUnionFind;

use super::*;

pub struct Graph {
    graph: GraphMap<PointerId, GraphEdge, Directed, BuildHasherDefault<FxHasher>>,
    nodes: GrowableUnionFind<PointerId>,
    points_to: FxHashMap<PointerId, SmallSet>,
    queue: UniqueQueue,
}

impl Graph {
    pub fn new() -> Self {
        Self {
            graph: GraphMap::default(),
            nodes: GrowableUnionFind::new(),
            points_to: FxHashMap::default(),
            queue: UniqueQueue::new(),
        }
    }

    pub(super) fn add_initial_edge(
        &mut self,
        src: PointerId,
        dest: PointerId,
        kind: GraphEdge,
    ) -> bool {
        let src = self.get_graph_node_id(src).0;
        let dest = self.get_graph_node_id(dest).0;

        if kind == GraphEdge::Subset {
            debug_assert_ne!(src, dest);
        }

        self.graph.add_edge(src, dest, kind).is_none()
    }

    fn make_subset_of<T: GetRepId, U: GetRepId>(&mut self, src: T, dest: U) -> bool {
        let src = src.get_rep_id(self);
        let dest = dest.get_rep_id(self);
        if src.0 == dest.0 {
            return false;
        }

        self.prioritise(src);
        self.prioritise(dest);

        let mut changed = self
            .graph
            .add_edge(src.0, dest.0, GraphEdge::Subset)
            .is_none();
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

        store.invalid_pointers.insert(PointerId::UNKNOWN);

        for pointer in 0..store.pointers.len() {
            let pointer = PointerId::from_usize(pointer);
            if store.pointers[pointer].is_concrete() {
                self.insert(pointer, pointer, store);
            }
        }

        let mut tarjan = TarjanScc::default();

        self.queue.priorities.reserve(self.graph.node_count());

        let priorities = &mut self.queue.priorities;

        let mut priority = 0_u32;
        tarjan.run(&self.graph, |scc| {
            for n in scc {
                priorities.insert(*n, priority);
            }
            priority += 1;
        });

        debug_assert_eq!(self.queue.priorities.len(), self.graph.node_count());

        self.queue.extend(self.graph.nodes());
        let mut edges = Vec::new();

        let mut first = true;

        loop {
            self.flow_edges(store, &mut edges);
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
                            .map(|concrete_callees| concrete_callees.contains(&PointerId::UNKNOWN))
                            .unwrap_or(false);

                        if unknown_callee {
                            if let Some(concrete_values) = self.get(node) {
                                // Invalidate arguments passed to unknown callers.
                                for value in concrete_values {
                                    invalidated |= store.invalidate(value);
                                }
                            }
                        }
                    }

                    if store.invalid_pointers.contains(pointer) {
                        if let Some(values) = self.get(node) {
                            for value in values {
                                invalidated |= store.invalidate(value);
                            }
                        }
                        for incoming in self.graph.edges_directed(node.0, Incoming) {
                            if *incoming.2 == GraphEdge::Subset {
                                invalidated |= store.invalidate(incoming.0);
                            }
                        }
                    }

                    if let Pointer::Prop(obj, _) = store.pointers[pointer] {
                        let obj_invalid = store.invalid_pointers.contains(obj);
                        if obj_invalid {
                        invalidated |= store.invalidate(pointer);
                        let changed = self.insert(node, PointerId::UNKNOWN, store);
                        if changed {
                            self.prioritise(node);
                            self.queue.push(node.0);
                        }
                        continue;
                        }
                    }

                    if first {
                    // Functions implicitly return undefined sometimes.
                    if matches!(store.pointers[pointer], Pointer::ReturnValue(_)) {
                        let changed = self.insert(node, PointerId::NULL_OR_VOID, store);
                        if changed {
                            self.prioritise(node);
                            self.queue.push(node.0);
                        }
                        continue;
                    }

                    if self.points_to_nothing(node) {
                        // Undefined properties on valid objects are undefined. We know the
                        // obj is valid as props on invalid objects are checked above.
                        if matches!(store.pointers[pointer], Pointer::Prop(_, _)) {
                            let changed = self.insert(node, PointerId::NULL_OR_VOID, store);
                            if changed {
                                self.queue.push(node.0);
                            }
                            continue;
                        }
                        invalidated |= store.invalidate(pointer);
                        self.insert(node, PointerId::UNKNOWN, store);
                        self.prioritise(node);
                        self.queue.push(node.0);
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
        }
    }

    fn flow_edges(
        &mut self,
        store: &mut Store,
        edges: &mut Vec<(PointerId, PointerId, GraphEdge)>,
    ) {
        while let Some(node) = self.queue.pop() {
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
                            self.queue.push(dest.0);
                        }
                    }
                    GraphEdge::Return => {
                        let callees = match self.get(node) {
                            Some(c) => c.clone(),
                            None => continue,
                        };
                        let mut changed = false;
                        let mut dest = dest;
                        for callee in &callees {
                            if !store.is_callable_pointer(callee) && callee != PointerId::UNKNOWN {
                                continue;
                            }
                            let return_node = store.pointers.insert(Pointer::ReturnValue(callee));
                            let return_node = self.get_graph_node_id(return_node);
                            if return_node.0 == dest.0 {
                                continue;
                            }
                            if self.make_subset_of(return_node, dest) {
                                changed = true;
                                dest = RepId(self.nodes.find_mut(dest.0));
                            }
                        }
                        if changed {
                            self.queue.push(dest.0);
                        }
                    }
                    GraphEdge::Prop(name) => {
                        let concrete_objects = match self.get(node) {
                            Some(o) => o.clone(),
                            None => continue,
                        };

                        let mut changed = false;
                        let mut dest = dest;
                        for concrete_object in &concrete_objects {
                            if concrete_object == PointerId::UNKNOWN {
                                changed |= self.insert(dest.0, PointerId::UNKNOWN, store);
                                continue;
                            }
                            if concrete_object == PointerId::NULL_OR_VOID {
                                changed |= self.insert(dest.0, PointerId::NULL_OR_VOID, store);
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

                            self.prioritise(rep);

                            changed = true;
                        }

                        if changed {
                            self.queue.push(dest.0);
                        }
                    }
                    GraphEdge::Arg(index) => {
                        let concrete_callees = match self.get(node) {
                            Some(c) => c.clone(),
                            None => continue,
                        };

                        let mut dest = dest;
                        for concrete_callee in &concrete_callees {
                            match store.pointers[concrete_callee] {
                                Pointer::Fn(callee) => {
                                    let func = store.functions.get(&callee).unwrap();
                                    match func.param_indices().nth(index as usize) {
                                        Some(param) => {
                                            let param = store.pointers.insert(Pointer::Var(param));
                                            let param = self.get_graph_node_id(param);
                                            if self.make_subset_of(dest, param) {
                                                dest = RepId(self.nodes.find_mut(dest.0));
                                                let param = RepId(self.nodes.find_mut(param.0));
                                                self.queue.push(param.0);
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

    fn prioritise<T: GetRepId>(&mut self, pointer: T) {
        let node = pointer.get_rep_id(self).0;
        if !self.queue.priorities.contains_key(&node) {
            let priority = self
                .graph
                .neighbors_directed(node, Incoming)
                .map(|n| self.queue.priorities[&n])
                .min()
                .unwrap_or(u32::MAX);
            self.queue.priorities.insert(node, priority);
        }
    }

    fn get_graph_node_id(&mut self, pointer: PointerId) -> RepId {
        RepId(self.nodes.find_mut(pointer))
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

    fn get<T: GetRepId>(&mut self, pointer: T) -> Option<&SmallSet> {
        let representative = { pointer.get_rep_id(self).0 };
        self.points_to.get(&representative)
    }

    pub(super) fn get_immutable(&self, pointer: PointerId) -> Option<&SmallSet> {
        let representative = self.nodes.find(pointer);
        self.points_to.get(&representative)
    }

    pub(super) fn get_dot(&mut self, store: &Store) -> String {
        let print_graph = self.graph.clone();

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

        let print_graph: petgraph::prelude::DiGraph<String, GraphEdge> = print_graph
            .into_graph()
            .map(|_, n| map(&store, *n), |_, e| *e);
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

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum GraphEdge {
    Subset,
    Return,
    Prop(NameId),
    Arg(u32),
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

#[derive(Debug)]
struct UniqueQueue {
    inner: BinaryHeap<PrioritizedNode>,
    priorities: FxHashMap<PointerId, u32>,
}

impl UniqueQueue {
    fn new() -> Self {
        Self {
            inner: BinaryHeap::new(),
            priorities: FxHashMap::default(),
        }
    }

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
            .extend(iter.map(|n| PrioritizedNode(priorities[&n], n)))
    }
}

#[derive(Debug, Clone)]
pub(super) enum SmallSet {
    Inline(ArrayVec<PointerId, 2>),
    Heap(Rc<FxHashSet<PointerId>>),
}

impl Default for SmallSet {
    fn default() -> Self {
        Self::Inline(ArrayVec::default())
    }
}

impl SmallSet {
    fn insert(&mut self, value: PointerId) -> bool {
        match self {
            SmallSet::Inline(set) => {
                if !set.contains(&value) {
                    if set.len() == set.capacity() {
                        let mut heap = FxHashSet::default();
                        heap.extend(set.iter());
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
            SmallSet::Heap(set) => set.contains(value),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            SmallSet::Inline(set) => set.len(),
            SmallSet::Heap(set) => set.len(),
        }
    }

    pub fn iter(&self) -> SmallSetIter<'_> {
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

    fn extend(&mut self, mut other: SmallSet) {
        match (&self, &other) {
            (SmallSet::Inline(_), SmallSet::Heap(_)) => {
                other = std::mem::replace(self, other);
            }
            (SmallSet::Heap(a), SmallSet::Heap(b)) => {
                if Rc::ptr_eq(a, b) {
                    return;
                }

        if other.len() > self.len() {
            other = std::mem::replace(self, other);
                }
            }
            _ => {}
        }

        for value in &other {
            self.insert(value);
        }
    }

    fn extend_ref(&mut self, other: &SmallSet) {
        match self {
            SmallSet::Inline(_) => {
        for value in other {
                    self.insert(value);
                }
            }
            SmallSet::Heap(set) => {
                if let SmallSet::Heap(other) = other {
                    if Rc::ptr_eq(set, other) {
                        return;
                    }
                }
                Rc::make_mut(set).extend(other);
            }
        }
    }

    pub fn unknown_set() -> Self {
        let mut s = ArrayVec::new();
        s.push(PointerId::UNKNOWN);
        Self::Inline(s)
    }
}

impl<'a> IntoIterator for &'a SmallSet {
    type IntoIter = SmallSetIter<'a>;
    type Item = PointerId;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Clone)]
pub(super) enum SmallSetIter<'a> {
    Slice(std::slice::Iter<'a, PointerId>),
    Set(std::collections::hash_set::Iter<'a, PointerId>),
}

impl<'a> Iterator for SmallSetIter<'a> {
    type Item = PointerId;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SmallSetIter::Slice(iter) => iter.next().copied(),
            SmallSetIter::Set(iter) => iter.next().copied(),
        }
    }
}
