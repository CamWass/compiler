use std::collections::VecDeque;
use std::fmt::Debug;
use std::hash::{BuildHasherDefault, Hash};
use std::num::NonZeroUsize;

use petgraph::graphmap::{GraphMap, NodeTrait};
use petgraph::visit::NodeIndexable;
use petgraph::Directed;
use petgraph::Direction::{Outgoing, Incoming};
use rustc_hash::{FxHashSet, FxHasher};

pub trait Visitor<N>
where
    N: Hash + Eq,
{
    /// Visit a node, adding its dependencies to `dependencies` and returning
    /// `true` if the node's state changed.
    fn visit_node(&mut self, node: N, dependencies: &mut FxHashSet<N>) -> bool;

    fn finish_node(&mut self, node: N);
}

pub fn process<N, V>(root: N, visitor: &mut V)
where
    N: Copy + Ord + Hash + Debug + Eq,
    V: Visitor<N>,
{
    let mut graph: GraphMap<N, (), Directed, BuildHasherDefault<FxHasher>> = GraphMap::default();
    graph.add_node(root);

    let mut fringe = vec![root];

    let mut fringe_candidates = FxHashSet::default();

    let mut scc_state = TarjanScc::new();

    let mut updated_deps = FxHashSet::default();

    let mut queue: VecDeque<N> = VecDeque::default();
    let mut queue_set: FxHashSet<N> = FxHashSet::default();

    let mut old_dependencies = Vec::new();

    enum SCCResult<N> {
        Finish,
        Update(N),
    }

    let mut finished = FxHashSet::default();

    let mut i = 1;

    loop {
        fringe_candidates.clear();

        if i % 1000 == 0 {
            // println!("On iteration {} for node {:?}", i, root);
        }
        i += 1;

        debug_assert!(fringe
            .iter()
            .all(|n| graph.neighbors_directed(*n, Outgoing).next().is_none()));

        if !fringe.is_empty() {
            // Fast(er) path; pull from pre-computed fringe.

            for &node in &fringe {
                updated_deps.clear();
                let node_changed = visitor.visit_node(node, &mut updated_deps);

                debug_assert!(!updated_deps.contains(&node));

                let deps_changed = !updated_deps.is_empty();

                for &new in &updated_deps {
                    debug_assert!(!graph.contains_edge(node, new));

                    if finished.contains(&new) {
                        continue;
                    }

                    if !graph.contains_node(new) {
                        fringe_candidates.insert(new);
                    }

                    graph.add_edge(node, new, ());
                }

                if !node_changed && !deps_changed {
                    visitor.finish_node(node);
                    finished.insert(node);
                    if node == root {
                        return;
                    }
                    debug_assert!(graph.neighbors_directed(node, Outgoing).next().is_none());

                    fringe_candidates.extend(graph.neighbors_directed(node, Incoming));

                    graph.remove_node(node);
                } else if updated_deps.is_empty() {
                    fringe_candidates.insert(node);
                }
            }
            fringe.clear();
        } else {
            // Slow(er) path; use Tarjan to find a batch of SCCs we can process in parallel.

            tarjan_scc(&graph, &mut scc_state, root);

            if scc_state.starts.is_empty() {
                unreachable!("root should be present and form SCC");
            }

            for &(start, end) in &scc_state.starts {
                let scc = &scc_state.components[start..end];
                // TODO: check for single node SCC?

                queue.clear();
                queue_set.clear();

                queue.extend(scc);
                queue_set.extend(scc);

                updated_deps.clear();

                let mut scc_invalidated = false;

                let mut node;
                loop {
                    node = queue.pop_front();
                    let node = match node {
                        Some(n) => n,
                        None => break,
                    };

                    queue_set.remove(&node);
                    updated_deps.clear();
                    let node_changed = visitor.visit_node(node, &mut updated_deps);

                    debug_assert!(!updated_deps.contains(&node));

                    let mut existing_dependency_count = 0;
                    let mut deps_changed = false;

                    for dep in graph.neighbors_directed(node, Outgoing) {
                        existing_dependency_count += 1;
                        if !updated_deps.contains(&dep) {
                            deps_changed = true;
                            break;
                        }
                    }

                    if existing_dependency_count != updated_deps.len() {
                        deps_changed = true;
                    }

                    if deps_changed {
                        scc_invalidated = true;
                        break;
                    }

                    if node_changed {
                        for dependant in graph.neighbors_directed(node, Incoming) {
                            if queue_set.insert(dependant) {
                                queue.push_back(dependant);
                            }
                        }
                    }
                }

                let result = if scc_invalidated {
                    SCCResult::Update(node.unwrap())
                } else {
                    SCCResult::Finish
                };

                match result {
                    SCCResult::Update(node) => {
                        debug_assert!(!updated_deps.contains(&node));

                        let has_dependencies = !updated_deps.is_empty();

                        old_dependencies.clear();

                        old_dependencies.extend(
                            graph
                                .neighbors_directed(node, Outgoing)
                                .filter(|n| !updated_deps.remove(n)),
                        );
                        for &old in &old_dependencies {
                            debug_assert!(graph.contains_edge(node, old));
                            graph.remove_edge(node, old);
                        }

                        for &new in &updated_deps {
                            debug_assert!(!graph.contains_edge(node, new));
                            if finished.contains(&new) {
                                continue;
                            }
                            if !graph.contains_node(new) {
                                fringe_candidates.insert(new);
                            }
                            graph.add_edge(node, new, ());
                        }

                        if !has_dependencies {
                            fringe_candidates.insert(node);
                        }
                    }
                    SCCResult::Finish => {
                        let scc = &scc_state.components[start..end];

                        let contains_root = scc.contains(&root);

                        debug_assert!(scc.iter().all(|n| graph
                            .neighbors_directed(*n, Outgoing)
                            .all(|n| scc.contains(&n))));

                        for &node in scc {
                            visitor.finish_node(node);

                            if contains_root {
                                continue;
                            }

                            for external_dependant in graph
                                .neighbors_directed(node, Incoming)
                                .filter(|n| !scc_state.yielded.contains(n))
                            {
                                fringe_candidates.insert(external_dependant);
                            }

                            finished.insert(node);

                            graph.remove_node(node);
                        }

                        if contains_root {
                            return;
                        }
                    }
                }
            }
        }

        debug_assert!(fringe.is_empty());

        for &candidate in &fringe_candidates {
            let mut outgoing = graph.neighbors_directed(candidate, Outgoing);
            let mut incoming = graph.neighbors_directed(candidate, Incoming);
            if outgoing.next().is_none() && incoming.next().is_some() {
                fringe.push(candidate);
            }
        }

        debug_assert!(fringe.iter().collect::<FxHashSet<_>>().len() == fringe.len());
    }
}

#[derive(Copy, Clone, Debug)]
struct NodeData {
    rootindex: Option<NonZeroUsize>,
}

// The following is taken from petgraph and modified to:
//  - start the traversal from a root nodes, and only emit SCCs reachable from
//    that root.
//  - only emit SCCs that can be processed in parallel (no interdependencies).

/// A reusable state for computing the strongly connected components Tarjan's algorithm.
#[derive(Debug)]
struct TarjanScc<N> {
    // From petgraph:
    index: usize,
    componentcount: usize,
    nodes: Vec<NodeData>,
    stack: Vec<N>,

    /// Nodes that have been emitted as a part of an SCC.
    yielded: FxHashSet<N>,
    /// Nodes of emitted all SCCs. SCCs are returned as regions of this buffer.
    components: Vec<N>,
    /// (start, end)
    starts: Vec<(usize, usize)>,
}

impl<N> TarjanScc<N>
where
    N: NodeTrait,
{
    fn new() -> Self {
        TarjanScc {
            index: 1,                   // Invariant: index < componentcount at all times.
            componentcount: usize::MAX, // Will hold if componentcount is initialized to number of nodes - 1 or higher.
            nodes: Vec::new(),
            stack: Vec::new(),

            yielded: FxHashSet::default(),
            components: Vec::new(),
            starts: Vec::new(),
        }
    }

    /// Resets the state, allowing it to used for a fresh traversal.
    fn clear(&mut self) {
        self.index = 1;
        self.componentcount = usize::MAX;
        self.nodes.clear();
        self.stack.clear();

        self.yielded.clear();
        self.components.clear();
        self.starts.clear();
    }

    fn run(&mut self, g: &GraphMap<N, (), Directed, BuildHasherDefault<FxHasher>>, start: N) {
        self.nodes.clear();
        self.nodes
            .resize(g.node_bound(), NodeData { rootindex: None });

        self.visit(start, g);
    }

    fn visit(&mut self, v: N, g: &GraphMap<N, (), Directed, BuildHasherDefault<FxHasher>>) -> bool {
        macro_rules! node {
            ($node:expr) => {
                self.nodes[g.to_index($node)]
            };
        }

        let node_v = &mut node![v];
        debug_assert!(node_v.rootindex.is_none());

        let mut v_is_local_root = true;
        let v_index = self.index;
        node_v.rootindex = NonZeroUsize::new(v_index);
        self.index += 1;

        for w in g.neighbors(v) {
            if node![w].rootindex.is_none() && self.visit(w, g) {
                // propagate abort
                return true;
            }
            if node![w].rootindex < node![v].rootindex {
                node![v].rootindex = node![w].rootindex;
                v_is_local_root = false;
            }
        }

        if v_is_local_root {
            // Pop the stack and generate an SCC.
            let mut indexadjustment = 1;
            let c = NonZeroUsize::new(self.componentcount);
            let nodes = &mut self.nodes;
            let start = self
                .stack
                .iter()
                .rposition(|&w| {
                    if nodes[g.to_index(v)].rootindex > nodes[g.to_index(w)].rootindex {
                        true
                    } else {
                        nodes[g.to_index(w)].rootindex = c;
                        indexadjustment += 1;
                        false
                    }
                })
                .map(|x| x + 1)
                .unwrap_or_default();
            nodes[g.to_index(v)].rootindex = c;

            self.stack.push(v); // Pushing the component root to the back right before getting rid of it is somewhat ugly, but it lets it be included in f.

            // This SCC should not be depended on by any previous ones.
            debug_assert!(self.stack[start..].iter().all(|n| g
                .neighbors_directed(*n, Incoming)
                .all(|n| !self.yielded.contains(&n))));

            if self.stack[start..].iter().any(|n| {
                g.neighbors_directed(*n, Outgoing)
                    .any(|n| self.yielded.contains(&n))
            }) {
                // This SCC depends on a previous one; stop the search.
                return true;
            }

            self.starts.push((
                self.components.len(),
                self.components.len() + self.stack[start..].len(),
            ));
            self.components.extend_from_slice(&self.stack[start..]);
            for n in &self.stack[start..] {
                self.yielded.insert(*n);
            }
            debug_assert_eq!(
                self.components.iter().position(|n| *n == self.stack[start]),
                self.starts.last().map(|(s, _)| *s)
            );

            self.stack.truncate(start);
            self.index -= indexadjustment; // Backtrack index back to where it was before we ever encountered the component.
            self.componentcount -= 1;
        } else {
            self.stack.push(v); // Stack is filled up when backtracking, unlike in Tarjans original algorithm.
        }
        false
    }
}

fn tarjan_scc<N>(
    g: &GraphMap<N, (), Directed, BuildHasherDefault<FxHasher>>,
    state: &mut TarjanScc<N>,
    start: N,
) where
    N: NodeTrait,
{
    debug_assert!(g.contains_node(start));
    state.clear();

    state.run(g, start);
}
