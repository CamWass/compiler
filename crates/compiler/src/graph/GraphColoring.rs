use index::{newtype_index, vec::IndexVec};
use rustc_hash::FxHashMap;
use std::cmp::Ordering;
use std::hash::Hash;

/// Annotates the graph with colors in a way that no connected node will have the
/// same color. Nodes of the same color can then be partitioned together and be
/// represented by a super node.
pub struct GreedyGraphColoring<T>
where
    T: Clone + Eq + Hash,
{
    color_map: FxHashMap<T, GraphColor>,
    partitions: IndexVec<GraphColor, Partition<T>>,
}

impl<T> GreedyGraphColoring<T>
where
    T: Clone + Eq + Hash,
{
    pub fn new() -> Self {
        Self {
            color_map: Default::default(),
            partitions: Default::default(),
        }
    }

    /// Using the coloring as partitions, finds the node that represents that
    /// partition as the super node. The first to retrieve its partition will
    /// become the super node.
    pub fn get_partition_super_node(&mut self, node: &T) -> &T {
        let color = self.color_map[node];
        self.partitions[color]
            .super_node
            .get_or_insert_with(|| node.clone())
    }

    /// Returns how many nodes this nodes's color is associated with.
    pub fn color_count(&self, node: &T) -> usize {
        let color = self.color_map[node];
        self.partitions[color].count
    }

    /// `tieBreaker`: In case of a tie between two nodes of the same degree,
    /// this comparator will determine which node should be colored first.
    ///
    /// Returns The number of unique colors need.
    pub fn color<C, W, S, F>(
        &mut self,
        mut nodes: Vec<T>,
        mut tie_breaker: C,
        mut weight: W,
        mut make_subgraph: F,
    ) -> usize
    where
        C: FnMut(&T, &T) -> Ordering,
        W: FnMut(&T) -> usize,

        S: SubGraph<T>,
        F: FnMut() -> S,
    {
        debug_assert!(self.color_map.is_empty());
        debug_assert!(self.partitions.is_empty());

        self.color_map.reserve(nodes.len());

        // Sort nodes by degree.
        nodes.sort_unstable_by(|a, b| {
            let result = weight(b).cmp(&weight(a));
            if result.is_eq() {
                tie_breaker(a, b)
            } else {
                result
            }
        });

        // Idea: From the highest to lowest degree, assign any uncolored node with
        // a unique color if none of its neighbors has been assigned that color.
        let mut count = 0;
        loop {
            let mut subgraph = make_subgraph();
            nodes.retain(|node| {
                if subgraph.is_independent_of(node) {
                    subgraph.add_node(node.clone());
                    let color = GraphColor::from_usize(count);
                    self.color_map.insert(node.clone(), color);
                    if let Some(p) = self.partitions.get_mut(color) {
                        p.count += 1;
                    } else {
                        let p = Partition {
                            super_node: None,
                            count: 1,
                        };
                        let idx = self.partitions.push(p);
                        debug_assert!(idx == color);
                    }
                    false
                } else {
                    true
                }
            });
            count += 1;

            if nodes.is_empty() {
                break;
            }
        }
        count
    }
}

pub trait SubGraph<N> {
    /// Returns true if the node is a neighbor of any node in this SubGraph.
    fn is_independent_of(&self, node: &N) -> bool;

    /// Adds the node into this subgraph.
    fn add_node(&mut self, value: N);
}

struct Partition<T> {
    super_node: Option<T>,
    /// The number of nodes in this partition.
    count: usize,
}

newtype_index! {
    pub struct GraphColor {
        DEBUG_FORMAT = "GraphColor({})"
    }
}
