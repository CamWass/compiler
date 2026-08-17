use index::{newtype_index, vec::IndexVec};
use rustc_hash::FxHashMap;
use std::cmp::Ordering;
use std::hash::Hash;

/// Assign a 'colour' to each graph node in a way that no connected nodes will
/// have the same colour. Nodes of the same colour can then be partitioned
/// together and be represented by a super node.
pub struct GraphColouring<T>
where
    T: Clone + Eq + Hash,
{
    colour_map: FxHashMap<T, GraphColour>,
    partitions: IndexVec<GraphColour, Partition<T>>,
}

impl<T> GraphColouring<T>
where
    T: Clone + Eq + Hash,
{
    pub fn new() -> Self {
        Self {
            colour_map: Default::default(),
            partitions: Default::default(),
        }
    }

    /// Using the colouring as partitions, finds the node that represents that
    /// partition as the super node. The first to retrieve its partition will
    /// become the super node.
    pub fn get_partition_super_node(&mut self, node: &T) -> &T {
        let colour = self.colour_map[node];
        self.partitions[colour]
            .super_node
            .get_or_insert_with(|| node.clone())
    }

    /// Returns how many nodes this nodes's colour is associated with.
    pub fn colour_count(&self, node: &T) -> usize {
        let colour = self.colour_map[node];
        self.partitions[colour].count
    }

    /// `tieBreaker`: In case of a tie between two nodes of the same degree,
    /// this comparator will determine which node should be coloured first.
    ///
    /// Returns the number of unique colours need.
    pub fn colour<C, W, S, F>(
        &mut self,
        mut nodes: Vec<T>,
        tie_breaker: C,
        weight: W,
        make_subgraph: F,
    ) -> usize
    where
        C: Fn(&T, &T) -> Ordering,
        W: Fn(&T) -> usize,
        S: SubGraph<T>,
        F: Fn() -> S,
    {
        debug_assert!(self.colour_map.is_empty());
        debug_assert!(self.partitions.is_empty());

        self.colour_map.reserve(nodes.len());

        // Sort nodes by degree.
        nodes.sort_unstable_by(|a, b| {
            let result = weight(b).cmp(&weight(a));
            if result.is_eq() {
                tie_breaker(a, b)
            } else {
                result
            }
        });

        // Idea: From the highest to lowest degree, assign any uncoloured node
        // with a unique colour if none of its neighbours has been assigned that
        // colour.
        let mut count = 0;
        loop {
            let mut subgraph = make_subgraph();
            nodes.retain(|node| {
                if subgraph.is_independent_of(node) {
                    subgraph.add_node(node.clone());
                    let colour = GraphColour::from_usize(count);
                    self.colour_map.insert(node.clone(), colour);
                    if let Some(p) = self.partitions.get_mut(colour) {
                        p.count += 1;
                    } else {
                        let p = Partition {
                            super_node: None,
                            count: 1,
                        };
                        let idx = self.partitions.push(p);
                        debug_assert!(idx == colour);
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
    /// Returns true if the node is a neighbour of any node in this SubGraph.
    fn is_independent_of(&self, node: &N) -> bool;

    /// Adds the node into this subgraph.
    fn add_node(&mut self, value: N);
}

struct Partition<T> {
    super_node: Option<T>,
    /// The number of nodes in this partition.
    count: usize,
}

newtype_index!(pub GraphColour);
