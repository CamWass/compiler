use index::{newtype_index, vec::IndexVec};
use petgraph::matrix_graph::UnMatrix;
use petgraph::visit::IntoNodeIdentifiers;
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
    pub fn new<C>(graph: UnMatrix<T, ()>, tie_breaker: C) -> Self
    where
        C: Fn(&T, &T) -> Ordering,
    {
        let mut colouring = Self {
            colour_map: Default::default(),
            partitions: Default::default(),
        };

        let mut nodes: Vec<_> = graph.node_identifiers().collect();

        colouring.colour_map.reserve(nodes.len());

        // Sort nodes by degree.
        nodes.sort_unstable_by(|&a, &b| {
            let a_degree = graph.neighbors(a).count();
            let b_degree = graph.neighbors(b).count();
            let result = b_degree.cmp(&a_degree);
            if result.is_eq() {
                tie_breaker(&graph[a], &graph[b])
            } else {
                result
            }
        });

        // Idea: From the highest to lowest degree, assign any uncoloured node
        // with a unique colour if none of its neighbours has been assigned that
        // colour.
        let mut count = 0;
        let mut subgraph = Vec::new();
        loop {
            nodes.retain(|&node| {
                let node_is_independent_from_subgraph =
                    !subgraph.iter().any(|n| graph.has_edge(*n, node));

                if node_is_independent_from_subgraph {
                    subgraph.push(node);
                    let colour = GraphColour::from_usize(count);
                    colouring.colour_map.insert(graph[node].clone(), colour);
                    if let Some(p) = colouring.partitions.get_mut(colour) {
                        p.count += 1;
                    } else {
                        let p = Partition {
                            super_node: None,
                            count: 1,
                        };
                        let idx = colouring.partitions.push(p);
                        debug_assert!(idx == colour);
                    }
                    false
                } else {
                    true
                }
            });
            count += 1;
            subgraph.clear();

            if nodes.is_empty() {
                break;
            }
        }
        colouring
    }
}

struct Partition<T> {
    super_node: Option<T>,
    /// The number of nodes in this partition.
    count: usize,
}

newtype_index!(pub GraphColour);
