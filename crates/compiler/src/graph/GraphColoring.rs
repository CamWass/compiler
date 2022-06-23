use rustc_hash::FxHashMap;
use std::cmp::Ordering;
use std::hash::Hash;

// TODO: comment
// /**
//  * Annotates the graph with a color in a way that no connected node will have
//  * the same color. Nodes of the same color can then be partitioned together and
//  * be represented by a super node. This class will merely annotate the nodes
//  * with a color using {@link GraphNode#setAnnotation(Annotation)} and provide
//  * a node to super node mapping with {@link #getPartitionSuperNode(Object)}. The
//  * given graph itself will not be modified.
//  *
//  * <p>This algorithm is <b>NOT</b> deterministic by default. Passes that
//  * requires deterministic output should provide a {@code Comparator} in the
//  * constructor as a tie-breaker. This tie-break will be used when deciding
//  * which node should be colored first when multiple nodes have the same degree.
//  *
//  * @param <N> Value type that the graph node stores.
//  * @param <E> Value type that the graph edge stores.
//  */
/**
 * Greedily assign nodes with high degree unique colors.
 */
pub struct GreedyGraphColoring<T>
where
    T: Copy + Eq + Hash,
{
    pub color_map: FxHashMap<T, usize>,
}

impl<T> GreedyGraphColoring<T>
where
    T: Copy + Eq + Hash,
{
    pub fn new() -> Self {
        Self {
            color_map: Default::default(),
        }
    }
    /**
     * @param tieBreaker In case of a tie between two nodes of the same degree,
     *     this comparator will determine which node should be colored first.
     *
     * returns The number of unique colors need.
     */
    pub fn color<C, W, S, F>(
        &mut self,
        mut nodes: Vec<T>,
        mut tieBreaker: C,
        mut weight: W,
        mut make_subgraph: F,
    ) -> usize
    where
        C: FnMut(T, T) -> Ordering,
        W: FnMut(T) -> usize,

        S: SubGraph<T>,
        F: FnMut() -> S,
    {
        // Sort nodes by degree.
        nodes.sort_unstable_by(|&a, &b| {
            let result = weight(b).cmp(&weight(a));
            if result.is_eq() {
                tieBreaker(a, b)
            } else {
                result
            }
        });

        // Idea: From the highest to lowest degree, assign any uncolored node with
        // a unique color if none of its neighbors has been assigned that color.
        let mut count = 0;
        loop {
            let mut subgraph = make_subgraph();
            nodes.retain(|&node| {
                if subgraph.isIndependentOf(node) {
                    subgraph.addNode(node);
                    self.color_map.insert(node, count);
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
    /** Returns true if the node is a neighbor of any node in this SubGraph. */
    fn isIndependentOf(&mut self, node: N) -> bool;

    /** Adds the node into this subgraph. */
    fn addNode(&mut self, value: N);
}
