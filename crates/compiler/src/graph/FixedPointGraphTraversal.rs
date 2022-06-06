use ahash::AHashSet;
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
use std::{
    cmp::{max, min},
    iter::FromIterator,
    marker::PhantomData,
};

// We cube the number of nodes to estimate the largest number of iterations we should allow before
// deciding that we aren't reaching a fixed point.
// We have to make sure that we don't hit integer overflow when calculating this value.
// We also don't want to wait longer than a full minute for any fixed point calculation.
// We'll be generous and assume each iteration takes only a nanosecond.
// That's 60*10^9 iterations, so we must cap the node count we use for calculation at the
// cube root of that number.
// 3914 = 60e9_f64.cbrt().floor()
// TODO: replace with calculation when float arithmetic is const:
// https://github.com/rust-lang/rust/issues/57241
// const MAX_NODE_COUNT_FOR_ITERATION_LIMIT: usize = 60e9_f64.cbrt().floor() as usize;
const MAX_NODE_COUNT_FOR_ITERATION_LIMIT: usize = 3914;

pub struct FixedPointGraphTraversal<N, E, C>
where
    C: EdgeCallback<N, E>,
{
    callback: C,
    traversalDirection: TraversalDirection,
    _n: PhantomData<N>,
    _e: PhantomData<E>,
}

impl<N, E, C> FixedPointGraphTraversal<N, E, C>
where
    C: EdgeCallback<N, E>,
{
    /**
     * Create a new traversal.
     *
     * @param callback A callback for updating the state of the graph each time an edge is traversed.
     */
    fn new(callback: C, traversalDirection: TraversalDirection) -> Self {
        Self {
            callback,
            traversalDirection,
            _n: PhantomData,
            _e: PhantomData,
        }
    }

    /** Helper method for creating new traversals that traverse from parent to child. */
    pub fn newTraversal(callback: C) -> Self {
        FixedPointGraphTraversal::new(callback, TraversalDirection::OUTWARDS)
    }

    /**
     * Compute a fixed point for the given graph.
     *
     * @param graph The graph to traverse.
     */
    pub fn computeFixedPoint(&mut self, graph: &DiGraph<N, E>) {
        let mut workSet = AHashSet::from_iter(graph.node_indices());
        let mut cycleCount: usize = 0;
        let nodeCount = min(graph.node_count(), MAX_NODE_COUNT_FOR_ITERATION_LIMIT);

        // Choose a bail-out heuristically in case the computation
        // doesn't converge.
        let maxIterations = max(nodeCount * nodeCount * nodeCount, 100);

        while !workSet.is_empty() && cycleCount < maxIterations {
            self.visitNode(*workSet.iter().next().unwrap(), &mut workSet, graph);
            cycleCount += 1;
        }

        debug_assert!(
            cycleCount != maxIterations,
            "Fixed point computation not halting"
        );
    }

    fn visitNode(
        &mut self,
        node: NodeIndex,
        workSet: &mut AHashSet<NodeIndex>,
        graph: &DiGraph<N, E>,
    ) {
        // For every out edge in the workSet, traverse that edge. If that
        // edge updates the state of the graph, then add the destination
        // node to the resultSet, so that we can update all of its out edges
        // on the next iteration.
        workSet.remove(&node);
        match self.traversalDirection {
            TraversalDirection::OUTWARDS => {
                let sourceValue = &graph[node];
                for edge in graph.edges(node) {
                    let dest = edge.target();
                    if self
                        .callback
                        .traverseEdge(sourceValue, edge.weight(), &graph[dest])
                    {
                        workSet.insert(dest);
                    }
                }
            }
            TraversalDirection::INWARDS => {
                todo!();
                // let revSourceValue = graph[node];
                // for edge in node.getInEdges() {
                //     let revDestValue = edge.getSource().getValue();
                //     if self
                //         .callback
                //         .traverseEdge(revSourceValue, edge.getValue(), revDestValue)
                //     {
                //         workSet.insert(edge.getSource());
                //     }
                // }
            }
        }
    }
}

/** Edge callback */
pub trait EdgeCallback<Node, Edge> {
    /**
     * Update the state of the destination node when the given edge is traversed.
     *
     * <p>Recall that depending on the direction of the traversal, {@code source} and {@code
     * destination} may be swapped compared to the orientation of the edge in the graph. In either
     * case, only the {@code destination} parameter may be mutated.
     *
     * @param source The start node.
     * @param e The edge.
     * @param destination The end node.
     * @return Whether the state of the destination node changed.
     */
    fn traverseEdge(&mut self, source: &Node, e: &Edge, destination: &Node) -> bool;
}

enum TraversalDirection {
    INWARDS,  // from a node to its incoming edges
    OUTWARDS, // from a node to its outgoing edges
}
