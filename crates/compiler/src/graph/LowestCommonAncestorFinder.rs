use std::collections::VecDeque;

use ahash::{AHashMap, AHashSet};
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use std::hash::Hash;

/**
 * Implements a lowest common ancestor search algorithm.
 *
 * <p>The LCA of a set of nodes is the node that is an ancestor to all of them but is not an
 * ancestor of any other common ancestor. In a non-tree, There may be multiple LCAs for a given set
 * of search nodes.
 *
 * <p>In a cyclic graph, the LCAs may not be well defined. Within a cycle, all elements are both
 * above and below one another, so there is no uniquely lowest element. If the set of common
 * ancestors is rooted on a cycle, this implementation returns one or more elements of that cycle.
 * Those elements are chosen arbitrarily but deterministically (as long as the underlying graph has
 * deterministic iteration).
 */
pub struct LowestCommonAncestorFinder {
    searchColoring: AHashMap<NodeIndex, Color>,
    searchQueue: VecDeque<NodeIndex>,
}

impl LowestCommonAncestorFinder {
    pub fn new() -> Self {
        Self {
            searchColoring: AHashMap::default(),
            searchQueue: VecDeque::new(),
        }
    }

    /**
     * Execute a search on all the elements of {@code roots}.
     *
     * <p>This is a general-purpose, bare-bones implementation. There are lots of special case
     * optimizations that could be applied.
     */
    // pub fn findAll(&mut self, roots: Vec<N>) -> impl Iterator<Item = N> {
    pub fn findAll<N, E>(&mut self, graph: &DiGraph<N, E>, roots: &[NodeIndex]) -> AHashSet<N>
    where
        N: Eq + Hash + Copy,
    {
        // We reserved the MSB of each Color for bookkeeping.
        debug_assert!(
            roots.len() <= std::mem::size_of::<isize>() - 1,
            "Too many roots."
        );
        debug_assert!(self.searchColoring.is_empty());

        // In two's-complement, (2^n - 1) sets the lowest n bits high.
        let allColor = Color::create((1 << roots.len()) - 1);

        /**
         * Paint up from each root using the color associated with that root.
         *
         * <p>When done, the set of common ancestors is the set of nodes painted `allColor`.
         */
        let mut bitForRoot = 1;
        for &root in roots {
            let color = Color::create(bitForRoot);

            // Preserve any existing colors.
            self.searchColoring
                .entry(root)
                .and_modify(|c| *c = c.mix(color))
                .or_insert(color);
            self.paintAncestors(graph, root, color);
            bitForRoot <<= 1;
        }

        /**
         * For every common ancestor, paint all of its ancestors with a color indicating it is not the
         * lowest.
         */
        // TODO: bad collect
        for (node, color) in self
            .searchColoring
            .iter()
            .map(|(&n, &c)| (n, c))
            .collect::<Vec<_>>()
        {
            if color == allColor {
                self.paintAncestors(graph, node, NOT_LOWEST);
            }
        }

        let mut results = AHashSet::default();
        for (&node, &color) in &self.searchColoring {
            if color == allColor {
                results.insert(graph[node]);
            }
        }

        self.searchColoring.clear();
        self.searchQueue.clear();
        results
    }

    /**
     * Paint all nodes above {@code root} with {@code color}.
     *
     * <p>{@code root} itself will not have its color changed. {@code color} will be mixed with all
     * existing colors on ancestor nodes.
     */
    fn paintAncestors<N, E>(&mut self, graph: &DiGraph<N, E>, root: NodeIndex, color: Color) {
        debug_assert!(self.searchQueue.is_empty());

        self.searchQueue.push_back(root);

        while !self.searchQueue.is_empty() {
            let curr = self.searchQueue.pop_front().unwrap();

            for parentEdge in graph.edges_directed(curr, Direction::Incoming) {
                let parent = parentEdge.source();
                if parent == root {
                    // Don't paint `root`.
                    continue;
                }

                let oldColor = self.searchColoring.get(&parent).copied().unwrap_or(BLANK);
                if !oldColor.contains(color) {
                    // Only explore in directions that have not yet been painted.
                    self.searchQueue.push_back(parent);
                    self.searchColoring.insert(parent, oldColor.mix(color));
                }
            }
        }
    }
}

// /**
//  * A color that when mixed with any other color {@code x} returns {@code x}.
//  *
//  * <p>Mixing this color is idempotent.
//  */
const BLANK: Color = Color { bitset: 0 };

/**
 * A color for common ancestors that indicates that there are lower common ancestors.
 *
 * <p>Because this color sets its MSB high, it can never equal any other color. Also notice that
 * mixing this {@link Color} with any other produces this {@link Color} again; mixing it is
 * nullipotent.
 */
const NOT_LOWEST: Color = Color { bitset: -1 };

/** A "color" for a node, encoded as a combination of other colors using a one-hot scheme. */
#[derive(Clone, Copy, PartialEq)]
struct Color {
    // TODO: size
    bitset: isize,
}

impl Color {
    fn create(bitset: isize) -> Color {
        if bitset < 0 {
            assert!(bitset == -1);
            NOT_LOWEST
        } else {
            Color { bitset }
        }
    }

    fn mix(&self, other: Color) -> Color {
        if self.bitset == other.bitset {
            return *self;
        }
        Color::create(self.bitset | other.bitset)
    }

    pub fn contains(&self, other: Color) -> bool {
        (self.bitset & other.bitset) == other.bitset
    }
}
