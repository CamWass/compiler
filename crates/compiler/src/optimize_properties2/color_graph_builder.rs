use crate::graph::LowestCommonAncestorFinder::LowestCommonAncestorFinder;

use super::{
    color::ColorId, color_graph_node::ColorGraphNodeId,
    color_graph_node_factory::ColorGraphNodeFactory,
};
use petgraph::graph::{DiGraph, NodeIndex};
use rustc_hash::FxHashMap;

/** Builds a graph of the {@link Color}s on the AST from a specified set of seed colors. */
pub struct ColorGraphBuilder<'col, 'nf> {
    node_factory: &'nf mut ColorGraphNodeFactory<'col>,
    // lcaFinder: LowestCommonAncestorFinder<ColorGraphNode, EdgeReason>,
    top_node: NodeIndex,

    /**
     * The graph of colors as defined by `holdsInstanceOf`.
     *
     * <p>We use `holdsInstanceOf` rather than `isSupertypeOf` because we only actually care about
     * edges that were "used" in the program. If instances never flow over an edge at runtime, then
     * properties also don't need to be tracked across that edge either. Of course, since we don't
     * track the types of all assignments in a program, many of the edges are `isSupertypeOf` edges
     * that we include to be conservative.
     *
     * <p>This graph, when fully constructed, is still only an approximation. This is the due to both
     * memory and time constraints. The following quirks are expected:
     *
     * <ul>
     *   <li>Some colors, such as primitives, will not have nodes.
     *   <li>Transitive edges (shortcut edges for which there exist alternate paths) are kept minimal.
     * </ul>
     *
     * <p>In practice, this could be declared as taking an {@link EdgeReason} instead of an Object,
     * but Object is used to indicate that EdgeReasons are only meant for debugging and not any actual
     * logic in (dis)ambiguation.
     */
    color_holds_instance_graph: DiGraph<ColorGraphNodeId, ()>,

    color_node_to_graph_node: FxHashMap<ColorGraphNodeId, NodeIndex>,
}

impl<'col, 'nf> ColorGraphBuilder<'col, 'nf> {
    pub fn new(node_factory: &'nf mut ColorGraphNodeFactory<'col>) -> Self {
        let mut color_holds_instance_graph = DiGraph::new();
        let top_color_node = node_factory.create_node(Some(node_factory.colours.unknown_color));
        let top_node = color_holds_instance_graph.add_node(top_color_node);
        Self {
            node_factory,
            // lcaFinder: LowestCommonAncestorFinder::new(colorHoldsInstanceGraph),
            top_node,
            color_holds_instance_graph,
            color_node_to_graph_node: FxHashMap::default(),
        }
    }

    pub fn add_all(&mut self, flats: &[ColorGraphNodeId]) {
        for &flat in flats {
            self.add_color_node(flat);
        }
    }

    pub fn build(mut self) -> DiGraph<ColorGraphNodeId, ()> {
        let mut lca_finder = LowestCommonAncestorFinder::new();
        for node in self.color_holds_instance_graph.node_indices() {
            self.connect_union_with_ancestors(node, &mut lca_finder);
        }
        self.color_holds_instance_graph
    }

    /**
     * During initial lattice construction unions were only given outbound edges. Here we add any
     * necessary inbound ones.
     *
     * <p>We defer this operation because adding union-to-union and common-supertype-to-union edges,
     * is a hard problem. Solving it after all other colors are in place makes it easier.
     */
    fn connect_union_with_ancestors(
        &mut self,
        union_node: NodeIndex,
        lca_finder: &mut LowestCommonAncestorFinder,
    ) {
        let flat_union = self.color_holds_instance_graph[union_node];
        let color = self.node_factory[flat_union].color;
        if !self.node_factory.colours[color].is_union() {
            return;
        }

        /*
         * Connect the LCAs to the union.
         *
         * <p>The union itself will be found in most cases, but since we don't add self-edges, that
         * won't matter.
         *
         * <p>Some of these edges may pollute the "lattice-ness" of the graph, but all the invariants we
         * actually care about will be maintained. If disambiguation is too slow and stricter invariants
         * would help, we could be more careful.
         */
        debug_assert!(self.color_holds_instance_graph.edges(union_node).count() != 0);
        // TODO:: bad collect
        let graph_nodes = self.node_factory.colours[color]
            .union_elements
            .iter()
            .copied()
            .collect::<Vec<_>>();
        // TODO:: bad collect
        let graph_nodes = graph_nodes
            .into_iter()
            .map(|c| {
                *self
                    .color_node_to_graph_node
                    .get(&self.node_factory.create_node(Some(c)))
                    .unwrap()
            })
            .collect::<Vec<_>>();
        for lca in lca_finder.findAll(&self.color_holds_instance_graph, &graph_nodes) {
            self.connect_source_to_dest(
                *self.color_node_to_graph_node.get(&lca).unwrap(),
                union_node,
            );
        }
    }

    /** Insert {@code color} and all necessary related colors into the datastructures of this pass. */
    fn add_color(&mut self, color: ColorId) -> NodeIndex {
        let node = self.node_factory.create_node(Some(color));
        self.add_color_node(node)
    }

    /** Insert {@code node} and all necessary related colors into the datastructures of this pass. */
    fn add_color_node(&mut self, node: ColorGraphNodeId) -> NodeIndex {
        if let Some(flat_node) = self.color_node_to_graph_node.get(&node) {
            return *flat_node;
        }
        let flat_node = self.color_holds_instance_graph.add_node(node);
        self.color_node_to_graph_node.insert(node, flat_node);

        let color = self.node_factory[node].color;

        if self.node_factory.colours[color].is_union() {
            let alts = self.node_factory.colours[color]
                .union_elements
                .iter()
                .copied()
                .collect::<Vec<_>>();
            for alt in alts {
                let dest = self.add_color(alt);
                self.connect_source_to_dest(flat_node, dest);
            }
            return flat_node;
        }

        self.connect_source_to_dest(self.top_node, flat_node);

        flat_node
    }

    fn connect_source_to_dest(&mut self, source: NodeIndex, dest: NodeIndex) {
        if source == dest {
            return;
        }
        self.color_holds_instance_graph
            .update_edge(source, dest, ());
    }
}
