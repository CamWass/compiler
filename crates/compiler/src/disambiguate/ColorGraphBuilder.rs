use crate::{colors::ColorId, graph::LowestCommonAncestorFinder::LowestCommonAncestorFinder};

use super::{ColorGraphNode::ColorGraphNodeId, ColorGraphNodeFactory::ColorGraphNodeFactory};
use ahash::AHashMap;
use petgraph::graph::{DiGraph, NodeIndex};

/** Builds a graph of the {@link Color}s on the AST from a specified set of seed colors. */
pub struct ColorGraphBuilder<'col, 'nf> {
    nodeFactory: &'nf mut ColorGraphNodeFactory<'col>,
    // lcaFinder: LowestCommonAncestorFinder<ColorGraphNode, EdgeReason>,
    topNode: NodeIndex,

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
    colorHoldsInstanceGraph: DiGraph<ColorGraphNodeId, EdgeReason>,

    color_node_to_graph_node: AHashMap<ColorGraphNodeId, NodeIndex>,
}

impl<'col, 'nf> ColorGraphBuilder<'col, 'nf> {
    pub fn new(nodeFactory: &'nf mut ColorGraphNodeFactory<'col>) -> Self {
        let mut colorHoldsInstanceGraph = DiGraph::new();
        let topColorNode = nodeFactory.createNode(Some(nodeFactory.colours.unknown_color));
        let topNode = colorHoldsInstanceGraph.add_node(topColorNode);
        Self {
            nodeFactory,
            // lcaFinder: LowestCommonAncestorFinder::new(colorHoldsInstanceGraph),
            topNode,
            colorHoldsInstanceGraph,
            color_node_to_graph_node: AHashMap::default(),
        }
    }

    pub fn addAll(&mut self, flats: &[ColorGraphNodeId]) {
        for &flat in flats {
            self.addColorNode(flat);
        }
    }

    pub fn build(mut self) -> DiGraph<ColorGraphNodeId, EdgeReason> {
        let mut lcaFinder = LowestCommonAncestorFinder::new();
        for node in self.colorHoldsInstanceGraph.node_indices() {
            self.connectUnionWithAncestors(node, &mut lcaFinder);
        }
        self.colorHoldsInstanceGraph
    }

    /**
     * During initial lattice construction unions were only given outbound edges. Here we add any
     * necessary inbound ones.
     *
     * <p>We defer this operation because adding union-to-union and common-supertype-to-union edges,
     * is a hard problem. Solving it after all other colors are in place makes it easier.
     */
    fn connectUnionWithAncestors(
        &mut self,
        unionNode: NodeIndex,
        lcaFinder: &mut LowestCommonAncestorFinder,
    ) {
        let flatUnion = self.colorHoldsInstanceGraph[unionNode];
        let color = self.nodeFactory[flatUnion].color;
        if !self.nodeFactory.colours.colors[color].isUnion() {
            return;
        }

        /**
         * Connect the LCAs to the union.
         *
         * <p>The union itself will be found in most cases, but since we don't add self-edges, that
         * won't matter.
         *
         * <p>Some of these edges may pollute the "lattice-ness" of the graph, but all the invariants we
         * actually care about will be maintained. If disambiguation is too slow and stricter invariants
         * would help, we could be more careful.
         */
        debug_assert!(self.colorHoldsInstanceGraph.edges(unionNode).count() != 0);
        // TODO:: bad collect
        let graphNodes = self.nodeFactory.colours.colors[color]
            .unionElements
            .iter()
            .copied()
            .collect::<Vec<_>>();
        // TODO:: bad collect
        let graphNodes = graphNodes
            .into_iter()
            .map(|c| {
                *self
                    .color_node_to_graph_node
                    .get(&self.nodeFactory.createNode(Some(c)))
                    .unwrap()
            })
            .collect::<Vec<_>>();
        for lca in lcaFinder.findAll(&self.colorHoldsInstanceGraph, &graphNodes) {
            self.connectSourceToDest(
                *self.color_node_to_graph_node.get(&lca).unwrap(),
                EdgeReason::ALGEBRAIC,
                unionNode,
            );
        }
    }

    /** Insert {@code color} and all necessary related colors into the datastructures of this pass. */
    fn addColor(&mut self, color: ColorId) -> NodeIndex {
        let node = self.nodeFactory.createNode(Some(color));
        self.addColorNode(node)
    }

    /** Insert {@code node} and all necessary related colors into the datastructures of this pass. */
    fn addColorNode(&mut self, node: ColorGraphNodeId) -> NodeIndex {
        if let Some(flatNode) = self.color_node_to_graph_node.get(&node) {
            return *flatNode;
        }
        let flatNode = self.colorHoldsInstanceGraph.add_node(node);
        self.color_node_to_graph_node.insert(node, flatNode);

        let color = self.nodeFactory[node].color;

        if self.nodeFactory.colours.colors[color].isUnion() {
            let alts = self.nodeFactory.colours.colors[color]
                .unionElements
                .iter()
                .copied()
                .collect::<Vec<_>>();
            for alt in alts {
                let dest = self.addColor(alt);
                self.connectSourceToDest(flatNode, EdgeReason::ALGEBRAIC, dest);
            }
            return flatNode;
        }

        if let Some(supertypes) = self.nodeFactory.colours.getDisambiguationSupertypes(color) {
            // TODO: bad collect
            let supertypes = supertypes.iter().copied().collect::<Vec<_>>();
            for supertype in supertypes {
                let source = self.addColor(supertype);
                self.connectSourceToDest(source, EdgeReason::CAN_HOLD, flatNode);
            }
        } else {
            self.connectSourceToDest(self.topNode, EdgeReason::ALGEBRAIC, flatNode);
        }

        /**
         * Add all instance and prototype colors when visiting a constructor. We won't necessarily see
         * all possible instance colors that exist at runtime during an AST traversal.
         *
         * <p>For example, a subclass constructor may never be explicitly initialized but instead passed
         * to some function expecting `function(new:Parent)`. See {@link
         * AmbiguatePropertiesTest#testImplementsAndExtends_respectsUndeclaredProperties()}
         */
        // TODO: bad collect
        // let prototypes = self.nodeFactory.colours.colors[color]
        //     .prototypes
        //     .iter()
        //     .copied()
        //     .collect::<Vec<_>>();
        // for prototype in prototypes {
        //     self.addColor(prototype);
        // }
        // // TODO: bad collect
        // let instanceColors = self.nodeFactory.colours.colors[color]
        //     .instanceColors
        //     .iter()
        //     .copied()
        //     .collect::<Vec<_>>();
        // for instanceColor in instanceColors {
        //     self.addColor(instanceColor);
        // }
        flatNode
    }

    fn connectSourceToDest(&mut self, source: NodeIndex, reason: EdgeReason, dest: NodeIndex) {
        if source == dest {
            return;
        }
        self.colorHoldsInstanceGraph
            .update_edge(source, dest, reason);
    }
}

/**
 * The relationship that caused an edge to be created.
 *
 * <p>This information is only retained for diagnostics, not correctness.
 */
#[derive(Debug)]
pub enum EdgeReason {
    ALGEBRAIC,
    CAN_HOLD,
}
