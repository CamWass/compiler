use std::fmt::Debug;
use std::ops::Index;

use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    EdgeDirection,
};
use rustc_hash::FxHashMap;

use crate::control_flow::node::Node;
use crate::control_flow::ControlFlowGraph::*;
use crate::DataFlowAnalysis::{LatticeElementId, LinearFlowState, UniqueQueue, MAX_STEPS_PER_NODE};

use super::{simple_set::IndexSet, JoinOp, Lattice, Store};

#[derive(Debug)]
pub(super) struct DataFlowAnalysis<'ast, 'p> {
    /// The set of nodes that need to be considered, ordered by their priority
    /// as determined by control flow analysis and data flow direction.
    workQueue: UniqueQueue<'p, Node<'ast>>,

    pub(super) lattice_elements: IndexSet<LatticeElementId, Lattice>,
    pub(super) cfg: ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,
    initial_lattice: LatticeElementId,
    in_fn: bool,
}

impl<'ast, 'p> DataFlowAnalysis<'ast, 'p> {
    pub fn new(
        cfg: ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,
        nodePriorities: &'p FxHashMap<Node<'ast>, usize>,
        in_fn: bool,
    ) -> Self {
        let mut lattice_elements = IndexSet::default();
        let initial_lattice = lattice_elements.insert(Lattice::default());
        Self {
            workQueue: UniqueQueue::new(nodePriorities, true),
            cfg,
            lattice_elements,
            initial_lattice,
            in_fn,
        }
    }

    pub fn analyze(&mut self, store: &mut Store<'ast>) {
        self.analyze_inner(store)
            .expect("Dataflow analysis appears to diverge");
    }

    // TODO: analyze is split into two because tests need to verify that divergence
    // is caught, but all other callers will just panic at the moment (until proper
    // error handling is implemented for the compiler).
    fn analyze_inner(&mut self, store: &mut Store<'ast>) -> Result<(), Node<'ast>> {
        self.initialize();
        while let Some(curNode) = self.workQueue.pop() {
            if self.cfg.node_annotations[&curNode].stepCount > MAX_STEPS_PER_NODE {
                return Err(curNode);
            }
            self.get_flow_state_mut(&curNode).stepCount += 1;

            self.joinInputs(curNode, store);
            if self.flow(curNode, store) {
                // If there is a change in the current node, we want to grab the list
                // of nodes that this node affects.
                let nextNodes = self.cfg.getDirectedSuccNodes(curNode);

                for nextNode in nextNodes {
                    let node = self.cfg[nextNode];
                    if node != self.cfg.implicit_return {
                        self.workQueue.push(node);
                    }
                }
            }
        }

        self.joinInputs(self.cfg.implicit_return, store);

        Ok(())
    }

    /** Initializes the work list and the control flow graph. */
    fn initialize(&mut self) {
        self.workQueue.clear();

        let mut i = 0;
        while i < self.cfg.graph.raw_nodes().len() {
            let node = self.cfg.graph.raw_nodes()[i].weight;
            let in_ = self.createInitialEstimateLattice();
            let out = self.createInitialEstimateLattice();
            self.cfg
                .node_annotations
                .insert(node, LinearFlowState::new(in_, out));
            if node != self.cfg.implicit_return {
                self.workQueue.push(node);
            }
            i += 1;
        }
    }

    /**
     * Performs a single flow through a node.
     *
     * @return {@code true} if the flow state differs from the previous state.
     */
    fn flow(&mut self, node: Node<'ast>, store: &mut Store<'ast>) -> bool {
        let state = &self.cfg.node_annotations[&node];
        let outBefore = state.out;
        let new_out = self.flowThrough(node, store, state.in_, self.in_fn);
        self.get_flow_state_mut(&node).out = new_out;
        outBefore != new_out
    }

    /**
     * Computes the new flow state at a given node's entry by merging the output (input) lattice of
     * the node's predecessor (successor).
     *
     * @param node Node to compute new join.
     */
    fn joinInputs(&mut self, node: Node<'ast>, store: &mut Store) {
        if self.cfg.entry == node {
            self.get_flow_state_mut(&node).in_ = self.createEntryLattice();
            return;
        }

        let dir = EdgeDirection::Incoming;
        let mut inEdges = self
            .cfg
            .graph
            .neighbors_directed(self.cfg.map[&node], dir)
            .detach();

        let result = if let Some(first) = inEdges.next(&self.cfg.graph) {
            if let Some(second) = inEdges.next(&self.cfg.graph) {
                let mut joiner = JoinOp::default();
                let first = self.getInputFromEdge(first);
                joiner.joinFlow(self, store, first);
                let second = self.getInputFromEdge(second);
                joiner.joinFlow(self, store, second);
                while let Some(inEdge) = inEdges.next(&self.cfg.graph) {
                    let id = self.getInputFromEdge(inEdge);
                    joiner.joinFlow(self, store, id);
                }
                self.add_lattice_element(joiner.finish())
            } else {
                // Only one relevant edge.
                self.getInputFromEdge(first)
            }
        } else {
            // No relevant edges.
            return;
        };

        self.get_flow_state_mut(&node).in_ = result;
    }

    fn get_flow_state_mut(&mut self, node: &Node<'ast>) -> &mut LinearFlowState {
        // All nodes should have had their state initialized.
        self.cfg.node_annotations.get_mut(node).unwrap()
    }

    fn getInputFromEdge(&self, edge: (EdgeIndex, NodeIndex)) -> LatticeElementId {
        let source = self.cfg.graph.edge_endpoints(edge.0).unwrap().0;
        let node = self.cfg.graph[source];
        let state = &self.cfg.node_annotations[&node];
        state.out
    }

    pub(super) fn add_lattice_element(&mut self, element: Lattice) -> LatticeElementId {
        self.lattice_elements.insert(element)
    }

    fn createEntryLattice(&mut self) -> LatticeElementId {
        self.initial_lattice
    }

    fn createInitialEstimateLattice(&mut self) -> LatticeElementId {
        self.initial_lattice
    }
}

impl Index<LatticeElementId> for DataFlowAnalysis<'_, '_> {
    type Output = Lattice;

    fn index(&self, index: LatticeElementId) -> &Self::Output {
        &self.lattice_elements[index]
    }
}
