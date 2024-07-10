use std::fmt::Debug;
use std::ops::Index;

use index::bit_set::{BitSet, GrowableBitSet};
use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    EdgeDirection,
};

use crate::control_flow::{node::Node, ControlFlowAnalysis::NodePriority};
use crate::DataFlowAnalysis::{LatticeElementId, LinearFlowState, UniqueQueue, MAX_STEPS_PER_NODE};
use crate::{control_flow::ControlFlowGraph::*, find_vars::VarId};

use super::{simple_set::IndexSet, types::ObjectId, FnId, JoinOp, Lattice, Store};

#[derive(Debug)]
pub(super) struct DataFlowAnalysis<'ast, 'p> {
    /// The set of nodes that need to be considered, ordered by their priority
    /// as determined by control flow analysis and data flow direction.
    work_queue: UniqueQueue<'p>,

    pub(super) lattice_elements: IndexSet<LatticeElementId, Lattice>,
    pub(super) cfg: ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,
    initial_lattice: LatticeElementId,
    in_fn: bool,

    pub(super) done_objects: &'p mut GrowableBitSet<ObjectId>,
    pub(super) done_functions: &'p mut BitSet<FnId>,
    pub(super) done_vars: &'p mut BitSet<VarId>,
}

impl<'ast, 'p> DataFlowAnalysis<'ast, 'p> {
    pub fn new(
        cfg: ControlFlowGraph<Node<'ast>, LinearFlowState, LatticeElementId>,
        node_priorities: &'p [NodePriority],
        in_fn: bool,
        done_objects: &'p mut GrowableBitSet<ObjectId>,
        done_functions: &'p mut BitSet<FnId>,
        done_vars: &'p mut BitSet<VarId>,
    ) -> Self {
        let mut lattice_elements = IndexSet::default();
        let initial_lattice = lattice_elements.insert(Lattice::default());
        Self {
            work_queue: UniqueQueue::new(node_priorities, true),
            cfg,
            lattice_elements,
            initial_lattice,
            in_fn,
            done_objects,
            done_functions,
            done_vars,
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

        while let Some(cur_node_idx) = self.work_queue.pop() {
            let cur_node = self.cfg.graph[cur_node_idx];
            if self.cfg.node_annotations[&cur_node].step_count > MAX_STEPS_PER_NODE {
                return Err(cur_node);
            }
            self.get_flow_state_mut(&cur_node).step_count += 1;

            self.join_inputs(cur_node, store);
            if self.flow(cur_node, store) {
                // If there is a change in the current node, we want to grab the list
                // of nodes that this node affects.
                let next_nodes = self.cfg.get_directed_succ_nodes(cur_node);

                for next_node in next_nodes {
                    let node = self.cfg[next_node];
                    if node != self.cfg.implicit_return {
                        self.work_queue.push(next_node);
                    }
                }
            }
        }

        self.join_inputs(self.cfg.implicit_return, store);

        Ok(())
    }

    /** Initializes the work list and the control flow graph. */
    fn initialize(&mut self) {
        self.work_queue.clear();

        for i in self.cfg.graph.node_indices() {
            let node = self.cfg.graph[i];
            let in_ = self.create_initial_estimate_lattice();
            let out = self.create_initial_estimate_lattice();
            self.cfg
                .node_annotations
                .insert(node, LinearFlowState::new(in_, out));
            if node != self.cfg.implicit_return {
                self.work_queue.push(i);
            }
        }
    }

    /**
     * Performs a single flow through a node.
     *
     * @return {@code true} if the flow state differs from the previous state.
     */
    fn flow(&mut self, node: Node<'ast>, store: &mut Store<'ast>) -> bool {
        let state = &self.cfg.node_annotations[&node];
        let out_before = state.out;
        let new_out = self.flow_through(node, store, state.in_, self.in_fn);
        self.get_flow_state_mut(&node).out = new_out;
        out_before != new_out
    }

    /**
     * Computes the new flow state at a given node's entry by merging the output (input) lattice of
     * the node's predecessor (successor).
     *
     * @param node Node to compute new join.
     */
    fn join_inputs(&mut self, node: Node<'ast>, store: &mut Store) {
        if self.cfg.entry == node {
            self.get_flow_state_mut(&node).in_ = self.create_entry_lattice();
            return;
        }

        let dir = EdgeDirection::Incoming;
        let mut in_edges = self
            .cfg
            .graph
            .neighbors_directed(self.cfg.map[&node], dir)
            .detach();

        let result = if let Some(first) = in_edges.next(&self.cfg.graph) {
            if let Some(second) = in_edges.next(&self.cfg.graph) {
                let mut joiner = JoinOp::default();
                let first = self.get_input_from_edge(first);
                joiner.join_flow(self, store, first);
                let second = self.get_input_from_edge(second);
                joiner.join_flow(self, store, second);
                while let Some(in_edge) = in_edges.next(&self.cfg.graph) {
                    let id = self.get_input_from_edge(in_edge);
                    joiner.join_flow(self, store, id);
                }
                self.add_lattice_element(joiner.finish())
            } else {
                // Only one relevant edge.
                self.get_input_from_edge(first)
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

    fn get_input_from_edge(&self, edge: (EdgeIndex, NodeIndex)) -> LatticeElementId {
        let source = self.cfg.graph.edge_endpoints(edge.0).unwrap().0;
        let node = self.cfg.graph[source];
        let state = &self.cfg.node_annotations[&node];
        state.out
    }

    pub(super) fn add_lattice_element(&mut self, element: Lattice) -> LatticeElementId {
        self.lattice_elements.insert(element)
    }

    fn create_entry_lattice(&mut self) -> LatticeElementId {
        self.initial_lattice
    }

    fn create_initial_estimate_lattice(&mut self) -> LatticeElementId {
        self.initial_lattice
    }
}

impl Index<LatticeElementId> for DataFlowAnalysis<'_, '_> {
    type Output = Lattice;

    fn index(&self, index: LatticeElementId) -> &Self::Output {
        &self.lattice_elements[index]
    }
}
