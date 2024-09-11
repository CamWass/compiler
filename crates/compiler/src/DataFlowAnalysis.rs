use std::hash::Hash;
use std::marker::PhantomData;
use std::{collections::BTreeSet, ops::Index};

use ecma_visit::{Visit, VisitWith};
use index::newtype_index;
use petgraph::{
    graph::{EdgeIndex, NodeIndex},
    EdgeDirection,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::control_flow::ControlFlowAnalysis::NodePriority;
use crate::control_flow::{node::CfgNode, ControlFlowGraph::*};
use crate::find_vars::{FunctionLike, VarId};
use crate::{Id, ToId};

#[cfg(test)]
mod tests;

/// The maximum number of steps per individual CFG node before we assume the analysis is divergent.
// <p>TODO(b/196398705): This is way too high. Find traversal ordering heuristic that reduces it.
pub const MAX_STEPS_PER_NODE: usize = 20000;

impl<N, I, L, J> AnnotationPrinter<LinearFlowState> for DataFlowAnalysis<'_, N, I, L, J>
where
    N: CfgNode,
    I: DataFlowAnalysisInner<N, L, J>,
    L: LatticeElement,
    J: FlowJoiner<L, I>,
{
    fn print(
        &self,
        annotation: &LinearFlowState,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        f.debug_struct("LinearFlowState")
            .field("stepCount", &annotation.step_count)
            .field("in_", &self.inner[annotation.in_])
            .field("out", &self.inner[annotation.out])
            .finish()
    }
}

/**
 * A framework to help writing static program analysis.
 *
 * <p>A subclass of this framework should specify how a single node changes the state of a program.
 * This class finds a safe estimate (a fixed-point) for the whole program. The proven facts about
 * the program will be annotated with {@link
 * com.google.javascript.jscomp.graph.GraphNode#setAnnotation} to the given control flow graph's
 * nodes in form of {@link LatticeElement} after calling {@link #analyze()}.
 *
 * <p>As a guideline, the following is a list of behaviors that any analysis can take:
 *
 * <ol>
 *   <li>Flow Direction: Is the analysis a forward or backward analysis?
 *   <li>Lattice Elements: How does the analysis represent the state of the program at any given
 *       point?
 *   <li>Branching: Does the analysis propagate a different lattice element along each branch
 *       exiting a node?
 *   <li>JOIN Operation: Given two incoming paths and a lattice state value, what can the compiler
 *       conclude at the join point?
 *   <li>Flow Equations: How does an instruction modify the state of program in terms of lattice
 *       values?
 *   <li>Initial Entry Value: What can the compiler assume at the beginning of the program?
 *   <li>Initial Estimate: What can the compiler assume at each point of the program? (What is the
 *       BOTTOM value of the lattice) By definition this lattice JOIN {@code x} for any {@code x}
 *       must also be {@code x}.
 *   <li>(Optional) Branch Operation: How should the flow branch along edges?
 * </ol>
 *
 * To make these behaviors known to the framework, the following steps must be taken.
 *
 * <ol>
 *   <li>Flow Direction: Implement {@link #isForward()}.
 *   <li>Lattice Elements: Implement {@link LatticeElement}.
 *   <li>JOIN Operation: Implement {@link JoinOp#apply}.
 *   <li>Flow Equations: Implement {@link #flowThrough(Object, LatticeElement)}.
 *   <li>Initial Entry Value: Implement {@link #createEntryLattice()}.
 *   <li>Initial Estimate: Implement {@link #createInitialEstimateLattice()}.
 *   <li>(Optional) Branch Operation: Return true from {@link #isBranched()} and implement {@link
 *       #createFlowBrancher}.
 * </ol>
 *
 * <p>Upon execution of the {@link #analyze()} method, nodes of the input control flow graph will be
 * annotated with a {@link FlowState} object that represents maximum fixed point solution. Any
 * previous annotations at the nodes of the control flow graph will be lost.
 *
 * @param <N> The control flow graph's node value type.
 * @param <L> Lattice element type.
 */
pub struct DataFlowAnalysis<'p, N, I, L, J>
where
    N: CfgNode,
    I: DataFlowAnalysisInner<N, L, J>,
    L: LatticeElement,
    J: FlowJoiner<L, I>,
{
    pub inner: I,

    /// The set of nodes that need to be considered, orderd by their priority
    /// as determined by control flow analysis and data flow direction.
    work_queue: UniqueQueue<'p>,

    _phantom1: PhantomData<L>,
    _phantom2: PhantomData<J>,
    _phantom3: PhantomData<N>,
}

impl<'p, N, I, L, J> DataFlowAnalysis<'p, N, I, L, J>
where
    N: CfgNode,
    I: DataFlowAnalysisInner<N, L, J>,
    L: LatticeElement,
    J: FlowJoiner<L, I>,
{
    /**
     * Constructs a data flow analysis.
     *
     * <p>Typical usage
     *
     * <pre>
     * DataFlowAnalysis dfa = ...
     * dfa.analyze();
     * </pre>
     *
     * {@link #analyze()} annotates the result to the control flow graph by means of {@link
     * DiGraphNode#setAnnotation} without any modification of the graph itself. Additional calls to
     * {@link #analyze()} recomputes the analysis which can be useful if the control flow graph has
     * been modified.
     *
     * @param cfg The control flow graph object that this object performs on. Modification of the
     *     graph requires a separate call to {@link #analyze()}.
     * @see #analyze()
     */
    pub fn new(inner: I, node_priorities: &'p [NodePriority]) -> Self {
        Self {
            work_queue: UniqueQueue::new(node_priorities, inner.is_forward()),

            inner,

            _phantom1: PhantomData,
            _phantom2: PhantomData,
            _phantom3: PhantomData,
        }
    }

    /**
     * Finds a fixed-point solution. The function has the side effect of replacing the existing node
     * annotations with the computed solutions using {@link
     * com.google.javascript.jscomp.graph.GraphNode#setAnnotation(Annotation)}.
     *
     * <p>Initially, each node's input and output flow state contains the value given by {@link
     * #createInitialEstimateLattice()} (with the exception of the entry node of the graph which takes
     * on the {@link #createEntryLattice()} value. Each node will use the output state of its
     * predecessor and compute an output state according to the instruction. At that time, any nodes
     * that depend on the node's newly modified output value will need to recompute their output state
     * again. Each step will perform a computation at one node until no extra computation will modify
     * any existing output state anymore.
     */
    pub fn analyze(&mut self) {
        self.analyze_inner()
            .expect("Dataflow analysis appears to diverge");
    }

    // TODO: analyze is split into two because tests need to verify that divergence
    // is caught, but all other callers will just panic at the moment (until proper
    // error handling is implemented for the compiler).
    fn analyze_inner(&mut self) -> Result<(), N> {
        self.initialize();
        while let Some(cur_node_idx) = self.work_queue.pop() {
            let cur_node = self.inner.cfg().graph[cur_node_idx];
            if self.inner.cfg().node_annotations[&cur_node].step_count > MAX_STEPS_PER_NODE {
                return Err(cur_node);
            }
            self.get_flow_state_mut(&cur_node).step_count += 1;

            self.join_inputs(cur_node);
            if self.flow(cur_node) {
                // If there is a change in the current node, we want to grab the list
                // of nodes that this node affects.
                let next_nodes = if self.inner.is_forward() {
                    self.inner.cfg().get_directed_succ_nodes(cur_node)
                } else {
                    self.inner.cfg().get_directed_pred_nodes(cur_node)
                };

                for next_node in next_nodes {
                    let node = self.inner.cfg()[next_node];
                    if node != self.inner.cfg().implicit_return {
                        self.work_queue.push(next_node);
                    }
                }
            }
        }
        if self.inner.is_forward() {
            self.join_inputs(self.inner.cfg().implicit_return);
        }
        Ok(())
    }

    /** Initializes the work list and the control flow graph. */
    fn initialize(&mut self) {
        self.work_queue.clear();

        for i in self.inner.cfg().graph.node_indices() {
            let node = self.inner.cfg().graph[i];
            let in_ = self.inner.create_initial_estimate_lattice();
            let out = self.inner.create_initial_estimate_lattice();
            self.inner
                .cfg_mut()
                .node_annotations
                .insert(node, LinearFlowState::new(in_, out));
            if node != self.inner.cfg().implicit_return {
                self.work_queue.push(i);
            }
        }
    }

    /**
     * Performs a single flow through a node.
     *
     * @return {@code true} if the flow state differs from the previous state.
     */
    fn flow(&mut self, node: N) -> bool {
        let state = &self.inner.cfg().node_annotations[&node];
        if self.inner.is_forward() {
            let out_before = state.out;
            let new_out = self.inner.flow_through(node, state.in_);
            self.get_flow_state_mut(&node).out = new_out;
            self.inner[out_before] != self.inner[new_out]
        } else {
            let in_before = state.in_;
            let new_in = self.inner.flow_through(node, state.out);
            self.get_flow_state_mut(&node).in_ = new_in;
            self.inner[in_before] != self.inner[new_in]
        }
    }

    /**
     * Computes the new flow state at a given node's entry by merging the output (input) lattice of
     * the node's predecessor (successor).
     *
     * @param node Node to compute new join.
     */
    fn join_inputs(&mut self, node: N) {
        if self.inner.is_forward() && self.inner.cfg().entry == node {
            self.get_flow_state_mut(&node).in_ = self.inner.create_entry_lattice();
            return;
        }

        let dir = if self.inner.is_forward() {
            EdgeDirection::Incoming
        } else {
            EdgeDirection::Outgoing
        };
        let mut in_edges = self
            .inner
            .cfg()
            .graph
            .neighbors_directed(self.inner.cfg().map[&node], dir)
            .detach();

        let result = if let Some(first) = in_edges.next(&self.inner.cfg().graph) {
            if let Some(second) = in_edges.next(&self.inner.cfg().graph) {
                let mut joiner = self.inner.create_flow_joiner();
                let first = get_input_from_edge(&mut self.inner, first);
                joiner.join_flow(&mut self.inner, first);
                let second = get_input_from_edge(&mut self.inner, second);
                joiner.join_flow(&mut self.inner, second);
                while let Some(in_edge) = in_edges.next(&self.inner.cfg().graph) {
                    let id = get_input_from_edge(&mut self.inner, in_edge);
                    joiner.join_flow(&mut self.inner, id);
                }
                self.inner.add_lattice_element(joiner.finish())
            } else {
                // Only one relevant edge.
                get_input_from_edge(&mut self.inner, first)
            }
        } else {
            // No relevant edges.
            return;
        };

        if self.inner.is_forward() {
            self.get_flow_state_mut(&node).in_ = result;
        } else {
            self.get_flow_state_mut(&node).out = result;
        }
    }

    fn get_flow_state_mut(&mut self, node: &N) -> &mut LinearFlowState {
        // All nodes should have had their state initialized.
        self.inner.cfg_mut().node_annotations.get_mut(node).unwrap()
    }
}

fn get_input_from_edge<N, I, L, J>(inner: &mut I, edge: (EdgeIndex, NodeIndex)) -> LatticeElementId
where
    N: CfgNode,
    I: DataFlowAnalysisInner<N, L, J>,
    L: LatticeElement,
    J: FlowJoiner<L, I>,
{
    if inner.is_forward() {
        let source = inner.cfg().graph.edge_endpoints(edge.0).unwrap().0;
        let node = inner.cfg().graph[source];
        let state = &inner.cfg().node_annotations[&node];
        state.out
    } else {
        let target = inner.cfg().graph.edge_endpoints(edge.0).unwrap().1;
        let node = inner.cfg().graph[target];
        if node == inner.cfg().implicit_return {
            return inner.create_entry_lattice();
        }
        let state = &inner.cfg().node_annotations[&node];
        state.in_
    }
}

pub trait DataFlowAnalysisInner<N, L, J>: Index<LatticeElementId, Output = L>
where
    N: CfgNode,
    L: LatticeElement,
    J: FlowJoiner<L, Self>,
    Self: Sized,
{
    fn add_lattice_element(&mut self, element: L) -> LatticeElementId;
    /**
     * Checks whether the analysis is a forward flow analysis or backward flow analysis.
     *
     * @return {@code true} if it is a forward analysis.
     */
    fn is_forward(&self) -> bool;
    /**
     * Gets the incoming state of the entry node.
     *
     * @return Entry state.
     */
    fn create_entry_lattice(&mut self) -> LatticeElementId;
    /**
     * Gets the state of the initial estimation at each node.
     *
     * @return Initial state.
     */
    fn create_initial_estimate_lattice(&mut self) -> LatticeElementId;
    /**
     * Gets a new joiner for an analysis step.
     *
     * <p>The joiner is invoked once for each input edge and then the final joined result is
     * retrieved. No joiner will be created for a single input.
     */
    fn create_flow_joiner(&self) -> J;
    /**
     * Computes the output state for a given node given its input state.
     *
     * @param node The node.
     * @param input Input lattice that should be read-only.
     * @return Output lattice.
     */
    fn flow_through(&mut self, node: N, input: LatticeElementId) -> LatticeElementId;

    fn cfg(&self) -> &ControlFlowGraph<N, LinearFlowState, LatticeElementId>;
    fn cfg_mut(&mut self) -> &mut ControlFlowGraph<N, LinearFlowState, LatticeElementId>;
}

/** A reducer that joins flow states from distinct input states into a single input state. */
pub trait FlowJoiner<L, I> {
    fn join_flow(&mut self, inner: &mut I, input: LatticeElementId);

    fn finish(self) -> L;
}

pub trait LatticeElement: Annotation + PartialEq {}

/** The in and out states of a node. */
#[derive(Debug, Clone, Copy)]
pub struct LinearFlowState {
    pub step_count: usize,
    pub in_: LatticeElementId,
    pub out: LatticeElementId,
}

impl LinearFlowState {
    pub fn new(in_: LatticeElementId, out: LatticeElementId) -> Self {
        Self {
            step_count: 0,
            in_,
            out,
        }
    }
}

impl Annotation for LinearFlowState {}

newtype_index!(pub struct LatticeElementId { .. });

impl Annotation for LatticeElementId {}

#[derive(Debug)]
pub struct PrioritizedNode(NodePriority, NodeIndex);

impl PartialEq for PrioritizedNode {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for PrioritizedNode {}

impl std::cmp::Ord for PrioritizedNode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

// `PartialOrd` needs to be implemented as well.
impl std::cmp::PartialOrd for PrioritizedNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub struct UniqueQueue<'p> {
    inner: BTreeSet<PrioritizedNode>,
    priorities: &'p [NodePriority],
    forwards: bool,
}

impl<'p> UniqueQueue<'p> {
    pub fn new(priorities: &'p [NodePriority], forwards: bool) -> Self {
        Self {
            inner: BTreeSet::new(),
            priorities,
            forwards,
        }
    }

    pub fn pop(&mut self) -> Option<NodeIndex> {
        if self.forwards {
            // Forwards analyses visit nodes with lower priorities first.
            self.inner.pop_first().map(|p| p.1)
        } else {
            // Backwards analyses visit nodes with higher priorities first.
            self.inner.pop_last().map(|p| p.1)
        }
    }

    pub fn push(&mut self, node: NodeIndex) {
        self.inner
            .insert(PrioritizedNode(self.priorities[node.index()], node));
    }

    pub fn clear(&mut self) {
        self.inner.clear()
    }
}

// TODO: comment. I think the only vars that get escaped are catch variables and variables referenced in nested functions.
/**
 * Compute set of escaped variables. When a variable is escaped in a dataflow analysis, it can be
 * referenced outside of the code that we are analyzing. A variable is escaped if any of the
 * following is true:
 *
 * <p>1. Exported variables as they can be needed after the script terminates. 2. Names of named
 * functions because in JavaScript, function foo(){} does not kill foo in the dataflow.
 *
 * @param jsScope Must be a function scope
 */
pub fn compute_escaped<'a, T>(
    fn_scope: &T,
    all_vars_in_fn: &FxHashMap<Id, VarId>,
    catch_vars: FxHashSet<Id>,
) -> FxHashSet<Id>
where
    T: FunctionLike<'a>,
{
    // TODO (simranarora) catch variables should not be considered escaped in ES6. Getting rid of
    // the catch check is causing breakages however
    let mut escaped = catch_vars;
    let mut v = EscapedVarFinder {
        all_vars_in_fn,
        escaped: &mut escaped,
    };
    fn_scope.visit_body_with(&mut v);
    escaped
}

struct EscapedVarFinder<'a> {
    all_vars_in_fn: &'a FxHashMap<Id, VarId>,
    escaped: &'a mut FxHashSet<Id>,
}

macro_rules! visit_fn {
    ($f:ident, $t:ident) => {
        fn $f(&mut self, node: &ast::$t) {
            let mut v = RefFinder {
                all_vars_in_fn: self.all_vars_in_fn,
                escaped: self.escaped,
            };
            // TODO: I think the function's params can also access vars from parent scope (e.g. in default values).
            node.body.visit_children_with(&mut v);
        }
    };
}

impl<'ast, 'a> Visit<'ast> for EscapedVarFinder<'a> {
    // Only search for references in nested functions.
    visit_fn!(visit_function, Function);
    visit_fn!(visit_constructor, Constructor);
    visit_fn!(visit_arrow_expr, ArrowExpr);
    visit_fn!(visit_getter_prop, GetterProp);
    visit_fn!(visit_setter_prop, SetterProp);
}
struct RefFinder<'a> {
    all_vars_in_fn: &'a FxHashMap<Id, VarId>,
    escaped: &'a mut FxHashSet<Id>,
}

impl<'ast, 'a> Visit<'ast> for RefFinder<'a> {
    fn visit_ident(&mut self, node: &'ast ast::Ident) {
        let id = node.to_id();
        if self.all_vars_in_fn.contains_key(&id) {
            self.escaped.insert(id);
        }
    }
}
