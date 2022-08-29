use std::hash::Hash;
use std::marker::PhantomData;
use std::{collections::BTreeSet, ops::Index};

use ecma_visit::{noop_visit_type, Visit, VisitWith};
use index::newtype_index;
use petgraph::{graph::EdgeReference, visit::EdgeRef, EdgeDirection};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::control_flow::ControlFlowGraph::AnnotationPrinter;
use crate::control_flow::{
    node::CfgNode,
    ControlFlowAnalysis::ControlFlowAnalysisResult,
    ControlFlowGraph::{Annotation, Branch, ControlFlowGraph},
};
use crate::Id;
use crate::{find_vars::*, ToId};

#[cfg(test)]
mod tests;

/// The maximum number of steps per individual CFG node before we assume the analysis is divergent.
// <p>TODO(b/196398705): This is way too high. Find traversal ordering heurisitc that reduces it.
pub const MAX_STEPS_PER_NODE: usize = 20000;

impl<N, I, L, J> AnnotationPrinter<LinearFlowState> for DataFlowAnalysis<N, I, L, J>
where
    N: CfgNode,
    I: DataFlowAnalysisInner<N, L, J>,
    L: LatticeElement,
    J: FlowJoiner<L>,
{
    fn print(
        &self,
        annotation: &LinearFlowState,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        f.debug_struct("LinearFlowState")
            .field("stepCount", &annotation.stepCount)
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
pub struct DataFlowAnalysis<N, I, L, J>
where
    N: CfgNode,
    I: DataFlowAnalysisInner<N, L, J>,
    L: LatticeElement,
    J: FlowJoiner<L>,
{
    pub inner: I,

    pub cfg: ControlFlowGraph<N, LinearFlowState, L>,
    /// The set of nodes that need to be considered, orderd by their priority
    /// as determined by control flow analysis and data flow direction.
    workQueue: UniqueQueue<N>,

    _phantom1: PhantomData<L>,
    _phantom2: PhantomData<J>,
}

impl<N, I, L, J> DataFlowAnalysis<N, I, L, J>
where
    N: CfgNode,
    I: DataFlowAnalysisInner<N, L, J>,
    L: LatticeElement,
    J: FlowJoiner<L>,
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
    pub fn new(inner: I, cfa: ControlFlowAnalysisResult<N, LinearFlowState, L>) -> Self {
        Self {
            cfg: cfa.cfg,
            workQueue: UniqueQueue::new(cfa.nodePriorities, inner.isForward()),

            inner,

            _phantom1: PhantomData,
            _phantom2: PhantomData,
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
        while let Some(curNode) = self.workQueue.pop() {
            if self.cfg.node_annotations[&curNode].stepCount > MAX_STEPS_PER_NODE {
                return Err(curNode);
            }
            self.cfg
                .node_annotations
                .get_mut(&curNode)
                .unwrap()
                .stepCount += 1;

            self.joinInputs(curNode);
            if self.flow(curNode) {
                // If there is a change in the current node, we want to grab the list
                // of nodes that this node affects.
                let nextNodes = if self.inner.isForward() {
                    self.cfg.getDirectedSuccNodes(curNode)
                } else {
                    self.cfg.getDirectedPredNodes(curNode)
                };

                for nextNode in nextNodes {
                    let node = self.cfg[nextNode];
                    if node != self.cfg.implicit_return {
                        self.workQueue.push(node);
                    }
                }
            }
        }
        if self.inner.isForward() {
            self.joinInputs(self.cfg.implicit_return);
        }
        Ok(())
    }

    /** Initializes the work list and the control flow graph. */
    fn initialize(&mut self) {
        self.workQueue.clear();
        for &node in self.cfg.graph.node_weights() {
            self.cfg.node_annotations.insert(
                node,
                LinearFlowState::new(
                    self.inner.createInitialEstimateLattice(),
                    self.inner.createInitialEstimateLattice(),
                ),
            );
            if node != self.cfg.implicit_return {
                self.workQueue.push(node);
            }
        }
    }

    /**
     * Performs a single flow through a node.
     *
     * @return {@code true} if the flow state differs from the previous state.
     */
    fn flow(&mut self, node: N) -> bool {
        let state = &self.cfg.node_annotations[&node];
        if self.inner.isForward() {
            let outBefore = state.out;
            let new_out = self.inner.flowThrough(node, state.in_, &self.cfg);
            self.cfg.node_annotations.get_mut(&node).unwrap().out = new_out;
            self.inner[outBefore] != self.inner[new_out]
        } else {
            let inBefore = state.in_;
            let new_in = self.inner.flowThrough(node, state.out, &self.cfg);
            self.cfg.node_annotations.get_mut(&node).unwrap().in_ = new_in;
            self.inner[inBefore] != self.inner[new_in]
        }
    }

    /**
     * Computes the new flow state at a given node's entry by merging the output (input) lattice of
     * the node's predecessor (successor).
     *
     * @param node Node to compute new join.
     */
    fn joinInputs(&mut self, node: N) {
        if self.inner.isForward() && self.cfg.entry == node {
            self.cfg.node_annotations.get_mut(&node).unwrap().in_ = self.inner.createEntryLattice();
            return;
        }

        let dir = if self.inner.isForward() {
            EdgeDirection::Incoming
        } else {
            EdgeDirection::Outgoing
        };
        let mut inEdges = self
            .cfg
            .graph
            .edges_directed(self.cfg.map[&node], dir)
            .peekable();

        let result = if let Some(first) = inEdges.next() {
            if let Some(second) = inEdges.next() {
                let mut joiner = self.inner.createFlowJoiner();
                let first = getInputFromEdge(&mut self.inner, &self.cfg, first);
                joiner.joinFlow(&self.inner[first]);
                let second = getInputFromEdge(&mut self.inner, &self.cfg, second);
                joiner.joinFlow(&self.inner[second]);
                for inEdge in inEdges {
                    let id = getInputFromEdge(&mut self.inner, &self.cfg, inEdge);
                    joiner.joinFlow(&self.inner[id]);
                }
                self.inner.add_lattice_element(joiner.finish())
            } else {
                // Only one relevant edge.
                getInputFromEdge(&mut self.inner, &self.cfg, first)
            }
        } else {
            // No relevant edges.
            return;
        };

        if self.inner.isForward() {
            self.cfg.node_annotations.get_mut(&node).unwrap().in_ = result;
        } else {
            self.cfg.node_annotations.get_mut(&node).unwrap().out = result;
        }
    }
}

fn getInputFromEdge<N, I, L, J>(
    inner: &mut I,
    cfg: &ControlFlowGraph<N, LinearFlowState, L>,
    edge: EdgeReference<Branch>,
) -> LatticeElementId
where
    N: CfgNode,
    I: DataFlowAnalysisInner<N, L, J>,
    L: LatticeElement,
    J: FlowJoiner<L>,
{
    if inner.isForward() {
        let node = cfg.graph[edge.source()];
        let state = &cfg.node_annotations[&node];
        state.out
    } else {
        let node = cfg.graph[edge.target()];
        if node == cfg.implicit_return {
            return inner.createEntryLattice();
        }
        let state = &cfg.node_annotations[&node];
        state.in_
    }
}

pub trait DataFlowAnalysisInner<N, L, J>: Index<LatticeElementId, Output = L>
where
    N: CfgNode,
    L: LatticeElement,
    J: FlowJoiner<L>,
{
    fn add_lattice_element(&mut self, element: L) -> LatticeElementId;
    /**
     * Checks whether the analysis is a forward flow analysis or backward flow analysis.
     *
     * @return {@code true} if it is a forward analysis.
     */
    fn isForward(&self) -> bool;
    /**
     * Gets the incoming state of the entry node.
     *
     * @return Entry state.
     */
    fn createEntryLattice(&mut self) -> LatticeElementId;
    /**
     * Gets the state of the initial estimation at each node.
     *
     * @return Initial state.
     */
    fn createInitialEstimateLattice(&mut self) -> LatticeElementId;
    /**
     * Gets a new joiner for an analysis step.
     *
     * <p>The joiner is invoked once for each input edge and then the final joined result is
     * retrieved. No joiner will be created for a single input.
     */
    fn createFlowJoiner(&self) -> J;
    /**
     * Computes the output state for a given node given its input state.
     *
     * @param node The node.
     * @param input Input lattice that should be read-only.
     * @return Output lattice.
     */
    fn flowThrough(
        &mut self,
        node: N,
        input: LatticeElementId,
        cfg: &ControlFlowGraph<N, LinearFlowState, L>,
    ) -> LatticeElementId;
}

/** A reducer that joins flow states from distinct input states into a single input state. */
pub trait FlowJoiner<L>
where
    L: LatticeElement,
{
    fn joinFlow(&mut self, input: &L);

    fn finish(self) -> L;
}

pub trait LatticeElement: Annotation + PartialEq {}

/** The in and out states of a node. */
#[derive(Debug)]
pub struct LinearFlowState {
    pub stepCount: usize,
    pub in_: LatticeElementId,
    pub out: LatticeElementId,
}

impl LinearFlowState {
    fn new(in_: LatticeElementId, out: LatticeElementId) -> Self {
        Self {
            stepCount: 0,
            in_,
            out,
        }
    }
}

impl Annotation for LinearFlowState {}

newtype_index!(pub struct LatticeElementId { .. });

#[derive(Debug)]
struct PrioritizedNode<N>(usize, N);

impl<N> PartialEq for PrioritizedNode<N> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<N> Eq for PrioritizedNode<N> {}

impl<N> PrioritizedNode<N> {
    fn mk(priority: usize, node: N) -> Self {
        Self(priority, node)
    }
}

impl<N> std::cmp::Ord for PrioritizedNode<N> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

// `PartialOrd` needs to be implemented as well.
impl<N> std::cmp::PartialOrd for PrioritizedNode<N> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
struct UniqueQueue<N> {
    inner: BTreeSet<PrioritizedNode<N>>,
    priorities: FxHashMap<N, usize>,
    forwards: bool,
}

impl<N> UniqueQueue<N>
where
    N: Copy + Hash + Eq,
{
    fn new(priorities: FxHashMap<N, usize>, forwards: bool) -> Self {
        Self {
            inner: BTreeSet::new(),
            priorities,
            forwards,
        }
    }

    fn pop(&mut self) -> Option<N> {
        if self.forwards {
            // Forwards analyses visit nodes with lower priorites first.
            self.inner.pop_first().map(|p| p.1)
        } else {
            // Backwards analyses visit nodes with higher priorites first.
            self.inner.pop_last().map(|p| p.1)
        }
    }

    fn push(&mut self, node: N) {
        self.inner
            .insert(PrioritizedNode(self.priorities[&node], node));
    }

    fn clear(&mut self) {
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
pub fn computeEscaped<'a, T>(
    fn_scope: &T,
    allVarsInFn: &FxHashMap<Id, VarId>,
    catch_vars: FxHashSet<Id>,
) -> FxHashSet<Id>
where
    T: FunctionLike<'a>,
{
    // TODO (simranarora) catch variables should not be considered escaped in ES6. Getting rid of
    // the catch check is causing breakages however
    let mut escaped = catch_vars;
    let mut v = EscapedVarFinder {
        allVarsInFn,
        escaped: &mut escaped,
    };
    fn_scope.visit_body_with(&mut v);
    escaped
}

struct EscapedVarFinder<'a> {
    allVarsInFn: &'a FxHashMap<Id, VarId>,
    escaped: &'a mut FxHashSet<Id>,
}

macro_rules! visit_fn {
    ($f:ident, $t:ident) => {
        fn $f(&mut self, node: &ast::$t) {
            let mut v = RefFinder {
                allVarsInFn: self.allVarsInFn,
                escaped: self.escaped,
            };
            node.body.visit_children_with(&mut v);
        }
    };
}

impl<'ast, 'a> Visit<'ast> for EscapedVarFinder<'a> {
    noop_visit_type!();

    // Only search for references in nested functions.
    visit_fn!(visit_function, Function);
    visit_fn!(visit_constructor, Constructor);
    visit_fn!(visit_arrow_expr, ArrowExpr);
    visit_fn!(visit_getter_prop, GetterProp);
    visit_fn!(visit_setter_prop, SetterProp);
}
struct RefFinder<'a> {
    allVarsInFn: &'a FxHashMap<Id, VarId>,
    escaped: &'a mut FxHashSet<Id>,
}

impl<'ast, 'a> Visit<'ast> for RefFinder<'a> {
    noop_visit_type!();

    fn visit_ident(&mut self, node: &'ast ast::Ident) {
        let id = node.to_id();
        if self.allVarsInFn.contains_key(&id) {
            self.escaped.insert(id);
        }
    }
}
