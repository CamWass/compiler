use crate::control_flow_graph::{Annotation, Branch, ControlFlowGraph, Node};
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use petgraph::graph::{EdgeIndex, NodeIndex};
use petgraph::visit::EdgeRef;
use std::collections::VecDeque;
use std::hash::Hash;
use std::marker::PhantomData;

/**
 * The maximum number of steps per individual CFG node before we assume the analysis is divergent.
 *
 * <p>TODO(b/196398705): This is way too high. Find traversal ordering heurisitc that reduces it.
 */
const MAX_STEPS_PER_NODE: u16 = 20000;

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
pub struct DataFlowAnalysis<'ast, L, J, B>
where
    L: Annotation + PartialEq,
    J: FlowJoiner<L>,
    B: FlowBrancher<L>,
{
    cfg: ControlFlowGraph<'ast, LinearFlowState<L>, L>,
    work_queue: UniqueQueue<Node<'ast>>,

    node_annotations: FxHashMap<Node<'ast>, LinearFlowState<L>>,
    edge_annotations: FxHashMap<EdgeIndex, L>,

    _j: PhantomData<J>,
    _b: PhantomData<B>,
}

impl<'ast, L, J, B> DataFlowAnalysis<'ast, L, J, B>
where
    L: Annotation + PartialEq,
    J: FlowJoiner<L>,
    B: FlowBrancher<L>,
{
    pub fn get_node_annotation(& self, node: Node<'ast>) -> LinearFlowState<L> {
        todo!();
    }

    pub fn set_node_annotation(&mut self, node: Node<'ast>, annotation: &LinearFlowState<L>) -> bool {
        todo!();
    }

    pub fn get_edge_annotation(& self, edge: EdgeIndex) -> L {
        todo!();
    }

    pub fn set_edge_annotation(&mut self, edge: EdgeIndex, annotation: &L) -> bool {
        todo!();
    }
}

pub trait D<'ast, L, J, B>
where
    L: Annotation + PartialEq,
    J: FlowJoiner<L>,
    B: FlowBrancher<L>,
{
    /// Returns the control flow graph that this analysis was performed on.
    /// Modifications can be done on this graph, however, the only time that the
    /// annotations are correct is after {@link #analyze()} is called and before
    /// the graph has been modified.
    fn get_cfg(&mut self) -> &mut ControlFlowGraph<'ast, LinearFlowState<L>, L>;

    /**
     * Checks whether the analysis is a forward flow analysis or backward flow analysis.
     *
     * @return {@code true} if it is a forward analysis.
     */
    fn isForward(&self) -> bool;

    /** Whether or not {@link #createFlowBrancher} should be used. */
    fn isBranched(&self) -> bool {
        false
    }

    fn join(&mut self, latticeA: L, latticeB: L) -> L {
        let joiner = self.createFlowJoiner();
        joiner.joinFlow(latticeA);
        joiner.joinFlow(latticeB);
        joiner.finish()
    }

    /**
     * Gets a new joiner for an analysis step.
     *
     * <p>The joiner is invoked once for each input edge and then the final joined result is
     * retrieved. No joiner will be created for a single input.
     */
    fn createFlowJoiner(&self) -> J;

    /**
     * Gets a new brancher for an analysis step.
     *
     * <p>The brancher is invoked for each output edge. The returned states are annotated onto each
     * edge and compared against the previous state to determine whether a fixed-point has been
     * reached.
     *
     * @param node The node at which to branch.
     * @param output The output state of {@link #flowThrough} at node.
     */
    fn createFlowBrancher(&mut self, node: Node, output: L) -> B {
        // throw new UnsupportedOperationException();
        panic!();
    }

    /**
     * Computes the output state for a given node given its input state.
     *
     * @param node The node.
     * @param input Input lattice that should be read-only.
     * @return Output lattice.
     */
    fn flowThrough(&mut self, node: Node, input: L) -> L;

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
    fn analyze(&mut self);

    /**
     * Gets the state of the initial estimation at each node.
     *
     * @return Initial state.
     */
    fn createInitialEstimateLattice(&mut self) -> L;

    /**
     * Gets the incoming state of the entry node.
     *
     * @return Entry state.
     */
    fn createEntryLattice(&mut self) -> L;

    /** Initializes the work list and the control flow graph. */
    fn initialize(&mut self);

    /**
     * Performs a single flow through a node.
     *
     * @return {@code true} if the flow state differs from the previous state.
     */
    fn flow(&mut self, node: Node<'ast>) -> bool;

    /**
     * Computes the new flow state at a given node's entry by merging the output (input) lattice of
     * the node's predecessor (successor).
     *
     * @param node Node to compute new join.
     */
    fn joinInputs(&mut self, node: Node<'ast>);

    fn getInputFromEdge(&self, edge: EdgeIndex) -> L;
}

impl<'ast, L, J, B> D<'ast, L, J, B> for DataFlowAnalysis<'ast, L, J, B>
where
    L: Annotation + PartialEq,
    J: FlowJoiner<L>,
    B: FlowBrancher<L>,
{
    fn get_cfg(&mut self) -> &mut ControlFlowGraph<'ast, LinearFlowState<L>, L> {
        &mut self.cfg
    }

    /**
     * Checks whether the analysis is a forward flow analysis or backward flow analysis.
     *
     * @return {@code true} if it is a forward analysis.
     */
    fn isForward(&self) -> bool {
        todo!()
    }

    fn join(&mut self, latticeA: L, latticeB: L) -> L {
        let joiner = self.createFlowJoiner();
        joiner.joinFlow(latticeA);
        joiner.joinFlow(latticeB);
        joiner.finish()
    }

    /**
     * Gets a new joiner for an analysis step.
     *
     * <p>The joiner is invoked once for each input edge and then the final joined result is
     * retrieved. No joiner will be created for a single input.
     */
    fn createFlowJoiner(&self) -> J {
        todo!()
    }

    /**
     * Gets a new brancher for an analysis step.
     *
     * <p>The brancher is invoked for each output edge. The returned states are annotated onto each
     * edge and compared against the previous state to determine whether a fixed-point has been
     * reached.
     *
     * @param node The node at which to branch.
     * @param output The output state of {@link #flowThrough} at node.
     */
    fn createFlowBrancher(&mut self, node: Node, output: L) -> B {
        // throw new UnsupportedOperationException();
        panic!();
    }

    /**
     * Computes the output state for a given node given its input state.
     *
     * @param node The node.
     * @param input Input lattice that should be read-only.
     * @return Output lattice.
     */
    fn flowThrough(&mut self, node: Node, input: L) -> L {
        todo!()
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
    fn analyze(&mut self) {
        self.initialize();
        while let Some(curNode) = self.work_queue.remove_first() {
            let mut curState = self.get_node_annotation(curNode);
            curState.stepCount += 1;
            if curState.stepCount > MAX_STEPS_PER_NODE {
                panic!("Dataflow analysis appears to diverge around: {:?}", curNode);
            }

            self.joinInputs(curNode);
            if self.flow(curNode) {
                todo!();
                // If there is a change in the current node, we want to grab the list
                // of nodes that this node affects.
                // List<? extends DiGraphNode<N, Branch>> nextNodes =
                //     isForward() ? cfg.getDirectedSuccNodes(curNode) : cfg.getDirectedPredNodes(curNode);

                // for (DiGraphNode<N, Branch> nextNode : nextNodes) {
                //   if (nextNode != cfg.getImplicitReturn()) {
                //     work_queue.add(nextNode);
                //   }
                // }
            }
        }
        if self.isForward() {
            self.joinInputs(self.cfg.implicit_return);
        }
    }

    /**
     * Gets the state of the initial estimation at each node.
     *
     * @return Initial state.
     */
    fn createInitialEstimateLattice(&mut self) -> L {
        todo!()
    }

    /**
     * Gets the incoming state of the entry node.
     *
     * @return Entry state.
     */
    fn createEntryLattice(&mut self) -> L {
        todo!()
    }

    /** Initializes the work list and the control flow graph. */
    fn initialize(&mut self) {
        todo!();
        // TODO(user): Calling clear doesn't deallocate the memory in a
        // LinkedHashSet. Consider creating a new work set if we plan to repeatedly
        // call analyze.
        self.work_queue.clear();
        // for (DiGraphNode<N, Branch> node : cfg.getNodes()) {
        //   node.setAnnotation(
        //       new LinearFlowState<>(createInitialEstimateLattice(), createInitialEstimateLattice()));
        //   if node != cfg.getImplicitReturn() {
        //     work_queue.add(node);
        //   }
        // }
        if self.isBranched() {
            //   for (DiGraphEdge<N, Branch> edge : cfg.getEdges()) {
            //     edge.setAnnotation(self.createInitialEstimateLattice());
            //   }
        }
    }

    /**
     * Performs a single flow through a node.
     *
     * @return {@code true} if the flow state differs from the previous state.
     */
    fn flow(&mut self, node: Node<'ast>) -> bool {
        let mut state = self.get_node_annotation(node);
        if self.isForward() {
            let outBefore = state.out;
            state.out = self.flowThrough(node, state._in);
            let mut changed = outBefore != state.out;

            if self.isBranched() {
                let mut brancher = self.createFlowBrancher(node, state.out);
                // TODO: this jank is only here to please the borrow checker:
                let iter = self.cfg.edges_from(node).map(|e| (e.id().clone(), e.weight().clone())).collect::<Vec<_>>();
                for (edge_index, branch) in iter {
                    let edge_index = edge_index;
                    // let outBranchBefore = self.get_edge_annotation(edge_index);
                    let new_annotation = brancher.branchFlow(branch);
                    let updated = self.set_edge_annotation(edge_index, &new_annotation);
                    if !changed {
                        // changed = outBranchBefore != new_annotation;
                        changed = updated;
                    }
                }
            }

            changed
        } else {
            let inBefore = state._in;
            state._in = self.flowThrough(node, state.out);
            inBefore != state._in
        }
    }

    /**
     * Computes the new flow state at a given node's entry by merging the output (input) lattice of
     * the node's predecessor (successor).
     *
     * @param node Node to compute new join.
     */
    fn joinInputs(&mut self, node: Node<'ast>) {
        let mut state = self.get_node_annotation(node);
        if self.isForward() && self.cfg.entry == node {
            state._in = self.createEntryLattice();
            return;
        }

        // TODO: the collect() could probably be avoided; we're only using it here for it's len().
        let in_edges = if self.isForward() {
            self.cfg.edges_to(node).collect::<Vec<_>>()
        } else {
            self.cfg.edges_from(node).collect::<Vec<_>>()
        };

        let result = match in_edges.len() {
            0 => return,
            1 => self.getInputFromEdge(in_edges[0].id()),
            _ => {
                let joiner = self.createFlowJoiner();
                for inEdge in in_edges {
                    joiner.joinFlow(self.getInputFromEdge(inEdge.id()));
                }
                joiner.finish()
            }
        };

        if self.isForward() {
            state._in = result;
        } else {
            state.out = result;
        }

        todo!();
    }

    fn getInputFromEdge(&self, edge: EdgeIndex) -> L {
        todo!();
        // if (this.isBranched()) {
        //   return edge.getAnnotation();
        // } else if (this.isForward()) {
        //   LinearFlowState<L> state = edge.getSource().getAnnotation();
        //   return state.getOut();
        // } else {
        //   DiGraphNode<N, Branch> node = edge.getDestination();
        //   if (node == this.cfg.getImplicitReturn()) {
        //     return this.createEntryLattice();
        //   }
        //   LinearFlowState<L> state = node.getAnnotation();
        //   return state.getIn();
        // }
    }
}

/** The in and out states of a node. */
pub struct LinearFlowState<L> {
    pub stepCount: u16,
    pub _in: L,
    pub out: L,
}

impl<L> LinearFlowState<L> {
    fn new(_in: L, out: L) -> Self {
        // checkNotNull(in);
        // checkNotNull(out);
        Self {
            _in,
            out,
            stepCount: 0,
        }
    }
}

impl<L> Annotation for LinearFlowState<L> {}

/// A reducer that joins flow states from distinct input states into a single input state.
pub trait FlowJoiner<L> {
    fn joinFlow(&self, input: L);

    fn finish(&self) -> L;
}

/// A callback that branches a flow state into a distinct state for each output edge.
pub trait FlowBrancher<L> {
    fn branchFlow(&mut self, branch: Branch) -> L;
}

struct UniqueQueue<T: Hash + Eq + Copy> {
    seen: FxHashSet<T>,
    queue: VecDeque<T>,
}

impl<T> UniqueQueue<T>
where
    T: Hash + Eq + Copy,
{
    // UniqueQueue(@Nullable Comparator<T> priority) {
    //     this.queue = (priority == null) ? new ArrayDeque<>() : new PriorityQueue<>(priority);
    //   }

    pub fn new() -> Self {
        Self {
            seen: FxHashSet::default(),
            queue: VecDeque::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            seen: FxHashSet::with_capacity_and_hasher(capacity, Default::default()),
            queue: VecDeque::with_capacity(capacity),
        }
    }

    pub fn reserve(&mut self, additional: usize) {
        self.seen.reserve(additional);
        self.queue.reserve(additional);
    }

    pub fn is_empty(&self) -> bool {
        return self.queue.is_empty();
    }

    pub fn remove_first(&mut self) -> Option<T> {
        let t = self.queue.pop_front();
        if let Some(t) = &t {
            self.seen.remove(t);
        }
        t
    }

    pub fn add(&mut self, t: T) {
        if self.seen.insert(t) {
            self.queue.push_back(t);
        }
    }

    pub fn clear(&mut self) {
        self.seen.clear();
        self.queue.clear();
    }
}
