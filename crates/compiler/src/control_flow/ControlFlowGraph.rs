use super::node::Node;
use ast::*;
use petgraph::{
    dot::Dot,
    graph::{DiGraph, EdgeIndex, Neighbors, NodeIndex},
    visit::EdgeRef,
    EdgeDirection::Outgoing,
};
use rustc_hash::FxHashMap;
use std::fmt;

// TODO: account for other function like types (such as methods/getters etc)

// TODO:
pub struct DummyAnnotation;
impl Annotation for DummyAnnotation {}

pub trait Annotation {}

/**
 * Control flow graph.
 *
 * @param <N> The instruction type of the control flow graph.
 */
pub struct ControlFlowGraph<'ast, N: Annotation, E: Annotation> {
    pub(super) map: FxHashMap<Node<'ast>, NodeIndex>,
    /**
     * A special node marked by the node value key null to a singleton
     * "return" when control is transferred outside of the current control flow
     * graph.
     */
    pub implicit_return: Node<'ast>,
    pub entry: Node<'ast>,
    pub(super) graph: DiGraph<Node<'ast>, Branch>,
    node_annotations: FxHashMap<Node<'ast>, N>,
    edge_annotations: FxHashMap<EdgeIndex, E>,
}

impl<'ast, N, E> ControlFlowGraph<'ast, N, E>
where
    N: Annotation,
    E: Annotation,
{
    pub fn new(entry: Node<'ast>) -> Self {
        let mut graph = DiGraph::<Node<'ast>, Branch>::new();
        let implicit_return = Node::ImplicitReturn;
        let implicit_return_index = graph.add_node(implicit_return);
        let entry_index = graph.add_node(entry);

        let mut map = FxHashMap::with_capacity_and_hasher(2, Default::default());
        map.insert(implicit_return, implicit_return_index);
        map.insert(entry, entry_index);

        Self {
            implicit_return,
            entry,
            graph,
            map,
            node_annotations: Default::default(),
            edge_annotations: Default::default(),
        }
    }

    fn create_node(&mut self, value: Node<'ast>) -> NodeIndex {
        match self.map.get(&value) {
            Some(index) => *index,
            None => {
                let index = self.graph.add_node(value);

                self.map.insert(value, index);

                index
            }
        }
    }

    /**
     * Connects the two nodes in the control flow graph.
     *
     * @param fromNode Source.
     * @param toNode Destination.
     */
    pub fn create_edge(&mut self, from: Node<'ast>, branch: Branch, to: Node<'ast>) {
        let from_node = self.create_node(from);
        let to_node = self.create_node(to);
        self.connect_if_not_found(from_node, branch, to_node);
    }

    /**
     * Checks whether two nodes in the graph are connected by the given
     * edge type.
     *
     * @param n1 Node 1.
     * @param e The edge type.
     * @param n2 Node 2.
     */
    fn is_connected(&self, n1: NodeIndex, edge: Branch, n2: NodeIndex) -> bool {
        self.graph
            .edges_connecting(n1, n2)
            .any(|e| *e.weight() == edge)
    }

    /**
     * Connects two nodes in the graph with an edge if such edge does not already
     * exists between the nodes.
     *
     * @param n1 First node.
     * @param edge The edge.
     * @param n2 Second node.
     */
    fn connect_if_not_found(&mut self, n1: NodeIndex, edge: Branch, n2: NodeIndex) {
        if !self.is_connected(n1, edge, n2) {
            self.graph.add_edge(n1, n2, edge);
        }
    }

    // pub fn edges_from(&self, node: Node) -> Edges<'_, Branch, Directed> {
    //     let index = match self.map.get(&node) {
    //         Some(i) => *i,
    //         // Note: We use ::end() as a sentinel value here. It should never be
    //         // assigned to a node, and passing an unused index to graph.edges()
    //         // conveniently returns an empty iterator, saving us from
    //         // constructing one manually.
    //         None => NodeIndex::end(),
    //     };
    //     self.graph.edges(index)
    // }

    // pub fn edges_to(&self, node: Node) -> Edges<'_, Branch, Directed> {
    //     let index = match self.map.get(&node) {
    //         Some(i) => *i,
    //         // Note: We use ::end() as a sentinel value here. It should never be
    //         // assigned to a node, and passing an unused index to
    //         // graph.edges_directed() conveniently returns an empty iterator,
    //         // saving us from constructing one manually.
    //         None => NodeIndex::end(),
    //     };
    //     self.graph.edges_directed(index, Incoming)
    // }

    /// Note: neighbor are listed in reverse order of their addition to the graph,
    /// so the most recently added edge's neighbor is listed first.
    pub fn getDirectedSuccNodes(&self, node: Node<'ast>) -> Neighbors<'_, Branch> {
        self.graph
            .neighbors_directed(*self.map.get(&node).unwrap(), Outgoing)
    }
}

/**
 * The edge object for the control flow graph.
 */
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Branch {
    /** Edge is taken if the condition is true. */
    ON_TRUE,
    /** Edge is taken if the condition is false. */
    ON_FALSE,
    /** Unconditional branch. */
    UNCOND,
    /**
     * Exception-handling code paths.
     * Conflates two kind of control flow passing:
     * - An exception is thrown, and falls into a catch or finally block
     * - During exception handling, a finally block finishes and control
     *   passes to the next finally block.
     * In theory, we need 2 different edge types. In practice, we
     * can just treat them as "the edges we can't really optimize".
     */
    ON_EX,
}

/**
 * @return True if n should be represented by a new CFG node in the control
 * flow graph.
 */
pub fn is_entering_new_cfg_node<'ast>(n: Node<'ast>, parent: Node<'ast>) -> bool {
    match parent {
        // TODO: what about Node::Module?
        Node::BlockStmt(_) | Node::Script(_) | Node::TryStmt(_) => true,
        Node::Function(f) => {
            // A function node represents the start of a function where the name
            // bleeds into the local scope and parameters are assigned
            // to the formal argument names. The node includes the name of the
            // function and the PARAM_LIST since we assume the whole set up process
            // is atomic without change in control flow. The next change of
            // control is going into the function's body, represented by the second
            // child.
            Some(n) != f.body.as_ref().map(|s| Node::BlockStmt(s))
        }
        // Node::WhileStmt(_) | Node::DoWhileStmt(_) | Node::IfStmt(_) => {
        //     // These control structures are represented by a node that holds the
        //     // condition. Each of them is a branch node based on its condition.
        //     getConditionExpression(parent) != Some(n)
        // }
        Node::WhileStmt(WhileStmt { test, .. })
        | Node::DoWhileStmt(DoWhileStmt { test, .. })
        | Node::IfStmt(IfStmt { test, .. }) => {
            // These control structures are represented by a node that holds the
            // condition. Each of them is a branch node based on its condition.
            Node::from(&**test) != n
        }
        // Node::ForStmt(_) => {
        //     // The FOR(;;) node differs from other control structures in that
        //     // it has an initialization and an increment statement. Those
        //     // two statements have corresponding CFG nodes to represent them.
        //     // The FOR node only represents the condition check for each iteration.
        //     // That way the following:
        //     // for(var x = 0; x < 10; x++) { } has a graph that is isomorphic to
        //     // var x = 0; while(x<10) {  x++; }
        //     getConditionExpression(parent) != Some(n)
        // }
        Node::ForStmt(ForStmt { test, .. }) => {
            // The FOR(;;) node differs from other control structures in that
            // it has an initialization and an increment statement. Those
            // two statements have corresponding CFG nodes to represent them.
            // The FOR node only represents the condition check for each iteration.
            // That way the following:
            // for(var x = 0; x < 10; x++) { } has a graph that is isomorphic to
            // var x = 0; while(x<10) {  x++; }
            test.as_ref().map(|test| Node::from(&**test)) != Some(n)
        }
        Node::ForInStmt(f) => {
            // TODO(user): Investigate how we should handle the case where
            // we have a very complex expression inside the FOR-IN header.
            // n != Node::from(&f.left)
            todo!()
        }
        Node::SwitchStmt(s) => n != Node::from(&*s.discriminant),
        Node::SwitchCase(c) => match &c.test {
            Some(test) => n != Node::from(&**test),
            None => false,
        },
        Node::CatchClause(c) => match &c.param {
            Some(param) => {
                // n != Node::from(&param)
                todo!()
            }
            None => false,
        },
        Node::WithStmt(w) => n != Node::from(&*w.obj),
        _ => false,
    }
}

impl<'ast, N, E> ControlFlowGraph<'ast, N, E>
where
    N: Annotation,
    E: Annotation,
{
    pub fn print_simplified(&self) {
        let mut dot = format!("{:?}", Dot::with_config(&self.graph, &[]));

        dot = recolour_graph(dot, self.graph.node_count());

        std::fs::write("cfg.dot", dot).expect("Failed to output control flow graph");
    }

    pub fn print_full(&self) {
        // Only used for custom debug impl.
        struct CustomNode<'ast>(Node<'ast>);

        impl<'ast> fmt::Debug for CustomNode<'ast> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.0 {
                    Node::Str(s) => f.write_fmt(format_args!("Str({})", s.value)),
                    Node::Number(n) => f.write_fmt(format_args!("Number({})", n.value)),
                    Node::Ident(s) => f.write_fmt(format_args!("Ident({})", s.sym)),
                    _ => f.write_fmt(format_args!("{:?}", self.0)),
                }
            }
        }

        // Create graph of AST nodes.
        let (map, graph) = super::print::ast_graph::<Branch>(&self.entry);

        // Map the nodes of the AST graph to our custom node type. This preserves their edges and node indicies.
        let mut graph = graph.map(|_, n| CustomNode(*n), |_, e| *e);

        // Implicit return isn't a 'real' AST node, so it won't be in the AST graph. Add it manually.
        let implicit_return = graph.add_node(CustomNode(self.implicit_return));

        // Add the control flow edges to the AST graph.
        for e in self.graph.edge_references() {
            // For each node of the CFG edge, use the map to find the corresponding
            // node in the AST graph. Since the CFG nodes are a sub-set of the
            // AST nodes, this lookup should never fail.
            let source = self.graph[e.source()];
            let source = if source == self.implicit_return {
                implicit_return
            } else {
                map[&source]
            };
            let target = self.graph[e.target()];
            let target = if target == self.implicit_return {
                implicit_return
            } else {
                map[&target]
            };
            graph.add_edge(source, target, super::print::Edge(Some(*e.weight())));
        }

        let mut dot = format!("{:?}", Dot::with_config(&graph, &[]));

        dot = recolour_graph(dot, graph.node_count());

        println!("creating dot file");

        std::fs::write("cfg.dot", dot).expect("Failed to output control flow graph");
    }
}

/// Changes the colours of control flow edges so they can be visually distinguished.
fn recolour_graph(mut dot: String, node_count: usize) -> String {
    dot = dot.replacen("[ ", "[color=lightblue2, style=filled, ", node_count);

    dot = dot
        .replace(
            "[ label = \"UNCOND\" ]",
            "[label=\"UNCOND\", fontcolor=\"purple\", color=\"purple\"]",
        )
        .replace(
            "[ label = \"ON_FALSE\" ]",
            "[label=\"ON_FALSE\", fontcolor=\"orange\", color=\"orange\"]",
        )
        .replace(
            "[ label = \"ON_TRUE\" ]",
            "[label=\"ON_TRUE\", fontcolor=\"green\", color=\"green\"]",
        )
        .replace(
            "[ label = \"ON_EX\" ]",
            "[label=\"ON_EX\", fontcolor=\"red\", color=\"red\"]",
        );

    dot
}
