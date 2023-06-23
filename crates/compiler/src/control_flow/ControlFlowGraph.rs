use super::node::*;
use ast::*;
use petgraph::{
    dot::Dot,
    graph::{DiGraph, EdgeIndex, Neighbors, NodeIndex},
    visit::EdgeRef,
    EdgeDirection::*,
};
use rustc_hash::FxHashMap;
use std::fmt::{self, Write};
use std::ops::Index;

// TODO: account for other function like types (such as methods/getters etc)

// TODO:
#[derive(Debug)]
pub struct DummyAnnotation;
impl Annotation for DummyAnnotation {}

pub trait Annotation: fmt::Debug {}

/**
 * Control flow graph.
 *
 * @param <N> The instruction type of the control flow graph.
 */
#[derive(Debug)]
pub struct ControlFlowGraph<N: CfgNode, NA: Annotation, EA: Annotation> {
    pub map: FxHashMap<N, NodeIndex>,
    /**
     * A special node marked by the node value key null to a singleton
     * "return" when control is transferred outside of the current control flow
     * graph.
     */
    pub implicit_return: N,
    pub entry: N,
    pub graph: DiGraph<N, Branch>,
    pub node_annotations: FxHashMap<N, NA>,
    pub edge_annotations: FxHashMap<EdgeIndex, EA>,
}

impl<'ast, N, NA, EA> ControlFlowGraph<N, NA, EA>
where
    N: CfgNode,
    NA: Annotation,
    EA: Annotation,
{
    pub fn new(entry: N) -> Self {
        let mut graph = DiGraph::<N, Branch>::new();
        let implicit_return = N::implicit_return();
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

    fn create_node(&mut self, value: N) -> NodeIndex {
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
    pub fn create_edge(&mut self, from: N, branch: Branch, to: N) {
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
    pub fn getDirectedPredNodes(&self, node: N) -> Neighbors<'_, Branch> {
        self.graph
            .neighbors_directed(*self.map.get(&node).unwrap(), Incoming)
    }

    /// Note: neighbor are listed in reverse order of their addition to the graph,
    /// so the most recently added edge's neighbor is listed first.
    pub fn getDirectedSuccNodes(&self, node: N) -> Neighbors<'_, Branch> {
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
    match parent.kind {
        // TODO: what about NodeKind::Module?
        NodeKind::BlockStmt(_) | NodeKind::Script(_) | NodeKind::TryStmt(_) => true,
        NodeKind::Function(f) => {
            // A function node represents the start of a function where the name
            // bleeds into the local scope and parameters are assigned
            // to the formal argument names. The node includes the name of the
            // function and the PARAM_LIST since we assume the whole set up process
            // is atomic without change in control flow. The next change of
            // control is going into the function's body, represented by the second
            // child.
            Some(n.node_id) != f.body.as_ref().map(|s| s.node_id)
        }
        // NodeKind::WhileStmt(_) | NodeKind::DoWhileStmt(_) | NodeKind::IfStmt(_) => {
        //     // These control structures are represented by a node that holds the
        //     // condition. Each of them is a branch node based on its condition.
        //     getConditionExpression(parent) != Some(n)
        // }
        NodeKind::WhileStmt(WhileStmt { test, .. })
        | NodeKind::DoWhileStmt(DoWhileStmt { test, .. })
        | NodeKind::IfStmt(IfStmt { test, .. }) => {
            // These control structures are represented by a node that holds the
            // condition. Each of them is a branch node based on its condition.
            test.node_id() != n.node_id
        }
        // NodeKind::ForStmt(_) => {
        //     // The FOR(;;) node differs from other control structures in that
        //     // it has an initialization and an increment statement. Those
        //     // two statements have corresponding CFG nodes to represent them.
        //     // The FOR node only represents the condition check for each iteration.
        //     // That way the following:
        //     // for(var x = 0; x < 10; x++) { } has a graph that is isomorphic to
        //     // var x = 0; while(x<10) {  x++; }
        //     getConditionExpression(parent) != Some(n)
        // }
        NodeKind::ForStmt(ForStmt { test, .. }) => {
            // The FOR(;;) node differs from other control structures in that
            // it has an initialization and an increment statement. Those
            // two statements have corresponding CFG nodes to represent them.
            // The FOR node only represents the condition check for each iteration.
            // That way the following:
            // for(var x = 0; x < 10; x++) { } has a graph that is isomorphic to
            // var x = 0; while(x<10) {  x++; }
            test.as_ref().map(|test| test.node_id()) != Some(n.node_id)
        }
        NodeKind::ForInStmt(f) => {
            // TODO(user): Investigate how we should handle the case where
            // we have a very complex expression inside the FOR-IN header.
            // n != NodeKind::from(&f.left)
            todo!()
        }
        NodeKind::SwitchStmt(s) => n.node_id != s.discriminant.node_id(),
        NodeKind::SwitchCase(c) => match &c.test {
            Some(test) => n.node_id != test.node_id(),
            None => false,
        },
        NodeKind::CatchClause(c) => match &c.param {
            Some(param) => {
                // n != NodeKind::from(&param)
                todo!()
            }
            None => false,
        },
        NodeKind::WithStmt(w) => n.node_id != w.obj.node_id(),
        _ => false,
    }
}

/// A custom version of [`Debug`][std::fmt::Debug] that allows control flow
/// graph annotations to printed using external state.
pub trait AnnotationPrinter<A: Annotation> {
    /// Formats the annotation using the given formatter.
    fn print(&self, annotation: &A, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

/// Default [`AnnotationPrinter`] that uses the annotations impl of [`Debug`][std::fmt::Debug].
pub struct DefaultPrinter;

impl<A> AnnotationPrinter<A> for DefaultPrinter
where
    A: Annotation,
{
    fn print(&self, annotation: &A, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", annotation))
    }
}

const CFG_DOT_FILE_NAME: &str = "cfg.dot";

impl<'ast, NA, EA> ControlFlowGraph<Node<'ast>, NA, EA>
where
    NA: Annotation,
    EA: Annotation,
{
    /// Prints a simple representation of the control flow graph to dot format.
    /// The resulting graph contains only the nodes and edges from the control
    /// flow graph.
    pub fn print_simple(&self) {
        let mut dot = format!("{:?}", Dot::with_config(&self.graph, &[]));

        dot = recolour_graph(dot, self.graph.node_count());

        std::fs::write(CFG_DOT_FILE_NAME, dot).expect("Failed to output control flow graph");
    }

    pub fn print_simple_with_annotations(&self) {
        // Only used for custom debug impl.
        struct CustomNode<'ast, 'a, NA>
        where
            NA: Annotation,
        {
            node: Node<'ast>,
            annotation: Option<&'a NA>,
        }

        impl<NA> fmt::Debug for CustomNode<'_, '_, NA>
        where
            NA: Annotation,
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.node.kind {
                    NodeKind::Str(s) => {
                        f.write_fmt(format_args!("Str({})", s.value.escape_debug()))?;
                    }
                    NodeKind::Number(n) => {
                        f.write_fmt(format_args!("Number({})", n.value))?;
                    }
                    NodeKind::Ident(s) => {
                        f.write_fmt(format_args!("Ident({})", s.sym))?;
                    }
                    _ => {
                        f.write_fmt(format_args!("{:?}", self.node))?;
                    }
                }
                if let Some(ann) = self.annotation {
                    f.write_char('\n');
                    DefaultPrinter.print(ann, f)
                } else {
                    Ok(())
                }
            }
        }

        let custom_node = |node| CustomNode {
            node,
            annotation: self.node_annotations.get(&node),
        };

        let mut graph = self.graph.map(|_, n| custom_node(*n), |_, e| *e);

        let mut dot = format!("{:?}", Dot::with_config(&graph, &[]));

        dot = recolour_graph(dot, graph.node_count());

        // println!("creating dot file");

        std::fs::write(CFG_DOT_FILE_NAME, dot).expect("Failed to output control flow graph");
    }

    /// Prints a rich representation of the control flow graph to dot format.
    /// The resulting graph contains represents the full AST of the entry node,
    /// as well as any control flow edges.
    pub fn print_full(&self) {
        self.print_full_inner::<DefaultPrinter>(None, "cfg")
    }
    /// Same as `print_full` but also prints node annotations using `printer`.
    /// If `printer` is `None`, the annotations will be printed using [`Debug`][std::fmt::Debug].
    pub fn print_full_with_annotations<P>(&self, printer: Option<&P>)
    where
        P: AnnotationPrinter<NA>,
    {
        match printer {
            Some(p) => self.print_full_inner(Some(p), "cfg"),
            None => self.print_full_inner(Some(&DefaultPrinter), "cfg"),
        }
    }

    pub fn print_full_with_annotations_name(&self, name: &str) {
        self.print_full_inner::<DefaultPrinter>(Some(&DefaultPrinter), name);
    }

    /// If `printer` is `None`, node annotations are not printed.
    fn print_full_inner<P>(&self, printer: Option<&P>, name: &str)
    where
        P: AnnotationPrinter<NA>,
    {
        // Only used for custom debug impl.
        struct CustomNode<'ast, 'a, NA, P>
        where
            NA: Annotation,
            P: AnnotationPrinter<NA>,
        {
            node: Node<'ast>,
            annotation: Option<&'a NA>,
            printer: Option<&'a P>,
        }

        impl<NA, P> fmt::Debug for CustomNode<'_, '_, NA, P>
        where
            NA: Annotation,
            P: AnnotationPrinter<NA>,
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.node.kind {
                    NodeKind::Str(s) => {
                        f.write_fmt(format_args!("Str({})", s.value.escape_debug()))?;
                    }
                    NodeKind::Number(n) => {
                        f.write_fmt(format_args!("Number({})", n.value))?;
                    }
                    NodeKind::Ident(s) => {
                        f.write_fmt(format_args!("Ident({})", s.sym))?;
                    }
                    _ => {
                        f.write_fmt(format_args!("{:?}", self.node))?;
                    }
                }
                if let (Some(printer), Some(ann)) = (self.printer, self.annotation) {
                    f.write_char('\n');
                    printer.print(ann, f)
                } else {
                    Ok(())
                }
            }
        }

        let custom_node = |node| CustomNode {
            node,
            annotation: self.node_annotations.get(&node),
            printer,
        };

        // Create graph of AST nodes.
        let (map, graph) = super::print::ast_graph::<Branch>(&self.entry);

        // Map the nodes of the AST graph to our custom node type. This preserves their edges and node indicies.
        let mut graph = graph.map(|_, n| custom_node(*n), |_, e| *e);

        // Implicit return isn't a 'real' AST node, so it won't be in the AST graph. Add it manually.
        let implicit_return = graph.add_node(custom_node(self.implicit_return));

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

        // println!("creating dot file");

        std::fs::create_dir_all("cfg").expect("failed to create cfg dir");

        std::fs::write(format!("cfg/{}.dot", name), dot)
            .expect("Failed to output control flow graph");

        // std::fs::write(CFG_DOT_FILE_NAME, dot).expect("Failed to output control flow graph");
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

impl<N, NA, EA> Index<NodeIndex> for ControlFlowGraph<N, NA, EA>
where
    N: CfgNode,
    NA: Annotation,
    EA: Annotation,
{
    type Output = N;

    #[inline]
    fn index(&self, index: NodeIndex) -> &Self::Output {
        &self.graph[index]
    }
}
