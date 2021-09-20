use ast::*;
use fxhash::FxHashMap;
use global_common::Spanned;
use petgraph::{Directed, EdgeDirection::Incoming, dot::Dot, graph::{DiGraph, EdgeIndex, Edges, NodeIndex}, visit::EdgeRef};
use std::fmt;
use std::hash::Hash;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Node<'ast> {
    ImplicitReturn,
    Block(&'ast BlockStmt),
    Module(&'ast Module),
    Script(&'ast Script),
    Fn(&'ast Function),
    Try(&'ast TryStmt),
    While(&'ast WhileStmt),
    If(&'ast IfStmt),
    For(&'ast ForStmt),
    ForIn(&'ast ForInStmt),
    Switch(&'ast SwitchStmt),
    Case(&'ast SwitchCase),
    Catch(&'ast CatchClause),
    With(&'ast WithStmt),
    Empty(&'ast EmptyStmt),
    Debugger(&'ast DebuggerStmt),
    Return(&'ast ReturnStmt),
    Labeled(&'ast LabeledStmt),
    Break(&'ast BreakStmt),
    Continue(&'ast ContinueStmt),
    Throw(&'ast ThrowStmt),
    DoWhile(&'ast DoWhileStmt),
    ForOf(&'ast ForOfStmt),
    Decl(&'ast Decl),
    ExprStmt(&'ast ExprStmt),
    Expr(&'ast Expr),
    VarDecl(&'ast VarDecl),
    // FnDecl(&'ast FnDecl)
    // Class(&'ast Class),
    // TODO: I think it would be simpler if constructor was represented the same
    // as a member function in the AST. Investigate why this is not the case already.
    Constructor(&'ast Constructor),
    ArrowExpr(&'ast ArrowExpr),
}

impl fmt::Debug for Node<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (name, span) = match self {
            Node::Block(n) => ("BlockStmt", Some(n.span())),
            Node::Module(n) => ("Module", Some(n.span())),
            Node::Script(n) => ("Script", Some(n.span())),
            Node::Fn(n) => ("Function", Some(n.span())),
            Node::Try(n) => ("TryStmt", Some(n.span())),
            Node::While(n) => ("WhileStmt", Some(n.span())),
            Node::If(n) => ("IfStmt", Some(n.span())),
            Node::For(n) => ("ForStmt", Some(n.span())),
            Node::ForIn(n) => ("ForInStmt", Some(n.span())),
            Node::Switch(n) => ("SwitchStmt", Some(n.span())),
            Node::Case(n) => ("SwitchCase", Some(n.span())),
            Node::Catch(n) => ("CatchClause", Some(n.span())),
            Node::With(n) => ("WithStmt", Some(n.span())),
            Node::Empty(n) => ("EmptyStmt", Some(n.span())),
            Node::Debugger(n) => ("DebuggerStmt", Some(n.span())),
            Node::Return(n) => ("ReturnStmt", Some(n.span())),
            Node::Labeled(n) => ("LabeledStmt", Some(n.span())),
            Node::Break(n) => ("BreakStmt", Some(n.span())),
            Node::Continue(n) => ("ContinueStmt", Some(n.span())),
            Node::Throw(n) => ("ThrowStmt", Some(n.span())),
            Node::DoWhile(n) => ("DoWhileStmt", Some(n.span())),
            Node::ForOf(n) => ("ForOfStmt", Some(n.span())),
            Node::ExprStmt(n) => ("ExprStmt", Some(n.span())),
            Node::Decl(d) => {
                let name = match d {
                    Decl::Class(..) => "Decl(ClassDecl)",
                    Decl::Fn(..) => "Decl(FnDecl)",
                    Decl::Var(..) => "Decl(VarDecl)",
                    Decl::TsInterface(..) => "Decl(TsInterfaceDecl)",
                    Decl::TsTypeAlias(..) => "Decl(TsTypeAliasDecl)",
                    Decl::TsEnum(..) => "Decl(TsEnumDecl)",
                    Decl::TsModule(..) => "Decl(TsModuleDecl)",
                };

                (name, Some(d.span()))
            }
            Node::Expr(n) => {
                let r = match n {
                    Expr::Ident(i) => {
                        // f.write_fmt(format_args!("(Ident('{}'), {:?})", i.sym, n.span()))
                        f.write_fmt(format_args!("Ident('{}')", i.sym))
                    }
                    Expr::Lit(l) => {
                        let v = match l {
                            Lit::Str(l) => format!("{:?}", l.value),
                            Lit::Bool(l) => format!("{:?}", l.value),
                            Lit::Null(_) => "Null".into(),
                            Lit::Num(l) => format!("{:?}", l.value),
                            Lit::BigInt(l) => format!("{:?}", l.value),
                            Lit::Regex(_) => "Regex".into(),
                            Lit::JSXText(l) => format!("{:?}", l.value),
                        };
                        // f.write_fmt(format_args!("(Lit({:?}), {:?})", v, n.span()))
                        f.write_fmt(format_args!("Lit({:?})", v))
                    }
                    _ => {
                        // f.write_fmt(format_args!("(Expr, {:?})", n.span()))
                        f.write_fmt(format_args!("Expr"))
                    }
                };
                return r;
            }
            Node::ImplicitReturn => ("implicit_return", None),
            Node::VarDecl(n) => ("VarDecl", Some(n.span())),
            // Node::FnDecl(n) => ("FnDecl", Some(n.span())),
            // Node::Class(n) => ("Class", Some(n.span())),
            Node::Constructor(n) => ("Constructor", Some(n.span())),
            Node::ArrowExpr(n) => ("ArrowExpr", Some(n.span())),
        };
        // f.write_fmt(format_args!("({}, {:?})", name, span))
        f.write_fmt(format_args!("{}", name))
    }
}

impl<'ast> From<&'ast BlockStmtOrExpr> for Node<'ast> {
    fn from(other: &'ast BlockStmtOrExpr) -> Self {
        match other {
            BlockStmtOrExpr::BlockStmt(n) => Node::Block(n),
            BlockStmtOrExpr::Expr(n) => Node::Expr(&**n),
        }
    }
}

impl<'ast> From<&'ast Stmt> for Node<'ast> {
    fn from(stmt: &'ast Stmt) -> Self {
        match stmt {
            Stmt::Block(s) => Self::Block(s),
            Stmt::Empty(s) => Self::Empty(s),
            Stmt::Debugger(s) => Self::Debugger(s),
            Stmt::With(s) => Self::With(s),
            Stmt::Return(s) => Self::Return(s),
            Stmt::Labeled(s) => Self::Labeled(s),
            Stmt::Break(s) => Self::Break(s),
            Stmt::Continue(s) => Self::Continue(s),
            Stmt::If(s) => Self::If(s),
            Stmt::Switch(s) => Self::Switch(s),
            Stmt::Throw(s) => Self::Throw(s),
            Stmt::Try(s) => Self::Try(s),
            Stmt::While(s) => Self::While(s),
            Stmt::DoWhile(s) => Self::DoWhile(s),
            Stmt::For(s) => Self::For(s),
            Stmt::ForIn(s) => Self::ForIn(s),
            Stmt::ForOf(s) => Self::ForOf(s),
            Stmt::Decl(s) => Self::Decl(s),
            Stmt::Expr(s) => Self::ExprStmt(s),
        }
    }
}

impl<'ast> From<Node<'ast>> for AstNode<'ast> {
    fn from(node: Node<'ast>) -> Self {
        match node {
            Node::ImplicitReturn => unreachable!(),
            Node::Block(n) => AstNode::from(n),
            Node::Module(n) => AstNode::from(n),
            Node::Script(n) => AstNode::from(n),
            Node::Fn(n) => AstNode::from(n),
            Node::Try(n) => AstNode::from(n),
            Node::While(n) => AstNode::from(n),
            Node::If(n) => AstNode::from(n),
            Node::For(n) => AstNode::from(n),
            Node::ForIn(n) => AstNode::from(n),
            Node::Switch(n) => AstNode::from(n),
            Node::Case(n) => AstNode::from(n),
            Node::Catch(n) => AstNode::from(n),
            Node::With(n) => AstNode::from(n),
            Node::Empty(n) => AstNode::from(n),
            Node::Debugger(n) => AstNode::from(n),
            Node::Return(n) => AstNode::from(n),
            Node::Labeled(n) => AstNode::from(n),
            Node::Break(n) => AstNode::from(n),
            Node::Continue(n) => AstNode::from(n),
            Node::Throw(n) => AstNode::from(n),
            Node::DoWhile(n) => AstNode::from(n),
            Node::ForOf(n) => AstNode::from(n),
            Node::Decl(n) => AstNode::from(n),
            Node::ExprStmt(n) => AstNode::from(n),
            Node::Expr(n) => AstNode::from(n),
            Node::VarDecl(n) => AstNode::from(n),
            // Node::FnDecl(n) => AstNode::from(n)
            // Node::Class(n) => AstNode::from(n),
            Node::Constructor(n) => AstNode::from(n),
            Node::ArrowExpr(n) => AstNode::from(n),
        }
    }
}

/**
 * Gets the condition of an ON_TRUE / ON_FALSE CFG edge.
 *
 * @param n a node with an outgoing conditional CFG edge
 * @return the condition node or null if the condition is not obviously a node
 */
pub fn get_condition_expression<'ast>(n: Node<'ast>) -> Option<Node<'ast>> {
    match n {
        Node::If(n) => Some(Node::Expr(&*n.test)),
        Node::While(n) => Some(Node::Expr(&*n.test)),
        Node::DoWhile(n) => Some(Node::Expr(&*n.test)),
        Node::For(n) => n.test.as_ref().map(|test| Node::Expr(&*test)),
        Node::ForIn(..) | Node::ForOf(..) | Node::Case(..) => None,
        _ => panic!("Node does not have a condition."),
    }
}

/**
 * @return True if n should be represented by a new CFG node in the control
 * flow graph.
 */
pub fn is_entering_new_cfg_node<'ast>(n: AstNode<'ast>, parent: AstNode<'ast>) -> bool {
    match parent {
        // TODO: what about Node::Module?
        AstNode::BlockStmt(_) | AstNode::Script(_) | AstNode::TryStmt(_) => true,
        AstNode::Function(f) => {
            // A function node represents the start of a function where the name
            // bleeds into the local scope and parameters are assigned
            // to the formal argument names. The node includes the name of the
            // function and the PARAM_LIST since we assume the whole set up process
            // is atomic without change in control flow. The next change of
            // control is going into the function's body, represented by the second
            // child.
            Some(n) != f.body.as_ref().map(|s| AstNode::BlockStmt(s))
        }
        // AstNode::WhileStmt(_) | AstNode::DoWhileStmt(_) | AstNode::IfStmt(_) => {
        //     // These control structures are represented by a node that holds the
        //     // condition. Each of them is a branch node based on its condition.
        //     getConditionExpression(parent) != Some(n)
        // }
        AstNode::WhileStmt(WhileStmt { test, .. })
        | AstNode::DoWhileStmt(DoWhileStmt { test, .. })
        | AstNode::IfStmt(IfStmt { test, .. }) => {
            // These control structures are represented by a node that holds the
            // condition. Each of them is a branch node based on its condition.
            AstNode::from(&**test) != n
        }
        // AstNode::ForStmt(_) => {
        //     // The FOR(;;) node differs from other control structures in that
        //     // it has an initialization and an increment statement. Those
        //     // two statements have corresponding CFG nodes to represent them.
        //     // The FOR node only represents the condition check for each iteration.
        //     // That way the following:
        //     // for(var x = 0; x < 10; x++) { } has a graph that is isomorphic to
        //     // var x = 0; while(x<10) {  x++; }
        //     getConditionExpression(parent) != Some(n)
        // }
        AstNode::ForStmt(ForStmt { test, .. }) => {
            // The FOR(;;) node differs from other control structures in that
            // it has an initialization and an increment statement. Those
            // two statements have corresponding CFG nodes to represent them.
            // The FOR node only represents the condition check for each iteration.
            // That way the following:
            // for(var x = 0; x < 10; x++) { } has a graph that is isomorphic to
            // var x = 0; while(x<10) {  x++; }
            test.as_ref().map(|test| AstNode::from(&**test)) != Some(n)
        }
        AstNode::ForInStmt(f) => {
            // TODO(user): Investigate how we should handle the case where
            // we have a very complex expression inside the FOR-IN header.
            // n != AstNode::from(&f.left)
            todo!()
        }
        AstNode::SwitchStmt(s) => n != AstNode::from(&*s.discriminant),
        AstNode::SwitchCase(c) => match &c.test {
            Some(test) => n != AstNode::from(&**test),
            None => false,
        },
        AstNode::CatchClause(c) => match &c.param {
            Some(param) => {
                // n != AstNode::from(&param)
                todo!()
            }
            None => false,
        },
        AstNode::WithStmt(w) => n != AstNode::from(&*w.obj),
        _ => false,
    }
}

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

// TODO:
pub struct DummyAnnotation;
impl Annotation for DummyAnnotation {}

pub trait Annotation {}

/**
 * Control flow graph.
 *
 * @param <N> The instruction type of the control flow graph.
 */
pub struct ControlFlowGraph<'ast, NA: Annotation, EA: Annotation> {
    map: FxHashMap<Node<'ast>, NodeIndex>,
    /**
     * A special node marked by the node value key null to a singleton
     * "return" when control is transferred outside of the current control flow
     * graph.
     */
    pub implicit_return: Node<'ast>,
    pub entry: Node<'ast>,
    graph: DiGraph<Node<'ast>, Branch>,
    node_annotations: FxHashMap<Node<'ast>, NA>,
    edge_annotations: FxHashMap<EdgeIndex, EA>,
}

impl<'ast, NA, EA> ControlFlowGraph<'ast, NA, EA>
where
    NA: Annotation,
    EA: Annotation,
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

    pub fn create_node(&mut self, value: Node<'ast>) -> NodeIndex {
        match self.map.get(&value) {
            Some(index) => *index,
            None => {
                let index = self.graph.add_node(value);

                self.map.insert(value, index);

                index
            }
        }
    }

    // pub fn get_node_annotation(&mut self, node: Node<'ast>) -> NA {
    //     todo!();
    // }

    // pub fn set_node_annotation(&mut self, node: Node<'ast>, annotation: &NA) {
    //     todo!();
    // }

    // pub fn get_edge_annotation(&mut self, edge: EdgeIndex) -> EA {
    //     todo!();
    // }

    // pub fn set_edge_annotation(&mut self, edge: EdgeIndex, annotation: &EA) {
    //     todo!();
    // }
    

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
    pub fn is_connected(&self, n1: NodeIndex, edge: Branch, n2: NodeIndex) -> bool {
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
    pub fn connect_if_not_found(&mut self, n1: NodeIndex, edge: Branch, n2: NodeIndex) {
        if !self.is_connected(n1, edge, n2) {
            self.graph.add_edge(n1, n2, edge);
        }
    }

    pub fn edges_from(&self, node: Node) -> Edges<'_, Branch, Directed> {
        let index = match self.map.get(&node) {
            Some(i) => *i,
            // Note: We use ::end() as a sentinel value here. It should never be
            // assigned to a node, and passing an unused index to graph.edges()
            // conveniently returns an empty iterator, saving us from
            // constructing one manually.
            None => NodeIndex::end(),
        };
        self.graph.edges(index)
    }

    pub fn edges_to(&self, node: Node) -> Edges<'_, Branch, Directed> {
        let index = match self.map.get(&node) {
            Some(i) => *i,
            // Note: We use ::end() as a sentinel value here. It should never be
            // assigned to a node, and passing an unused index to
            // graph.edges_directed() conveniently returns an empty iterator,
            // saving us from constructing one manually.
            None => NodeIndex::end(),
        };
        self.graph.edges_directed(index, Incoming)
    }

    pub fn print_simplified(&self) {
        let mut dot = format!(
            "{:?}",
            Dot::with_config(&self.graph, &[/*Config::NodeIndexLabel*/])
        );

        dot = recolour_graph(dot, self.graph.node_count());

        std::fs::write("cfg.dot", dot).expect("Failed to output control flow graph");
    }

    pub fn print_full(&self) {
        let root = &AstNode::from(self.entry);

        enum CombineNode<'ast> {
            AstNode(AstNode<'ast>),
            ImplicitReturn,
        }

        impl<'ast> fmt::Debug for CombineNode<'ast> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    CombineNode::AstNode(n) => match n {
                        AstNode::Str(s) => f.write_fmt(format_args!("Str({})", s.value)),
                        AstNode::Number(n) => f.write_fmt(format_args!("Number({})", n.value)),
                        AstNode::Ident(s) => f.write_fmt(format_args!("Ident({})", s.sym)),
                        _ => f.write_fmt(format_args!("{:?}", n)),
                    },
                    CombineNode::ImplicitReturn => f.write_str("implicit_return"),
                }
            }
        }

        impl<'ast> From<AstNode<'ast>> for CombineNode<'ast> {
            fn from(other: AstNode<'ast>) -> Self {
                CombineNode::AstNode(other)
            }
        }

        impl<'ast> From<Node<'ast>> for CombineNode<'ast> {
            fn from(other: Node<'ast>) -> Self {
                match other {
                    Node::ImplicitReturn => CombineNode::ImplicitReturn,
                    Node::Block(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Module(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Script(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Fn(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Try(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::While(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::If(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::For(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::ForIn(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Switch(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Case(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Catch(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::With(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Empty(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Debugger(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Return(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Labeled(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Break(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Continue(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Throw(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::DoWhile(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::ForOf(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Decl(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::ExprStmt(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Expr(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::VarDecl(n) => CombineNode::AstNode(AstNode::from(n)),
                    // Node::FnDecl(n) => CombineNode::AstNode(AstNode::from(n))
                    // Node::Class(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::Constructor(n) => CombineNode::AstNode(AstNode::from(n)),
                    Node::ArrowExpr(n) => CombineNode::AstNode(AstNode::from(n)),
                }
            }
        }

        let (mut map, graph) = crate::print::ast_graph::<Branch>(root);

        let mut graph = graph.map(|_, n| CombineNode::AstNode(*n), |_, e| *e);

        let mut implicit_return = None;

        for n in self.graph.node_weights() {
            if let Node::ImplicitReturn = n {
                implicit_return = Some(graph.add_node(CombineNode::ImplicitReturn));
            } else {
                let ast_node = AstNode::from(*n);
                if let None = map.get(&ast_node) {
                    let index = graph.add_node(CombineNode::AstNode(ast_node));
                    map.insert(ast_node, index);
                }
            }
        }

        for e in self.graph.edge_references() {
            let source = self.graph[e.source()];
            let source = if let Node::ImplicitReturn = source {
                implicit_return.unwrap()
            } else {
                map[&AstNode::from(self.graph[e.source()])]
            };
            let target = self.graph[e.target()];
            let target = if let Node::ImplicitReturn = target {
                implicit_return.unwrap()
            } else {
                map[&AstNode::from(self.graph[e.target()])]
            };
            graph.add_edge(source, target, crate::print::Edge(Some(*e.weight())));
        }

        let mut dot = format!(
            "{:?}",
            Dot::with_config(&graph, &[/*Config::NodeIndexLabel*/])
        );

        dot = recolour_graph(dot, graph.node_count());

        std::fs::write("cfg.dot", dot).expect("Failed to output control flow graph");
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
