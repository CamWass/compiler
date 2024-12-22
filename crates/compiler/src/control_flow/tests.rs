use crate::control_flow::ControlFlowAnalysis::ControlFlowRoot;

use super::node::{Node, NodeKind};
use super::print::ast_graph;
use super::ControlFlowAnalysis::ControlFlowAnalysis;
use super::ControlFlowGraph::{Branch, ControlFlowGraph};

use ast::NodeId;
use global_common::{
    errors::{ColorConfig, Handler},
    sync::Lrc,
    FileName, SourceMap,
};
use parser::{Parser, Syntax};
use petgraph::algo::has_path_connecting;
use petgraph::graph::{DiGraph, NodeIndex};
use rustc_hash::FxHashMap;

// TODO: tests for other function like things (e.g. methods, arrow funcs) and classes.

macro_rules! make {
    ($($field:ident,)*) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum Token {
            ImplicitReturn,
            $($field,)*
        }

        impl <'a> From<Node<'a>> for Token {
            fn from(other: Node<'a>) -> Token {
                match other.kind {
                    NodeKind::ImplicitReturn => Token::ImplicitReturn,
                    $(NodeKind::$field(_) => Token::$field,)*
                }
            }
        }
    };
}

#[derive(Clone, Copy)]
struct SimpleNode {
    node_id: NodeId,
    token: Token,
    is_default_switch_case: bool,
}

impl<'a> From<Node<'a>> for SimpleNode {
    fn from(value: Node<'a>) -> Self {
        Self {
            node_id: value.node_id,
            token: value.into(),
            is_default_switch_case: matches!(value.kind, NodeKind::SwitchCase(c) if c.is_default()),
        }
    }
}

#[derive(Clone, Copy)]
struct SimpleEdge {
    source: SimpleNode,
    weight: Branch,
    target: SimpleNode,
}

make!(
    // class
    Class,
    ExtendsClause,
    ClassProp,
    PrivateProp,
    ClassMethod,
    PrivateMethod,
    Constructor,
    // decl
    FnDecl,
    ClassDecl,
    VarDecl,
    VarDeclarator,
    // Expr
    ThisExpr,
    ArrayLit,
    ObjectLit,
    SpreadElement,
    UnaryExpr,
    UpdateExpr,
    BinExpr,
    FnExpr,
    ClassExpr,
    AssignExpr,
    MemberExpr,
    CondExpr,
    CallExpr,
    NewExpr,
    SeqExpr,
    ArrowExpr,
    YieldExpr,
    MetaPropExpr,
    AwaitExpr,
    Tpl,
    TaggedTpl,
    TplElement,
    Super,
    OptChainExpr,
    // function
    Function,
    Param,
    //ident
    BindingIdent,
    Ident,
    PrivateName,
    //lib
    Invalid,
    // Lit
    Str,
    Bool,
    Null,
    Number,
    BigInt,
    Regex,
    // module_decl
    ExportDefaultExpr,
    ExportDecl,
    ImportDecl,
    ExportAll,
    NamedExport,
    ExportDefaultDecl,
    ImportDefaultSpecifier,
    ImportStarAsSpecifier,
    ImportNamedSpecifier,
    ExportNamespaceSpecifier,
    ExportDefaultSpecifier,
    ExportNamedSpecifier,
    //module
    Script,
    Module,
    //pat
    ArrayPat,
    ObjectPat,
    AssignPat,
    RestPat,
    KeyValuePatProp,
    //prop
    KeyValueProp,
    AssignProp,
    GetterProp,
    SetterProp,
    MethodProp,
    ComputedPropName,
    SpreadAssignment,
    //stmt
    BlockStmt,
    ExprStmt,
    EmptyStmt,
    DebuggerStmt,
    WithStmt,
    ReturnStmt,
    LabeledStmt,
    BreakStmt,
    ContinueStmt,
    IfStmt,
    SwitchStmt,
    ThrowStmt,
    TryStmt,
    WhileStmt,
    DoWhileStmt,
    ForStmt,
    ForInStmt,
    ForOfStmt,
    SwitchCase,
    CatchClause,
);

fn test_script<F>(src: &str, mut op: F)
where
    F: FnMut(&ControlFlowGraph<Node<'_>, ()>, &AstGraph),
{
    let root = parse_script(src);
    let cfg = create_cfg(&root);
    let root_node = Node::from(&root);
    let ast_graph = AstGraph::new(&root_node);

    op(&cfg, &ast_graph);
}

fn parse_script(input: &str) -> ast::Script {
    let cm = Lrc::<SourceMap>::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

    let fm = cm.new_source_file(FileName::Real("input".into()), input.into());

    let mut program_data = Default::default();
    let mut p = Parser::new(Syntax::Es(Default::default()), &fm, &mut program_data);
    let res = match p.parse_script() {
        Ok(p) => p,
        Err(e) => {
            e.into_diagnostic(&handler).emit();
            panic!("Failed to parse");
        }
    };

    let mut error = false;

    for e in p.take_errors() {
        e.into_diagnostic(&handler).emit();
        error = true;
    }

    if error {
        panic!("Failed to parse");
    }

    res
}

// fn parse_module(input: &str) -> ast::Module {
//     let cm = Lrc::<SourceMap>::default();
//     let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false, Some(cm.clone()));

//     let fm = cm.new_source_file(FileName::Real("input".into()), input.into());

//     let mut p = Parser::new(Syntax::Es(Default::default()), &fm, Default::default());
//     let res = match p.parse_module() {
//         Ok(p) => p,
//         Err(e) => {
//             e.into_diagnostic(&handler).emit();
//             panic!("Failed to parse");
//         }
//     };

//     let mut error = false;

//     for e in p.take_errors() {
//         e.into_diagnostic(&handler).emit();
//         error = true;
//     }

//     if error {
//         panic!("Failed to parse");
//     }

//     res
// }

struct AstGraph {
    map: FxHashMap<NodeId, NodeIndex>,
    graph: DiGraph<NodeId, super::print::Edge<()>>,
}

impl AstGraph {
    fn new(root: &Node) -> Self {
        let (map, graph) = ast_graph(root);
        let map = map.into_iter().map(|(k, v)| (k.node_id, v)).collect();
        let graph = graph.map(|_, n| n.node_id, |_, e| *e);
        Self { map, graph }
    }

    fn is_ancestor(&self, node: NodeId, maybe_descendant: NodeId) -> bool {
        let from = *self.map.get(&node).unwrap();
        let to = *self.map.get(&maybe_descendant).unwrap();
        has_path_connecting(&self.graph, from, to, None)
    }
}

// /**
// * Given an input in JavaScript, test if the control flow analysis
// * creates the proper control flow graph by comparing the expected
// * Dot file output.
// *
// * @param input Input JavaScript.
// * @param expected Expected Graphviz Dot file.
// */
// private void testCfg(String input, String expected) {
//  testCfg(input, expected, true);
// }

// /**
// * Given an input in JavaScript, test if the control flow analysis creates the proper control flow
// * graph by comparing the expected Dot file output.
// *
// * @param input Input JavaScript.
// * @param expected Expected Graphviz Dot file.
// * @param shouldTraverseFunctions Whether to traverse functions when constructing the CFG (true by
// *     default). Passed in to the constructor of {@link ControlFlowAnalysis}.
// */
// private void testCfg(String input, String expected, boolean shouldTraverseFunctions)
//    {
//  Compiler compiler = new Compiler();
//  ControlFlowAnalysis cfa = new ControlFlowAnalysis(compiler, shouldTraverseFunctions, true);

//  Node root = compiler.parseSyntheticCode("cfgtest", input);
//  cfa.process(null, root);
//  ControlFlowGraph<Node> cfg = cfa.getCfg();
//  assertThat(DotFormatter.toDot(root, cfg)).isEqualTo(expected);
// }

/**
* Gets all the edges of the graph.
*/
fn get_all_edges_of_CFG(cfg: &ControlFlowGraph<Node<'_>, ()>) -> Vec<SimpleEdge> {
    cfg.graph
        .raw_edges()
        .iter()
        .map(|e| SimpleEdge {
            source: cfg.graph[e.source()].into(),
            weight: e.weight,
            target: cfg.graph[e.target()].into(),
        })
        .collect()
}

/**
* Gets all the control flow edges from some node with the first token to
* some node with the second token.
*/
fn get_all_edges(
    cfg: &ControlFlowGraph<Node<'_>, ()>,
    start_token: Token,
    end_token: Token,
) -> Vec<SimpleEdge> {
    get_all_edges_of_CFG(cfg)
        .into_iter()
        .filter(|e| e.source.token == start_token && e.target.token == end_token)
        .collect()
}

/**
* Gets all the control flow edges from some node with the first token to
* some node with the second token.
*/
fn get_all_edges_predicate<S, E>(
    cfg: &ControlFlowGraph<Node<'_>, ()>,
    start: S,
    end: E,
) -> Vec<SimpleEdge>
where
    S: Fn(SimpleNode) -> bool,
    E: Fn(SimpleNode) -> bool,
{
    get_all_edges_of_CFG(cfg)
        .into_iter()
        .filter(|e| start(e.source) && end(e.target))
        .collect()
}

/**
* Gets all the control flow edges of the given type from some node with the
* first token to some node with the second token.
*/
fn get_all_edges_of_type(
    cfg: &ControlFlowGraph<Node<'_>, ()>,
    start_token: Token,
    end_token: Token,
    edge_type: Branch,
) -> Vec<SimpleEdge> {
    get_all_edges(cfg, start_token, end_token)
        .into_iter()
        .filter(|e| e.weight == edge_type)
        .collect()
}

/**
* Gets all the control flow edges of the given type from some node with the
* first token to some node with the second token.
*/
fn get_all_edges_of_type_predicate<S, E>(
    cfg: &ControlFlowGraph<Node<'_>, ()>,
    start: S,
    end: E,
    edge_type: Branch,
) -> Vec<SimpleEdge>
where
    S: Fn(SimpleNode) -> bool,
    E: Fn(SimpleNode) -> bool,
{
    get_all_edges_predicate(cfg, start, end)
        .into_iter()
        .filter(|e| e.weight == edge_type)
        .collect()
}

/**
* Gets all the control flow edges of the given type from some node with
* the first token to some node with the second token.
* This edge must flow from a parent to one of its descendants.
*/
fn get_all_down_edges<'a>(
    cfg: &'a ControlFlowGraph<Node<'_>, ()>,
    ast_graph: &'a AstGraph,
    start_token: Token,
    end_token: Token,
    edge_type: Branch,
) -> Vec<SimpleEdge> {
    get_all_edges_of_type(cfg, start_token, end_token, edge_type)
        .into_iter()
        .filter(|e| ast_graph.is_ancestor(e.source.node_id, e.target.node_id))
        .collect()
}

/**
* Gets all the control flow edges of the given type from some node with
* the first token to some node with the second token.
* This edge must flow from a parent to one of its descendants.
*/
fn get_all_down_edges_predicate<'a, S, E>(
    cfg: &'a ControlFlowGraph<Node<'_>, ()>,
    ast_graph: &'a AstGraph,
    start: S,
    end: E,
    edge_type: Branch,
) -> Vec<SimpleEdge>
where
    S: Fn(SimpleNode) -> bool,
    E: Fn(SimpleNode) -> bool,
{
    get_all_edges_of_type_predicate(cfg, start, end, edge_type)
        .into_iter()
        .filter(|e| ast_graph.is_ancestor(e.source.node_id, e.target.node_id))
        .collect()
}

/**
* Assert that there exists no control flow edge of the given type from some node with the first
* token to some node with the second token.
*/
fn assert_no_edge(cfg: &ControlFlowGraph<Node<'_>, ()>, start_token: Token, end_token: Token) {
    assert!(get_all_edges(cfg, start_token, end_token).is_empty());
}

/**
* Assert that there exists a control flow edge of the given type
* from some node with the first token to some node with the second token.
* This edge must flow from a parent to one of its descendants.
*/
fn assert_down_edge(
    cfg: &ControlFlowGraph<Node<'_>, ()>,
    ast_graph: &AstGraph,
    start_token: Token,
    end_token: Token,
    edge_type: Branch,
) {
    let edge_exists =
        !get_all_down_edges(cfg, ast_graph, start_token, end_token, edge_type).is_empty();
    assert!(
        edge_exists,
        "No down edge found between {:?} and {:?}",
        start_token, end_token
    );
}

/**
* Assert that there exists a control flow edge of the given type
* from some node with the first token to some node with the second token.
* This edge must flow from a node to one of its ancestors.
*/
fn assert_up_edge(
    cfg: &ControlFlowGraph<Node<'_>, ()>,
    ast_graph: &AstGraph,
    start_token: Token,
    end_token: Token,
    edge_type: Branch,
) {
    let edge_exists =
        !get_all_down_edges(cfg, ast_graph, end_token, start_token, edge_type).is_empty();
    assert!(
        edge_exists,
        "No up edge found between {:?} and {:?}",
        start_token, end_token
    );
}

/**
* Assert that there exists a control flow edge of the given type
* from some node with the first token to some node with the second token.
* This edge must flow between two nodes that are not in the same subtree.
*/
fn assert_cross_edge(
    cfg: &ControlFlowGraph<Node<'_>, ()>,
    ast_graph: &AstGraph,
    start_token: Token,
    end_token: Token,
    edge_type: Branch,
) {
    let num_down_edges =
        get_all_down_edges(cfg, ast_graph, start_token, end_token, edge_type).len();
    let num_up_edges = get_all_down_edges(cfg, ast_graph, end_token, start_token, edge_type).len();
    let num_edges = get_all_edges_of_type(cfg, start_token, end_token, edge_type).len();
    assert!(
        num_down_edges + num_up_edges < num_edges,
        "No cross edges found between {:?} and {:?}",
        start_token,
        end_token
    );
}

/**
* Assert that there exists a control flow edge of the given type
* from some node with the first token to some node with the second token.
* This edge must flow between two nodes that are not in the same subtree.
*/
fn assert_cross_edge_predicate<S, E>(
    cfg: &ControlFlowGraph<Node<'_>, ()>,
    ast_graph: &AstGraph,
    start: S,
    end: E,
    edge_type: Branch,
) where
    S: Fn(SimpleNode) -> bool,
    E: Fn(SimpleNode) -> bool,
{
    let num_down_edges =
        get_all_down_edges_predicate(cfg, ast_graph, &start, &end, edge_type).len();
    let num_up_edges = get_all_down_edges_predicate(cfg, ast_graph, &end, &start, edge_type).len();
    let num_edges = get_all_edges_of_type_predicate(cfg, &start, &end, edge_type).len();
    assert!(
        num_down_edges + num_up_edges < num_edges,
        "No cross edges found"
    );
}

/**
* Assert that there exists a control flow edge of the given type
* from some node with the first token to the return node.
*/
fn assert_return_edge(cfg: &ControlFlowGraph<Node<'_>, ()>, start_token: Token) {
    let edges = get_all_edges_of_CFG(cfg);
    for edge in edges {
        if edge.source.token == start_token && edge.target.token == Token::ImplicitReturn {
            return;
        }
    }

    panic!("No return edge found from {:?}", start_token);
}

/**
* Assert that there exists no control flow edge of the given type
* from some node with the first token to the return node.
*/
fn assert_no_return_edge(cfg: &ControlFlowGraph<Node<'_>, ()>, start_token: Token) {
    let edges = get_all_edges_of_CFG(cfg);
    for edge in edges {
        if edge.source.token == start_token {
            assert!(
                edge.target.token != Token::ImplicitReturn,
                "Token {:?} should not have an out going edge to the implicit return",
                start_token
            );
            return;
        }
    }
}

/**
* Given an input in JavaScript, get a control flow graph for it.
*
* @param input Input JavaScript.
*/
fn create_cfg<'ast, T>(root: T) -> ControlFlowGraph<Node<'ast>, ()>
where
    T: Into<ControlFlowRoot<'ast>>,
{
    let cfa = ControlFlowAnalysis::analyze(root.into(), true);
    cfa.cfg
}

#[test]
fn testSimpleStatementsInScript() {
    let src = "var a; a = a; a = a";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::Script,
            Token::VarDecl,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::VarDecl,
            Token::ExprStmt,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::ExprStmt,
            Token::ExprStmt,
            Branch::Unconditional,
        );
    });
}

// TODO:
// #[test]
// fn testSimpleStatementsInGoogModule() {
//  let src = "goog.module('myMod'); var a; a = a; a = a";
//  ControlFlowGraph<Node> cfg = createCfg(src);
//  assertDownEdge(cfg, Token::Script, Token.MODULE_BODY, Branch::UNCOND);
//  // the EXPR_RESULT is the goog.module(...) call
//  assertDownEdge(cfg, Token.MODULE_BODY, Token::ExprStmt, Branch::UNCOND);
//  assertCrossEdge(cfg, Token::ExprStmt, Token::VarDecl, Branch::UNCOND); // goog.module() -> var a;
// }

// TODO:
// #[test]
// fn testSimpleStatementsInEsModule() {
//     let src = "var a; a = a; export default a;";
//     let root = parse_module(src);
//     let cfg = createCfg(&root);
//     let root_ast_node = AstNode::Module(&root);
//     let ast_graph = AstGraph::new(&root_ast_node);

//     //  assertDownEdge(&cfg, &ast_graph, Token::Script, Token.MODULE_BODY, Branch::UNCOND);
//     assertDownEdge(
//         &cfg,
//         &ast_graph,
//         Token::Module,
//         Token::VarDecl,
//         Branch::UNCOND,
//     );
//     assertCrossEdge(
//         &cfg,
//         &ast_graph,
//         Token::VarDecl,
//         Token::ExprStmt,
//         Branch::UNCOND,
//     );
//     assertCrossEdge(
//         &cfg,
//         &ast_graph,
//         Token::ExprStmt,
//         Token::ExportDefaultExpr,
//         Branch::UNCOND,
//     );
// }

// Test a simple IF control flow.
#[test]
fn testSimpleIf() {
    let src = "var x; if (x) { x() } else { x() };";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::Script,
            Token::VarDecl,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::VarDecl,
            Token::IfStmt,
            Branch::Unconditional,
        );
        assert_down_edge(
            cfg,
            ast_graph,
            Token::IfStmt,
            Token::BlockStmt,
            Branch::True,
        );
        assert_down_edge(
            cfg,
            ast_graph,
            Token::BlockStmt,
            Token::ExprStmt,
            Branch::Unconditional,
        );
        assert_no_edge(cfg, Token::ExprStmt, Token::CallExpr);
        assert_down_edge(
            cfg,
            ast_graph,
            Token::IfStmt,
            Token::BlockStmt,
            Branch::False,
        );
        assert_return_edge(cfg, Token::EmptyStmt);
    });
}

#[test]
fn testBreakingBlock() {
    let src = "X: { while(1) { break } }";
    test_script(src, |cfg, ast_graph| {
        assert_up_edge(
            cfg,
            ast_graph,
            Token::BreakStmt,
            Token::BlockStmt,
            Branch::Unconditional,
        );
    });
}

#[test]
fn testBreakingWhile() {
    let src = "var x; while(true) { break; } x();";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::WhileStmt,
            Token::BlockStmt,
            Branch::True,
        );
        assert_down_edge(
            cfg,
            ast_graph,
            Token::BlockStmt,
            Token::BreakStmt,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::BreakStmt,
            Token::ExprStmt,
            Branch::Unconditional,
        );
    });
}

#[test]
fn testInifiteLoopWhile() {
    let src = "var x; while(true) { } x();";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::WhileStmt,
            Token::BlockStmt,
            Branch::True,
        );
        assert_no_edge(cfg, Token::WhileStmt, Token::ExprStmt);
    });
}

#[test]
fn testInifiteLoopFor_emptyCond() {
    let src = "var x; for(;;) { } x();";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::ForStmt,
            Token::BlockStmt,
            Branch::True,
        );
        assert_no_edge(cfg, Token::ForStmt, Token::ExprStmt);
    });
}

#[test]
fn testBreakingFor_emptyCond() {
    let src = "var x; for(;;) { break; } x();";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::ForStmt,
            Token::BlockStmt,
            Branch::True,
        );
        assert_down_edge(
            cfg,
            ast_graph,
            Token::BlockStmt,
            Token::BreakStmt,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::BreakStmt,
            Token::ExprStmt,
            Branch::Unconditional,
        );
    });
}

#[test]
fn testInifiteLoopFor_trueCond() {
    let src = "var x; for(;true;) { } x();";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::ForStmt,
            Token::BlockStmt,
            Branch::True,
        );
        assert_no_edge(cfg, Token::ForStmt, Token::ExprStmt);
    });
}

#[test]
fn testBreakingFor_trueCond() {
    let src = "var x; for(;true;) { break; } x();";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::ForStmt,
            Token::BlockStmt,
            Branch::True,
        );
        assert_down_edge(
            cfg,
            ast_graph,
            Token::BlockStmt,
            Token::BreakStmt,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::BreakStmt,
            Token::ExprStmt,
            Branch::Unconditional,
        );
    });
}

// // TODO:
// #[test]
// fn testThrowInCatchBlock() {
//     let src = "try { throw ''; } catch (e) { throw e;} finally {}";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"TRY\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"BLOCK\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"THROW\"];\n"
//     //          + "  node2 -> node3 [weight=1];\n"
//     //          + "  node4 [label=\"STRINGLIT\"];\n"
//     //          + "  node3 -> node4 [weight=1];\n"
//     //          + "  node5 [label=\"BLOCK\"];\n"
//     //          + "  node3 -> node5 [label=\"ON_EX\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node2 -> node3 [label=\"UNCOND\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node1 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"CATCH\"];\n"
//     //          + "  node5 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"NAME(e)\"];\n"
//     //          + "  node6 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"BLOCK\"];\n"
//     //          + "  node6 -> node8 [weight=1];\n"
//     //          + "  node9 [label=\"THROW\"];\n"
//     //          + "  node8 -> node9 [weight=1];\n"
//     //          + "  node10 [label=\"NAME(e)\"];\n"
//     //          + "  node9 -> node10 [weight=1];\n"
//     //          + "  node11 [label=\"BLOCK\"];\n"
//     //          + "  node9 -> node11 [label=\"ON_EX\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node8 -> node9 [label=\"UNCOND\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node6 -> node8 [label=\"UNCOND\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node5 -> node6 [label=\"UNCOND\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node1 -> node11 [weight=1];\n"
//     //          + "  node11 -> RETURN [label=\"UNCOND\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node1 -> node2 [label=\"UNCOND\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node0 -> node1 [label=\"UNCOND\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

#[test]
fn testBreakingTryBlock() {
    let src = "a: try { break a; } finally {} if(x) {}";
    test_script(src, |cfg, ast_graph| {
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::BreakStmt,
            Token::IfStmt,
            Branch::Unconditional,
        );
    });

    let src = "a: try {} finally {break a;} if(x) {}";
    test_script(src, |cfg, ast_graph| {
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::BreakStmt,
            Token::IfStmt,
            Branch::Unconditional,
        );
    });

    let src = "a: try {} catch(e) {break a;} if(x) {}";
    test_script(src, |cfg, ast_graph| {
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::BreakStmt,
            Token::IfStmt,
            Branch::Unconditional,
        );
    });
}

#[test]
fn testWithStatement() {
    let src = "var x, y; with(x) { y() }";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::WithStmt,
            Token::BlockStmt,
            Branch::Unconditional,
        );
        assert_no_edge(cfg, Token::WithStmt, Token::Ident);
        assert_no_edge(cfg, Token::Ident, Token::BlockStmt);
        assert_down_edge(
            cfg,
            ast_graph,
            Token::BlockStmt,
            Token::ExprStmt,
            Branch::Unconditional,
        );
        assert_return_edge(cfg, Token::ExprStmt);
    });
}

// Test a simple WHILE control flow with BREAKs.
#[test]
fn testSimpleWhile() {
    let src = "var x; while (x) { x(); if (x) { break; } x() }";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::WhileStmt,
            Token::BlockStmt,
            Branch::True,
        );
        assert_down_edge(
            cfg,
            ast_graph,
            Token::BlockStmt,
            Token::ExprStmt,
            Branch::Unconditional,
        );
        assert_down_edge(
            cfg,
            ast_graph,
            Token::IfStmt,
            Token::BlockStmt,
            Branch::True,
        );
        assert_return_edge(cfg, Token::BreakStmt);
    });
}

#[test]
fn testSimpleSwitch() {
    let src = "var x; switch(x){ case(1): x(); case('x'): x(); break; default: x();}";
    test_script(src, |cfg, ast_graph| {
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::VarDecl,
            Token::SwitchStmt,
            Branch::Unconditional,
        );
        assert_no_edge(cfg, Token::SwitchStmt, Token::Ident);
        // Transfer between cases and default.
        assert_down_edge(
            cfg,
            ast_graph,
            Token::SwitchStmt,
            Token::SwitchCase,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::SwitchCase,
            Token::SwitchCase,
            Branch::False,
        );
        assert_cross_edge_predicate(
            cfg,
            ast_graph,
            |n| n.token == Token::SwitchCase,
            |n| n.is_default_switch_case,
            Branch::False,
        );
        // Within each case.
        assert_down_edge(
            cfg,
            ast_graph,
            Token::SwitchCase,
            Token::ExprStmt,
            Branch::Unconditional,
        );
        assert_no_edge(cfg, Token::ExprStmt, Token::CallExpr);
        assert_no_edge(cfg, Token::CallExpr, Token::Ident);
    });
}

#[test]
fn testSimpleNoDefault() {
    let src = "var x; switch(x){ case(1): break; } x();";
    test_script(src, |cfg, ast_graph| {
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::SwitchCase,
            Token::ExprStmt,
            Branch::False,
        );
    });
}

#[test]
fn testSwitchDefaultFirst() {
    // DEFAULT appears first. But it is should evaluated last.
    let src = "var x; switch(x){ default: break; case 1: break; }";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::SwitchStmt,
            Token::SwitchCase,
            Branch::Unconditional,
        );
        assert_cross_edge_predicate(
            cfg,
            ast_graph,
            |n| n.token == Token::SwitchCase,
            |n| n.is_default_switch_case,
            Branch::False,
        );
    });
}

#[test]
fn testSwitchDefaultInMiddle() {
    // DEFAULT appears in the middle. But it is should evaluated last.
    let src = "var x; switch(x){ case 1: break; default: break; case 2: break; }";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::SwitchStmt,
            Token::SwitchCase,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::SwitchCase,
            Token::SwitchCase,
            Branch::False,
        );
        assert_cross_edge_predicate(
            cfg,
            ast_graph,
            |n| n.token == Token::SwitchCase,
            |n| n.is_default_switch_case,
            Branch::False,
        );
    });
}

#[test]
fn testSwitchEmpty() {
    // TODO: remove comment?
    // DEFAULT appears first. But it is should evaluated last.
    let src = "var x; switch(x){}; x()";
    test_script(src, |cfg, ast_graph| {
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::SwitchStmt,
            Token::EmptyStmt,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::EmptyStmt,
            Token::ExprStmt,
            Branch::Unconditional,
        );
    });
}

#[test]
fn testReturnThrowingException() {
    let src = "function f() {try { return a(); } catch (e) {e()}}";
    test_script(src, |cfg, ast_graph| {
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::ReturnStmt,
            Token::CatchClause,
            Branch::Exception,
        );
    });
}

// // TODO:
// // Test a simple FOR loop.
// #[test]
// fn testSimpleFor() {
//     let src = "var a; for (var x = 0; x < 100; x++) { a(); }";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"VAR\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"NAME(a)\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"VAR\"];\n"
//     //          + "  node1 -> node3 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 [label=\"FOR\"];\n"
//     //          + "  node0 -> node4 [weight=1];\n"
//     //          + "  node4 -> node3 [weight=1];\n"
//     //          + "  node5 [label=\"NAME(x)\"];\n"
//     //          + "  node3 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"NUMBER\"];\n"
//     //          + "  node5 -> node6 [weight=1];\n"
//     //          + "  node3 -> node4 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node7 [label=\"LT\"];\n"
//     //          + "  node4 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"NAME(x)\"];\n"
//     //          + "  node7 -> node8 [weight=1];\n"
//     //          + "  node9 [label=\"NUMBER\"];\n"
//     //          + "  node7 -> node9 [weight=1];\n"
//     //          + "  node10 [label=\"INC\"];\n"
//     //          + "  node4 -> node10 [weight=1];\n"
//     //          + "  node11 [label=\"NAME(x)\"];\n"
//     //          + "  node10 -> node11 [weight=1];\n"
//     //          + "  node10 -> node4 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node12 [label=\"BLOCK\"];\n"
//     //          + "  node4 -> node12 [weight=1];\n"
//     //          + "  node13 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node12 -> node13 [weight=1];\n"
//     //          + "  node14 [label=\"CALL\"];\n"
//     //          + "  node13 -> node14 [weight=1];\n"
//     //          + "  node15 [label=\"NAME(a)\"];\n"
//     //          + "  node14 -> node15 [weight=1];\n"
//     //          + "  node13 -> node10 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node12 -> node13 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 -> RETURN "
//     //          + "[label=\"ON_FALSE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 -> node12 "
//     //          + "[label=\"ON_TRUE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node1 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

// // TODO:
// #[test]
// fn testSimpleForWithContinue() {
//     let src = "var a; for (var x = 0; x < 100; x++) {a();continue;a()}";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"VAR\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"NAME(a)\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"VAR\"];\n"
//     //          + "  node1 -> node3 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 [label=\"FOR\"];\n"
//     //          + "  node0 -> node4 [weight=1];\n"
//     //          + "  node4 -> node3 [weight=1];\n"
//     //          + "  node5 [label=\"NAME(x)\"];\n"
//     //          + "  node3 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"NUMBER\"];\n"
//     //          + "  node5 -> node6 [weight=1];\n"
//     //          + "  node3 -> node4 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node7 [label=\"LT\"];\n"
//     //          + "  node4 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"NAME(x)\"];\n"
//     //          + "  node7 -> node8 [weight=1];\n"
//     //          + "  node9 [label=\"NUMBER\"];\n"
//     //          + "  node7 -> node9 [weight=1];\n"
//     //          + "  node10 [label=\"INC\"];\n"
//     //          + "  node4 -> node10 [weight=1];\n"
//     //          + "  node11 [label=\"NAME(x)\"];\n"
//     //          + "  node10 -> node11 [weight=1];\n"
//     //          + "  node10 -> node4 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node12 [label=\"BLOCK\"];\n"
//     //          + "  node4 -> node12 [weight=1];\n"
//     //          + "  node13 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node12 -> node13 [weight=1];\n"
//     //          + "  node14 [label=\"CALL\"];\n"
//     //          + "  node13 -> node14 [weight=1];\n"
//     //          + "  node15 [label=\"NAME(a)\"];\n"
//     //          + "  node14 -> node15 [weight=1];\n"
//     //          + "  node16 [label=\"CONTINUE\"];\n"
//     //          + "  node13 -> node16 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node12 -> node16 [weight=1];\n"
//     //          + "  node16 -> node10 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node17 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node12 -> node17 [weight=1];\n"
//     //          + "  node18 [label=\"CALL\"];\n"
//     //          + "  node17 -> node18 [weight=1];\n"
//     //          + "  node19 [label=\"NAME(a)\"];\n"
//     //          + "  node18 -> node19 [weight=1];\n"
//     //          + "  node17 -> node10 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node12 -> node13 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 -> RETURN "
//     //          + "[label=\"ON_FALSE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 -> node12 "
//     //          + "[label=\"ON_TRUE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node1 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

// // TODO:
// #[test]
// fn testNestedFor() {
//     // This is tricky as the inner FOR branches to "x++" ON_FALSE.
//     let src = "var a,b;a();for(var x=0;x<100;x++){for(var y=0;y<100;y++){continue;b();}}";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"VAR\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"NAME(a)\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"NAME(b)\"];\n"
//     //          + "  node1 -> node3 [weight=1];\n"
//     //          + "  node4 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node1 -> node4 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node4 [weight=1];\n"
//     //          + "  node5 [label=\"CALL\"];\n"
//     //          + "  node4 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"NAME(a)\"];\n"
//     //          + "  node5 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"VAR\"];\n"
//     //          + "  node4 -> node7 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node8 [label=\"FOR\"];\n"
//     //          + "  node0 -> node8 [weight=1];\n"
//     //          + "  node8 -> node7 [weight=1];\n"
//     //          + "  node9 [label=\"NAME(x)\"];\n"
//     //          + "  node7 -> node9 [weight=1];\n"
//     //          + "  node10 [label=\"NUMBER\"];\n"
//     //          + "  node9 -> node10 [weight=1];\n"
//     //          + "  node7 -> node8 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node11 [label=\"LT\"];\n"
//     //          + "  node8 -> node11 [weight=1];\n"
//     //          + "  node12 [label=\"NAME(x)\"];\n"
//     //          + "  node11 -> node12 [weight=1];\n"
//     //          + "  node13 [label=\"NUMBER\"];\n"
//     //          + "  node11 -> node13 [weight=1];\n"
//     //          + "  node14 [label=\"INC\"];\n"
//     //          + "  node8 -> node14 [weight=1];\n"
//     //          + "  node15 [label=\"NAME(x)\"];\n"
//     //          + "  node14 -> node15 [weight=1];\n"
//     //          + "  node14 -> node8 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node16 [label=\"BLOCK\"];\n"
//     //          + "  node8 -> node16 [weight=1];\n"
//     //          + "  node17 [label=\"FOR\"];\n"
//     //          + "  node16 -> node17 [weight=1];\n"
//     //          + "  node18 [label=\"VAR\"];\n"
//     //          + "  node17 -> node18 [weight=1];\n"
//     //          + "  node19 [label=\"NAME(y)\"];\n"
//     //          + "  node18 -> node19 [weight=1];\n"
//     //          + "  node20 [label=\"NUMBER\"];\n"
//     //          + "  node19 -> node20 [weight=1];\n"
//     //          + "  node18 -> node17 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node21 [label=\"LT\"];\n"
//     //          + "  node17 -> node21 [weight=1];\n"
//     //          + "  node22 [label=\"NAME(y)\"];\n"
//     //          + "  node21 -> node22 [weight=1];\n"
//     //          + "  node23 [label=\"NUMBER\"];\n"
//     //          + "  node21 -> node23 [weight=1];\n"
//     //          + "  node24 [label=\"INC\"];\n"
//     //          + "  node17 -> node24 [weight=1];\n"
//     //          + "  node25 [label=\"NAME(y)\"];\n"
//     //          + "  node24 -> node25 [weight=1];\n"
//     //          + "  node24 -> node17 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node26 [label=\"BLOCK\"];\n"
//     //          + "  node17 -> node26 [weight=1];\n"
//     //          + "  node27 [label=\"CONTINUE\"];\n"
//     //          + "  node26 -> node27 [weight=1];\n"
//     //          + "  node27 -> node24 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node28 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node26 -> node28 [weight=1];\n"
//     //          + "  node29 [label=\"CALL\"];\n"
//     //          + "  node28 -> node29 [weight=1];\n"
//     //          + "  node30 [label=\"NAME(b)\"];\n"
//     //          + "  node29 -> node30 [weight=1];\n"
//     //          + "  node28 -> node24 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node26 -> node27 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node17 -> node14 "
//     //          + "[label=\"ON_FALSE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node17 -> node26 "
//     //          + "[label=\"ON_TRUE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node16 -> node18 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node8 -> RETURN "
//     //          + "[label=\"ON_FALSE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node8 -> node16 "
//     //          + "[label=\"ON_TRUE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node1 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

// // TODO:
// #[test]
// fn testNestedDoWithBreak() {
//     // The BREAK branches to a() with UNCOND.
//     let src = "var a;do{do{break}while(a);do{a()}while(a)}while(a);";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"VAR\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"NAME(a)\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"BLOCK\"];\n"
//     //          + "  node1 -> node3 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 [label=\"DO\"];\n"
//     //          + "  node0 -> node4 [weight=1];\n"
//     //          + "  node4 -> node3 [weight=1];\n"
//     //          + "  node5 [label=\"DO\"];\n"
//     //          + "  node3 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"BLOCK\"];\n"
//     //          + "  node5 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"BREAK\"];\n"
//     //          + "  node6 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"BLOCK\"];\n"
//     //          + "  node7 -> node8 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node6 -> node7 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node9 [label=\"NAME(a)\"];\n"
//     //          + "  node5 -> node9 [weight=1];\n"
//     //          + "  node5 -> node6 "
//     //          + "[label=\"ON_TRUE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node5 -> node8 "
//     //          + "[label=\"ON_FALSE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node10 [label=\"DO\"];\n"
//     //          + "  node3 -> node10 [weight=1];\n"
//     //          + "  node10 -> node8 [weight=1];\n"
//     //          + "  node11 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node8 -> node11 [weight=1];\n"
//     //          + "  node12 [label=\"CALL\"];\n"
//     //          + "  node11 -> node12 [weight=1];\n"
//     //          + "  node13 [label=\"NAME(a)\"];\n"
//     //          + "  node12 -> node13 [weight=1];\n"
//     //          + "  node11 -> node10 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node8 -> node11 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node14 [label=\"NAME(a)\"];\n"
//     //          + "  node10 -> node14 [weight=1];\n"
//     //          + "  node10 -> node4 "
//     //          + "[label=\"ON_FALSE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node10 -> node8 "
//     //          + "[label=\"ON_TRUE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node3 -> node6 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node15 [label=\"NAME(a)\"];\n"
//     //          + "  node4 -> node15 [weight=1];\n"
//     //          + "  node4 -> RETURN "
//     //          + "[label=\"ON_FALSE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 -> node3 "
//     //          + "[label=\"ON_TRUE\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node1 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

// // TODO:
// #[test]
// fn testForIn() {
//     let src = "var a,b;for(a in b){a()};";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"VAR\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"NAME(a)\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"NAME(b)\"];\n"
//     //          + "  node1 -> node3 [weight=1];\n"
//     //          + "  node4 [label=\"NAME(b)\"];\n"
//     //          + "  node1 -> node4 [label=\"UNCOND\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node5 [label=\"FOR_IN\"];\n"
//     //          + "  node0 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"NAME(a)\"];\n"
//     //          + "  node5 -> node6 [weight=1];\n"
//     //          + "  node5 -> node4 [weight=1];\n"
//     //          + "  node4 -> node5 [label=\"UNCOND\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node7 [label=\"BLOCK\"];\n"
//     //          + "  node5 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node7 -> node8 [weight=1];\n"
//     //          + "  node9 [label=\"CALL\"];\n"
//     //          + "  node8 -> node9 [weight=1];\n"
//     //          + "  node10 [label=\"NAME(a)\"];\n"
//     //          + "  node9 -> node10 [weight=1];\n"
//     //          + "  node8 -> node5 [label=\"UNCOND\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node7 -> node8 [label=\"UNCOND\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node11 [label=\"EMPTY\"];\n"
//     //          + "  node5 -> node11 [label=\"ON_FALSE\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node5 -> node7 [label=\"ON_TRUE\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node0 -> node11 [weight=1];\n"
//     //          + "  node11 -> RETURN [label=\"UNCOND\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "  node0 -> node1 [label=\"UNCOND\", fontcolor=\"red\", weight=0.01,"
//     //          + " color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

// // TODO:
// #[test]
// fn testThrow() {
//     let src = "function f() { throw 1; f() }";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"FUNCTION\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"NAME(f)\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"PARAM_LIST\"];\n"
//     //          + "  node1 -> node3 [weight=1];\n"
//     //          + "  node4 [label=\"BLOCK\"];\n"
//     //          + "  node1 -> node4 [weight=1];\n"
//     //          + "  node5 [label=\"THROW\"];\n"
//     //          + "  node4 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"NUMBER\"];\n"
//     //          + "  node5 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node4 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"CALL\"];\n"
//     //          + "  node7 -> node8 [weight=1];\n"
//     //          + "  node9 [label=\"NAME(f)\"];\n"
//     //          + "  node8 -> node9 [weight=1];\n"
//     //          + "  node7 -> RETURN "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 -> node5 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node1 -> node4 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> RETURN "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

// // TODO:
// // Test a simple FUNCTION.
// #[test]
// fn testSimpleFunction() {
//     let src = "function f() { f() } f()";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"FUNCTION\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"NAME(f)\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"PARAM_LIST\"];\n"
//     //          + "  node1 -> node3 [weight=1];\n"
//     //          + "  node4 [label=\"BLOCK\"];\n"
//     //          + "  node1 -> node4 [weight=1];\n"
//     //          + "  node5 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node4 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"CALL\"];\n"
//     //          + "  node5 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"NAME(f)\"];\n"
//     //          + "  node6 -> node7 [weight=1];\n"
//     //          + "  node5 -> RETURN "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 -> node5 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node1 -> node4 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node8 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node0 -> node8 [weight=1];\n"
//     //          + "  node9 [label=\"CALL\"];\n"
//     //          + "  node8 -> node9 [weight=1];\n"
//     //          + "  node10 [label=\"NAME(f)\"];\n"
//     //          + "  node9 -> node10 [weight=1];\n"
//     //          + "  node8 -> RETURN "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node8 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

// // TODO:
// #[test]
// fn testSimpleClass() {
//     let src = "class C{} f();";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"CLASS\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"NAME(C)\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"EMPTY\"];\n"
//     //          + "  node1 -> node3 [weight=1];\n"
//     //          + "  node4 [label=\"CLASS_MEMBERS\"];\n"
//     //          + "  node1 -> node4 [weight=1];\n"
//     //          + "  node5 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node1 -> node5 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"CALL\"];\n"
//     //          + "  node5 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"NAME(f)\"];\n"
//     //          + "  node6 -> node7 [weight=1];\n"
//     //          + "  node5 -> RETURN "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node1 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

// // TODO:
// #[test]
// fn testClassWithMemberFunctions() {
//     let src = "class C{ f(){} g(){} }";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"CLASS\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"NAME(C)\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"EMPTY\"];\n"
//     //          + "  node1 -> node3 [weight=1];\n"
//     //          + "  node4 [label=\"CLASS_MEMBERS\"];\n"
//     //          + "  node1 -> node4 [weight=1];\n"
//     //          + "  node5 [label=\"MEMBER_FUNCTION_DEF\"];\n"
//     //          + "  node4 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"FUNCTION\"];\n"
//     //          + "  node5 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"NAME\"];\n"
//     //          + "  node6 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"PARAM_LIST\"];\n"
//     //          + "  node6 -> node8 [weight=1];\n"
//     //          + "  node9 [label=\"BLOCK\"];\n"
//     //          + "  node6 -> node9 [weight=1];\n"
//     //          + "  node9 -> RETURN "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node6 -> node9 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node10 [label=\"MEMBER_FUNCTION_DEF\"];\n"
//     //          + "  node4 -> node10 [weight=1];\n"
//     //          + "  node11 [label=\"FUNCTION\"];\n"
//     //          + "  node10 -> node11 [weight=1];\n"
//     //          + "  node12 [label=\"NAME\"];\n"
//     //          + "  node11 -> node12 [weight=1];\n"
//     //          + "  node13 [label=\"PARAM_LIST\"];\n"
//     //          + "  node11 -> node13 [weight=1];\n"
//     //          + "  node14 [label=\"BLOCK\"];\n"
//     //          + "  node11 -> node14 [weight=1];\n"
//     //          + "  node14 -> RETURN "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node11 -> node14 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node1 -> RETURN "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node1 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

// // TODO:
// #[test]
// fn testSimpleCatch() {
//     let src = "try{ throw x; x(); x['stuff']; x.x; x} catch (e) { e() }";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"TRY\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"BLOCK\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"THROW\"];\n"
//     //          + "  node2 -> node3 [weight=1];\n"
//     //          + "  node4 [label=\"NAME(x)\"];\n"
//     //          + "  node3 -> node4 [weight=1];\n"
//     //          + "  node5 [label=\"BLOCK\"];\n"
//     //          + "  node3 -> node5 [label=\"ON_EX\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node6 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node2 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"CALL\"];\n"
//     //          + "  node6 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"NAME(x)\"];\n"
//     //          + "  node7 -> node8 [weight=1];\n"
//     //          + "  node9 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node6 -> node5 [label=\"ON_EX\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node6 -> node9 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node2 -> node9 [weight=1];\n"
//     //          + "  node10 [label=\"GETELEM\"];\n"
//     //          + "  node9 -> node10 [weight=1];\n"
//     //          + "  node11 [label=\"NAME(x)\"];\n"
//     //          + "  node10 -> node11 [weight=1];\n"
//     //          + "  node12 [label=\"STRINGLIT(stuff)\"];\n"
//     //          + "  node10 -> node12 [weight=1];\n"
//     //          + "  node13 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node9 -> node13 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node9 -> node5 [label=\"ON_EX\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node2 -> node13 [weight=1];\n"
//     //          + "  node14 [label=\"GETPROP(x)\"];\n"
//     //          + "  node13 -> node14 [weight=1];\n"
//     //          + "  node15 [label=\"NAME(x)\"];\n"
//     //          + "  node14 -> node15 [weight=1];\n"
//     //          + "  node16 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node13 -> node16 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node13 -> node5 [label=\"ON_EX\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node2 -> node16 [weight=1];\n"
//     //          + "  node17 [label=\"NAME(x)\"];\n"
//     //          + "  node16 -> node17 [weight=1];\n"
//     //          + "  node16 -> RETURN [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node2 -> node3 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node1 -> node5 [weight=1];\n"
//     //          + "  node18 [label=\"CATCH\"];\n"
//     //          + "  node5 -> node18 [weight=1];\n"
//     //          + "  node19 [label=\"NAME(e)\"];\n"
//     //          + "  node18 -> node19 [weight=1];\n"
//     //          + "  node20 [label=\"BLOCK\"];\n"
//     //          + "  node18 -> node20 [weight=1];\n"
//     //          + "  node21 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node20 -> node21 [weight=1];\n"
//     //          + "  node22 [label=\"CALL\"];\n"
//     //          + "  node21 -> node22 [weight=1];\n"
//     //          + "  node23 [label=\"NAME(e)\"];\n"
//     //          + "  node22 -> node23 [weight=1];\n"
//     //          + "  node21 -> RETURN [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node20 -> node21 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node18 -> node20 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node5 -> node18 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node1 -> node2 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node1 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";

//     //  testCfg(src, expected);
// }

// // TODO:
// #[test]
// fn testFunctionWithinTry() {
//     // Make sure we don't search for the handler outside of the function.
//     let src = "try { var f = function() {throw 1;} } catch (e) { }";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"TRY\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"BLOCK\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"VAR\"];\n"
//     //          + "  node2 -> node3 [weight=1];\n"
//     //          + "  node4 [label=\"NAME(f)\"];\n"
//     //          + "  node3 -> node4 [weight=1];\n"
//     //          + "  node5 [label=\"FUNCTION\"];\n"
//     //          + "  node4 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"NAME\"];\n"
//     //          + "  node5 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"PARAM_LIST\"];\n"
//     //          + "  node5 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"BLOCK\"];\n"
//     //          + "  node5 -> node8 [weight=1];\n"
//     //          + "  node9 [label=\"THROW\"];\n"
//     //          + "  node8 -> node9 [weight=1];\n"
//     //          + "  node10 [label=\"NUMBER\"];\n"
//     //          + "  node9 -> node10 [weight=1];\n"
//     //          + "  node3 -> RETURN [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node2 -> node3 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node11 [label=\"BLOCK\"];\n"
//     //          + "  node1 -> node11 [weight=1];\n"
//     //          + "  node12 [label=\"CATCH\"];\n"
//     //          + "  node11 -> node12 [weight=1];\n"
//     //          + "  node13 [label=\"NAME(e)\"];\n"
//     //          + "  node12 -> node13 [weight=1];\n"
//     //          + "  node14 [label=\"BLOCK\"];\n"
//     //          + "  node12 -> node14 [weight=1];\n"
//     //          + "  node14 -> RETURN [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node12 -> node14 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node11 -> node12 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node1 -> node2 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node1 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

// // TODO:
// #[test]
// fn testNestedCatch() {
//     // Make sure we are going to the right handler.
//     let src = "try{try{throw 1;}catch(e){throw 2}}catch(f){}";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"TRY\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"BLOCK\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"TRY\"];\n"
//     //          + "  node2 -> node3 [weight=1];\n"
//     //          + "  node4 [label=\"BLOCK\"];\n"
//     //          + "  node3 -> node4 [weight=1];\n"
//     //          + "  node5 [label=\"THROW\"];\n"
//     //          + "  node4 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"NUMBER\"];\n"
//     //          + "  node5 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"BLOCK\"];\n"
//     //          + "  node5 -> node7 [label=\"ON_EX\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 -> node5 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node3 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"CATCH\"];\n"
//     //          + "  node7 -> node8 [weight=1];\n"
//     //          + "  node9 [label=\"NAME(e)\"];\n"
//     //          + "  node8 -> node9 [weight=1];\n"
//     //          + "  node10 [label=\"BLOCK\"];\n"
//     //          + "  node8 -> node10 [weight=1];\n"
//     //          + "  node11 [label=\"THROW\"];\n"
//     //          + "  node10 -> node11 [weight=1];\n"
//     //          + "  node12 [label=\"NUMBER\"];\n"
//     //          + "  node11 -> node12 [weight=1];\n"
//     //          + "  node13 [label=\"BLOCK\"];\n"
//     //          + "  node11 -> node13 [label=\"ON_EX\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node10 -> node11 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node8 -> node10 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node7 -> node8 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node3 -> node4 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node2 -> node3 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node1 -> node13 [weight=1];\n"
//     //          + "  node14 [label=\"CATCH\"];\n"
//     //          + "  node13 -> node14 [weight=1];\n"
//     //          + "  node15 [label=\"NAME(f)\"];\n"
//     //          + "  node14 -> node15 [weight=1];\n"
//     //          + "  node16 [label=\"BLOCK\"];\n"
//     //          + "  node14 -> node16 [weight=1];\n"
//     //          + "  node16 -> RETURN [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node14 -> node16 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node13 -> node14 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node1 -> node2 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node1 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

#[test]
fn testSimpleFinally() {
    let src = "try{var x; foo()}finally{}";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::TryStmt,
            Token::BlockStmt,
            Branch::Unconditional,
        );
        assert_down_edge(
            cfg,
            ast_graph,
            Token::BlockStmt,
            Token::VarDecl,
            Branch::Unconditional,
        );
        // VAR to FINALLY.
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::ExprStmt,
            Token::BlockStmt,
            Branch::Unconditional,
        );
        // No CATCH to FINALLY.
        assert_no_edge(cfg, Token::BlockStmt, Token::BlockStmt);
    });
}

// // TODO:
// #[test]
// fn testSimpleCatchFinally() {
//     // Make sure we are going to the right handler.
//     let src = "try{ if(a){throw 1}else{a} } catch(e){a}finally{a}";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"TRY\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"BLOCK\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"IF\"];\n"
//     //          + "  node2 -> node3 [weight=1];\n"
//     //          + "  node4 [label=\"NAME(a)\"];\n"
//     //          + "  node3 -> node4 [weight=1];\n"
//     //          + "  node5 [label=\"BLOCK\"];\n"
//     //          + "  node3 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"THROW\"];\n"
//     //          + "  node5 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"NUMBER\"];\n"
//     //          + "  node6 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"BLOCK\"];\n"
//     //          + "  node6 -> node8 [label=\"ON_EX\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node5 -> node6 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node9 [label=\"BLOCK\"];\n"
//     //          + "  node3 -> node9 [weight=1];\n"
//     //          + "  node10 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node9 -> node10 [weight=1];\n"
//     //          + "  node11 [label=\"NAME(a)\"];\n"
//     //          + "  node10 -> node11 [weight=1];\n"
//     //          + "  node12 [label=\"BLOCK\"];\n"
//     //          + "  node10 -> node12 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node9 -> node10 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node3 -> node5 [label=\"ON_TRUE\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node3 -> node9 [label=\"ON_FALSE\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node2 -> node3 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node1 -> node8 [weight=1];\n"
//     //          + "  node13 [label=\"CATCH\"];\n"
//     //          + "  node8 -> node13 [weight=1];\n"
//     //          + "  node14 [label=\"NAME(e)\"];\n"
//     //          + "  node13 -> node14 [weight=1];\n"
//     //          + "  node15 [label=\"BLOCK\"];\n"
//     //          + "  node13 -> node15 [weight=1];\n"
//     //          + "  node16 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node15 -> node16 [weight=1];\n"
//     //          + "  node17 [label=\"NAME(a)\"];\n"
//     //          + "  node16 -> node17 [weight=1];\n"
//     //          + "  node16 -> node12 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node15 -> node16 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node13 -> node15 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node8 -> node13 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node1 -> node12 [weight=1];\n"
//     //          + "  node18 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node12 -> node18 [weight=1];\n"
//     //          + "  node19 [label=\"NAME(a)\"];\n"
//     //          + "  node18 -> node19 [weight=1];\n"
//     //          + "  node18 -> RETURN [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node12 -> node18 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node1 -> node2 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node1 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

#[test]
fn testComplicatedFinally2() {
    // Now the most nasty case.....
    let src = "while(1){try{if(a){a;continue;}else if(b){b;break;} else if(c) throw 1; else a}catch(e){}finally{c()}bar}foo";
    test_script(src, |cfg, ast_graph| {
        // Focus only on the ON_EX edges.
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::ContinueStmt,
            Token::BlockStmt,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::BreakStmt,
            Token::BlockStmt,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::ThrowStmt,
            Token::CatchClause,
            Branch::Exception,
        );
    });
}

#[test]
fn testDeepNestedBreakwithFinally() {
    let src = "X:while(1){try{while(2){try{var a;break X;}finally{}}}finally{}}";
    test_script(src, |cfg, ast_graph| {
        assert_down_edge(
            cfg,
            ast_graph,
            Token::WhileStmt,
            Token::BlockStmt,
            Branch::True,
        );
        assert_down_edge(
            cfg,
            ast_graph,
            Token::BlockStmt,
            Token::TryStmt,
            Branch::Unconditional,
        );
        assert_down_edge(
            cfg,
            ast_graph,
            Token::BlockStmt,
            Token::VarDecl,
            Branch::Unconditional,
        );
        // BREAK to FINALLY.
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::BreakStmt,
            Token::BlockStmt,
            Branch::Unconditional,
        );
        // FINALLY to FINALLY.
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::BlockStmt,
            Token::BlockStmt,
            Branch::Exception,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::WhileStmt,
            Token::BlockStmt,
            Branch::False,
        );
        assert_return_edge(cfg, Token::BlockStmt);
    });
}

#[test]
fn testDeepNestedFinally() {
    let src = "try{try{try{throw 1}finally{1;var a}}finally{2;if(a);}}finally{3;a()}";
    test_script(src, |cfg, ast_graph| {
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::ThrowStmt,
            Token::BlockStmt,
            Branch::Exception,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::VarDecl,
            Token::BlockStmt,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::IfStmt,
            Token::BlockStmt,
            Branch::Exception,
        );
    });
}

#[test]
fn testReturn() {
    let src = "function f() { return; }";
    test_script(src, |cfg, _| {
        assert_return_edge(cfg, Token::ReturnStmt);
    });
}

#[test]
fn testReturnInFinally() {
    let src = "function f(x){ try{} finally {return x;} }";
    test_script(src, |cfg, _| {
        assert_return_edge(cfg, Token::ReturnStmt);
    });
}

#[test]
fn testReturnInFinally2() {
    let src = "function f(x){ try{ try{}finally{var dummy; return x;} } finally {} }";
    test_script(src, |cfg, ast_graph| {
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::VarDecl,
            Token::ReturnStmt,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::ReturnStmt,
            Token::BlockStmt,
            Branch::Unconditional,
        );
        assert_return_edge(cfg, Token::BlockStmt);
        assert_no_return_edge(cfg, Token::ReturnStmt);
    });
}

#[test]
fn testReturnInTry() {
    let src = "function f(x){ try{x; return x()} finally {} var y;}";
    test_script(src, |cfg, ast_graph| {
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::ExprStmt,
            Token::ReturnStmt,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::ReturnStmt,
            Token::BlockStmt,
            Branch::Unconditional,
        );
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::BlockStmt,
            Token::VarDecl,
            Branch::Unconditional,
        );
        assert_return_edge(cfg, Token::VarDecl);
        assert_return_edge(cfg, Token::BlockStmt);
        assert_no_return_edge(cfg, Token::ReturnStmt);
    });
}

// // TODO:
// #[test]
// fn testOptionNotToTraverseFunctions() {
//     let src = "var x = 1; function f() { x = null; }";
//     //  String expectedWhenNotTraversingFunctions =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"VAR\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"NAME(x)\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"NUMBER\"];\n"
//     //          + "  node2 -> node3 [weight=1];\n"
//     //          + "  node1 -> RETURN "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 [label=\"FUNCTION\"];\n"
//     //          + "  node0 -> node4 [weight=1];\n"
//     //          + "  node5 [label=\"NAME(f)\"];\n"
//     //          + "  node4 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"PARAM_LIST\"];\n"
//     //          + "  node4 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"BLOCK\"];\n"
//     //          + "  node4 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node7 -> node8 [weight=1];\n"
//     //          + "  node9 [label=\"ASSIGN\"];\n"
//     //          + "  node8 -> node9 [weight=1];\n"
//     //          + "  node10 [label=\"NAME(x)\"];\n"
//     //          + "  node9 -> node10 [weight=1];\n"
//     //          + "  node11 [label=\"NULL\"];\n"
//     //          + "  node9 -> node11 [weight=1];\n"
//     //          + "  node0 -> node1 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"VAR\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"NAME(x)\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"NUMBER\"];\n"
//     //          + "  node2 -> node3 [weight=1];\n"
//     //          + "  node1 -> RETURN "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 [label=\"FUNCTION\"];\n"
//     //          + "  node0 -> node4 [weight=1];\n"
//     //          + "  node5 [label=\"NAME(f)\"];\n"
//     //          + "  node4 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"PARAM_LIST\"];\n"
//     //          + "  node4 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"BLOCK\"];\n"
//     //          + "  node4 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node7 -> node8 [weight=1];\n"
//     //          + "  node9 [label=\"ASSIGN\"];\n"
//     //          + "  node8 -> node9 [weight=1];\n"
//     //          + "  node10 [label=\"NAME(x)\"];\n"
//     //          + "  node9 -> node10 [weight=1];\n"
//     //          + "  node11 [label=\"NULL\"];\n"
//     //          + "  node9 -> node11 [weight=1];\n"
//     //          + "  node8 -> RETURN "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node7 -> node8 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node4 -> node7 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node1 "
//     //          + "[label=\"UNCOND\", fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
//     //  testCfg(src, expectedWhenNotTraversingFunctions, false);
// }

#[test]
fn testInstanceOf() {
    let src = "try { x instanceof 'x' } catch (e) { }";
    test_script(src, |cfg, ast_graph| {
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::ExprStmt,
            Token::CatchClause,
            Branch::Exception,
        );
    });
}

// TODO:
// #[test]
// fn testSynBlock() {
//  let src = "START(); var x; END(); var y;";
//  ControlFlowGraph<Node> cfg = createCfg(src, true);
//  assertCrossEdge(cfg, Token::BlockStmt, Token::ExprStmt, Branch.SYN_BLOCK);
// }

// TODO:
// #[test]
// fn testPartialTraversalOfScope() {
//  Compiler compiler = new Compiler();
//  ControlFlowAnalysis cfa = new ControlFlowAnalysis(compiler, true, true);

//  Node script1 = compiler.parseSyntheticCode("cfgtest", "var foo;");
//  Node script2 = compiler.parseSyntheticCode("cfgtest2", "var bar;");
//  // Create a parent node for the scripts
//  new Node(Token::BlockStmt, script1, script2);

//  cfa.process(null, script1);
//  ControlFlowGraph<Node> cfg = cfa.getCfg();

//  assertThat(cfg.getNode(script1)).isNotNull();
//  assertThat(cfg.getNode(script2)).isNull();
// }

#[test]
fn testForLoopOrder() {
    assert_node_order(
        "for (var i = 0; i < 5; i++) { var x = 3; } if (true) {}",
        &[
            Token::Script,
            Token::VarDecl,
            Token::ForStmt,
            Token::BlockStmt,
            Token::VarDecl,
            Token::UpdateExpr, /* i++ */
            Token::IfStmt,
            Token::BlockStmt,
        ],
    );
}

#[test]
fn testLabelledForInLoopOrder() {
    assert_node_order(
        "
var i = 0; var y = {};
label: for (var x in y) {
    if (x) { break label; } else { i++ }
    x();
}",
        &[
            Token::Script,
            Token::VarDecl,
            Token::VarDecl,
            Token::Ident,
            Token::ForInStmt,
            Token::BlockStmt,
            Token::IfStmt,
            Token::BlockStmt,
            Token::BreakStmt,
            Token::BlockStmt,
            Token::ExprStmt,
            Token::ExprStmt,
        ],
    );
}

// TODO:
// #[test]
// fn testSimpleExportDeclarationsInEsModule() {
//  assertNodeOrder(
//      createCfg("export let a = 0; export default a; export {b} from './mod';"),
//      ImmutableList.of(
//          Token::Script, Token.MODULE_BODY, Token.EXPORT, Token.EXPORT, Token.EXPORT));
// }

// TODO:
// #[test]
// fn testSimpleImportDeclarationsInEsModule() {
//  assertNodeOrder(
//      createCfg("import x from './mod'; import {y} from './mod'; import * as z from './mod';"),
//      ImmutableList.of(
//          Token::Script, Token.MODULE_BODY, Token.IMPORT, Token.IMPORT, Token.IMPORT));
// }

#[test]
fn testLocalFunctionOrder() {
    assert_node_order(
        "function f() { while (x) { x++; } } var x = 3;",
        &[
            Token::Script,
            Token::VarDecl,
            Token::Function,
            Token::BlockStmt,
            Token::WhileStmt,
            Token::BlockStmt,
            Token::ExprStmt,
        ],
    );
}

#[test]
fn testDoWhileOrder() {
    assert_node_order(
        "do { var x = 3; } while (true); void x;",
        &[
            Token::Script,
            Token::BlockStmt,
            Token::VarDecl,
            Token::DoWhileStmt,
            Token::ExprStmt,
        ],
    );
}

#[test]
fn testForOfOrder() {
    assert_node_order(
        "async function f() { for (x of y) { z; } return 0; }",
        &[
            Token::Script,
            Token::Function,
            Token::BlockStmt,
            Token::Ident,
            Token::ForOfStmt,
            Token::BlockStmt,
            Token::ExprStmt,
            Token::ReturnStmt,
        ],
    );
}

#[test]
fn testForAwaitOfOrder() {
    assert_node_order(
        "async function f() { for await (x of y) { z; } return 0; }",
        &[
            Token::Script,
            Token::Function,
            Token::BlockStmt,
            Token::Ident,
            Token::ForOfStmt,
            Token::BlockStmt,
            Token::ExprStmt,
            Token::ReturnStmt,
        ],
    );
}

#[test]
fn testForAwaitOfOrderBreakAndContinue() {
    assert_node_order(
        "
async function f() {
    outer: for await (let x of y) {
        inner: for await (let z of x) {
            if (z) break inner;
            else continue outer;
        }
    }
    return 0;
}",
        &[
            Token::Script,
            Token::Function,
            Token::BlockStmt,
            Token::Ident,
            Token::ForOfStmt,
            Token::BlockStmt,
            Token::Ident,
            Token::ForOfStmt,
            Token::BlockStmt,
            Token::IfStmt,
            Token::BreakStmt,
            Token::ContinueStmt,
            Token::ReturnStmt,
        ],
    );
}

#[test]
fn testForAwaitOfOrderBreakAndContinueAndYield() {
    assert_node_order(
        "
async function* f() {
  outer: for await (let x of y) {
    inner: for await (let z of x) {
        if (z > 0) break inner;
        else if (z < 0) continue outer;
        yield z;
    }
  }
}",
        &[
            Token::Script,
            Token::Function,
            Token::BlockStmt,
            Token::Ident,
            Token::ForOfStmt,
            Token::BlockStmt,
            Token::Ident,
            Token::ForOfStmt,
            Token::BlockStmt,
            Token::IfStmt,
            Token::BreakStmt,
            Token::IfStmt,
            Token::ContinueStmt,
            Token::ExprStmt,
        ],
    );
}

// // TODO:
// #[test]
// fn testBreakInFinally1() {
//     let src = "
// f = function() {
//     var action;
//     a: {
//         var proto = null;
//         try {
//             proto = new Proto
//         } finally {
//             action = proto;
//             break a  // Remove this...
//         }
//     }
//     alert(action) // but not this.
// };";
//     //  String expected =
//     //      "digraph AST {\n"
//     //          + "  node [color=lightblue2, style=filled];\n"
//     //          + "  node0 [label=\"SCRIPT\"];\n"
//     //          + "  node1 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node0 -> node1 [weight=1];\n"
//     //          + "  node2 [label=\"ASSIGN\"];\n"
//     //          + "  node1 -> node2 [weight=1];\n"
//     //          + "  node3 [label=\"NAME(f)\"];\n"
//     //          + "  node2 -> node3 [weight=1];\n"
//     //          + "  node4 [label=\"FUNCTION\"];\n"
//     //          + "  node2 -> node4 [weight=1];\n"
//     //          + "  node5 [label=\"NAME\"];\n"
//     //          + "  node4 -> node5 [weight=1];\n"
//     //          + "  node6 [label=\"PARAM_LIST\"];\n"
//     //          + "  node4 -> node6 [weight=1];\n"
//     //          + "  node7 [label=\"BLOCK\"];\n"
//     //          + "  node4 -> node7 [weight=1];\n"
//     //          + "  node8 [label=\"VAR\"];\n"
//     //          + "  node7 -> node8 [weight=1];\n"
//     //          + "  node9 [label=\"NAME(action)\"];\n"
//     //          + "  node8 -> node9 [weight=1];\n"
//     //          + "  node10 [label=\"LABEL\"];\n"
//     //          + "  node7 -> node10 [weight=1];\n"
//     //          + "  node11 [label=\"LABEL_NAME\"];\n"
//     //          + "  node10 -> node11 [weight=1];\n"
//     //          + "  node12 [label=\"BLOCK\"];\n"
//     //          + "  node10 -> node12 [weight=1];\n"
//     //          + "  node13 [label=\"VAR\"];\n"
//     //          + "  node12 -> node13 [weight=1];\n"
//     //          + "  node14 [label=\"NAME(proto)\"];\n"
//     //          + "  node13 -> node14 [weight=1];\n"
//     //          + "  node15 [label=\"NULL\"];\n"
//     //          + "  node14 -> node15 [weight=1];\n"
//     //          + "  node16 [label=\"TRY\"];\n"
//     //          + "  node12 -> node16 [weight=1];\n"
//     //          + "  node17 [label=\"BLOCK\"];\n"
//     //          + "  node16 -> node17 [weight=1];\n"
//     //          + "  node18 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node17 -> node18 [weight=1];\n"
//     //          + "  node19 [label=\"ASSIGN\"];\n"
//     //          + "  node18 -> node19 [weight=1];\n"
//     //          + "  node20 [label=\"NAME(proto)\"];\n"
//     //          + "  node19 -> node20 [weight=1];\n"
//     //          + "  node21 [label=\"NEW\"];\n"
//     //          + "  node19 -> node21 [weight=1];\n"
//     //          + "  node22 [label=\"NAME(Proto)\"];\n"
//     //          + "  node21 -> node22 [weight=1];\n"
//     //          + "  node23 [label=\"BLOCK\"];\n"
//     //          + "  node16 -> node23 [weight=1];\n"
//     //          + "  node24 [label=\"BLOCK\"];\n"
//     //          + "  node16 -> node24 [weight=1];\n"
//     //          + "  node25 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node24 -> node25 [weight=1];\n"
//     //          + "  node26 [label=\"ASSIGN\"];\n"
//     //          + "  node25 -> node26 [weight=1];\n"
//     //          + "  node27 [label=\"NAME(action)\"];\n"
//     //          + "  node26 -> node27 [weight=1];\n"
//     //          + "  node28 [label=\"NAME(proto)\"];\n"
//     //          + "  node26 -> node28 [weight=1];\n"
//     //          + "  node29 [label=\"BREAK\"];\n"
//     //          + "  node24 -> node29 [weight=1];\n"
//     //          + "  node30 [label=\"LABEL_NAME\"];\n"
//     //          + "  node29 -> node30 [weight=1];\n"
//     //          + "  node31 [label=\"EXPR_RESULT\"];\n"
//     //          + "  node7 -> node31 [weight=1];\n"
//     //          + "  node32 [label=\"CALL\"];\n"
//     //          + "  node31 -> node32 [weight=1];\n"
//     //          + "  node33 [label=\"NAME(alert)\"];\n"
//     //          + "  node32 -> node33 [weight=1];\n"
//     //          + "  node34 [label=\"NAME(action)\"];\n"
//     //          + "  node32 -> node34 [weight=1];\n"
//     //          + "  node1 -> RETURN [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "  node0 -> node1 [label=\"UNCOND\", "
//     //          + "fontcolor=\"red\", weight=0.01, color=\"red\"];\n"
//     //          + "}\n";
//     //  testCfg(src, expected);
// }

#[test]
fn testBreakInFinally2() {
    let src = "
var action;
a: {
    var proto = null;
    try {
        proto = new Proto;
    } finally {
        action = proto;
        break a;
    }
}
alert(action)";

    test_script(src, |cfg, ast_graph| {
        assert_cross_edge(
            cfg,
            ast_graph,
            Token::BreakStmt,
            Token::ExprStmt,
            Branch::Unconditional,
        );
        assert_no_edge(cfg, Token::BreakStmt, Token::BlockStmt);
    });
}

/**
* Asserts the priority order of CFG nodes.
*
* Checks that the node type of the highest-priority node matches the
* first element of the list, the type of the second node matches the
* second element of the list, and so on.
*
* @param cfg The control flow graph.
* @param nodeTypes The expected node types, in order.
*/
fn assert_node_order(src: &str, expected: &[Token]) {
    let root = parse_script(src);
    let cfg = create_cfg(&root);
    let root = ControlFlowRoot::Script(&root);

    let cfa = ControlFlowAnalysis::<'_, ()>::analyze(root, true);

    let mut actual = cfa
        .node_priorities
        .iter()
        .enumerate()
        .map(|(n, p)| (cfg.graph[NodeIndex::from(n as u32)], *p))
        .collect::<Vec<_>>();
    actual.sort_unstable_by_key(|(_, p)| *p);
    let mut actual = actual.into_iter().map(|(n, _)| n).collect::<Vec<_>>();

    // Implicit return must always be last
    assert!(actual.pop().unwrap().is_implicit_return());

    let eq = actual
        .iter()
        .copied()
        .eq_by(expected.iter().copied(), |n, t| Token::from(n) == t);
    assert!(
        eq,
        "Expected order: {:#?}, actual order: {:#?}",
        expected, actual
    );
}
