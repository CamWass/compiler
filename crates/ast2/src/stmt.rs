use crate::{
    decl::{Decl, VarDecl},
    expr::Expr,
    ident::Ident,
    node::NodeId,
    pat::Pat,
};
use ast_node2::ast_node;
use global_common::{EqIgnoreSpan, Span};

/// Use when only block statements are allowed.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct BlockStmt<'ast> {
    pub node_id: NodeId,
    /// Span including the braces.
    pub span: Span,

    pub stmts: Vec<Stmt<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum Stmt<'ast> {
    Block(&'ast BlockStmt<'ast>),
    Empty(&'ast EmptyStmt),
    Debugger(&'ast DebuggerStmt),
    With(&'ast WithStmt<'ast>),

    Return(&'ast ReturnStmt<'ast>),
    Labeled(&'ast LabeledStmt<'ast>),

    Break(&'ast BreakStmt<'ast>),

    Continue(&'ast ContinueStmt<'ast>),

    If(&'ast IfStmt<'ast>),
    Switch(&'ast SwitchStmt<'ast>),
    Throw(&'ast ThrowStmt<'ast>),

    /// A try statement. If handler is null then finalizer must be a BlockStmt.
    Try(&'ast TryStmt<'ast>),

    While(&'ast WhileStmt<'ast>),
    DoWhile(&'ast DoWhileStmt<'ast>),

    For(&'ast ForStmt<'ast>),
    ForIn(&'ast ForInStmt<'ast>),
    ForOf(&'ast ForOfStmt<'ast>),
    Decl(Decl<'ast>),
    Expr(&'ast ExprStmt<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExprStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub expr: Expr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct EmptyStmt {
    pub node_id: NodeId,
    /// Span of semicolon.
    pub span: Span,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct DebuggerStmt {
    pub node_id: NodeId,
    pub span: Span,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct WithStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub obj: Expr<'ast>,
    pub body: Stmt<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ReturnStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub arg: Option<Expr<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct LabeledStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub label: &'ast Ident,
    pub body: Stmt<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct BreakStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub label: Option<&'ast Ident>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ContinueStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub label: Option<&'ast Ident>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct IfStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub test: Expr<'ast>,

    pub cons: Stmt<'ast>,

    pub alt: Option<Stmt<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SwitchStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub discriminant: Expr<'ast>,
    pub cases: Vec<&'ast SwitchCase<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ThrowStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub arg: Expr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TryStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub block: &'ast BlockStmt<'ast>,
    pub handler: Option<&'ast CatchClause<'ast>>,
    pub finalizer: Option<&'ast BlockStmt<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct WhileStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub test: Expr<'ast>,
    pub body: Stmt<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct DoWhileStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub test: Expr<'ast>,
    pub body: Stmt<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ForStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub init: Option<VarDeclOrExpr<'ast>>,
    pub test: Option<Expr<'ast>>,
    pub update: Option<Expr<'ast>>,

    pub body: Stmt<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ForInStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub left: VarDeclOrPat<'ast>,
    pub right: Expr<'ast>,
    pub body: Stmt<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ForOfStmt<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    /// Span of the await token.
    ///
    /// es2018
    ///
    /// for-await-of statements, e.g., `for await (const x of xs) {`
    pub await_token: Option<Span>,
    pub left: VarDeclOrPat<'ast>,
    pub right: Expr<'ast>,
    pub body: Stmt<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SwitchCase<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    /// None for `default:`
    pub test: Option<Expr<'ast>>,

    pub cons: Vec<Stmt<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct CatchClause<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    /// es2019
    ///
    /// The param is null if the catch binding is omitted. E.g., try { foo() }
    /// catch { bar() }
    pub param: Option<Pat<'ast>>,

    pub body: &'ast BlockStmt<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum VarDeclOrPat<'ast> {
    VarDecl(&'ast VarDecl<'ast>),

    Pat(Pat<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum VarDeclOrExpr<'ast> {
    VarDecl(&'ast VarDecl<'ast>),

    Expr(Expr<'ast>),
}
