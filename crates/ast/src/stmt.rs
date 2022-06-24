use crate::{
    decl::{Decl, VarDecl},
    expr::Expr,
    ident::Ident,
    pat::Pat,
    NodeId,
};
use global_common::{ast_node, EqIgnoreSpan, Span};

/// Use when only block statements are allowed.
#[ast_node("BlockStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct BlockStmt {
    pub node_id: NodeId,

    /// Span including the braces.
    pub span: Span,

    pub stmts: Vec<Stmt>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum Stmt {
    Block(BlockStmt),

    Empty(EmptyStmt),

    Debugger(DebuggerStmt),

    With(WithStmt),

    Return(ReturnStmt),

    Labeled(LabeledStmt),

    Break(BreakStmt),

    Continue(ContinueStmt),

    If(IfStmt),

    Switch(SwitchStmt),

    Throw(ThrowStmt),

    /// A try statement. If handler is null then finalizer must be a BlockStmt.
    Try(TryStmt),

    While(WhileStmt),

    DoWhile(DoWhileStmt),

    For(ForStmt),

    ForIn(ForInStmt),

    ForOf(ForOfStmt),

    Decl(Decl),

    Expr(ExprStmt),
}

#[ast_node("ExpressionStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExprStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub expr: Box<Expr>,
}

#[ast_node("EmptyStatement")]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct EmptyStmt {
    pub node_id: NodeId,

    /// Span of semicolon.
    pub span: Span,
}

#[ast_node("DebuggerStatement")]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct DebuggerStmt {
    pub node_id: NodeId,

    pub span: Span,
}

#[ast_node("WithStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct WithStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub obj: Box<Expr>,
    pub body: Box<Stmt>,
}

#[ast_node("ReturnStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ReturnStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub arg: Option<Box<Expr>>,
}

#[ast_node("LabeledStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct LabeledStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub label: Ident,
    pub body: Box<Stmt>,
}

#[ast_node("BreakStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct BreakStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub label: Option<Ident>,
}

#[ast_node("ContinueStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ContinueStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub label: Option<Ident>,
}

#[ast_node("IfStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct IfStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub test: Box<Expr>,

    pub cons: Box<Stmt>,

    pub alt: Option<Box<Stmt>>,
}

#[ast_node("SwitchStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SwitchStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub discriminant: Box<Expr>,
    pub cases: Vec<SwitchCase>,
}

#[ast_node("ThrowStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ThrowStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub arg: Box<Expr>,
}

#[ast_node("TryStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TryStmt {
    pub node_id: NodeId,

    pub span: Span,

    pub block: BlockStmt,

    pub handler: Option<CatchClause>,

    pub finalizer: Option<BlockStmt>,
}

#[ast_node("WhileStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct WhileStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub test: Box<Expr>,
    pub body: Box<Stmt>,
}

#[ast_node("DoWhileStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct DoWhileStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub test: Box<Expr>,
    pub body: Box<Stmt>,
}

#[ast_node("ForStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ForStmt {
    pub node_id: NodeId,

    pub span: Span,

    pub init: Option<VarDeclOrExpr>,

    pub test: Option<Box<Expr>>,

    pub update: Option<Box<Expr>>,

    pub body: Box<Stmt>,
}

#[ast_node("ForInStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ForInStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub left: VarDeclOrPat,
    pub right: Box<Expr>,
    pub body: Box<Stmt>,
}

#[ast_node("ForOfStatement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ForOfStmt {
    pub node_id: NodeId,

    pub span: Span,
    /// Span of the await token.
    ///
    /// es2018
    ///
    /// for-await-of statements, e.g., `for await (const x of xs) {`
    pub await_token: Option<Span>,
    pub left: VarDeclOrPat,
    pub right: Box<Expr>,
    pub body: Box<Stmt>,
}

#[ast_node("SwitchCase")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SwitchCase {
    pub node_id: NodeId,

    pub span: Span,

    /// None for `default:`
    pub test: Option<Box<Expr>>,

    pub cons: Vec<Stmt>,
}

#[ast_node("CatchClause")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct CatchClause {
    pub node_id: NodeId,

    pub span: Span,
    /// es2019
    ///
    /// The param is null if the catch binding is omitted. E.g., try { foo() }
    /// catch { bar() }
    pub param: Option<Pat>,

    pub body: BlockStmt,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum VarDeclOrPat {
    VarDecl(VarDecl),

    Pat(Pat),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum VarDeclOrExpr {
    VarDecl(VarDecl),

    Expr(Box<Expr>),
}
