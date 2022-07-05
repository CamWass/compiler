use crate::{
    decl::{Decl, VarDecl},
    expr::Expr,
    ident::Ident,
    pat::Pat,
    GetNodeId, NodeId,
};
use ast_node::ast_node;
use global_common::{util::take::Take, EqIgnoreSpan, Span, DUMMY_SP};

/// Use when only block statements are allowed.
#[ast_node]
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

impl Take for Stmt {
    fn dummy() -> Self {
        Self::Empty(EmptyStmt {
            node_id: NodeId::MAX,
            span: DUMMY_SP,
        })
    }
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExprStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub expr: Box<Expr>,
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
pub struct WithStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub obj: Box<Expr>,
    pub body: Box<Stmt>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ReturnStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub arg: Option<Box<Expr>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct LabeledStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub label: Ident,
    pub body: Box<Stmt>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct BreakStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub label: Option<Ident>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ContinueStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub label: Option<Ident>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct IfStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub test: Box<Expr>,

    pub cons: Box<Stmt>,

    pub alt: Option<Box<Stmt>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SwitchStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub discriminant: Box<Expr>,
    pub cases: Vec<SwitchCase>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ThrowStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub arg: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TryStmt {
    pub node_id: NodeId,

    pub span: Span,

    pub block: BlockStmt,

    pub handler: Option<CatchClause>,

    pub finalizer: Option<BlockStmt>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct WhileStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub test: Box<Expr>,
    pub body: Box<Stmt>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct DoWhileStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub test: Box<Expr>,
    pub body: Box<Stmt>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ForStmt {
    pub node_id: NodeId,

    pub span: Span,

    pub init: Option<VarDeclOrExpr>,

    pub test: Option<Box<Expr>>,

    pub update: Option<Box<Expr>>,

    pub body: Box<Stmt>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ForInStmt {
    pub node_id: NodeId,

    pub span: Span,
    pub left: VarDeclOrPat,
    pub right: Box<Expr>,
    pub body: Box<Stmt>,
}

#[ast_node]
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

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SwitchCase {
    pub node_id: NodeId,

    pub span: Span,

    /// None for `default:`
    pub test: Option<Box<Expr>>,

    pub cons: Vec<Stmt>,
}

#[ast_node]
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
