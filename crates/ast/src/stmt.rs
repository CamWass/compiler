use crate::{
    decl::{Decl, VarDecl},
    expr::Expr,
    ident::Ident,
    pat::Pat,
    GetNodeId, NodeId,
};
use clone_node::CloneNode;
use global_common::util::take::Take;
use node_id::GetNodeIdMacro;

/// Use when only block statements are allowed.
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct BlockStmt {
    pub node_id: NodeId,

    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
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
            node_id: NodeId::DUMMY,
        })
    }
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ExprStmt {
    pub node_id: NodeId,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct EmptyStmt {
    pub node_id: NodeId,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct DebuggerStmt {
    pub node_id: NodeId,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct WithStmt {
    pub node_id: NodeId,
    pub obj: Box<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ReturnStmt {
    pub node_id: NodeId,
    pub arg: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct LabeledStmt {
    pub node_id: NodeId,
    pub label: Ident,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct BreakStmt {
    pub node_id: NodeId,
    pub label: Option<Ident>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ContinueStmt {
    pub node_id: NodeId,
    pub label: Option<Ident>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct IfStmt {
    pub node_id: NodeId,
    pub test: Box<Expr>,

    pub cons: Box<Stmt>,

    pub alt: Option<Box<Stmt>>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct SwitchStmt {
    pub node_id: NodeId,
    pub discriminant: Box<Expr>,
    pub cases: Vec<SwitchCase>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ThrowStmt {
    pub node_id: NodeId,
    pub arg: Box<Expr>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct TryStmt {
    pub node_id: NodeId,

    pub block: BlockStmt,

    pub handler: Option<CatchClause>,

    pub finalizer: Option<BlockStmt>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct WhileStmt {
    pub node_id: NodeId,
    pub test: Box<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct DoWhileStmt {
    pub node_id: NodeId,
    pub test: Box<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ForStmt {
    pub node_id: NodeId,

    pub init: Option<VarDeclOrExpr>,

    pub test: Option<Box<Expr>>,

    pub update: Option<Box<Expr>>,

    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ForInStmt {
    pub node_id: NodeId,
    pub left: VarDeclOrPat,
    pub right: Box<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ForOfStmt {
    pub node_id: NodeId,
    /// for-await-of statements, e.g., `for await (const x of xs) {`
    pub is_await: bool,
    pub left: VarDeclOrPat,
    pub right: Box<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct SwitchCase {
    pub node_id: NodeId,

    /// None for `default:`
    pub test: Option<Box<Expr>>,

    pub cons: Vec<Stmt>,
}

impl SwitchCase {
    pub fn is_default(&self) -> bool {
        self.test.is_none()
    }
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct CatchClause {
    pub node_id: NodeId,
    /// es2019
    ///
    /// The param is null if the catch binding is omitted. E.g., try { foo() }
    /// catch { bar() }
    pub param: Option<Pat>,

    pub body: BlockStmt,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum VarDeclOrPat {
    VarDecl(VarDecl),

    Pat(Pat),
}

impl Take for VarDeclOrPat {
    fn dummy() -> Self {
        VarDeclOrPat::Pat(Pat::dummy())
    }
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
#[allow(variant_size_differences)]
pub enum VarDeclOrExpr {
    VarDecl(VarDecl),

    Expr(Box<Expr>),
}
