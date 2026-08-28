use crate::{
    GetNodeId, Invalid, NodeId, Param,
    class::Class,
    function::Function,
    ident::{Ident, PrivateName},
    lit::{Bool, Lit, Number, Str},
    operators::{AssignOp, BinaryOp, UnaryOp, UpdateOp},
    pat::Pat,
    prop::Prop,
    stmt::BlockStmt,
};
use clone_node::CloneNode;
use common::util::take::Take;
use node_eq::NodeEq;
use node_id::GetNodeIdMacro;
use serde::Serialize;

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub enum Expr {
    This(ThisExpr),

    Array(ArrayLit),

    Object(ObjectLit),

    Fn(FnExpr),

    Unary(UnaryExpr),

    /// `++v`, `--v`, `v++`, `v--`
    Update(UpdateExpr),

    Bin(BinExpr),

    Assign(AssignExpr),

    Member(MemberExpr),

    /// true ? 'a' : 'b'
    Cond(CondExpr),

    Call(CallExpr),

    /// `new Cat()`
    New(NewExpr),

    Seq(SeqExpr),

    Ident(Ident),

    Lit(Lit),

    Tpl(Tpl),

    TaggedTpl(TaggedTpl),

    Arrow(Box<ArrowExpr>),

    Class(Box<ClassExpr>),

    Yield(YieldExpr),

    MetaProp(MetaPropExpr),

    Await(AwaitExpr),

    PrivateName(PrivateName),

    OptChain(OptChainExpr),

    Invalid(Invalid),
}

impl Take for Expr {
    fn dummy() -> Self {
        Self::Invalid(Invalid {
            node_id: NodeId::DUMMY,
        })
    }
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct ThisExpr {
    pub node_id: NodeId,
}

/// Array literal.
#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct ArrayLit {
    pub node_id: NodeId,

    pub elems: Vec<Option<ExprOrSpread>>,
}

/// Object literal.
#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct ObjectLit {
    pub node_id: NodeId,

    pub props: Vec<Prop>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct SpreadElement {
    pub node_id: NodeId,
    pub expr: Box<Expr>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct UnaryExpr {
    pub node_id: NodeId,

    pub op: UnaryOp,

    pub arg: Box<Expr>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct UpdateExpr {
    pub node_id: NodeId,

    pub op: UpdateOp,

    pub prefix: bool,

    pub arg: Box<Expr>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct BinExpr {
    pub node_id: NodeId,

    pub op: BinaryOp,

    pub left: Box<Expr>,

    pub right: Box<Expr>,
}

/// Function expression.
#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct FnExpr {
    pub node_id: NodeId,

    pub ident: Option<Ident>,
    pub function: Box<Function>,
}

/// Class expression.
#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct ClassExpr {
    pub node_id: NodeId,

    pub ident: Option<Ident>,
    pub class: Class,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct AssignExpr {
    pub node_id: NodeId,

    pub op: AssignOp,

    pub left: PatOrExpr,

    pub right: Box<Expr>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct MemberExpr {
    pub node_id: NodeId,

    pub obj: ExprOrSuper,

    pub prop: Box<Expr>,

    pub computed: bool,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct CondExpr {
    pub node_id: NodeId,

    pub test: Box<Expr>,

    pub cons: Box<Expr>,

    pub alt: Box<Expr>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct CallExpr {
    pub node_id: NodeId,

    pub callee: ExprOrSuper,

    pub args: Vec<ExprOrSpread>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct NewExpr {
    pub node_id: NodeId,

    pub callee: Box<Expr>,

    pub args: Option<Vec<ExprOrSpread>>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct SeqExpr {
    pub node_id: NodeId,

    pub exprs: Vec<Expr>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct ArrowExpr {
    pub node_id: NodeId,

    pub params: Vec<Param>,

    pub body: BlockStmt,

    pub is_async: bool,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct YieldExpr {
    pub node_id: NodeId,

    pub arg: Option<Box<Expr>>,

    pub delegate: bool,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct MetaPropExpr {
    pub node_id: NodeId,

    pub meta: Ident,

    pub prop: Ident,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct AwaitExpr {
    pub node_id: NodeId,

    pub arg: Box<Expr>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct Tpl {
    pub node_id: NodeId,

    pub exprs: Vec<Expr>,

    pub quasis: Vec<TplElement>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct TaggedTpl {
    pub node_id: NodeId,

    pub tag: Box<Expr>,

    pub tpl: Box<Tpl>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct TplElement {
    pub node_id: NodeId,
    pub raw: Str,
}

#[allow(variant_size_differences)]
#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub enum ExprOrSuper {
    Super(Super),

    Expr(Box<Expr>),
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct Super {
    pub node_id: NodeId,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub enum ExprOrSpread {
    Spread(SpreadElement),
    Expr(Box<Expr>),
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub enum PatOrExpr {
    Expr(Box<Expr>),
    Pat(Box<Pat>),
}

impl From<Bool> for Expr {
    fn from(v: Bool) -> Self {
        Expr::Lit(Lit::Bool(v))
    }
}

impl From<Number> for Expr {
    fn from(v: Number) -> Self {
        Expr::Lit(Lit::Num(v))
    }
}

impl From<Str> for Expr {
    fn from(v: Str) -> Self {
        Expr::Lit(Lit::Str(v))
    }
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct OptChainExpr {
    pub node_id: NodeId,
    pub expr: Box<Expr>,
}
