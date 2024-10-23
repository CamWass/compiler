#![allow(clippy::vec_box)]
use crate::{
    class::Class,
    function::Function,
    ident::{Ident, PrivateName},
    lit::{Bool, Lit, Number, Str},
    operators::{AssignOp, BinaryOp, UnaryOp, UpdateOp},
    pat::Pat,
    prop::Prop,
    stmt::BlockStmt,
    GetNodeId, Invalid, NodeId, ParamWithoutDecorators,
};
use clone_node::CloneNode;
use global_common::util::take::Take;
use node_id::GetNodeIdMacro;

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
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

    //
    // Logical {
    //
    //     op: LogicalOp,
    //     left: Box<Expr>,
    //     right: Box<Expr>,
    // },
    /// A member expression. If computed is true, the node corresponds to a
    /// computed (a[b]) member expression and property is an Expression. If
    /// computed is false, the node corresponds to a static (a.b) member
    /// expression and property is an Identifier.
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

    Arrow(ArrowExpr),

    Class(ClassExpr),

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

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ThisExpr {
    pub node_id: NodeId,
}

/// Array literal.
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ArrayLit {
    pub node_id: NodeId,

    pub elems: Vec<Option<ExprOrSpread>>,
}

/// Object literal.
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ObjectLit {
    pub node_id: NodeId,

    pub props: Vec<Prop>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct SpreadElement {
    pub node_id: NodeId,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct UnaryExpr {
    pub node_id: NodeId,

    pub op: UnaryOp,

    pub arg: Box<Expr>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct UpdateExpr {
    pub node_id: NodeId,

    pub op: UpdateOp,

    pub prefix: bool,

    pub arg: Box<Expr>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct BinExpr {
    pub node_id: NodeId,

    pub op: BinaryOp,

    pub left: Box<Expr>,

    pub right: Box<Expr>,
}

/// Function expression.
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct FnExpr {
    pub node_id: NodeId,

    pub ident: Option<Ident>,
    pub function: Function,
}

/// Class expression.
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ClassExpr {
    pub node_id: NodeId,

    pub ident: Option<Ident>,
    pub class: Class,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct AssignExpr {
    pub node_id: NodeId,

    pub op: AssignOp,

    pub left: PatOrExpr,

    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct MemberExpr {
    pub node_id: NodeId,

    pub obj: ExprOrSuper,

    pub prop: Box<Expr>,

    pub computed: bool,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct CondExpr {
    pub node_id: NodeId,

    pub test: Box<Expr>,

    pub cons: Box<Expr>,

    pub alt: Box<Expr>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct CallExpr {
    pub node_id: NodeId,

    pub callee: ExprOrSuper,

    pub args: Vec<ExprOrSpread>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct NewExpr {
    pub node_id: NodeId,

    pub callee: Box<Expr>,

    pub args: Option<Vec<ExprOrSpread>>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct SeqExpr {
    pub node_id: NodeId,

    pub exprs: Vec<Box<Expr>>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ArrowExpr {
    pub node_id: NodeId,

    // TODO:
    pub params: Vec<ParamWithoutDecorators>,

    pub body: BlockStmt,

    pub is_async: bool,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct YieldExpr {
    pub node_id: NodeId,

    pub arg: Option<Box<Expr>>,

    pub delegate: bool,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct MetaPropExpr {
    pub node_id: NodeId,

    pub meta: Ident,

    pub prop: Ident,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct AwaitExpr {
    pub node_id: NodeId,

    pub arg: Box<Expr>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Tpl {
    pub node_id: NodeId,

    pub exprs: Vec<Box<Expr>>,

    pub quasis: Vec<TplElement>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct TaggedTpl {
    pub node_id: NodeId,

    pub tag: Box<Expr>,

    pub tpl: Tpl,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct TplElement {
    pub node_id: NodeId,
    pub tail: bool,
    pub cooked: Option<Str>,
    pub raw: Str,
}

#[allow(variant_size_differences)]
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum ExprOrSuper {
    Super(Super),

    Expr(Box<Expr>),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Super {
    pub node_id: NodeId,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum ExprOrSpread {
    Spread(SpreadElement),
    Expr(Box<Expr>),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
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

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct OptChainExpr {
    pub node_id: NodeId,
    pub expr: Box<Expr>,
}
