use crate::{
    ArrayAssignmentPat, BindingIdent, BindingPatOrIdent, FunctionParams, GetNodeId, NodeId,
    ObjectAssignmentPat, ProgramData,
    class::Class,
    function::Function,
    ident::{Ident, PrivateName},
    lit::Lit,
    operators::{AssignOp, BinaryOp, UnaryOp, UpdateOp},
    pat::AssignmentPat,
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
}

impl Take for Expr {
    fn dummy() -> Self {
        Expr::Ident(Ident::dummy())
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

    pub arg: Box<SimpleAssignTarget>,
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

    pub left: Box<AssignTarget>,

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

    pub params: FunctionParams,

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
    pub value: TplString,
}

#[derive(Debug, CloneNode, NodeEq, Serialize, Clone, PartialEq)]
pub enum TplString {
    Cooked(Box<String>),
    /// Only used when the raw value contains invalid unicode escapes, which are
    /// only possible in tagged template in ES2017 and later.
    Raw(Box<String>),
}

impl TplString {
    pub fn has_invalid_escape(&self) -> bool {
        matches!(self, TplString::Raw(_))
    }

    pub fn is_empty(&self) -> bool {
        match &self {
            TplString::Cooked(s) => s.is_empty(),
            TplString::Raw(s) => s.is_empty(),
        }
    }
}

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
pub enum AssignTarget {
    Simple(SimpleAssignTarget),
    AssignmentPat(AssignmentPat),
}

impl AssignTarget {
    pub fn from_binding_pat_or_ident(
        binding_pat_or_ident: BindingPatOrIdent,
        program_data: &mut ProgramData,
    ) -> Self {
        match binding_pat_or_ident {
            BindingPatOrIdent::Array(array_binding_pat) => {
                AssignTarget::AssignmentPat(AssignmentPat::Array(
                    ArrayAssignmentPat::from_array_binding_pat(array_binding_pat, program_data),
                ))
            }
            BindingPatOrIdent::Object(object_binding_pat) => {
                AssignTarget::AssignmentPat(AssignmentPat::Object(
                    ObjectAssignmentPat::from_object_binding_pat(object_binding_pat, program_data),
                ))
            }
            BindingPatOrIdent::Ident(binding_ident) => {
                AssignTarget::Simple(SimpleAssignTarget::Ident(binding_ident))
            }
        }
    }
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub enum SimpleAssignTarget {
    Ident(BindingIdent),
    Member(MemberExpr),
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct OptChainExpr {
    pub node_id: NodeId,
    pub base: Box<OptChainBase>,
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub enum OptChainBase {
    Call(CallExpr),
    Member(MemberExpr),
}
