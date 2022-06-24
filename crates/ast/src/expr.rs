#![allow(clippy::vec_box)]
use crate::{
    class::Class,
    function::Function,
    ident::{Ident, PrivateName},
    jsx::{JSXElement, JSXEmptyExpr, JSXFragment, JSXMemberExpr, JSXNamespacedName},
    lit::{Bool, Lit, Number, Str},
    operators::{AssignOp, BinaryOp, UnaryOp, UpdateOp},
    pat::Pat,
    prop::Prop,
    stmt::BlockStmt,
    typescript::{
        TsAsExpr, TsConstAssertion, TsNonNullExpr, TsTypeAnn, TsTypeAssertion,
        TsTypeParamInstantiation,
    },
    GetNodeId, Invalid, NodeId, ParamWithoutDecorators, TsTypeParamDecl,
};
use ast_node::ast_node;
use global_common::{EqIgnoreSpan, Span};

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
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

    Paren(ParenExpr),

    JSXMember(JSXMemberExpr),

    JSXNamespacedName(JSXNamespacedName),

    JSXEmpty(JSXEmptyExpr),

    JSXElement(Box<JSXElement>),

    JSXFragment(JSXFragment),

    TsTypeAssertion(TsTypeAssertion),

    TsConstAssertion(TsConstAssertion),

    TsNonNull(TsNonNullExpr),

    TsAs(TsAsExpr),

    PrivateName(PrivateName),

    OptChain(OptChainExpr),

    Invalid(Invalid),
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct ThisExpr {
    pub node_id: NodeId,

    pub span: Span,
}

/// Array literal.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ArrayLit {
    pub node_id: NodeId,

    pub span: Span,

    pub elems: Vec<Option<ExprOrSpread>>,
}

/// Object literal.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ObjectLit {
    pub node_id: NodeId,

    pub span: Span,

    pub props: Vec<Prop>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SpreadElement {
    pub node_id: NodeId,

    #[span(lo)]
    pub dot3_token: Span,

    #[span(hi)]
    pub expr: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct UnaryExpr {
    pub node_id: NodeId,

    pub span: Span,

    pub op: UnaryOp,

    pub arg: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct UpdateExpr {
    pub node_id: NodeId,

    pub span: Span,

    pub op: UpdateOp,

    pub prefix: bool,

    pub arg: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct BinExpr {
    pub node_id: NodeId,

    pub span: Span,

    pub op: BinaryOp,

    pub left: Box<Expr>,

    pub right: Box<Expr>,
}

/// Function expression.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct FnExpr {
    pub node_id: NodeId,

    pub ident: Option<Ident>,

    #[span]
    pub function: Function,
}

/// Class expression.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ClassExpr {
    pub node_id: NodeId,

    pub ident: Option<Ident>,

    #[span]
    pub class: Class,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AssignExpr {
    pub node_id: NodeId,

    pub span: Span,

    pub op: AssignOp,

    pub left: PatOrExpr,

    pub right: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct MemberExpr {
    pub node_id: NodeId,

    pub span: Span,

    pub obj: ExprOrSuper,

    pub prop: Box<Expr>,

    pub computed: bool,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct CondExpr {
    pub node_id: NodeId,

    pub span: Span,

    pub test: Box<Expr>,

    pub cons: Box<Expr>,

    pub alt: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct CallExpr {
    pub node_id: NodeId,

    pub span: Span,

    pub callee: ExprOrSuper,

    pub args: Vec<ExprOrSpread>,

    pub type_args: Option<TsTypeParamInstantiation>,
    // pub type_params: Option<TsTypeParamInstantiation>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct NewExpr {
    pub node_id: NodeId,

    pub span: Span,

    pub callee: Box<Expr>,

    pub args: Option<Vec<ExprOrSpread>>,

    pub type_args: Option<TsTypeParamInstantiation>,
    // pub type_params: Option<TsTypeParamInstantiation>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SeqExpr {
    pub node_id: NodeId,

    pub span: Span,

    pub exprs: Vec<Box<Expr>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ArrowExpr {
    pub node_id: NodeId,

    pub span: Span,

    // TODO:
    pub params: Vec<ParamWithoutDecorators>,

    pub body: BlockStmtOrExpr,

    pub is_async: bool,

    pub type_params: Option<Vec<TsTypeParamDecl>>,

    pub return_type: Option<TsTypeAnn>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct YieldExpr {
    pub node_id: NodeId,

    pub span: Span,

    pub arg: Option<Box<Expr>>,

    pub delegate: bool,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct MetaPropExpr {
    pub node_id: NodeId,

    #[span(lo)]
    pub meta: Ident,

    #[span(hi)]
    pub prop: Ident,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AwaitExpr {
    pub node_id: NodeId,

    pub span: Span,

    pub arg: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Tpl {
    pub node_id: NodeId,

    pub span: Span,

    pub exprs: Vec<Box<Expr>>,

    pub quasis: Vec<TplElement>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TaggedTpl {
    pub node_id: NodeId,

    pub span: Span,

    pub tag: Box<Expr>,

    pub type_params: Option<TsTypeParamInstantiation>,

    pub tpl: Tpl,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TplElement {
    pub node_id: NodeId,

    pub span: Span,
    pub tail: bool,
    pub cooked: Option<Str>,
    pub raw: Str,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ParenExpr {
    pub node_id: NodeId,

    pub span: Span,

    pub expr: Box<Expr>,
}

#[ast_node]
#[allow(variant_size_differences)]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ExprOrSuper {
    Super(Super),

    Expr(Box<Expr>),
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct Super {
    pub node_id: NodeId,

    pub span: Span,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ExprOrSpread {
    Spread(SpreadElement),
    Expr(Box<Expr>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum BlockStmtOrExpr {
    BlockStmt(BlockStmt),
    Expr(Box<Expr>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
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

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct OptChainExpr {
    pub node_id: NodeId,

    pub span: Span,
    pub question_dot_token: Span,
    pub expr: Box<Expr>,
}
