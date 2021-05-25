#![allow(clippy::vec_box)]
use super::{
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
        TsAsExpr, TsConstAssertion, TsNonNullExpr, TsTypeAnn, TsTypeAssertion, TsTypeParamDecl,
        TsTypeParamInstantiation,
    },
    Invalid,
};
use global_common::{ast_node, EqIgnoreSpan, Span, Spanned, DUMMY_SP};

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

#[ast_node("ThisExpression")]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct ThisExpr {
    pub span: Span,
}

/// Array literal.
#[ast_node("ArrayExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ArrayLit {
    pub span: Span,
    pub elems: Vec<Option<ExprOrSpread>>,
}

/// Object literal.
#[ast_node("ObjectExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ObjectLit {
    pub span: Span,
    pub props: Vec<PropOrSpread>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum PropOrSpread {
    /// Spread properties, e.g., `{a: 1, ...obj, b: 2}`.
    Spread(SpreadElement),

    Prop(Box<Prop>),
}
#[ast_node("SpreadElement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SpreadElement {
    #[span(lo)]
    pub dot3_token: Span,

    #[span(hi)]
    pub expr: Box<Expr>,
}

#[ast_node("UnaryExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct UnaryExpr {
    pub span: Span,
    pub op: UnaryOp,

    pub arg: Box<Expr>,
}

#[ast_node("UpdateExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct UpdateExpr {
    pub span: Span,
    pub op: UpdateOp,

    pub prefix: bool,

    pub arg: Box<Expr>,
}

#[ast_node("BinaryExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct BinExpr {
    pub span: Span,
    pub op: BinaryOp,

    pub left: Box<Expr>,

    pub right: Box<Expr>,
}

/// Function expression.
#[ast_node("FunctionExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct FnExpr {
    pub ident: Option<Ident>,

    #[span]
    pub function: Function,
}

/// Class expression.
#[ast_node("ClassExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ClassExpr {
    pub ident: Option<Ident>,

    #[span]
    pub class: Class,
}

#[ast_node("AssignmentExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AssignExpr {
    pub span: Span,
    pub op: AssignOp,

    pub left: PatOrExpr,

    pub right: Box<Expr>,
}

#[ast_node("MemberExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct MemberExpr {
    pub span: Span,
    pub obj: ExprOrSuper,

    pub prop: Box<Expr>,

    pub computed: bool,
}

#[ast_node("ConditionalExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct CondExpr {
    pub span: Span,
    pub test: Box<Expr>,

    pub cons: Box<Expr>,

    pub alt: Box<Expr>,
}

#[ast_node("CallExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct CallExpr {
    pub span: Span,
    pub callee: ExprOrSuper,

    pub args: Vec<ExprOrSpread>,

    pub type_args: Option<TsTypeParamInstantiation>,
    // pub type_params: Option<TsTypeParamInstantiation>,
}

#[ast_node("NewExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct NewExpr {
    pub span: Span,
    pub callee: Box<Expr>,

    pub args: Option<Vec<ExprOrSpread>>,

    pub type_args: Option<TsTypeParamInstantiation>,
    // pub type_params: Option<TsTypeParamInstantiation>,
}

#[ast_node("SequenceExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SeqExpr {
    pub span: Span,
    pub exprs: Vec<Box<Expr>>,
}

#[ast_node("ArrowFunctionExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ArrowExpr {
    pub span: Span,
    pub params: Vec<Pat>,

    pub body: BlockStmtOrExpr,

    pub is_async: bool,

    pub is_generator: bool,

    pub type_params: Option<TsTypeParamDecl>,

    pub return_type: Option<TsTypeAnn>,
}

#[ast_node("YieldExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct YieldExpr {
    pub span: Span,
    pub arg: Option<Box<Expr>>,

    pub delegate: bool,
}

#[ast_node("MetaProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct MetaPropExpr {
    #[span(lo)]
    pub meta: Ident,

    #[span(hi)]
    pub prop: Ident,
}

#[ast_node("AwaitExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AwaitExpr {
    pub span: Span,
    pub arg: Box<Expr>,
}

#[ast_node("TemplateLiteral")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Tpl {
    pub span: Span,
    pub exprs: Vec<Box<Expr>>,

    pub quasis: Vec<TplElement>,
}

#[ast_node("TaggedTemplateExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TaggedTpl {
    pub span: Span,
    pub tag: Box<Expr>,

    pub type_params: Option<TsTypeParamInstantiation>,

    pub tpl: Tpl,
}

#[ast_node("TemplateElement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TplElement {
    pub span: Span,
    pub tail: bool,
    pub cooked: Option<Str>,
    pub raw: Str,
}

#[ast_node("ParenthesisExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ParenExpr {
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

#[ast_node("Super")]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct Super {
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub struct ExprOrSpread {
    pub spread: Option<Span>,

    pub expr: Box<Expr>,
}

impl Spanned for ExprOrSpread {
    fn span(&self) -> Span {
        let expr = self.expr.span();
        match self.spread {
            Some(spread) => expr.with_lo(spread.lo()),
            None => expr,
        }
    }
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

impl From<bool> for Expr {
    fn from(value: bool) -> Self {
        Expr::Lit(Lit::Bool(Bool {
            span: DUMMY_SP,
            value,
        }))
    }
}

impl From<f64> for Expr {
    fn from(value: f64) -> Self {
        Expr::Lit(Lit::Num(Number {
            span: DUMMY_SP,
            value,
        }))
    }
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

#[ast_node("OptionalChainingExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct OptChainExpr {
    pub span: Span,
    pub question_dot_token: Span,
    pub expr: Box<Expr>,
}
