#![allow(clippy::vec_box)]
use crate::{
    class::Class,
    function::Function,
    ident::{Ident, PrivateName},
    jsx::{JSXElement, JSXEmptyExpr, JSXFragment, JSXMemberExpr, JSXNamespacedName},
    lit::{Bool, Lit, Number, Str},
    node::NodeId,
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
use ast_node2::ast_node;
use global_common::EqIgnoreSpan;
use global_common::{Span, Spanned, DUMMY_SP};

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum Expr<'ast> {
    This(&'ast ThisExpr),
    Array(&'ast ArrayLit<'ast>),
    Object(&'ast ObjectLit<'ast>),
    Fn(&'ast FnExpr<'ast>),
    Unary(&'ast UnaryExpr<'ast>),
    /// `++v`, `--v`, `v++`, `v--`
    Update(&'ast UpdateExpr<'ast>),
    Bin(&'ast BinExpr<'ast>),
    Assign(&'ast AssignExpr<'ast>),
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
    Member(&'ast MemberExpr<'ast>),
    /// true ? 'a' : 'b'
    Cond(&'ast CondExpr<'ast>),
    Call(&'ast CallExpr<'ast>),
    /// `new Cat()`
    New(&'ast NewExpr<'ast>),
    Seq(&'ast SeqExpr<'ast>),
    Ident(&'ast Ident),
    Lit(Lit<'ast>),
    Tpl(&'ast Tpl<'ast>),
    TaggedTpl(&'ast TaggedTpl<'ast>),
    Arrow(&'ast ArrowExpr<'ast>),
    Class(&'ast ClassExpr<'ast>),
    Yield(&'ast YieldExpr<'ast>),
    MetaProp(&'ast MetaPropExpr<'ast>),
    Await(&'ast AwaitExpr<'ast>),
    Paren(&'ast ParenExpr<'ast>),
    JSXMember(&'ast JSXMemberExpr<'ast>),
    JSXNamespacedName(&'ast JSXNamespacedName<'ast>),
    JSXEmpty(&'ast JSXEmptyExpr),
    JSXElement(&'ast JSXElement<'ast>),
    JSXFragment(&'ast JSXFragment<'ast>),
    TsTypeAssertion(&'ast TsTypeAssertion<'ast>),
    TsConstAssertion(&'ast TsConstAssertion<'ast>),
    TsNonNull(&'ast TsNonNullExpr<'ast>),
    TsAs(&'ast TsAsExpr<'ast>),
    PrivateName(&'ast PrivateName<'ast>),
    OptChain(&'ast OptChainExpr<'ast>),
    Invalid(&'ast Invalid),
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
pub struct ArrayLit<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub elems: Vec<Option<&'ast ExprOrSpread<'ast>>>,
}

/// Object literal.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ObjectLit<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub props: Vec<PropOrSpread<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum PropOrSpread<'ast> {
    /// Spread properties, e.g., `{a: 1, ...obj, b: 2}`.
    Spread(&'ast SpreadElement<'ast>),
    Prop(Prop<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SpreadElement<'ast> {
    pub node_id: NodeId,
    #[span(lo)]
    pub dot3_token: Span,
    #[span(hi)]
    pub expr: Expr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct UnaryExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub op: UnaryOp,
    pub arg: Expr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct UpdateExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub op: UpdateOp,
    pub prefix: bool,
    pub arg: Expr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct BinExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub op: BinaryOp,
    pub left: Expr<'ast>,
    pub right: Expr<'ast>,
}

/// Function expression.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct FnExpr<'ast> {
    pub node_id: NodeId,
    pub ident: Option<&'ast Ident>,
    #[span]
    pub function: &'ast Function<'ast>,
}

/// Class expression.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ClassExpr<'ast> {
    pub node_id: NodeId,
    pub ident: Option<&'ast Ident>,
    #[span]
    pub class: &'ast Class<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AssignExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub op: AssignOp,
    pub left: PatOrExpr<'ast>,
    pub right: Expr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct MemberExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub obj: ExprOrSuper<'ast>,
    pub prop: Expr<'ast>,
    pub computed: bool,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct CondExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub test: Expr<'ast>,
    pub cons: Expr<'ast>,
    pub alt: Expr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct CallExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub callee: ExprOrSuper<'ast>,
    pub args: Vec<&'ast ExprOrSpread<'ast>>,
    pub type_args: Option<&'ast TsTypeParamInstantiation<'ast>>,
    // pub type_params: Option<TsTypeParamInstantiation>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct NewExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub callee: Expr<'ast>,
    pub args: Option<Vec<&'ast ExprOrSpread<'ast>>>,
    pub type_args: Option<&'ast TsTypeParamInstantiation<'ast>>,
    // pub type_params: Option<TsTypeParamInstantiation>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SeqExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub exprs: Vec<Expr<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ArrowExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub params: Vec<Pat<'ast>>,
    pub body: BlockStmtOrExpr<'ast>,
    pub is_async: bool,
    pub is_generator: bool,
    pub type_params: Option<&'ast TsTypeParamDecl<'ast>>,
    pub return_type: Option<&'ast TsTypeAnn<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct YieldExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub arg: Option<Expr<'ast>>,
    pub delegate: bool,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct MetaPropExpr<'ast> {
    pub node_id: NodeId,
    #[span(lo)]
    pub meta: &'ast Ident,

    #[span(hi)]
    pub prop: &'ast Ident,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AwaitExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub arg: Expr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Tpl<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub exprs: Vec<Expr<'ast>>,

    pub quasis: Vec<&'ast TplElement<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TaggedTpl<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub tag: Expr<'ast>,

    pub type_params: Option<&'ast TsTypeParamInstantiation<'ast>>,

    pub tpl: &'ast Tpl<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TplElement<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub tail: bool,
    pub cooked: Option<&'ast Str>,
    pub raw: &'ast Str,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ParenExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub expr: Expr<'ast>,
}

#[ast_node]
#[allow(variant_size_differences)]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum ExprOrSuper<'ast> {
    Super(&'ast Super),

    Expr(Expr<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct Super {
    pub node_id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub struct ExprOrSpread<'ast> {
    pub node_id: NodeId,

    pub spread: Option<Span>,

    pub expr: Expr<'ast>,
}

impl Spanned for ExprOrSpread<'_> {
    fn span(&self) -> Span {
        let expr = self.expr.span();
        match self.spread {
            Some(spread) => expr.with_lo(spread.lo()),
            None => expr,
        }
    }
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum BlockStmtOrExpr<'ast> {
    BlockStmt(&'ast BlockStmt<'ast>),

    Expr(Expr<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum PatOrExpr<'ast> {
    Expr(Expr<'ast>),

    Pat(Pat<'ast>),
}

// impl From<bool> for Expr {
//     fn from(value: bool) -> Self {
//         Expr::Lit(Lit::Bool(Bool {
//             span: DUMMY_SP,
//             value,
//         }))
//     }
// }

// impl From<f64> for Expr {
//     fn from(value: f64) -> Self {
//         Expr::Lit(Lit::Num(Number {
//             span: DUMMY_SP,
//             value,
//         }))
//     }
// }

// impl From<Bool> for Expr {
//     fn from(v: Bool) -> Self {
//         Expr::Lit(Lit::Bool(v))
//     }
// }

// impl From<Number> for Expr {
//     fn from(v: Number) -> Self {
//         Expr::Lit(Lit::Num(v))
//     }
// }

// impl From<Str> for Expr {
//     fn from(v: Str) -> Self {
//         Expr::Lit(Lit::Str(v))
//     }
// }

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct OptChainExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub question_dot_token: Span,
    pub expr: Expr<'ast>,
}
