use crate::{
    expr::Expr,
    ident::{BindingIdent, Ident},
    prop::PropName,
    typescript::TsTypeAnn,
    Invalid, NodeId,
};
use global_common::{ast_node, EqIgnoreSpan, Span};

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum Pat {
    Ident(BindingIdent),

    Array(ArrayPat),

    Rest(RestPat),

    Object(ObjectPat),

    Assign(AssignPat),

    Invalid(Invalid),

    /// Only for for-in / for-of loops. This is *syntactically* valid.
    Expr(Box<Expr>),
}

#[ast_node("ArrayPattern")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ArrayPat {
    pub node_id: NodeId,

    pub span: Span,

    pub elems: Vec<Option<Pat>>,

    /// Only in an ambient context
    pub optional: bool,

    pub type_ann: Option<TsTypeAnn>,
}

#[ast_node("ObjectPattern")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ObjectPat {
    pub node_id: NodeId,

    pub span: Span,

    pub props: Vec<ObjectPatProp>,

    /// Only in an ambient context
    pub optional: bool,

    pub type_ann: Option<TsTypeAnn>,
}

#[ast_node("AssignmentPattern")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AssignPat {
    pub node_id: NodeId,

    pub span: Span,

    pub left: Box<Pat>,

    pub right: Box<Expr>,

    pub type_ann: Option<TsTypeAnn>,
}

/// EsTree `RestElement`
#[ast_node("RestElement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct RestPat {
    pub node_id: NodeId,

    pub span: Span,

    pub dot3_token: Span,

    pub arg: Box<Pat>,

    pub type_ann: Option<TsTypeAnn>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ObjectPatProp {
    KeyValue(KeyValuePatProp),

    Assign(AssignPatProp),

    Rest(RestPat),
}

/// `{key: value}`
#[ast_node("KeyValuePatternProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct KeyValuePatProp {
    pub node_id: NodeId,

    #[span(lo)]
    pub key: PropName,

    #[span(hi)]
    pub value: Box<Pat>,
}
/// `{key}` or `{key = value}`
#[ast_node("AssignmentPatternProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AssignPatProp {
    pub node_id: NodeId,

    pub span: Span,
    pub key: Ident,

    pub value: Option<Box<Expr>>,
}
