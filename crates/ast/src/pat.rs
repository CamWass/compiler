use crate::{
    expr::Expr,
    ident::{BindingIdent, Ident},
    prop::PropName,
    GetNodeId, Invalid, NodeId,
};
use ast_node::ast_node;
use global_common::{util::take::Take, EqIgnoreSpan, Span, DUMMY_SP};

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

impl Take for Pat {
    fn dummy() -> Self {
        Pat::Invalid(Invalid {
            node_id: NodeId::DUMMY,
            span: DUMMY_SP,
        })
    }
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ArrayPat {
    pub node_id: NodeId,

    pub span: Span,

    pub elems: Vec<Option<Pat>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ObjectPat {
    pub node_id: NodeId,

    pub span: Span,

    pub props: Vec<ObjectPatProp>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AssignPat {
    pub node_id: NodeId,

    pub span: Span,

    pub left: Box<Pat>,

    pub right: Box<Expr>,
}

/// EsTree `RestElement`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct RestPat {
    pub node_id: NodeId,

    pub span: Span,

    pub dot3_token: Span,

    pub arg: Box<Pat>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ObjectPatProp {
    KeyValue(KeyValuePatProp),

    Assign(AssignPatProp),

    Rest(RestPat),
}

impl Take for ObjectPatProp {
    fn dummy() -> Self {
        ObjectPatProp::Assign(AssignPatProp {
            node_id: NodeId::DUMMY,
            span: DUMMY_SP,
            key: Ident::dummy(),
            value: None,
        })
    }
}

/// `{key: value}`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct KeyValuePatProp {
    pub node_id: NodeId,

    #[span(lo)]
    pub key: PropName,

    #[span(hi)]
    pub value: Box<Pat>,
}
/// `{key}` or `{key = value}`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AssignPatProp {
    pub node_id: NodeId,

    pub span: Span,
    pub key: Ident,

    pub value: Option<Box<Expr>>,
}
