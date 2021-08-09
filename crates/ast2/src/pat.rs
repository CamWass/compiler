use crate::ident::BindingIdent;
use crate::{
    expr::Expr, ident::Ident, node::NodeId, prop::PropName, typescript::TsTypeAnn, Invalid,
};
use ast_node2::ast_node;
use global_common::{EqIgnoreSpan, Span};

// impl From<Ident> for Pat {
//     fn from(i: Ident) -> Self {
//         BindingIdent::from(i).into()
//     }
// }

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum Pat<'ast> {
    Ident(&'ast BindingIdent<'ast>),
    Array(&'ast ArrayPat<'ast>),
    Rest(&'ast RestPat<'ast>),
    Object(&'ast ObjectPat<'ast>),
    Assign(&'ast AssignPat<'ast>),
    Invalid(&'ast Invalid),

    /// Only for for-in / for-of loops. This is *syntactically* valid.
    Expr(Expr<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ArrayPat<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub elems: Vec<Option<Pat<'ast>>>,

    /// Only in an ambient context
    pub optional: bool,

    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ObjectPat<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub props: Vec<ObjectPatProp<'ast>>,

    /// Only in an ambient context
    pub optional: bool,

    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AssignPat<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub left: Pat<'ast>,

    pub right: Expr<'ast>,

    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,
}

/// EsTree `RestElement`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct RestPat<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub dot3_token: Span,

    pub arg: Pat<'ast>,

    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum ObjectPatProp<'ast> {
    KeyValue(&'ast KeyValuePatProp<'ast>),
    Assign(&'ast AssignPatProp<'ast>),
    Rest(&'ast RestPat<'ast>),
}

/// `{key: value}`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct KeyValuePatProp<'ast> {
    pub node_id: NodeId,
    #[span(lo)]
    pub key: PropName<'ast>,

    #[span(hi)]
    pub value: Pat<'ast>,
}
/// `{key}` or `{key = value}`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AssignPatProp<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub key: &'ast Ident,
    pub value: Option<Expr<'ast>>,
}
