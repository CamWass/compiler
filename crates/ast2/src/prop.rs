use crate::{
    expr::Expr,
    function::Function,
    ident::Ident,
    lit::{BigInt, Number, Str},
    node::NodeId,
    pat::Pat,
    stmt::BlockStmt,
    typescript::TsTypeAnn,
};
use ast_node2::ast_node;
use global_common::{EqIgnoreSpan, Span};

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum Prop<'ast> {
    /// `a` in `{ a, }`
    Shorthand(&'ast Ident),

    /// `key: value` in `{ key: value, }`
    KeyValue(&'ast KeyValueProp<'ast>),

    /// This is **invalid** for object literal.
    Assign(&'ast AssignProp<'ast>),
    Getter(&'ast GetterProp<'ast>),
    Setter(&'ast SetterProp<'ast>),
    Method(&'ast MethodProp<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct KeyValueProp<'ast> {
    pub node_id: NodeId,
    #[span(lo)]
    pub key: PropName<'ast>,

    #[span(hi)]
    pub value: Expr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AssignProp<'ast> {
    pub node_id: NodeId,
    #[span(lo)]
    pub key: &'ast Ident,
    #[span(hi)]
    pub value: Expr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct GetterProp<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub key: PropName<'ast>,

    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,

    pub body: Option<&'ast BlockStmt<'ast>>,
}
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SetterProp<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub key: PropName<'ast>,
    pub param: Pat<'ast>,

    pub body: Option<&'ast BlockStmt<'ast>>,
}
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct MethodProp<'ast> {
    pub node_id: NodeId,
    pub key: PropName<'ast>,

    #[span]
    pub function: &'ast Function<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum PropName<'ast> {
    Ident(&'ast Ident),
    /// String literal.
    Str(&'ast Str),
    /// Numeric literal.
    Num(&'ast Number),
    Computed(&'ast ComputedPropName<'ast>),
    BigInt(&'ast BigInt),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ComputedPropName<'ast> {
    pub node_id: NodeId,
    /// Span including `[` and `]`.
    pub span: Span,

    pub expr: Expr<'ast>,
}
