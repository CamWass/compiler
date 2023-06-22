use crate::{
    expr::Expr,
    function::Function,
    ident::Ident,
    lit::{Number, Str},
    stmt::BlockStmt,
    typescript::TsTypeAnn,
    GetNodeId, NodeId, ParamWithoutDecorators,
};
use ast_node::ast_node;
use global_common::{util::take::Take, EqIgnoreSpan, Span, DUMMY_SP};
use swc_atoms::js_word;

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum Prop {
    /// `a` in `{ a, }`
    Shorthand(Ident),

    /// `key: value` in `{ key: value, }`
    KeyValue(KeyValueProp),

    /// This is **invalid** for object literal.
    Assign(AssignProp),

    Getter(GetterProp),

    Setter(SetterProp),

    Method(MethodProp),

    /// Spread properties, e.g., `{a: 1, ...obj, b: 2}`.
    Spread(SpreadAssignment),
}

impl Take for Prop {
    fn dummy() -> Self {
        Self::Shorthand(Ident {
            node_id: NodeId::DUMMY,
            span: DUMMY_SP,
            sym: js_word!(""),
            optional: false,
        })
    }
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct KeyValueProp {
    pub node_id: NodeId,

    #[span(lo)]
    pub key: PropName,

    #[span(hi)]
    pub value: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AssignProp {
    pub node_id: NodeId,

    #[span(lo)]
    pub key: Ident,
    #[span(hi)]
    pub value: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct GetterProp {
    pub node_id: NodeId,

    pub span: Span,
    pub key: PropName,
    pub type_ann: Option<TsTypeAnn>,
    pub body: Option<BlockStmt>,
}
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SetterProp {
    pub node_id: NodeId,

    pub span: Span,
    pub key: PropName,
    // TODO:
    pub param: ParamWithoutDecorators,
    pub body: Option<BlockStmt>,
}
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct MethodProp {
    pub node_id: NodeId,

    pub key: PropName,

    #[span]
    pub function: Function,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum PropName {
    Ident(Ident),
    /// String literal.
    Str(Str),
    /// Numeric literal.
    Num(Number),
    Computed(ComputedPropName),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ComputedPropName {
    pub node_id: NodeId,

    /// Span including `[` and `]`.
    pub span: Span,
    pub expr: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SpreadAssignment {
    pub node_id: NodeId,

    #[span(lo)]
    pub dot3_token: Span,

    #[span(hi)]
    pub expr: Box<Expr>,
}
