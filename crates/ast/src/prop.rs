use crate::{
    expr::Expr,
    function::Function,
    ident::Ident,
    lit::{Number, Str},
    stmt::BlockStmt,
    typescript::TsTypeAnn,
    NodeId, ParamWithoutDecorators,
};
use global_common::{ast_node, EqIgnoreSpan, Span};

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum Prop {
    /// `a` in `{ a, }`
    #[tag("Identifier")]
    Shorthand(Ident),

    /// `key: value` in `{ key: value, }`
    #[tag("KeyValueProperty")]
    KeyValue(KeyValueProp),

    /// This is **invalid** for object literal.
    #[tag("AssignmentProperty")]
    Assign(AssignProp),

    #[tag("GetterProperty")]
    Getter(GetterProp),

    #[tag("SetterProperty")]
    Setter(SetterProp),

    #[tag("MethodProperty")]
    Method(MethodProp),

    /// Spread properties, e.g., `{a: 1, ...obj, b: 2}`.
    #[tag("SpreadAssignment")]
    Spread(SpreadAssignment),
}

#[ast_node("KeyValueProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct KeyValueProp {
    pub node_id: NodeId,

    #[span(lo)]
    pub key: PropName,

    #[span(hi)]
    pub value: Box<Expr>,
}

#[ast_node("AssignmentProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AssignProp {
    pub node_id: NodeId,

    #[span(lo)]
    pub key: Ident,
    #[span(hi)]
    pub value: Box<Expr>,
}

#[ast_node("GetterProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct GetterProp {
    pub node_id: NodeId,

    pub span: Span,
    pub key: PropName,
    #[serde(default, rename = "typeAnnotation")]
    pub type_ann: Option<TsTypeAnn>,
    #[serde(default)]
    pub body: Option<BlockStmt>,
}
#[ast_node("SetterProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SetterProp {
    pub node_id: NodeId,

    pub span: Span,
    pub key: PropName,
    // TODO:
    pub param: ParamWithoutDecorators,
    #[serde(default)]
    pub body: Option<BlockStmt>,
}
#[ast_node("MethodProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct MethodProp {
    pub node_id: NodeId,

    pub key: PropName,

    #[serde(flatten)]
    #[span]
    pub function: Function,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum PropName {
    #[tag("Identifier")]
    Ident(Ident),
    /// String literal.
    #[tag("StringLiteral")]
    Str(Str),
    /// Numeric literal.
    #[tag("NumericLiteral")]
    Num(Number),
    #[tag("Computed")]
    Computed(ComputedPropName),
}

#[ast_node("Computed")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ComputedPropName {
    pub node_id: NodeId,

    /// Span including `[` and `]`.
    pub span: Span,
    #[serde(rename = "expression")]
    pub expr: Box<Expr>,
}

#[ast_node("SpreadAssignment")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SpreadAssignment {
    pub node_id: NodeId,

    #[serde(rename = "spread")]
    #[span(lo)]
    pub dot3_token: Span,

    #[serde(rename = "arguments")]
    #[span(hi)]
    pub expr: Box<Expr>,
}
