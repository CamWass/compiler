use super::{
    expr::Expr,
    function::Function,
    ident::Ident,
    lit::{BigInt, Number, Str},
    pat::Pat,
    stmt::BlockStmt,
    typescript::TsTypeAnn,
};
use global_common::{ast_node, EqIgnoreSpan, Span};

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
}

#[ast_node("KeyValueProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct KeyValueProp {
    #[span(lo)]
    pub key: PropName,

    #[span(hi)]
    pub value: Box<Expr>,
}

#[ast_node("AssignmentProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct AssignProp {
    #[span(lo)]
    pub key: Ident,
    #[span(hi)]
    pub value: Box<Expr>,
}

#[ast_node("GetterProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct GetterProp {
    pub span: Span,
    pub key: PropName,
    pub type_ann: Option<TsTypeAnn>,
    pub body: Option<BlockStmt>,
}
#[ast_node("SetterProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct SetterProp {
    pub span: Span,
    pub key: PropName,
    pub param: Pat,
    pub body: Option<BlockStmt>,
}
#[ast_node("MethodProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct MethodProp {
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
    BigInt(BigInt),
}

#[ast_node("Computed")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ComputedPropName {
    /// Span including `[` and `]`.
    pub span: Span,
    pub expr: Box<Expr>,
}
