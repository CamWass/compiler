use super::{
    class::Decorator,
    pat::Pat,
    stmt::BlockStmt,
    typescript::{TsParamProp, TsTypeAnn, TsTypeParamDecl},
};
use global_common::{ast_node, EqIgnoreSpan, Span};

/// Common parts of function and method.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Function {
    pub params: Vec<Param>,

    pub decorators: Vec<Decorator>,

    pub span: Span,

    pub body: Option<BlockStmt>,

    /// if it's a generator.
    pub is_generator: bool,

    /// if it's an async function.
    pub is_async: bool,

    pub type_params: Option<TsTypeParamDecl>,

    pub return_type: Option<TsTypeAnn>,
}

#[ast_node("Parameter")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Param {
    pub span: Span,
    pub decorators: Vec<Decorator>,
    pub pat: Pat,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ParamOrTsParamProp {
    TsParamProp(TsParamProp),
    Param(Param),
}
