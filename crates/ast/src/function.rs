use crate::{
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

    #[serde(default)]
    pub decorators: Vec<Decorator>,

    pub span: Span,

    #[serde(default)]
    pub body: Option<BlockStmt>,

    /// if it's a generator.
    #[serde(default, rename = "generator")]
    pub is_generator: bool,

    /// if it's an async function.
    #[serde(default, rename = "async")]
    pub is_async: bool,

    #[serde(default, rename = "typeParameters")]
    pub type_params: Option<Vec<TsTypeParamDecl>>,

    #[serde(default)]
    pub return_type: Option<TsTypeAnn>,
}

#[ast_node("Parameter")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Param {
    pub span: Span,
    #[serde(default)]
    pub decorators: Vec<Decorator>,
    pub pat: Pat,
}

#[ast_node("ParamWithoutDecorators")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ParamWithoutDecorators {
    #[span]
    pub pat: Pat,
}

impl From<Pat> for ParamWithoutDecorators {
    fn from(pat: Pat) -> Self {
        Self { pat }
    }
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ParamOrTsParamProp {
    #[tag("TsParameterProperty")]
    TsParamProp(TsParamProp),
    #[tag("Parameter")]
    Param(Param),
}
