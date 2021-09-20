use crate::{
  class::Decorator,
  node::NodeId,
  pat::Pat,
  stmt::BlockStmt,
  typescript::{TsParamProp, TsTypeAnn, TsTypeParamDecl},
};
use ast_node2::ast_node;
use global_common::{EqIgnoreSpan, Span};

/// Common parts of function and method.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Function<'ast> {
  pub node_id: NodeId,
  pub params: Vec<&'ast Param<'ast>>,
  pub decorators: Vec<&'ast Decorator<'ast>>,

  pub span: Span,
  pub body: Option<&'ast BlockStmt<'ast>>,

  /// if it's a generator.
  pub is_generator: bool,

  /// if it's an async function.
  pub is_async: bool,

  pub type_params: Option<&'ast TsTypeParamDecl<'ast>>,
  pub return_type: Option<&'ast TsTypeAnn<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Param<'ast> {
  pub node_id: NodeId,
  pub span: Span,

  pub decorators: Vec<&'ast Decorator<'ast>>,
  pub pat: Pat<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum ParamOrTsParamProp<'ast> {
  TsParamProp(&'ast TsParamProp<'ast>),
  Param(&'ast Param<'ast>),
}