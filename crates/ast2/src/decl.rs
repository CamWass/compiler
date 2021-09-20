use crate::{
  class::Class,
  expr::Expr,
  function::Function,
  ident::Ident,
  node::NodeId,
  pat::Pat,
  typescript::{TsEnumDecl, TsInterfaceDecl, TsModuleDecl, TsTypeAliasDecl},
};
use ast_node2::ast_node;
use global_common::{EqIgnoreSpan, Span};
use string_enum2::StringEnum;

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum Decl<'ast> {
  Class(&'ast ClassDecl<'ast>),
  Fn(&'ast FnDecl<'ast>),
  Var(&'ast VarDecl<'ast>),
  TsInterface(&'ast TsInterfaceDecl<'ast>),
  TsTypeAlias(&'ast TsTypeAliasDecl<'ast>),
  TsEnum(&'ast TsEnumDecl<'ast>),
  TsModule(&'ast TsModuleDecl<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct FnDecl<'ast> {
  pub node_id: NodeId,
  pub ident: &'ast Ident,
  pub declare: bool,
  #[span]
  pub function: &'ast Function<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ClassDecl<'ast> {
  pub node_id: NodeId,
  pub ident: &'ast Ident,
  pub declare: bool,
  #[span]
  pub class: &'ast Class<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct VarDecl<'ast> {
  pub node_id: NodeId,
  pub span: Span,
  pub kind: VarDeclKind,
  pub declare: bool,
  pub decls: Vec<&'ast VarDeclarator<'ast>>,
}

#[derive(StringEnum, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, EqIgnoreSpan)]
pub enum VarDeclKind {
  /// `var`
  Var,
  /// `let`
  Let,
  /// `const`
  Const,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct VarDeclarator<'ast> {
  pub node_id: NodeId,
  pub span: Span,
  pub name: Pat<'ast>,
  /// Initialization expression.
  pub init: Option<Expr<'ast>>,
  /// Typescript only
  pub definite: bool,
}