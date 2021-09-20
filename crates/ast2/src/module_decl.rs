use crate::{
  decl::Decl,
  expr::{ClassExpr, Expr, FnExpr},
  ident::Ident,
  lit::Str,
  node::NodeId,
  typescript::{TsExportAssignment, TsImportEqualsDecl, TsInterfaceDecl, TsNamespaceExportDecl},
  ObjectLit,
};
use ast_node2::ast_node;
use global_common::{EqIgnoreSpan, Span};

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum ModuleDecl<'ast> {
  Import(&'ast ImportDecl<'ast>),
  ExportDecl(&'ast ExportDecl<'ast>),
  ExportNamed(&'ast NamedExport<'ast>),
  ExportDefaultDecl(&'ast ExportDefaultDecl<'ast>),
  ExportDefaultExpr(&'ast ExportDefaultExpr<'ast>),
  ExportAll(&'ast ExportAll<'ast>),
  TsImportEquals(&'ast TsImportEqualsDecl<'ast>),
  TsExportAssignment(&'ast TsExportAssignment<'ast>),
  TsNamespaceExport(&'ast TsNamespaceExportDecl<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDefaultExpr<'ast> {
  pub node_id: NodeId,
  pub span: Span,

  pub expr: Expr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDecl<'ast> {
  pub node_id: NodeId,
  pub span: Span,

  pub decl: Decl<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportDecl<'ast> {
  pub node_id: NodeId,
  pub span: Span,
  pub specifiers: Vec<ImportSpecifier<'ast>>,

  pub src: &'ast Str,

  pub type_only: bool,
  pub asserts: Option<&'ast ObjectLit<'ast>>,
}

/// `export * from 'mod'`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportAll<'ast> {
  pub node_id: NodeId,
  pub span: Span,

  pub src: &'ast Str,
  pub asserts: Option<&'ast ObjectLit<'ast>>,
}

/// `export { foo } from 'mod'`
/// `export { foo as bar } from 'mod'`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct NamedExport<'ast> {
  pub node_id: NodeId,
  pub span: Span,

  pub specifiers: Vec<ExportSpecifier<'ast>>,

  pub src: Option<&'ast Str>,

  pub type_only: bool,
  pub asserts: Option<&'ast ObjectLit<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDefaultDecl<'ast> {
  pub node_id: NodeId,
  pub span: Span,

  pub decl: DefaultDecl<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum DefaultDecl<'ast> {
  Class(&'ast ClassExpr<'ast>),
  Fn(&'ast FnExpr<'ast>),
  TsInterfaceDecl(&'ast TsInterfaceDecl<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum ImportSpecifier<'ast> {
  Named(&'ast ImportNamedSpecifier<'ast>),
  Default(&'ast ImportDefaultSpecifier<'ast>),
  Namespace(&'ast ImportStarAsSpecifier<'ast>),
}

/// e.g. `import foo from 'mod.js'`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportDefaultSpecifier<'ast> {
  pub node_id: NodeId,
  pub span: Span,

  pub local: &'ast Ident,
}
/// e.g. `import * as foo from 'mod.js'`.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportStarAsSpecifier<'ast> {
  pub node_id: NodeId,
  pub span: Span,

  pub local: &'ast Ident,
}
/// e.g. local = foo, imported = None `import { foo } from 'mod.js'`
/// e.g. local = bar, imported = Some(foo) for `import { foo as bar } from
/// 'mod.js'`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportNamedSpecifier<'ast> {
  pub node_id: NodeId,
  pub span: Span,

  pub local: &'ast Ident,
  pub imported: Option<&'ast Ident>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum ExportSpecifier<'ast> {
  Namespace(&'ast ExportNamespaceSpecifier<'ast>),
  Default(&'ast ExportDefaultSpecifier<'ast>),
  Named(&'ast ExportNamedSpecifier<'ast>),
}

/// `export * as foo from 'src';`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportNamespaceSpecifier<'ast> {
  pub node_id: NodeId,
  pub span: Span,

  pub name: &'ast Ident,
}

// export v from 'mod';
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDefaultSpecifier<'ast> {
  pub node_id: NodeId,
  #[span]
  pub exported: &'ast Ident,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportNamedSpecifier<'ast> {
  pub node_id: NodeId,
  pub span: Span,
  /// `foo` in `export { foo as bar }`
  pub orig: &'ast Ident,
  /// `Some(bar)` in `export { foo as bar }`
  pub exported: Option<&'ast Ident>,
}