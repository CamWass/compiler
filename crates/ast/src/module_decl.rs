use crate::{
    decl::Decl,
    expr::{ClassExpr, Expr, FnExpr},
    ident::Ident,
    lit::Str,
    typescript::{TsExportAssignment, TsImportEqualsDecl, TsInterfaceDecl, TsNamespaceExportDecl},
    NodeId, ObjectLit,
};
use global_common::{ast_node, EqIgnoreSpan, Span};

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ModuleDecl {
    #[tag("ImportDeclaration")]
    Import(ImportDecl),

    #[tag("ExportDeclaration")]
    ExportDecl(ExportDecl),

    #[tag("ExportNamedDeclaration")]
    ExportNamed(NamedExport),

    #[tag("ExportDefaultDeclaration")]
    ExportDefaultDecl(ExportDefaultDecl),

    #[tag("ExportDefaultExpression")]
    ExportDefaultExpr(ExportDefaultExpr),

    #[tag("ExportAllDeclaration")]
    ExportAll(ExportAll),

    #[tag("TsImportEqualsDeclaration")]
    TsImportEquals(TsImportEqualsDecl),

    #[tag("TsExportAssignment")]
    TsExportAssignment(TsExportAssignment),

    #[tag("TsNamespaceExportDeclaration")]
    TsNamespaceExport(TsNamespaceExportDecl),
}

#[ast_node("ExportDefaultExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDefaultExpr {
    pub node_id: NodeId,

    pub span: Span,

    #[serde(rename = "expression")]
    pub expr: Box<Expr>,
}

#[ast_node("ExportDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDecl {
    pub node_id: NodeId,

    pub span: Span,

    #[serde(rename = "declaration")]
    pub decl: Decl,
}

#[ast_node("ImportDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportDecl {
    pub node_id: NodeId,

    pub span: Span,

    #[serde(default)]
    pub specifiers: Vec<ImportSpecifier>,

    #[serde(rename = "source")]
    pub src: Str,

    #[serde(rename = "typeOnly")]
    pub type_only: bool,

    #[serde(default)]
    pub asserts: Option<ObjectLit>,
}

/// `export * from 'mod'`
#[ast_node("ExportAllDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportAll {
    pub node_id: NodeId,

    pub span: Span,

    #[serde(rename = "source")]
    pub src: Str,

    #[serde(default)]
    pub asserts: Option<ObjectLit>,
}

/// `export { foo } from 'mod'`
/// `export { foo as bar } from 'mod'`
#[ast_node("ExportNamedDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct NamedExport {
    pub node_id: NodeId,

    pub span: Span,

    pub specifiers: Vec<ExportSpecifier>,

    #[serde(rename = "source")]
    pub src: Option<Str>,

    #[serde(rename = "typeOnly")]
    pub type_only: bool,

    #[serde(default)]
    pub asserts: Option<ObjectLit>,
}

#[ast_node("ExportDefaultDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDefaultDecl {
    pub node_id: NodeId,

    pub span: Span,

    pub decl: DefaultDecl,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum DefaultDecl {
    #[tag("ClassExpression")]
    Class(ClassExpr),

    #[tag("FunctionExpression")]
    Fn(FnExpr),

    #[tag("TsInterfaceDeclaration")]
    TsInterfaceDecl(TsInterfaceDecl),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ImportSpecifier {
    #[tag("ImportSpecifier")]
    Named(ImportNamedSpecifier),
    #[tag("ImportDefaultSpecifier")]
    Default(ImportDefaultSpecifier),
    #[tag("ImportNamespaceSpecifier")]
    Namespace(ImportStarAsSpecifier),
}

/// e.g. `import foo from 'mod.js'`
#[ast_node("ImportDefaultSpecifier")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportDefaultSpecifier {
    pub node_id: NodeId,

    pub span: Span,

    pub local: Ident,
}
/// e.g. `import * as foo from 'mod.js'`.
#[ast_node("ImportNamespaceSpecifier")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportStarAsSpecifier {
    pub node_id: NodeId,

    pub span: Span,

    pub local: Ident,
}
/// e.g. local = foo, imported = None `import { foo } from 'mod.js'`
/// e.g. local = bar, imported = Some(foo) for `import { foo as bar } from
/// 'mod.js'`
#[ast_node("ImportSpecifier")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportNamedSpecifier {
    pub node_id: NodeId,

    pub span: Span,

    pub local: Ident,

    #[serde(default)]
    pub imported: Option<Ident>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ExportSpecifier {
    #[tag("ExportNamespaceSpecifier")]
    Namespace(ExportNamespaceSpecifier),

    #[tag("ExportDefaultSpecifier")]
    Default(ExportDefaultSpecifier),

    #[tag("ExportSpecifier")]
    Named(ExportNamedSpecifier),
}

/// `export * as foo from 'src';`
#[ast_node("ExportNamespaceSpecifier")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportNamespaceSpecifier {
    pub node_id: NodeId,

    pub span: Span,

    pub name: Ident,
}

// export v from 'mod';
#[ast_node("ExportDefaultSpecifier")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDefaultSpecifier {
    pub node_id: NodeId,

    #[span]
    pub exported: Ident,
}

#[ast_node("ExportSpecifier")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportNamedSpecifier {
    pub node_id: NodeId,

    pub span: Span,
    /// `foo` in `export { foo as bar }`
    pub orig: Ident,
    /// `Some(bar)` in `export { foo as bar }`
    #[serde(default)]
    pub exported: Option<Ident>,
}
