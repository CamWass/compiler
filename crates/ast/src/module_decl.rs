use super::{
    decl::Decl,
    expr::{ClassExpr, Expr, FnExpr},
    ident::Ident,
    lit::Str,
    typescript::{TsExportAssignment, TsImportEqualsDecl, TsInterfaceDecl, TsNamespaceExportDecl},
    ObjectLit,
};
use global_common::{ast_node, EqIgnoreSpan, Span};

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ModuleDecl {
    Import(ImportDecl),

    ExportDecl(ExportDecl),

    ExportNamed(NamedExport),

    ExportDefaultDecl(ExportDefaultDecl),

    ExportDefaultExpr(ExportDefaultExpr),

    ExportAll(ExportAll),

    TsImportEquals(TsImportEqualsDecl),

    TsExportAssignment(TsExportAssignment),

    TsNamespaceExport(TsNamespaceExportDecl),
}

#[ast_node("ExportDefaultExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDefaultExpr {
    pub span: Span,

    pub expr: Box<Expr>,
}

#[ast_node("ExportDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDecl {
    pub span: Span,

    pub decl: Decl,
}

#[ast_node("ImportDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportDecl {
    pub span: Span,

    pub specifiers: Vec<ImportSpecifier>,

    pub src: Str,

    pub type_only: bool,

    pub asserts: Option<ObjectLit>,
}

/// `export * from 'mod'`
#[ast_node("ExportAllDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportAll {
    pub span: Span,

    pub src: Str,

    pub asserts: Option<ObjectLit>,
}

/// `export { foo } from 'mod'`
/// `export { foo as bar } from 'mod'`
#[ast_node("ExportNamedDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct NamedExport {
    pub span: Span,

    pub specifiers: Vec<ExportSpecifier>,

    pub src: Option<Str>,

    pub type_only: bool,

    pub asserts: Option<ObjectLit>,
}

#[ast_node("ExportDefaultDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDefaultDecl {
    pub span: Span,

    pub decl: DefaultDecl,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum DefaultDecl {
    Class(ClassExpr),

    Fn(FnExpr),

    TsInterfaceDecl(TsInterfaceDecl),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ImportSpecifier {
    Named(ImportNamedSpecifier),

    Default(ImportDefaultSpecifier),

    Namespace(ImportStarAsSpecifier),
}

/// e.g. `import foo from 'mod.js'`
#[ast_node("ImportDefaultSpecifier")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportDefaultSpecifier {
    pub span: Span,

    pub local: Ident,
}
/// e.g. `import * as foo from 'mod.js'`.
#[ast_node("ImportNamespaceSpecifier")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportStarAsSpecifier {
    pub span: Span,

    pub local: Ident,
}
/// e.g. local = foo, imported = None `import { foo } from 'mod.js'`
/// e.g. local = bar, imported = Some(foo) for `import { foo as bar } from
/// 'mod.js'`
#[ast_node("ImportSpecifier")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportNamedSpecifier {
    pub span: Span,

    pub local: Ident,

    pub imported: Option<Ident>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ExportSpecifier {
    Namespace(ExportNamespaceSpecifier),

    Default(ExportDefaultSpecifier),

    Named(ExportNamedSpecifier),
}

/// `export * as foo from 'src';`
#[ast_node("ExportNamespaceSpecifier")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportNamespaceSpecifier {
    pub span: Span,

    pub name: Ident,
}

// export v from 'mod';
#[ast_node("ExportDefaultSpecifier")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDefaultSpecifier {
    #[span]
    pub exported: Ident,
}

#[ast_node("ExportSpecifier")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportNamedSpecifier {
    pub span: Span,
    /// `foo` in `export { foo as bar }`
    pub orig: Ident,
    /// `Some(bar)` in `export { foo as bar }`
    pub exported: Option<Ident>,
}
