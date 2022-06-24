use crate::{
    decl::Decl,
    expr::{ClassExpr, Expr, FnExpr},
    ident::Ident,
    lit::Str,
    typescript::{TsExportAssignment, TsImportEqualsDecl, TsInterfaceDecl, TsNamespaceExportDecl},
    GetNodeId, NodeId, ObjectLit,
};
use ast_node::ast_node;
use global_common::{EqIgnoreSpan, Span};

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

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDefaultExpr {
    pub node_id: NodeId,

    pub span: Span,

    pub expr: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDecl {
    pub node_id: NodeId,

    pub span: Span,

    pub decl: Decl,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportDecl {
    pub node_id: NodeId,

    pub span: Span,

    pub specifiers: Vec<ImportSpecifier>,

    pub src: Str,

    pub type_only: bool,

    pub asserts: Option<ObjectLit>,
}

/// `export * from 'mod'`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportAll {
    pub node_id: NodeId,

    pub span: Span,

    pub src: Str,

    pub asserts: Option<ObjectLit>,
}

/// `export { foo } from 'mod'`
/// `export { foo as bar } from 'mod'`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct NamedExport {
    pub node_id: NodeId,

    pub span: Span,

    pub specifiers: Vec<ExportSpecifier>,

    pub src: Option<Str>,

    pub type_only: bool,

    pub asserts: Option<ObjectLit>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDefaultDecl {
    pub node_id: NodeId,

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
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportDefaultSpecifier {
    pub node_id: NodeId,

    pub span: Span,

    pub local: Ident,
}
/// e.g. `import * as foo from 'mod.js'`.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportStarAsSpecifier {
    pub node_id: NodeId,

    pub span: Span,

    pub local: Ident,
}
/// e.g. local = foo, imported = None `import { foo } from 'mod.js'`
/// e.g. local = bar, imported = Some(foo) for `import { foo as bar } from
/// 'mod.js'`
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ImportNamedSpecifier {
    pub node_id: NodeId,

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
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportNamespaceSpecifier {
    pub node_id: NodeId,

    pub span: Span,

    pub name: Ident,
}

// export v from 'mod';
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportDefaultSpecifier {
    pub node_id: NodeId,

    #[span]
    pub exported: Ident,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExportNamedSpecifier {
    pub node_id: NodeId,

    pub span: Span,
    /// `foo` in `export { foo as bar }`
    pub orig: Ident,
    /// `Some(bar)` in `export { foo as bar }`
    pub exported: Option<Ident>,
}
