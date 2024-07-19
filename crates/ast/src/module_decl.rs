use crate::{
    decl::Decl,
    expr::{ClassExpr, Expr, FnExpr},
    ident::Ident,
    lit::Str,
    GetNodeId, NodeId, ObjectLit,
};
use node_id::GetNodeIdMacro;
use clone_node::CloneNode;

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum ModuleDecl {
    Import(ImportDecl),

    ExportDecl(ExportDecl),

    ExportNamed(NamedExport),

    ExportDefaultDecl(ExportDefaultDecl),

    ExportDefaultExpr(ExportDefaultExpr),

    ExportAll(ExportAll),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ExportDefaultExpr {
    pub node_id: NodeId,

    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ExportDecl {
    pub node_id: NodeId,

    pub decl: Decl,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ImportDecl {
    pub node_id: NodeId,

    pub specifiers: Vec<ImportSpecifier>,

    pub src: Str,

    pub asserts: Option<ObjectLit>,
}

/// `export * from 'mod'`
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ExportAll {
    pub node_id: NodeId,

    pub src: Str,

    pub asserts: Option<ObjectLit>,
}

/// `export { foo } from 'mod'`
/// `export { foo as bar } from 'mod'`
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct NamedExport {
    pub node_id: NodeId,

    pub specifiers: Vec<ExportSpecifier>,

    pub src: Option<Str>,

    pub asserts: Option<ObjectLit>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ExportDefaultDecl {
    pub node_id: NodeId,

    pub decl: DefaultDecl,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum DefaultDecl {
    Class(ClassExpr),

    Fn(FnExpr),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum ImportSpecifier {
    Named(ImportNamedSpecifier),
    Default(ImportDefaultSpecifier),
    Namespace(ImportStarAsSpecifier),
}

/// e.g. `import foo from 'mod.js'`
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ImportDefaultSpecifier {
    pub node_id: NodeId,

    pub local: Ident,
}
/// e.g. `import * as foo from 'mod.js'`.
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ImportStarAsSpecifier {
    pub node_id: NodeId,

    pub local: Ident,
}
/// e.g. local = foo, imported = None `import { foo } from 'mod.js'`
/// e.g. local = bar, imported = Some(foo) for `import { foo as bar } from
/// 'mod.js'`
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ImportNamedSpecifier {
    pub node_id: NodeId,

    pub local: Ident,

    pub imported: Option<Ident>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum ExportSpecifier {
    Namespace(ExportNamespaceSpecifier),

    Default(ExportDefaultSpecifier),

    Named(ExportNamedSpecifier),
}

/// `export * as foo from 'src';`
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ExportNamespaceSpecifier {
    pub node_id: NodeId,

    pub name: Ident,
}

// export v from 'mod';
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ExportDefaultSpecifier {
    pub node_id: NodeId,
    pub exported: Ident,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ExportNamedSpecifier {
    pub node_id: NodeId,
    /// `foo` in `export { foo as bar }`
    pub orig: Ident,
    /// `Some(bar)` in `export { foo as bar }`
    pub exported: Option<Ident>,
}
