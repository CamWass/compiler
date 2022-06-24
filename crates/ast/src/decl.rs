use crate::{
    class::Class,
    expr::Expr,
    function::Function,
    ident::Ident,
    pat::Pat,
    typescript::{TsEnumDecl, TsInterfaceDecl, TsModuleDecl, TsTypeAliasDecl},
    GetNodeId, NodeId,
};
use ast_node::ast_node;
use global_common::{EqIgnoreSpan, Span};
use string_enum::StringEnum;

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum Decl {
    Class(ClassDecl),
    Fn(FnDecl),
    Var(VarDecl),
    TsInterface(TsInterfaceDecl),
    TsTypeAlias(TsTypeAliasDecl),
    TsEnum(TsEnumDecl),
    TsModule(TsModuleDecl),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct FnDecl {
    pub node_id: NodeId,

    pub ident: Ident,

    pub declare: bool,

    #[span]
    pub function: Function,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ClassDecl {
    pub node_id: NodeId,

    pub ident: Ident,

    pub declare: bool,

    #[span]
    pub class: Class,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct VarDecl {
    pub node_id: NodeId,

    pub span: Span,

    pub kind: VarDeclKind,

    pub declare: bool,

    pub decls: Vec<VarDeclarator>,
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
pub struct VarDeclarator {
    pub node_id: NodeId,

    pub span: Span,

    pub name: Pat,

    /// Initialization expression.
    pub init: Option<Box<Expr>>,

    /// Typescript only
    pub definite: bool,
}
