use crate::{
    class::Class,
    expr::Expr,
    function::Function,
    ident::Ident,
    pat::Pat,
    typescript::{TsEnumDecl, TsInterfaceDecl, TsModuleDecl, TsTypeAliasDecl},
    NodeId,
};
use global_common::{ast_node, EqIgnoreSpan, Span};
use string_enum::StringEnum;

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum Decl {
    #[tag("ClassDeclaration")]
    Class(ClassDecl),
    #[tag("FunctionDeclaration")]
    Fn(FnDecl),
    #[tag("VariableDeclaration")]
    Var(VarDecl),
    #[tag("TsInterfaceDeclaration")]
    TsInterface(TsInterfaceDecl),
    #[tag("TsTypeAliasDeclaration")]
    TsTypeAlias(TsTypeAliasDecl),
    #[tag("TsEnumDeclaration")]
    TsEnum(TsEnumDecl),
    #[tag("TsModuleDeclaration")]
    TsModule(TsModuleDecl),
}

#[ast_node("FunctionDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct FnDecl {
    pub node_id: NodeId,

    #[serde(rename = "identifier")]
    pub ident: Ident,

    #[serde(default)]
    pub declare: bool,

    #[serde(flatten)]
    #[span]
    pub function: Function,
}

#[ast_node("ClassDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ClassDecl {
    pub node_id: NodeId,

    #[serde(rename = "identifier")]
    pub ident: Ident,

    #[serde(default)]
    pub declare: bool,

    #[serde(flatten)]
    #[span]
    pub class: Class,
}

#[ast_node("VariableDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct VarDecl {
    pub node_id: NodeId,

    pub span: Span,

    pub kind: VarDeclKind,

    #[serde(default)]
    pub declare: bool,

    #[serde(rename = "declarations")]
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

#[ast_node("VariableDeclarator")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct VarDeclarator {
    pub node_id: NodeId,

    pub span: Span,
    #[serde(rename = "id")]
    pub name: Pat,

    /// Initialization expression.
    #[serde(default)]
    pub init: Option<Box<Expr>>,

    /// Typescript only
    #[serde(default)]
    pub definite: bool,
}
