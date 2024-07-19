use crate::{
    class::Class, expr::Expr, function::Function, ident::Ident, pat::Pat, GetNodeId, NodeId,
};
use clone_node::CloneNode;
use string_enum::StringEnum;
use node_id::GetNodeIdMacro;

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum Decl {
    Class(ClassDecl),
    Fn(FnDecl),
    Var(VarDecl),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct FnDecl {
    pub node_id: NodeId,

    pub ident: Ident,
    pub function: Function,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ClassDecl {
    pub node_id: NodeId,

    pub ident: Ident,
    pub class: Class,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct VarDecl {
    pub node_id: NodeId,

    pub kind: VarDeclKind,

    pub decls: Vec<VarDeclarator>,
}

#[derive(StringEnum, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, CloneNode)]
pub enum VarDeclKind {
    /// `var`
    Var,
    /// `let`
    Let,
    /// `const`
    Const,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct VarDeclarator {
    pub node_id: NodeId,

    pub name: Pat,

    /// Initialization expression.
    pub init: Option<Box<Expr>>,
}
