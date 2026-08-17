use crate::{
    GetNodeId, NodeId, class::Class, expr::Expr, function::Function, ident::Ident, pat::Pat,
};
use clone_node::CloneNode;
use node_eq::NodeEq;
use node_id::GetNodeIdMacro;
use serde::Serialize;

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub enum Decl {
    Class(ClassDecl),
    Fn(FnDecl),
    Var(VarDecl),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct FnDecl {
    pub node_id: NodeId,

    pub ident: Ident,
    pub function: Box<Function>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct ClassDecl {
    pub node_id: NodeId,

    pub ident: Ident,
    pub class: Box<Class>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct VarDecl {
    pub node_id: NodeId,

    pub kind: VarDeclKind,

    pub decls: Vec<VarDeclarator>,
}

#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord, CloneNode, NodeEq, Serialize)]
pub enum VarDeclKind {
    /// `var`
    Var,
    /// `let`
    Let,
    /// `const`
    Const,
}

impl VarDeclKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            VarDeclKind::Var => "var",
            VarDeclKind::Let => "let",
            VarDeclKind::Const => "const",
        }
    }
}

impl std::fmt::Debug for VarDeclKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = self.as_str();
        std::fmt::Debug::fmt(s, f)
    }
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct VarDeclarator {
    pub node_id: NodeId,

    pub name: Pat,

    /// Initialization expression.
    pub init: Option<Box<Expr>>,
}
