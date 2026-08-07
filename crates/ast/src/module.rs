use crate::{GetNodeId, NodeId, module_decl::ModuleDecl, stmt::Stmt};
use clone_node::CloneNode;
use node_eq::NodeEq;
use node_id::GetNodeIdMacro;

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Eq, Hash)]
pub enum Program {
    Module(Module),
    Script(Script),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Eq, Hash)]
pub struct Module {
    pub node_id: NodeId,

    pub body: Vec<ModuleItem>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Eq, Hash)]
pub struct Script {
    pub node_id: NodeId,

    pub body: Vec<Stmt>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Eq, Hash)]
pub enum ModuleItem {
    ModuleDecl(ModuleDecl),
    Stmt(Stmt),
}

impl From<Stmt> for ModuleItem {
    fn from(s: Stmt) -> Self {
        Self::Stmt(s)
    }
}
