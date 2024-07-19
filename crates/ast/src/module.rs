use crate::{module_decl::ModuleDecl, stmt::Stmt, GetNodeId, NodeId};
use atoms::JsWord;
use node_id::GetNodeIdMacro;
use clone_node::CloneNode;

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum Program {
    Module(Module),
    Script(Script),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Module {
    pub node_id: NodeId,

    pub body: Vec<ModuleItem>,

    pub shebang: Option<JsWord>,
}



#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Script {
    pub node_id: NodeId,

    pub body: Vec<Stmt>,

    pub shebang: Option<JsWord>,
}



#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum ModuleItem {
    ModuleDecl(ModuleDecl),
    Stmt(Stmt),
}

impl From<Stmt> for ModuleItem {
    fn from(s: Stmt) -> Self {
        Self::Stmt(s)
    }
}
