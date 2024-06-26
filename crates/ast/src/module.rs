use crate::{module_decl::ModuleDecl, stmt::Stmt, GetNodeId, NodeId};
use ast_node::ast_node;
use atoms::JsWord;

#[ast_node]
#[derive(Eq, Hash)]
pub enum Program {
    Module(Module),
    Script(Script),
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct Module {
    pub node_id: NodeId,

    pub body: Vec<ModuleItem>,

    pub shebang: Option<JsWord>,
}

#[cfg(feature = "arbitrary")]
impl arbitrary::Arbitrary for Module {
    fn arbitrary(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        let span = u.arbitrary()?;
        let body = u.arbitrary()?;
        Ok(Self {
            span,
            body,
            shebang: None,
        })
    }
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct Script {
    pub node_id: NodeId,

    pub body: Vec<Stmt>,

    pub shebang: Option<JsWord>,
}

#[cfg(feature = "arbitrary")]
impl arbitrary::Arbitrary for Script {
    fn arbitrary(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        let span = u.arbitrary()?;
        let body = u.arbitrary()?;
        Ok(Self {
            span,
            body,
            shebang: None,
        })
    }
}

#[ast_node]
#[derive(Eq, Hash)]
pub enum ModuleItem {
    ModuleDecl(ModuleDecl),
    Stmt(Stmt),
}

impl From<Stmt> for ModuleItem {
    fn from(s: Stmt) -> Self {
        Self::Stmt(s)
    }
}
