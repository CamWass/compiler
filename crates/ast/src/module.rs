use crate::{module_decl::ModuleDecl, stmt::Stmt, NodeId};
use global_common::{ast_node, EqIgnoreSpan, Span};
use swc_atoms::JsWord;

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum Program {
    Module(Module),
    Script(Script),
}

#[ast_node("Module")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Module {
    pub node_id: NodeId,

    pub span: Span,

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

#[ast_node("Script")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Script {
    pub node_id: NodeId,

    pub span: Span,

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
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ModuleItem {
    ModuleDecl(ModuleDecl),
    Stmt(Stmt),
}
