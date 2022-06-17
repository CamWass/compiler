use crate::{module_decl::ModuleDecl, stmt::Stmt, NodeId};
use global_common::{ast_node, EqIgnoreSpan, Span};
use swc_atoms::JsWord;

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum Program {
    #[tag("Module")]
    Module(Module),
    #[tag("Script")]
    Script(Script),
}

#[ast_node("Module")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Module {
    pub node_id: NodeId,

    pub span: Span,

    pub body: Vec<ModuleItem>,

    #[serde(default, rename = "interpreter")]
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

    #[serde(default, rename = "interpreter")]
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
    #[tag("ImportDeclaration")]
    #[tag("ExportDeclaration")]
    #[tag("ExportNamedDeclaration")]
    #[tag("ExportDefaultDeclaration")]
    #[tag("ExportDefaultExpression")]
    #[tag("ExportAllDeclaration")]
    #[tag("TsImportEqualsDeclaration")]
    #[tag("TsExportAssignment")]
    #[tag("TsNamespaceExportDeclaration")]
    ModuleDecl(ModuleDecl),
    #[tag("*")]
    Stmt(Stmt),
}
