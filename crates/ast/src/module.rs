use super::{module_decl::ModuleDecl, stmt::Stmt};
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
    pub span: Span,
    pub body: Vec<ModuleItem>,

    pub shebang: Option<JsWord>,
}

#[ast_node("Script")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Script {
    pub span: Span,
    pub body: Vec<Stmt>,

    pub shebang: Option<JsWord>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ModuleItem {
    ModuleDecl(ModuleDecl),

    Stmt(Stmt),
}
