use crate::{module_decl::ModuleDecl, node::NodeId, stmt::Stmt};
use ast_node2::ast_node;
use global_common::{EqIgnoreSpan, Span};
use swc_atoms::JsWord;

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum Program<'ast> {
    Module(&'ast Module<'ast>),
    Script(&'ast Script<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Module<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub body: Vec<ModuleItem<'ast>>,

    pub shebang: Option<JsWord>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Script<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub body: Vec<Stmt<'ast>>,

    pub shebang: Option<JsWord>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum ModuleItem<'ast> {
    ModuleDecl(ModuleDecl<'ast>),

    Stmt(Stmt<'ast>),
}