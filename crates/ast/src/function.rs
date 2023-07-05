use crate::{class::Decorator, pat::Pat, stmt::BlockStmt, GetNodeId, NodeId};
use ast_node::ast_node;
use global_common::{EqIgnoreSpan, Span};

/// Common parts of function and method.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Function {
    pub node_id: NodeId,

    pub params: Vec<Param>,

    pub decorators: Vec<Decorator>,

    pub span: Span,

    pub body: BlockStmt,

    /// if it's a generator.
    pub is_generator: bool,

    /// if it's an async function.
    pub is_async: bool,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Param {
    pub node_id: NodeId,

    pub span: Span,
    pub decorators: Vec<Decorator>,
    pub pat: Pat,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ParamWithoutDecorators {
    pub node_id: NodeId,

    #[span]
    pub pat: Pat,
}

impl ParamWithoutDecorators {
    pub fn from_pat(pat: Pat, node_id: NodeId) -> Self {
        Self { node_id, pat }
    }
}
