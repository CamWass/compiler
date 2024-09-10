use crate::{class::Decorator, pat::Pat, stmt::BlockStmt, GetNodeId, NodeId, ProgramData};
use clone_node::CloneNode;
use node_id::GetNodeIdMacro;

/// Common parts of function and method.
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Function {
    pub node_id: NodeId,

    pub params: Vec<Param>,

    pub decorators: Vec<Decorator>,

    pub body: BlockStmt,

    /// if it's a generator.
    pub is_generator: bool,

    /// if it's an async function.
    pub is_async: bool,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Param {
    pub node_id: NodeId,
    pub decorators: Vec<Decorator>,
    pub pat: Pat,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ParamWithoutDecorators {
    pub node_id: NodeId,
    pub pat: Pat,
}

impl ParamWithoutDecorators {
    pub fn from_pat(pat: Pat, program_data: &mut ProgramData) -> Self {
        Self {
            node_id: program_data.new_id_from(pat.node_id()),
            pat,
        }
    }
}
