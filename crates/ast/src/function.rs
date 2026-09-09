use crate::{
    BindingRestElement, GetNodeId, NodeId, ProgramData, pat::BindingElement, stmt::BlockStmt,
};
use bitflags::bitflags;
use clone_node::CloneNode;
use node_eq::NodeEq;
use node_id::GetNodeIdMacro;
use serde::Serialize;

/// Common parts of function and method.
#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct Function {
    pub node_id: NodeId,

    pub params: FunctionParams,

    pub body: BlockStmt,

    pub flags: FnFlags,
}

impl Function {
    pub fn is_async(&self) -> bool {
        self.flags.contains(FnFlags::ASYNC)
    }

    pub fn is_generator(&self) -> bool {
        self.flags.contains(FnFlags::GENERATOR)
    }
}

bitflags! {
    #[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize)]
    pub struct FnFlags: u8 {
        const GENERATOR = 1 << 0;
        const ASYNC = 1 << 1;
    }
}

impl crate::CloneNode for FnFlags {
    fn clone_node(&self, _: &mut ProgramData) -> Self {
        *self
    }
}

impl crate::NodeEq for FnFlags {
    fn eq_ignoring_node_id(&self, other: &Self) -> bool {
        self == other
    }
}

#[derive(Debug, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct Param {
    pub node_id: NodeId,
    pub pat: BindingElement,
}

impl Param {
    pub fn from_pat(pat: BindingElement, program_data: &mut ProgramData) -> Self {
        Self {
            node_id: program_data.new_id_from(pat.node_id()),
            pat,
        }
    }
}

#[derive(Debug, CloneNode, NodeEq, Serialize)]
pub struct FunctionParams {
    pub params: Vec<Param>,
    pub rest_param: Option<BindingRestElement>,
}

impl FunctionParams {
    /// Returns true if there are any standard params or a rest param.
    pub fn is_empty(&self) -> bool {
        self.params.is_empty() && self.rest_param.is_none()
    }
}
