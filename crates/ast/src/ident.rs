use crate::{GetNodeId, NodeId, ProgramData};
use atoms::{js_word, JsWord};
use clone_node::CloneNode;
use global_common::{util::take::Take, SyntaxContext};
use node_id::GetNodeIdMacro;

/// Identifier used as a pattern.
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct BindingIdent {
    pub node_id: NodeId,
    pub id: Ident,
}

impl BindingIdent {
    pub fn from_ident(id: Ident, program_data: &mut ProgramData) -> Self {
        Self {
            node_id: program_data.new_id_from(id.node_id),
            id,
        }
    }
}

/// Ident.
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Ident {
    pub node_id: NodeId,
    pub sym: JsWord,
    pub ctxt: SyntaxContext,
}

impl Take for Ident {
    fn dummy() -> Self {
        Ident {
            node_id: NodeId::DUMMY,
            sym: js_word!(""),
            ctxt: SyntaxContext::empty(),
        }
    }
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct PrivateName {
    pub node_id: NodeId,
    pub id: Ident,
}
