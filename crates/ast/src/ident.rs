use crate::{GetNodeId, NameId, NodeId};
use clone_node::CloneNode;
use common::util::take::Take;
use node_eq::NodeEq;
use node_id::GetNodeIdMacro;
use serde::Serialize;

/// Identifier used as a pattern.
#[derive(Debug, PartialEq, CloneNode, NodeEq, Serialize, Eq, Hash)]
pub struct BindingIdent {
    pub id: Ident,
}

impl BindingIdent {
    pub fn from_ident(id: Ident) -> Self {
        Self { id }
    }
}

impl GetNodeId for BindingIdent {
    fn node_id(&self) -> NodeId {
        // At the time of writing, NodeIds are used to (1) store spans for each
        // node, and (2) allow the compiler to differentiate nodes and store
        // information about them.
        // As a transparent wrapper, BindingIdent always has the same span as
        // the inner Ident, and we currently don't need to store info about
        // BindingIdents specifically (we store the info using the Ident's
        // NodeId).
        // Therefore, we don't give BindingIdents their own NodeId, and don't
        // have to store spans for them.
        self.id.node_id
    }
}

/// Ident.
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize, Eq, Hash)]
pub struct Ident {
    pub node_id: NodeId,
    pub name: NameId,
}

impl Take for Ident {
    fn dummy() -> Self {
        Ident {
            node_id: NodeId::DUMMY,
            name: NameId::DUMMY,
        }
    }
}

// From https://github.com/swc-project/swc/blob/250c94f48b6148724648e8f8f326015cf0b28802/crates/swc_ecma_ast/src/ident.rs#L237
#[repr(C, align(64))]
struct Align64<T>(pub(crate) T);

const T: bool = true;
const F: bool = false;

impl Ident {
    #[inline]
    pub fn is_valid_ascii_start(c: u8) -> bool {
        debug_assert!(c.is_ascii());
        // This contains `$` (36) and `_` (95)
        const ASCII_START: Align64<[bool; 128]> = Align64([
            F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
            F, F, F, F, F, F, F, T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
            F, F, F, F, F, F, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
            T, T, T, T, F, F, F, F, T, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
            T, T, T, T, T, T, T, F, F, F, F, F,
        ]);
        ASCII_START.0[c as usize]
    }

    pub fn is_valid_non_ascii_start(c: char) -> bool {
        debug_assert!(!c.is_ascii());
        unicode_id_start::is_id_start_unicode(c)
    }

    /// Returns true if `c` is a valid character for an identifier start.
    #[inline]
    pub fn is_valid_start(c: char) -> bool {
        if c.is_ascii() {
            Self::is_valid_ascii_start(c as u8)
        } else {
            Self::is_valid_non_ascii_start(c)
        }
    }

    #[inline]
    pub fn is_valid_non_ascii_continue(c: char) -> bool {
        debug_assert!(!c.is_ascii());
        unicode_id_start::is_id_continue_unicode(c)
    }

    #[inline]
    pub fn is_valid_ascii_continue(c: u8) -> bool {
        debug_assert!(c.is_ascii());
        // This contains `$` (36)
        const ASCII_CONTINUE: Align64<[bool; 128]> = Align64([
            F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
            F, F, F, F, F, F, F, T, F, F, F, F, F, F, F, F, F, F, F, T, T, T, T, T, T, T, T, T, T,
            F, F, F, F, F, F, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
            T, T, T, T, F, F, F, F, T, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T,
            T, T, T, T, T, T, T, F, F, F, F, F,
        ]);
        ASCII_CONTINUE.0[c as usize]
    }

    /// Returns true if `c` is a valid character for an identifier part after
    /// start.
    #[inline]
    pub fn is_valid_continue(c: char) -> bool {
        if c.is_ascii() {
            Self::is_valid_ascii_continue(c as u8)
        } else {
            Self::is_valid_non_ascii_continue(c)
        }
    }
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize, Eq, Hash)]
pub struct PrivateName {
    pub node_id: NodeId,
    pub id: Ident,
}
