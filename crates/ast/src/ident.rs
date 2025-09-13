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

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct PrivateName {
    pub node_id: NodeId,
    pub id: Ident,
}
