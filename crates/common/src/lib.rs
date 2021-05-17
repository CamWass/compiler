pub use self::{
    eq::{EqIgnoreSpan, TypeEq},
    pos::{BytePos, CharPos, Pos, Span, Spanned, DUMMY_SP},
};
pub use ast_node::{ast_node, Spanned};
pub use eq_ignore_macros::{EqIgnoreSpan, TypeEq};
use std::fmt::Debug;

mod eq;
pub mod input;
mod pos;

/// A trait for ast nodes.
pub trait AstNode: Debug + PartialEq + Clone + Spanned {
    const TYPE: &'static str;
}
