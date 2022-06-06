//! # Cargo features
//!
//! ## `tty-emitter`
//!
//! Adds default implementation of Emitter.
//! Enabling this feature will add tty-related dependencies.
//!
//! ## `sourcemap`
//!
//! Adds methods to generate web sourcemap.
#![deny(unused)]

pub use self::{
    eq::{EqIgnoreSpan, TypeEq},
    errors::{SourceMapper, SourceMapperDyn},
    pos::{
        hygiene, BytePos, CharPos, FileName, Globals, Loc, LocWithOpt, Mark, MultiSpan, SourceFile,
        SourceFileAndBytePos, SourceFileAndLine, Span, SpanLinesError, Spanned, SyntaxContext,
        DUMMY_SP, GLOBALS, NO_EXPANSION,
    },
    source_map::{FileLines, FileLoader, FilePathMapping, SourceMap, SpanSnippetError},
    syntax_pos::{LineCol, Pos},
};
pub use ast_node::{ast_node, ast_serde, DeserializeEnum, Spanned};
pub use eq_ignore_macros::{EqIgnoreSpan, TypeEq};
pub use from_variant::FromVariant;
pub use global_visit::chain;
use serde::Serialize;
use std::fmt::Debug;
#[doc(hidden)]
pub mod private;

/// A trait for ast nodes.
pub trait AstNode: Debug + PartialEq + Clone + Spanned + Serialize {
    const TYPE: &'static str;
}

pub mod chars;
pub mod comments;
mod eq;
pub mod errors;
pub mod iter;
pub mod macros;
pub mod pass;
mod pos;
mod rustc_data_structures;
pub mod serializer;
mod source_map;
pub mod sync;
mod syntax_pos;
