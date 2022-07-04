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
    eq::EqIgnoreSpan,
    errors::{SourceMapper, SourceMapperDyn},
    pos::{
        hygiene, BytePos, CharPos, FileName, Globals, Loc, LocWithOpt, Mark, MultiSpan, SourceFile,
        SourceFileAndBytePos, SourceFileAndLine, Span, SpanLinesError, Spanned, SyntaxContext,
        DUMMY_SP, GLOBALS, NO_EXPANSION,
    },
    source_map::{FileLines, FileLoader, FilePathMapping, SourceMap, SpanSnippetError},
    syntax_pos::{LineCol, Pos},
};
pub use eq_ignore_macros::EqIgnoreSpan;
pub use from_variant::FromVariant;
pub use global_visit::chain;

pub mod chars;
pub mod comments;
mod eq;
pub mod errors;
pub mod integer_decode;
pub mod iter;
pub mod macros;
pub mod pass;
mod pos;
mod rustc_data_structures;
mod source_map;
pub mod sync;
mod syntax_pos;
pub mod util;
