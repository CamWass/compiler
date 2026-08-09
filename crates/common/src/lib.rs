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
#![warn(unused)]

pub use self::syntax_pos::{
    BytePos, CharPos, DUMMY_SP, FileName, GLOBALS, Globals, Loc, LocWithOpt, Mark, MultiSpan,
    SourceFile, SourceFileAndBytePos, SourceFileAndLine, Span, SpanLinesError, SyntaxContext,
    hygiene,
};
pub use self::{
    errors::{SourceMapper, SourceMapperDyn},
    source_map::{FileLines, FileLoader, FilePathMapping, SourceMap, SpanSnippetError},
    syntax_pos::{LineCol, Pos},
};

pub mod chars;
pub mod errors;
mod rustc_data_structures;
mod source_map;
pub mod sync;
mod syntax_pos;
pub mod util;
