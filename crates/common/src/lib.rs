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

pub use self::syntax_pos::{
    hygiene, BytePos, CharPos, FileName, Globals, Loc, LocWithOpt, Mark, MultiSpan, SourceFile,
    SourceFileAndBytePos, SourceFileAndLine, Span, SpanLinesError, SyntaxContext, DUMMY_SP,
    GLOBALS,
};
pub use self::{
    errors::{SourceMapper, SourceMapperDyn},
    source_map::{FileLines, FileLoader, FilePathMapping, SourceMap, SpanSnippetError},
    syntax_pos::{LineCol, Pos},
};
pub use global_visit::chain;

pub mod chars;
pub mod errors;
pub mod integer_decode;
pub mod iter;
pub mod pass;
mod rustc_data_structures;
mod source_map;
pub mod sync;
mod syntax_pos;
pub mod util;
