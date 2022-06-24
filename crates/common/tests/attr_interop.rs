//! Test that `#[span]` and `#[fold]` can be used at same time.
use global_common::{self, ast_node, Span, Spanned};

#[ast_node("Class")]
// See https://github.com/rust-lang/rust/issues/44925
pub struct Class {
    #[span]
    pub has_span: HasSpan,
    pub s: String,
}

#[ast_node("Tuple")]
pub struct Tuple(#[span] HasSpan, usize, usize);

#[derive(Debug, Clone, PartialEq, Eq, Spanned)]
pub struct HasSpan {
    pub span: Span,
}

#[ast_node]
pub enum Node {
    Class(Class),
    Tuple(Tuple),
}
