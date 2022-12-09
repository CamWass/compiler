// //! This module defines Concrete Syntax Tree (CST), used by rust-analyzer.
// //!
// //! The CST includes comments and whitespace, provides a single node type,
// //! `SyntaxNode`, and a basic traversal API (parent, children, siblings).
// //!
// //! The *real* implementation is in the (language-agnostic) `rowan` crate, this
// //! module just wraps its API.

// use rowan::{GreenNodeBuilder, Language};

// use crate::{Parse, SyntaxKind, TextSize};

// pub(crate) use rowan::{GreenNode, GreenToken, NodeOrToken};

// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub enum RustLanguage {}
// impl Language for RustLanguage {
//     type Kind = SyntaxKind;

//     fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
//         SyntaxKind::from(raw.0)
//     }

//     fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
//         rowan::SyntaxKind(kind.into())
//     }
// }

// pub type SyntaxNode = rowan::SyntaxNode<RustLanguage>;
// pub type SyntaxToken = rowan::SyntaxToken<RustLanguage>;
// pub type SyntaxElement = rowan::SyntaxElement<RustLanguage>;
// pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<RustLanguage>;
// pub type SyntaxElementChildren = rowan::SyntaxElementChildren<RustLanguage>;
// pub type PreorderWithTokens = rowan::api::PreorderWithTokens<RustLanguage>;

// #[derive(Default)]
// pub struct SyntaxTreeBuilder {
//     inner: GreenNodeBuilder<'static>,
// }

// impl SyntaxTreeBuilder {
//     pub fn finish(self) -> GreenNode {
//         self.inner.finish()
//     }

//     pub fn token(&mut self, kind: SyntaxKind, text: &str) {
//         let kind = RustLanguage::kind_to_raw(kind);
//         self.inner.token(kind, text);
//     }

//     pub fn start_node(&mut self, kind: SyntaxKind) {
//         let kind = RustLanguage::kind_to_raw(kind);
//         self.inner.start_node(kind);
//     }

//     pub fn finish_node(&mut self) {
//         self.inner.finish_node();
//     }
// }
