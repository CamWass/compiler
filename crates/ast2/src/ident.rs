use crate::{node::NodeId, typescript::TsTypeAnn};
use ast_node2::ast_node;
use global_common::EqIgnoreSpan;
use global_common::Span;
use global_common::Spanned;
use swc_atoms::JsWord;

/// Identifier used as a pattern.
#[derive(Spanned, Clone, Debug, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub struct BindingIdent<'ast> {
    pub node_id: NodeId,
    #[span]
    pub id: &'ast Ident,

    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,
}

// impl From<Ident> for BindingIdent {
//     fn from(id: Ident) -> Self {
//         Self { id, type_ann: None }
//     }
// }

/// Ident with span.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Ident {
    pub node_id: NodeId,
    pub span: Span,

    pub sym: JsWord,

    /// TypeScript only. Used in case of an optional parameter.
    pub optional: bool,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct PrivateName<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub id: &'ast Ident,
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.sym
    }
}

pub trait IdentExt: AsRef<str> {
    fn is_reserved_for_es3(&self) -> bool {
        [
            "abstract",
            "boolean",
            "break",
            "byte",
            "case",
            "catch",
            "char",
            "class",
            "const",
            "continue",
            "debugger",
            "default",
            "delete",
            "do",
            "double",
            "else",
            "enum",
            "export",
            "extends",
            "false",
            "final",
            "finally",
            "float",
            "for",
            "function",
            "goto",
            "if",
            "implements",
            "import",
            "in",
            "instanceof",
            "int",
            "interface",
            "long",
            "native",
            "new",
            "null",
            "package",
            "private",
            "protected",
            "public",
            "return",
            "short",
            "static",
            "super",
            "switch",
            "synchronized",
            "this",
            "throw",
            "throws",
            "transient",
            "true",
            "try",
            "typeof",
            "var",
            "void",
            "volatile",
            "while",
            "with",
        ]
        .contains(&self.as_ref())
    }

    fn is_reserved_only_for_es3(&self) -> bool {
        [
            "abstract",
            "boolean",
            "byte",
            "char",
            "double",
            "enum",
            "final",
            "float",
            "goto",
            "implements",
            "int",
            "interface",
            "long",
            "native",
            "package",
            "private",
            "protected",
            "public",
            "short",
            "static",
            "synchronized",
            "throws",
            "transient",
            "volatile",
        ]
        .contains(&self.as_ref())
    }
}

impl IdentExt for JsWord {}
impl IdentExt for Ident {}