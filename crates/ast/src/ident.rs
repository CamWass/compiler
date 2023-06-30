use crate::{GetNodeId, NodeId};
use ast_node::ast_node;
use atoms::{js_word, JsWord};
use global_common::{util::take::Take, EqIgnoreSpan, Span, DUMMY_SP};

/// Identifer used as a pattern.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct BindingIdent {
    pub node_id: NodeId,

    #[span]
    pub id: Ident,
}

impl BindingIdent {
    pub fn from_ident(id: Ident, node_id: NodeId) -> Self {
        Self { id, node_id }
    }
}

/// Ident with span.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Ident {
    pub node_id: NodeId,

    pub span: Span,
    pub sym: JsWord,
}

impl Take for Ident {
    fn dummy() -> Self {
        Ident {
            node_id: NodeId::DUMMY,
            span: DUMMY_SP,
            sym: js_word!(""),
        }
    }
}

#[cfg(feature = "arbitrary")]
impl arbitrary::Arbitrary for Ident {
    fn arbitrary(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        let span = u.arbitrary()?;
        let sym = u.arbitrary::<String>()?;
        if sym.is_empty() {
            return Err(arbitrary::Error::NotEnoughData);
        }
        let sym = sym.into();

        Ok(Self { span, sym })
    }
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct PrivateName {
    pub node_id: NodeId,

    pub span: Span,
    pub id: Ident,
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.sym
    }
}

// impl Ident {
//     pub const fn new(sym: JsWord, span: Span) -> Self {
//         Ident {
//             span,
//             sym,
//         }
//     }
// }

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
