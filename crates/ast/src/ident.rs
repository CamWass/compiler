use crate::{GetNodeId, NodeId, ProgramData};
use ast_node::ast_node;
use atoms::{js_word, JsWord};
use global_common::{util::take::Take, SyntaxContext};

/// Identifier used as a pattern.
#[ast_node]
#[derive(Eq, Hash)]
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
#[ast_node]
#[derive(Eq, Hash)]
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

// TODO: remove this stuff
#[cfg(feature = "arbitrary")]
impl arbitrary::Arbitrary for Ident {
    fn arbitrary(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        let sym = u.arbitrary::<String>()?;
        if sym.is_empty() {
            return Err(arbitrary::Error::NotEnoughData);
        }
        let sym = sym.into();

        Ok(Self { sym })
    }
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct PrivateName {
    pub node_id: NodeId,
    pub id: Ident,
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
