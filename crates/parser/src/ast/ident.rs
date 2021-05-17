use super::typescript::TsTypeAnn;
use global_common::{ast_node, EqIgnoreSpan, Span, Spanned};
use swc_atoms::JsWord;

/// Identifer used as a pattern.
#[derive(Spanned, Clone, Debug, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub struct BindingIdent {
    #[span]
    pub id: Ident,
    pub type_ann: Option<TsTypeAnn>,
}

impl From<Ident> for BindingIdent {
    fn from(id: Ident) -> Self {
        Self { id, type_ann: None }
    }
}

/// Ident with span.
#[ast_node("Identifier")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Ident {
    pub span: Span,
    pub sym: JsWord,

    /// TypeScript only. Used in case of an optional parameter.
    pub optional: bool,
}

#[ast_node("PrivateName")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct PrivateName {
    pub span: Span,
    pub id: Ident,
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.sym
    }
}

impl Ident {
    pub const fn new(sym: JsWord, span: Span) -> Self {
        Ident {
            span,
            sym,
            optional: false,
        }
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

// impl IdentExt for JsWord {}
impl IdentExt for Ident {}
