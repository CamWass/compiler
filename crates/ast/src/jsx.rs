use crate::{
    expr::{Expr, SpreadElement},
    ident::Ident,
    lit::Lit,
    typescript::TsTypeParamInstantiation,
    GetNodeId, NodeId,
};
use ast_node::ast_node;
use atoms::JsWord;
use global_common::{EqIgnoreSpan, Span};

/// Used for `obj` property of `JSXMemberExpr`.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum JSXObject {
    JSXMemberExpr(Box<JSXMemberExpr>),
    Ident(Ident),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXMemberExpr {
    pub node_id: NodeId,

    #[span(lo)]
    pub obj: JSXObject,

    #[span(hi)]
    pub prop: Ident,
}

/// XML-based namespace syntax:
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXNamespacedName {
    pub node_id: NodeId,

    #[span(lo)]
    pub ns: Ident,
    #[span(hi)]
    pub name: Ident,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct JSXEmptyExpr {
    pub node_id: NodeId,

    pub span: Span,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXExprContainer {
    pub node_id: NodeId,

    pub span: Span,

    pub expr: JSXExpr,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum JSXExpr {
    JSXEmptyExpr(JSXEmptyExpr),
    Expr(Box<Expr>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXSpreadChild {
    pub node_id: NodeId,

    pub span: Span,

    pub expr: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum JSXElementName {
    Ident(Ident),
    JSXMemberExpr(JSXMemberExpr),
    JSXNamespacedName(JSXNamespacedName),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXOpeningElement {
    pub node_id: NodeId,

    pub name: JSXElementName,

    pub span: Span,

    pub attrs: Vec<JSXAttrOrSpread>,

    pub self_closing: bool,

    /// Note: This field's name is different from one from babel because it is
    /// misleading
    pub type_args: Option<TsTypeParamInstantiation>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum JSXAttrOrSpread {
    JSXAttr(JSXAttr),
    SpreadElement(SpreadElement),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXClosingElement {
    pub node_id: NodeId,

    pub span: Span,
    pub name: JSXElementName,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXAttr {
    pub node_id: NodeId,

    pub span: Span,
    pub name: JSXAttrName,
    /// Babel uses Expr instead of JSXAttrValue
    pub value: Option<JSXAttrValue>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum JSXAttrName {
    Ident(Ident),
    JSXNamespacedName(JSXNamespacedName),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum JSXAttrValue {
    Lit(Lit),

    JSXExprContainer(JSXExprContainer),

    JSXElement(Box<JSXElement>),

    JSXFragment(JSXFragment),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXText {
    pub node_id: NodeId,

    pub span: Span,
    pub value: JsWord,
    pub raw: JsWord,
}

#[cfg(feature = "arbitrary")]
impl arbitrary::Arbitrary for JSXText {
    fn arbitrary(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        let span = u.arbitrary()?;
        let value = u.arbitrary::<String>()?.into();
        let raw = u.arbitrary::<String>()?.into();

        Ok(Self { span, value, raw })
    }
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXElement {
    pub node_id: NodeId,

    pub span: Span,
    pub opening: JSXOpeningElement,
    pub children: Vec<JSXElementChild>,
    pub closing: Option<JSXClosingElement>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum JSXElementChild {
    JSXText(JSXText),

    JSXExprContainer(JSXExprContainer),

    JSXSpreadChild(JSXSpreadChild),

    JSXElement(Box<JSXElement>),

    JSXFragment(JSXFragment),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXFragment {
    pub node_id: NodeId,

    pub span: Span,

    pub opening: JSXOpeningFragment,

    pub children: Vec<JSXElementChild>,

    pub closing: JSXClosingFragment,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct JSXOpeningFragment {
    pub node_id: NodeId,

    pub span: Span,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct JSXClosingFragment {
    pub node_id: NodeId,

    pub span: Span,
}
