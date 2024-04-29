use crate::{
    expr::{Expr, SpreadElement},
    ident::Ident,
    lit::Lit,
    GetNodeId, NodeId,
};
use ast_node::ast_node;
use atoms::JsWord;

/// Used for `obj` property of `JSXMemberExpr`.
#[ast_node]
#[derive(Eq, Hash)]
#[allow(variant_size_differences)]
pub enum JSXObject {
    JSXMemberExpr(Box<JSXMemberExpr>),
    Ident(Ident),
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct JSXMemberExpr {
    pub node_id: NodeId,

    pub obj: JSXObject,

    pub prop: Ident,
}

/// XML-based namespace syntax:
#[ast_node]
#[derive(Eq, Hash)]
pub struct JSXNamespacedName {
    pub node_id: NodeId,

    pub ns: Ident,
    pub name: Ident,
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct JSXEmptyExpr {
    pub node_id: NodeId,
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct JSXExprContainer {
    pub node_id: NodeId,

    pub expr: JSXExpr,
}

#[ast_node]
#[derive(Eq, Hash)]
#[allow(variant_size_differences)]
pub enum JSXExpr {
    JSXEmptyExpr(JSXEmptyExpr),
    Expr(Box<Expr>),
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct JSXSpreadChild {
    pub node_id: NodeId,

    pub expr: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash)]
pub enum JSXElementName {
    Ident(Ident),
    JSXMemberExpr(JSXMemberExpr),
    JSXNamespacedName(JSXNamespacedName),
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct JSXOpeningElement {
    pub node_id: NodeId,

    pub name: JSXElementName,

    pub attrs: Vec<JSXAttrOrSpread>,

    pub self_closing: bool,
}

#[ast_node]
#[derive(Eq, Hash)]
#[allow(variant_size_differences)]
pub enum JSXAttrOrSpread {
    JSXAttr(JSXAttr),
    SpreadElement(SpreadElement),
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct JSXClosingElement {
    pub node_id: NodeId,
    pub name: JSXElementName,
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct JSXAttr {
    pub node_id: NodeId,
    pub name: JSXAttrName,
    /// Babel uses Expr instead of JSXAttrValue
    pub value: Option<JSXAttrValue>,
}

#[ast_node]
#[derive(Eq, Hash)]
pub enum JSXAttrName {
    Ident(Ident),
    JSXNamespacedName(JSXNamespacedName),
}

#[ast_node]
#[derive(Eq, Hash)]
pub enum JSXAttrValue {
    Lit(Lit),

    JSXExprContainer(JSXExprContainer),

    JSXElement(Box<JSXElement>),

    JSXFragment(JSXFragment),
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct JSXText {
    pub node_id: NodeId,
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
#[derive(Eq, Hash)]
pub struct JSXElement {
    pub node_id: NodeId,
    pub opening: JSXOpeningElement,
    pub children: Vec<JSXElementChild>,
    pub closing: Option<JSXClosingElement>,
}

#[ast_node]
#[derive(Eq, Hash)]
pub enum JSXElementChild {
    JSXText(JSXText),

    JSXExprContainer(JSXExprContainer),

    JSXSpreadChild(JSXSpreadChild),

    JSXElement(Box<JSXElement>),

    JSXFragment(JSXFragment),
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct JSXFragment {
    pub node_id: NodeId,

    pub opening: JSXOpeningFragment,

    pub children: Vec<JSXElementChild>,

    pub closing: JSXClosingFragment,
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct JSXOpeningFragment {
    pub node_id: NodeId,
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct JSXClosingFragment {
    pub node_id: NodeId,
}
