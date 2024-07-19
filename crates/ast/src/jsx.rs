use crate::{
    expr::{Expr, SpreadElement},
    ident::Ident,
    lit::Lit,
    GetNodeId, NodeId,
};
use atoms::JsWord;
use node_id::GetNodeIdMacro;
use clone_node::CloneNode;

/// Used for `obj` property of `JSXMemberExpr`.
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
#[allow(variant_size_differences)]
pub enum JSXObject {
    JSXMemberExpr(Box<JSXMemberExpr>),
    Ident(Ident),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct JSXMemberExpr {
    pub node_id: NodeId,

    pub obj: JSXObject,

    pub prop: Ident,
}

/// XML-based namespace syntax:
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct JSXNamespacedName {
    pub node_id: NodeId,

    pub ns: Ident,
    pub name: Ident,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct JSXEmptyExpr {
    pub node_id: NodeId,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct JSXExprContainer {
    pub node_id: NodeId,

    pub expr: JSXExpr,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
#[allow(variant_size_differences)]
pub enum JSXExpr {
    JSXEmptyExpr(JSXEmptyExpr),
    Expr(Box<Expr>),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct JSXSpreadChild {
    pub node_id: NodeId,

    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum JSXElementName {
    Ident(Ident),
    JSXMemberExpr(JSXMemberExpr),
    JSXNamespacedName(JSXNamespacedName),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct JSXOpeningElement {
    pub node_id: NodeId,

    pub name: JSXElementName,

    pub attrs: Vec<JSXAttrOrSpread>,

    pub self_closing: bool,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
#[allow(variant_size_differences)]
pub enum JSXAttrOrSpread {
    JSXAttr(JSXAttr),
    SpreadElement(SpreadElement),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct JSXClosingElement {
    pub node_id: NodeId,
    pub name: JSXElementName,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct JSXAttr {
    pub node_id: NodeId,
    pub name: JSXAttrName,
    /// Babel uses Expr instead of JSXAttrValue
    pub value: Option<JSXAttrValue>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum JSXAttrName {
    Ident(Ident),
    JSXNamespacedName(JSXNamespacedName),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum JSXAttrValue {
    Lit(Lit),

    JSXExprContainer(JSXExprContainer),

    JSXElement(Box<JSXElement>),

    JSXFragment(JSXFragment),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
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

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct JSXElement {
    pub node_id: NodeId,
    pub opening: JSXOpeningElement,
    pub children: Vec<JSXElementChild>,
    pub closing: Option<JSXClosingElement>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum JSXElementChild {
    JSXText(JSXText),

    JSXExprContainer(JSXExprContainer),

    JSXSpreadChild(JSXSpreadChild),

    JSXElement(Box<JSXElement>),

    JSXFragment(JSXFragment),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct JSXFragment {
    pub node_id: NodeId,

    pub opening: JSXOpeningFragment,

    pub children: Vec<JSXElementChild>,

    pub closing: JSXClosingFragment,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct JSXOpeningFragment {
    pub node_id: NodeId,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct JSXClosingFragment {
    pub node_id: NodeId,
}
