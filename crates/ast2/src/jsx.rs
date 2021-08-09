use crate::{
    expr::{Expr, SpreadElement},
    ident::Ident,
    lit::Lit,
    node::NodeId,
    typescript::TsTypeParamInstantiation,
};
use ast_node2::ast_node;
use global_common::{EqIgnoreSpan, Span};
use swc_atoms::JsWord;

/// Used for `obj` property of `JSXMemberExpr`.
#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum JSXObject<'ast> {
    JSXMemberExpr(&'ast JSXMemberExpr<'ast>),
    Ident(&'ast Ident),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXMemberExpr<'ast> {
    pub node_id: NodeId,

    #[span(lo)]
    pub obj: JSXObject<'ast>,

    #[span(hi)]
    pub prop: &'ast Ident,
}

/// XML-based namespace syntax:
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXNamespacedName<'ast> {
    pub node_id: NodeId,

    #[span(lo)]
    pub ns: &'ast Ident,
    #[span(hi)]
    pub name: &'ast Ident,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub struct JSXEmptyExpr {
    pub node_id: NodeId,
    pub span: Span,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXExprContainer<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub expr: JSXExpr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum JSXExpr<'ast> {
    JSXEmptyExpr(&'ast JSXEmptyExpr),

    Expr(Expr<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXSpreadChild<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub expr: Expr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum JSXElementName<'ast> {
    Ident(&'ast Ident),
    JSXMemberExpr(&'ast JSXMemberExpr<'ast>),
    JSXNamespacedName(&'ast JSXNamespacedName<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXOpeningElement<'ast> {
    pub node_id: NodeId,
    pub name: JSXElementName<'ast>,

    pub span: Span,

    pub attrs: Vec<JSXAttrOrSpread<'ast>>,

    pub self_closing: bool,

    /// Note: This field's name is different from one from babel because it is
    /// misleading
    pub type_args: Option<&'ast TsTypeParamInstantiation<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum JSXAttrOrSpread<'ast> {
    JSXAttr(&'ast JSXAttr<'ast>),
    SpreadElement(&'ast SpreadElement<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXClosingElement<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub name: JSXElementName<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXAttr<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub name: JSXAttrName<'ast>,
    /// Babel uses Expr instead of JSXAttrValue
    pub value: Option<JSXAttrValue<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum JSXAttrName<'ast> {
    Ident(&'ast Ident),
    JSXNamespacedName(&'ast JSXNamespacedName<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum JSXAttrValue<'ast> {
    Lit(Lit<'ast>),
    JSXExprContainer(&'ast JSXExprContainer<'ast>),
    JSXElement(&'ast JSXElement<'ast>),
    JSXFragment(&'ast JSXFragment<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXText {
    pub span: Span,
    pub value: JsWord,
    pub raw: JsWord,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXElement<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub opening: &'ast JSXOpeningElement<'ast>,
    pub children: Vec<JSXElementChild<'ast>>,
    pub closing: Option<&'ast JSXClosingElement<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum JSXElementChild<'ast> {
    JSXText(&'ast JSXText),
    JSXExprContainer(&'ast JSXExprContainer<'ast>),
    JSXSpreadChild(&'ast JSXSpreadChild<'ast>),
    JSXElement(&'ast JSXElement<'ast>),
    JSXFragment(&'ast JSXFragment<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct JSXFragment<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub opening: &'ast JSXOpeningFragment,
    pub children: Vec<JSXElementChild<'ast>>,

    pub closing: &'ast JSXClosingFragment,
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
