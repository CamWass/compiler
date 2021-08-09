#![allow(missing_copy_implementations)]
use crate::BindingIdent;
use crate::{
    class::Decorator,
    expr::Expr,
    ident::Ident,
    lit::{Bool, Number, Str},
    module::ModuleItem,
    node::NodeId,
    pat::{ArrayPat, AssignPat, ObjectPat, Pat, RestPat},
    BigInt, TplElement,
};
use ast_node2::ast_node;
use global_common::{EqIgnoreSpan, Span};
use std::fmt;
use string_enum2::StringEnum;

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeAnn<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub type_ann: TsType<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeParamDecl<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub params: Vec<&'ast TsTypeParam<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeParam<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub name: &'ast Ident,
    pub constraint: Option<TsType<'ast>>,
    pub default: Option<TsType<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeParamInstantiation<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub params: Vec<TsType<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsParamProp<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub decorators: Vec<&'ast Decorator<'ast>>,
    /// At least one of `accessibility` or `readonly` must be set.
    pub accessibility: Option<Accessibility>,

    pub is_override: bool,
    pub readonly: bool,
    pub param: TsParamPropParam<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum TsParamPropParam<'ast> {
    Ident(&'ast BindingIdent<'ast>),
    Assign(&'ast AssignPat<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsQualifiedName<'ast> {
    pub node_id: NodeId,
    #[span(lo)]
    pub left: TsEntityName<'ast>,
    #[span(hi)]
    pub right: &'ast Ident,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum TsEntityName<'ast> {
    TsQualifiedName(&'ast TsQualifiedName<'ast>),
    Ident(&'ast Ident),
}

// ================
// TypeScript type members (for type literal / interface / class)
// ================

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum TsTypeElement<'ast> {
    TsCallSignatureDecl(&'ast TsCallSignatureDecl<'ast>),
    TsConstructSignatureDecl(&'ast TsConstructSignatureDecl<'ast>),
    TsPropertySignature(&'ast TsPropertySignature<'ast>),
    TsGetterSignature(&'ast TsGetterSignature<'ast>),
    TsSetterSignature(&'ast TsSetterSignature<'ast>),
    TsMethodSignature(&'ast TsMethodSignature<'ast>),
    TsIndexSignature(&'ast TsIndexSignature<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsCallSignatureDecl<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub params: Vec<TsFnParam<'ast>>,

    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,

    pub type_params: Option<&'ast TsTypeParamDecl<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsConstructSignatureDecl<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub params: Vec<TsFnParam<'ast>>,

    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,

    pub type_params: Option<&'ast TsTypeParamDecl<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsPropertySignature<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub readonly: bool,
    pub key: Expr<'ast>,
    pub computed: bool,
    pub optional: bool,

    pub init: Option<Expr<'ast>>,
    pub params: Vec<TsFnParam<'ast>>,

    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,

    pub type_params: Option<&'ast TsTypeParamDecl<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsGetterSignature<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub readonly: bool,
    pub key: Expr<'ast>,
    pub computed: bool,
    pub optional: bool,

    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsSetterSignature<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub readonly: bool,
    pub key: Expr<'ast>,
    pub computed: bool,
    pub optional: bool,
    pub param: TsFnParam<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsMethodSignature<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub readonly: bool,
    pub key: Expr<'ast>,
    pub computed: bool,
    pub optional: bool,
    pub params: Vec<TsFnParam<'ast>>,

    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,

    pub type_params: Option<&'ast TsTypeParamDecl<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsIndexSignature<'ast> {
    pub node_id: NodeId,
    pub params: Vec<TsFnParam<'ast>>,

    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,

    pub readonly: bool,

    pub is_static: bool,
    pub span: Span,
}

// ================
// TypeScript types
// ================

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum TsType<'ast> {
    TsKeywordType(&'ast TsKeywordType),
    TsThisType(&'ast TsThisType),
    TsFnOrConstructorType(TsFnOrConstructorType<'ast>),
    TsTypeRef(&'ast TsTypeRef<'ast>),
    TsTypeQuery(&'ast TsTypeQuery<'ast>),
    TsTypeLit(&'ast TsTypeLit<'ast>),
    TsArrayType(&'ast TsArrayType<'ast>),
    TsTupleType(&'ast TsTupleType<'ast>),
    TsOptionalType(&'ast TsOptionalType<'ast>),
    TsRestType(&'ast TsRestType<'ast>),
    TsUnionOrIntersectionType(TsUnionOrIntersectionType<'ast>),
    TsConditionalType(&'ast TsConditionalType<'ast>),
    TsInferType(&'ast TsInferType<'ast>),
    TsParenthesizedType(&'ast TsParenthesizedType<'ast>),
    TsTypeOperator(&'ast TsTypeOperator<'ast>),
    TsIndexedAccessType(&'ast TsIndexedAccessType<'ast>),
    TsMappedType(&'ast TsMappedType<'ast>),
    TsLitType(&'ast TsLitType<'ast>),
    TsTypePredicate(&'ast TsTypePredicate<'ast>),
    TsImportType(&'ast TsImportType<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum TsFnOrConstructorType<'ast> {
    TsFnType(&'ast TsFnType<'ast>),
    TsConstructorType(&'ast TsConstructorType<'ast>),
}

// impl From<TsFnType> for TsType {
//     fn from(t: TsFnType) -> Self {
//         TsFnOrConstructorType::TsFnType(t).into()
//     }
// }

// impl From<TsConstructorType> for TsType {
//     fn from(t: TsConstructorType) -> Self {
//         TsFnOrConstructorType::TsConstructorType(t).into()
//     }
// }

// impl From<TsUnionType> for TsType {
//     fn from(t: TsUnionType) -> Self {
//         TsUnionOrIntersectionType::TsUnionType(t).into()
//     }
// }

// impl From<TsIntersectionType> for TsType {
//     fn from(t: TsIntersectionType) -> Self {
//         TsUnionOrIntersectionType::TsIntersectionType(t).into()
//     }
// }

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsKeywordType {
    pub node_id: NodeId,
    pub span: Span,
    pub kind: TsKeywordTypeKind,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub enum TsKeywordTypeKind {
    TsAnyKeyword,
    TsUnknownKeyword,
    TsNumberKeyword,
    TsObjectKeyword,
    TsBooleanKeyword,
    TsBigIntKeyword,
    TsStringKeyword,
    TsSymbolKeyword,
    TsVoidKeyword,
    TsUndefinedKeyword,
    TsNullKeyword,
    TsNeverKeyword,
    TsIntrinsicKeyword,
}

#[ast_node]
#[derive(Copy, Eq, Hash, EqIgnoreSpan)]
pub struct TsThisType {
    pub node_id: NodeId,
    pub span: Span,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum TsFnParam<'ast> {
    Ident(&'ast BindingIdent<'ast>),
    Array(&'ast ArrayPat<'ast>),
    Rest(&'ast RestPat<'ast>),
    Object(&'ast ObjectPat<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsFnType<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub params: Vec<TsFnParam<'ast>>,
    pub type_params: Option<&'ast TsTypeParamDecl<'ast>>,

    pub type_ann: &'ast TsTypeAnn<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsConstructorType<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub params: Vec<TsFnParam<'ast>>,

    pub type_params: Option<&'ast TsTypeParamDecl<'ast>>,

    pub type_ann: &'ast TsTypeAnn<'ast>,
    pub is_abstract: bool,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeRef<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub type_name: TsEntityName<'ast>,

    pub type_params: Option<&'ast TsTypeParamInstantiation<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypePredicate<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub asserts: bool,
    pub param_name: TsThisTypeOrIdent<'ast>,

    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum TsThisTypeOrIdent<'ast> {
    TsThisType(&'ast TsThisType),
    Ident(&'ast Ident),
}

/// `typeof` operator
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeQuery<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub expr_name: TsTypeQueryExpr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum TsTypeQueryExpr<'ast> {
    TsEntityName(TsEntityName<'ast>),
    Import(&'ast TsImportType<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsImportType<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub arg: &'ast Str,
    pub qualifier: Option<TsEntityName<'ast>>,

    pub type_args: Option<&'ast TsTypeParamInstantiation<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeLit<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub members: Vec<TsTypeElement<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsArrayType<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub elem_type: TsType<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTupleType<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub elem_types: Vec<&'ast TsTupleElement<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTupleElement<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    /// `Ident` or `RestPat { arg: Ident }`
    pub label: Option<Pat<'ast>>,
    pub ty: TsType<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsOptionalType<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub type_ann: TsType<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsRestType<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub type_ann: TsType<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum TsUnionOrIntersectionType<'ast> {
    TsUnionType(&'ast TsUnionType<'ast>),
    TsIntersectionType(&'ast TsIntersectionType<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsUnionType<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub types: Vec<TsType<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsIntersectionType<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub types: Vec<TsType<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsConditionalType<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub check_type: TsType<'ast>,
    pub extends_type: TsType<'ast>,
    pub true_type: TsType<'ast>,
    pub false_type: TsType<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsInferType<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub type_param: &'ast TsTypeParam<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsParenthesizedType<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub type_ann: TsType<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeOperator<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub op: TsTypeOperatorOp,

    pub type_ann: TsType<'ast>,
}

#[derive(StringEnum, Clone, Copy, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub enum TsTypeOperatorOp {
    /// `keyof`
    KeyOf,
    /// `unique`
    Unique,
    /// `readonly`
    ReadOnly,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsIndexedAccessType<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub readonly: bool,

    pub obj_type: TsType<'ast>,
    pub index_type: TsType<'ast>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub enum TruePlusMinus {
    True,
    Plus,
    Minus,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsMappedType<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub readonly: Option<TruePlusMinus>,
    pub type_param: &'ast TsTypeParam<'ast>,

    pub name_type: Option<TsType<'ast>>,

    pub optional: Option<TruePlusMinus>,

    pub type_ann: Option<TsType<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsLitType<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub lit: TsLit<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum TsLit<'ast> {
    Number(&'ast Number),
    Str(&'ast Str),
    Bool(&'ast Bool),
    BigInt(&'ast BigInt),
    Tpl(&'ast TsTplLitType<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTplLitType<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub types: Vec<TsType<'ast>>,

    pub quasis: Vec<&'ast TplElement<'ast>>,
}

// // ================
// // TypeScript declarations
// // ================

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsInterfaceDecl<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub id: &'ast Ident,
    pub declare: bool,

    pub type_params: Option<&'ast TsTypeParamDecl<'ast>>,
    pub extends: Vec<&'ast TsExprWithTypeArgs<'ast>>,
    pub body: &'ast TsInterfaceBody<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsInterfaceBody<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub body: Vec<TsTypeElement<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsExprWithTypeArgs<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub expr: TsEntityName<'ast>,

    pub type_args: Option<&'ast TsTypeParamInstantiation<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeAliasDecl<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub declare: bool,
    pub id: &'ast Ident,

    pub type_params: Option<&'ast TsTypeParamDecl<'ast>>,

    pub type_ann: TsType<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsEnumDecl<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub declare: bool,
    pub is_const: bool,
    pub id: &'ast Ident,
    pub members: Vec<&'ast TsEnumMember<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsEnumMember<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub id: TsEnumMemberId<'ast>,

    pub init: Option<Expr<'ast>>,
}

///
/// - Invalid: [Ident] with empty symbol.
#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum TsEnumMemberId<'ast> {
    Ident(&'ast Ident),
    Str(&'ast Str),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsModuleDecl<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub declare: bool,
    /// In TypeScript, this is only available through`node.flags`.
    pub global: bool,
    pub id: TsModuleName<'ast>,

    pub body: Option<TsNamespaceBody<'ast>>,
}

/// `namespace A.B { }` is a namespace named `A` with another TsNamespaceDecl as
/// its body.
#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum TsNamespaceBody<'ast> {
    TsModuleBlock(&'ast TsModuleBlock<'ast>),
    TsNamespaceDecl(&'ast TsNamespaceDecl<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsModuleBlock<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub body: Vec<ModuleItem<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsNamespaceDecl<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub declare: bool,
    /// In TypeScript, this is only available through`node.flags`.
    pub global: bool,
    pub id: &'ast Ident,
    pub body: TsNamespaceBody<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum TsModuleName<'ast> {
    Ident(&'ast Ident),
    Str(&'ast Str),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsImportEqualsDecl<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub declare: bool,
    pub is_export: bool,
    pub is_type_only: bool,
    pub id: &'ast Ident,
    pub module_ref: TsModuleRef<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum TsModuleRef<'ast> {
    TsEntityName(TsEntityName<'ast>),
    TsExternalModuleRef(&'ast TsExternalModuleRef<'ast>),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsExternalModuleRef<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub expr: &'ast Str,
}

/// TypeScript's own parser uses ExportAssignment for both `export default` and
/// `export =`. But for @babel/parser, `export default` is an ExportDefaultDecl,
/// so a TsExportAssignment is always `export =`.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsExportAssignment<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub expr: Expr<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsNamespaceExportDecl<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub id: &'ast Ident,
}

// // ================
// // TypeScript exprs
// // ================

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsAsExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub expr: Expr<'ast>,

    pub type_ann: TsType<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeAssertion<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub expr: Expr<'ast>,

    pub type_ann: TsType<'ast>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsNonNullExpr<'ast> {
    pub node_id: NodeId,
    pub span: Span,

    pub expr: Expr<'ast>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub enum Accessibility {
    Public,
    Protected,
    Private,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsConstAssertion<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub expr: Expr<'ast>,
}
