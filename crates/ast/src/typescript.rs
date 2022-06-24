#![allow(clippy::vec_box)]
#![allow(missing_copy_implementations)]
use crate::{
    class::Decorator,
    expr::Expr,
    ident::Ident,
    lit::{Bool, Number, Str},
    module::ModuleItem,
    pat::{ArrayPat, AssignPat, ObjectPat, Pat, RestPat},
    BigInt, GetNodeId, NodeId, TplElement,
};
use crate::{BindingIdent, PropName};
use ast_node::ast_node;
use global_common::{EqIgnoreSpan, Span};
use string_enum::StringEnum;

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeAnn {
    pub node_id: NodeId,

    pub span: Span,
    pub type_ann: Box<TsType>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeParamDecl {
    pub node_id: NodeId,

    pub span: Span,
    pub name: Ident,

    pub constraint: Option<Box<TsType>>,

    pub default: Option<Box<TsType>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeParamInstantiation {
    pub node_id: NodeId,

    pub span: Span,
    pub params: Vec<Box<TsType>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsParamProp {
    pub node_id: NodeId,

    pub span: Span,
    pub decorators: Vec<Decorator>,
    /// At least one of `accessibility` or `readonly` must be set.
    pub accessibility: Option<Accessibility>,
    pub is_override: bool,
    pub readonly: bool,
    pub param: TsParamPropParam,
}

// TODO: maybe rename to TsParamPropPat
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsParamPropParam {
    Ident(BindingIdent),

    Assign(AssignPat),
}

impl From<TsParamPropParam> for Pat {
    fn from(other: TsParamPropParam) -> Self {
        match other {
            TsParamPropParam::Ident(n) => Pat::Ident(n),
            TsParamPropParam::Assign(n) => Pat::Assign(n),
        }
    }
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsQualifiedName {
    pub node_id: NodeId,

    #[span(lo)]
    pub left: TsEntityName,
    #[span(hi)]
    pub right: Ident,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum TsEntityName {
    TsQualifiedName(Box<TsQualifiedName>),

    Ident(Ident),
}

// ================
// TypeScript type members (for type literal / interface / class)
// ================

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsTypeElement {
    TsCallSignatureDecl(TsCallSignatureDecl),

    TsConstructSignatureDecl(TsConstructSignatureDecl),

    TsPropertySignature(TsPropertySignature),

    TsGetterSignature(TsGetterSignature),

    TsSetterSignature(TsSetterSignature),

    TsMethodSignature(TsMethodSignature),

    TsIndexSignature(TsIndexSignature),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsCallSignatureDecl {
    pub node_id: NodeId,

    pub span: Span,
    pub params: Vec<TsAmbientParam>,
    pub type_ann: Option<TsTypeAnn>,
    pub type_params: Option<Vec<TsTypeParamDecl>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsConstructSignatureDecl {
    pub node_id: NodeId,

    pub span: Span,
    pub params: Vec<TsAmbientParam>,
    pub type_ann: Option<TsTypeAnn>,
    pub type_params: Option<Vec<TsTypeParamDecl>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsPropertySignature {
    pub node_id: NodeId,

    pub span: Span,
    pub readonly: bool,
    pub key: PropName,
    pub optional: bool,
    pub type_ann: Option<TsTypeAnn>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsGetterSignature {
    pub node_id: NodeId,

    pub span: Span,
    pub readonly: bool,
    pub key: PropName,
    pub optional: bool,
    pub type_ann: Option<TsTypeAnn>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsSetterSignature {
    pub node_id: NodeId,

    pub span: Span,
    pub readonly: bool,
    pub key: PropName,
    pub optional: bool,
    pub param: TsAmbientParam,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsMethodSignature {
    pub node_id: NodeId,

    pub span: Span,
    pub readonly: bool,
    pub key: PropName,
    pub optional: bool,
    pub params: Vec<TsAmbientParam>,
    pub type_ann: Option<TsTypeAnn>,
    pub type_params: Option<Vec<TsTypeParamDecl>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsIndexSignature {
    pub node_id: NodeId,

    pub params: Vec<TsAmbientParam>,
    pub type_ann: Option<TsTypeAnn>,

    pub readonly: bool,
    pub is_static: bool,
    pub span: Span,
}

// ================
// TypeScript types
// ================

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsType {
    TsKeywordType(TsKeywordType),

    TsThisType(TsThisType),

    TsFnOrConstructorType(TsFnOrConstructorType),

    TsTypeRef(TsTypeRef),

    TsTypeQuery(TsTypeQuery),

    TsTypeLit(TsTypeLit),

    TsArrayType(TsArrayType),

    TsTupleType(TsTupleType),

    TsOptionalType(TsOptionalType),

    TsRestType(TsRestType),

    TsUnionOrIntersectionType(TsUnionOrIntersectionType),

    TsConditionalType(TsConditionalType),

    TsInferType(TsInferType),

    TsParenthesizedType(TsParenthesizedType),

    TsTypeOperator(TsTypeOperator),

    TsIndexedAccessType(TsIndexedAccessType),

    TsMappedType(TsMappedType),

    TsLitType(TsLitType),

    TsTypePredicate(TsTypePredicate),

    TsImportType(TsImportType),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsFnOrConstructorType {
    TsFnType(TsFnType),
    TsConstructorType(TsConstructorType),
}

impl From<TsFnType> for TsType {
    fn from(t: TsFnType) -> Self {
        TsFnOrConstructorType::TsFnType(t).into()
    }
}

impl From<TsConstructorType> for TsType {
    fn from(t: TsConstructorType) -> Self {
        TsFnOrConstructorType::TsConstructorType(t).into()
    }
}

impl From<TsUnionType> for TsType {
    fn from(t: TsUnionType) -> Self {
        TsUnionOrIntersectionType::TsUnionType(t).into()
    }
}

impl From<TsIntersectionType> for TsType {
    fn from(t: TsIntersectionType) -> Self {
        TsUnionOrIntersectionType::TsIntersectionType(t).into()
    }
}

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
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsAmbientParam {
    pub node_id: NodeId,

    #[span]
    pub pat: TsAmbientParamPat,
}

impl TsAmbientParam {
    pub fn from_ambient_param_pat(pat: TsAmbientParamPat, node_id: NodeId) -> Self {
        Self { node_id, pat }
    }
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsAmbientParamPat {
    Ident(BindingIdent),

    Array(ArrayPat),

    Rest(RestPat),

    Object(ObjectPat),
}

impl From<TsAmbientParamPat> for Pat {
    fn from(other: TsAmbientParamPat) -> Self {
        match other {
            TsAmbientParamPat::Ident(n) => Pat::Ident(n),
            TsAmbientParamPat::Array(n) => Pat::Array(n),
            TsAmbientParamPat::Rest(n) => Pat::Rest(n),
            TsAmbientParamPat::Object(n) => Pat::Object(n),
        }
    }
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsFnType {
    pub node_id: NodeId,

    pub span: Span,
    pub params: Vec<TsAmbientParam>,

    pub type_params: Option<Vec<TsTypeParamDecl>>,
    pub type_ann: TsTypeAnn,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsConstructorType {
    pub node_id: NodeId,

    pub span: Span,
    pub params: Vec<TsAmbientParam>,
    pub type_params: Option<Vec<TsTypeParamDecl>>,
    pub type_ann: TsTypeAnn,
    pub is_abstract: bool,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeRef {
    pub node_id: NodeId,

    pub span: Span,
    pub type_name: TsEntityName,
    pub type_params: Option<TsTypeParamInstantiation>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypePredicate {
    pub node_id: NodeId,

    pub span: Span,
    pub asserts: bool,
    pub param_name: TsThisTypeOrIdent,
    pub type_ann: Option<TsTypeAnn>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
#[allow(variant_size_differences)]
pub enum TsThisTypeOrIdent {
    TsThisType(TsThisType),

    Ident(Ident),
}

/// `typeof` operator
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeQuery {
    pub node_id: NodeId,

    pub span: Span,
    pub expr_name: TsTypeQueryExpr,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsTypeQueryExpr {
    TsEntityName(TsEntityName),
    Import(TsImportType),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsImportType {
    pub node_id: NodeId,

    pub span: Span,
    pub arg: Str,
    pub qualifier: Option<TsEntityName>,
    pub type_args: Option<TsTypeParamInstantiation>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeLit {
    pub node_id: NodeId,

    pub span: Span,
    pub members: Vec<TsTypeElement>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsArrayType {
    pub node_id: NodeId,

    pub span: Span,
    pub elem_type: Box<TsType>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTupleType {
    pub node_id: NodeId,

    pub span: Span,
    pub elem_types: Vec<TsTupleElement>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTupleElement {
    pub node_id: NodeId,

    pub span: Span,
    // TODO: is `label` unused?:
    /// `Ident` or `RestPat { arg: Ident }`
    pub label: Option<Pat>,
    pub ty: TsType,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsOptionalType {
    pub node_id: NodeId,

    pub span: Span,
    pub type_ann: Box<TsType>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsRestType {
    pub node_id: NodeId,

    pub span: Span,
    pub type_ann: Box<TsType>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsUnionOrIntersectionType {
    TsUnionType(TsUnionType),

    TsIntersectionType(TsIntersectionType),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsUnionType {
    pub node_id: NodeId,

    pub span: Span,
    pub types: Vec<Box<TsType>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsIntersectionType {
    pub node_id: NodeId,

    pub span: Span,
    pub types: Vec<Box<TsType>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsConditionalType {
    pub node_id: NodeId,

    pub span: Span,
    pub check_type: Box<TsType>,
    pub extends_type: Box<TsType>,
    pub true_type: Box<TsType>,
    pub false_type: Box<TsType>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsInferType {
    pub node_id: NodeId,

    pub span: Span,
    pub type_param: TsTypeParamDecl,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsParenthesizedType {
    pub node_id: NodeId,

    pub span: Span,
    pub type_ann: Box<TsType>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeOperator {
    pub node_id: NodeId,

    pub span: Span,
    pub op: TsTypeOperatorOp,
    pub type_ann: Box<TsType>,
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
pub struct TsIndexedAccessType {
    pub node_id: NodeId,

    pub span: Span,
    pub readonly: bool,
    pub obj_type: Box<TsType>,
    pub index_type: Box<TsType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub enum TruePlusMinus {
    True,
    Plus,
    Minus,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsMappedType {
    pub node_id: NodeId,

    pub span: Span,
    pub readonly: Option<TruePlusMinus>,
    pub type_param: TsTypeParamDecl,
    pub name_type: Option<Box<TsType>>,
    pub optional: Option<TruePlusMinus>,
    pub type_ann: Option<Box<TsType>>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsLitType {
    pub node_id: NodeId,

    pub span: Span,
    pub lit: TsLit,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsLit {
    Number(Number),

    Str(Str),

    Bool(Bool),

    BigInt(BigInt),

    Tpl(TsTplLitType),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTplLitType {
    pub node_id: NodeId,

    pub span: Span,

    pub types: Vec<Box<TsType>>,

    pub quasis: Vec<TplElement>,
}

// // ================
// // TypeScript declarations
// // ================

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsInterfaceDecl {
    pub node_id: NodeId,

    pub span: Span,
    pub id: Ident,
    pub declare: bool,
    pub type_params: Option<Vec<TsTypeParamDecl>>,
    pub extends: Vec<TsExprWithTypeArgs>,
    pub body: TsInterfaceBody,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsInterfaceBody {
    pub node_id: NodeId,

    pub span: Span,
    pub body: Vec<TsTypeElement>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsExprWithTypeArgs {
    pub node_id: NodeId,

    pub span: Span,
    pub expr: TsEntityName,
    pub type_args: Option<TsTypeParamInstantiation>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeAliasDecl {
    pub node_id: NodeId,

    pub span: Span,
    pub declare: bool,
    pub id: Ident,
    pub type_params: Option<Vec<TsTypeParamDecl>>,
    pub type_ann: Box<TsType>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsEnumDecl {
    pub node_id: NodeId,

    pub span: Span,
    pub declare: bool,
    pub is_const: bool,
    pub id: Ident,
    pub members: Vec<TsEnumMember>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsEnumMember {
    pub node_id: NodeId,

    pub span: Span,
    pub id: TsEnumMemberId,
    pub init: Option<Box<Expr>>,
}

///
/// - Invalid: [Ident] with empty symbol.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsEnumMemberId {
    Ident(Ident),

    Str(Str),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsModuleDecl {
    pub node_id: NodeId,

    pub span: Span,
    pub declare: bool,
    /// In TypeScript, this is only available through`node.flags`.
    pub global: bool,
    pub id: TsModuleName,
    pub body: Option<TsNamespaceBody>,
}

/// `namespace A.B { }` is a namespace named `A` with another TsNamespaceDecl as
/// its body.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsNamespaceBody {
    TsModuleBlock(TsModuleBlock),

    TsNamespaceDecl(TsNamespaceDecl),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsModuleBlock {
    pub node_id: NodeId,

    pub span: Span,
    pub body: Vec<ModuleItem>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsNamespaceDecl {
    pub node_id: NodeId,

    pub span: Span,
    pub declare: bool,
    /// In TypeScript, this is only available through`node.flags`.
    pub global: bool,
    pub id: Ident,
    pub body: Box<TsNamespaceBody>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsModuleName {
    Ident(Ident),

    Str(Str),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsImportEqualsDecl {
    pub node_id: NodeId,

    pub span: Span,
    pub declare: bool,
    pub is_export: bool,
    pub is_type_only: bool,
    pub id: Ident,
    pub module_ref: TsModuleRef,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsModuleRef {
    TsEntityName(TsEntityName),

    TsExternalModuleRef(TsExternalModuleRef),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsExternalModuleRef {
    pub node_id: NodeId,

    pub span: Span,
    pub expr: Str,
}

/// TypeScript's own parser uses ExportAssignment for both `export default` and
/// `export =`. But for @babel/parser, `export default` is an ExportDefaultDecl,
/// so a TsExportAssignment is always `export =`.
#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsExportAssignment {
    pub node_id: NodeId,

    pub span: Span,
    pub expr: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsNamespaceExportDecl {
    pub node_id: NodeId,

    pub span: Span,
    pub id: Ident,
}

// // ================
// // TypeScript exprs
// // ================

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsAsExpr {
    pub node_id: NodeId,

    pub span: Span,
    pub expr: Box<Expr>,
    pub type_ann: Box<TsType>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeAssertion {
    pub node_id: NodeId,

    pub span: Span,
    pub expr: Box<Expr>,
    pub type_ann: Box<TsType>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsNonNullExpr {
    pub node_id: NodeId,

    pub span: Span,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub enum Accessibility {
    Public,
    Protected,
    Private,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsConstAssertion {
    pub node_id: NodeId,

    pub span: Span,
    pub expr: Box<Expr>,
}
