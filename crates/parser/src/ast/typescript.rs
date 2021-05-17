#![allow(clippy::vec_box)]
#![allow(missing_copy_implementations)]
use super::BindingIdent;
use super::{
    class::Decorator,
    expr::Expr,
    ident::Ident,
    lit::{Bool, Number, Str},
    module::ModuleItem,
    pat::{ArrayPat, AssignPat, ObjectPat, Pat, RestPat},
    BigInt, TplElement,
};
use global_common::{ast_node, EqIgnoreSpan, Span};
use string_enum::StringEnum;

#[ast_node("TsTypeAnnotation")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeAnn {
    pub span: Span,

    pub type_ann: Box<TsType>,
}

#[ast_node("TsTypeParameterDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeParamDecl {
    pub span: Span,

    pub params: Vec<TsTypeParam>,
}

#[ast_node("TsTypeParameter")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeParam {
    pub span: Span,
    pub name: Ident,

    pub constraint: Option<Box<TsType>>,

    pub default: Option<Box<TsType>>,
}

#[ast_node("TsTypeParameterInstantiation")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeParamInstantiation {
    pub span: Span,
    pub params: Vec<Box<TsType>>,
}

#[ast_node("TsParameterProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsParamProp {
    pub span: Span,

    pub decorators: Vec<Decorator>,
    /// At least one of `accessibility` or `readonly` must be set.
    pub accessibility: Option<Accessibility>,
    pub readonly: bool,
    pub param: TsParamPropParam,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsParamPropParam {
    Ident(BindingIdent),

    Assign(AssignPat),
}

#[ast_node("TsQualifiedName")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsQualifiedName {
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

#[ast_node("TsCallSignatureDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsCallSignatureDecl {
    pub span: Span,
    pub params: Vec<TsFnParam>,

    pub type_ann: Option<TsTypeAnn>,

    pub type_params: Option<TsTypeParamDecl>,
}

#[ast_node("TsConstructSignatureDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsConstructSignatureDecl {
    pub span: Span,
    pub params: Vec<TsFnParam>,

    pub type_ann: Option<TsTypeAnn>,

    pub type_params: Option<TsTypeParamDecl>,
}

#[ast_node("TsPropertySignature")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsPropertySignature {
    pub span: Span,
    pub readonly: bool,
    pub key: Box<Expr>,
    pub computed: bool,
    pub optional: bool,

    pub init: Option<Box<Expr>>,
    pub params: Vec<TsFnParam>,

    pub type_ann: Option<TsTypeAnn>,

    pub type_params: Option<TsTypeParamDecl>,
}

#[ast_node("TsGetterSignature")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsGetterSignature {
    pub span: Span,
    pub readonly: bool,
    pub key: Box<Expr>,
    pub computed: bool,
    pub optional: bool,

    pub type_ann: Option<TsTypeAnn>,
}

#[ast_node("TsSetterSignature")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsSetterSignature {
    pub span: Span,
    pub readonly: bool,
    pub key: Box<Expr>,
    pub computed: bool,
    pub optional: bool,
    pub param: TsFnParam,
}

#[ast_node("TsMethodSignature")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsMethodSignature {
    pub span: Span,
    pub readonly: bool,
    pub key: Box<Expr>,
    pub computed: bool,
    pub optional: bool,
    pub params: Vec<TsFnParam>,

    pub type_ann: Option<TsTypeAnn>,

    pub type_params: Option<TsTypeParamDecl>,
}

#[ast_node("TsIndexSignature")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsIndexSignature {
    pub params: Vec<TsFnParam>,

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

#[ast_node("TsKeywordType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsKeywordType {
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

#[ast_node("TsThisType")]
#[derive(Copy, Eq, Hash, EqIgnoreSpan)]
pub struct TsThisType {
    pub span: Span,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsFnParam {
    Ident(BindingIdent),

    Array(ArrayPat),

    Rest(RestPat),

    Object(ObjectPat),
}

#[ast_node("TsFunctionType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsFnType {
    pub span: Span,
    pub params: Vec<TsFnParam>,

    pub type_params: Option<TsTypeParamDecl>,

    pub type_ann: TsTypeAnn,
}

#[ast_node("TsConstructorType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsConstructorType {
    pub span: Span,
    pub params: Vec<TsFnParam>,

    pub type_params: Option<TsTypeParamDecl>,

    pub type_ann: TsTypeAnn,
    pub is_abstract: bool,
}

#[ast_node("TsTypeReference")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeRef {
    pub span: Span,
    pub type_name: TsEntityName,

    pub type_params: Option<TsTypeParamInstantiation>,
}

#[ast_node("TsTypePredicate")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypePredicate {
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
#[ast_node("TsTypeQuery")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeQuery {
    pub span: Span,
    pub expr_name: TsTypeQueryExpr,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsTypeQueryExpr {
    TsEntityName(TsEntityName),

    Import(TsImportType),
}

#[ast_node("TsImportType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsImportType {
    pub span: Span,

    pub arg: Str,
    pub qualifier: Option<TsEntityName>,

    pub type_args: Option<TsTypeParamInstantiation>,
}

#[ast_node("TsTypeLiteral")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeLit {
    pub span: Span,
    pub members: Vec<TsTypeElement>,
}

#[ast_node("TsArrayType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsArrayType {
    pub span: Span,
    pub elem_type: Box<TsType>,
}

#[ast_node("TsTupleType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTupleType {
    pub span: Span,
    pub elem_types: Vec<TsTupleElement>,
}

#[ast_node("TsTupleElement")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTupleElement {
    pub span: Span,
    /// `Ident` or `RestPat { arg: Ident }`
    pub label: Option<Pat>,
    pub ty: TsType,
}

#[ast_node("TsOptionalType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsOptionalType {
    pub span: Span,

    pub type_ann: Box<TsType>,
}

#[ast_node("TsRestType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsRestType {
    pub span: Span,

    pub type_ann: Box<TsType>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsUnionOrIntersectionType {
    TsUnionType(TsUnionType),

    TsIntersectionType(TsIntersectionType),
}

#[ast_node("TsUnionType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsUnionType {
    pub span: Span,
    pub types: Vec<Box<TsType>>,
}

#[ast_node("TsIntersectionType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsIntersectionType {
    pub span: Span,
    pub types: Vec<Box<TsType>>,
}

#[ast_node("TsConditionalType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsConditionalType {
    pub span: Span,
    pub check_type: Box<TsType>,
    pub extends_type: Box<TsType>,
    pub true_type: Box<TsType>,
    pub false_type: Box<TsType>,
}

#[ast_node("TsInferType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsInferType {
    pub span: Span,
    pub type_param: TsTypeParam,
}

#[ast_node("TsParenthesizedType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsParenthesizedType {
    pub span: Span,

    pub type_ann: Box<TsType>,
}

#[ast_node("TsTypeOperator")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeOperator {
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

#[ast_node("TsIndexedAccessType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsIndexedAccessType {
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

#[ast_node("TsMappedType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsMappedType {
    pub span: Span,

    pub readonly: Option<TruePlusMinus>,
    pub type_param: TsTypeParam,

    pub name_type: Option<Box<TsType>>,

    pub optional: Option<TruePlusMinus>,

    pub type_ann: Option<Box<TsType>>,
}

#[ast_node("TsLiteralType")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsLitType {
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

#[ast_node("TemplateLiteral")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTplLitType {
    pub span: Span,

    pub types: Vec<Box<TsType>>,

    pub quasis: Vec<TplElement>,
}

// // ================
// // TypeScript declarations
// // ================

#[ast_node("TsInterfaceDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsInterfaceDecl {
    pub span: Span,
    pub id: Ident,
    pub declare: bool,

    pub type_params: Option<TsTypeParamDecl>,
    pub extends: Vec<TsExprWithTypeArgs>,
    pub body: TsInterfaceBody,
}

#[ast_node("TsInterfaceBody")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsInterfaceBody {
    pub span: Span,
    pub body: Vec<TsTypeElement>,
}

#[ast_node("TsExpressionWithTypeArguments")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsExprWithTypeArgs {
    pub span: Span,

    pub expr: TsEntityName,

    pub type_args: Option<TsTypeParamInstantiation>,
}

#[ast_node("TsTypeAliasDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeAliasDecl {
    pub span: Span,
    pub declare: bool,
    pub id: Ident,

    pub type_params: Option<TsTypeParamDecl>,

    pub type_ann: Box<TsType>,
}

#[ast_node("TsEnumDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsEnumDecl {
    pub span: Span,
    pub declare: bool,
    pub is_const: bool,
    pub id: Ident,
    pub members: Vec<TsEnumMember>,
}

#[ast_node("TsEnumMember")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsEnumMember {
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

#[ast_node("TsModuleDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsModuleDecl {
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

#[ast_node("TsModuleBlock")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsModuleBlock {
    pub span: Span,
    pub body: Vec<ModuleItem>,
}

#[ast_node("TsNamespaceDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsNamespaceDecl {
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

#[ast_node("TsImportEqualsDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsImportEqualsDecl {
    pub span: Span,
    pub declare: bool,
    pub is_export: bool,
    pub id: Ident,
    pub module_ref: TsModuleRef,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum TsModuleRef {
    TsEntityName(TsEntityName),

    TsExternalModuleRef(TsExternalModuleRef),
}

#[ast_node("TsExternalModuleReference")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsExternalModuleRef {
    pub span: Span,

    pub expr: Str,
}

/// TypeScript's own parser uses ExportAssignment for both `export default` and
/// `export =`. But for @babel/parser, `export default` is an ExportDefaultDecl,
/// so a TsExportAssignment is always `export =`.
#[ast_node("TsExportAssignment")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsExportAssignment {
    pub span: Span,

    pub expr: Box<Expr>,
}

#[ast_node("TsNamespaceExportDeclaration")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsNamespaceExportDecl {
    pub span: Span,
    pub id: Ident,
}

// // ================
// // TypeScript exprs
// // ================

#[ast_node("TsAsExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsAsExpr {
    pub span: Span,

    pub expr: Box<Expr>,

    pub type_ann: Box<TsType>,
}

#[ast_node("TsTypeAssertion")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsTypeAssertion {
    pub span: Span,

    pub expr: Box<Expr>,

    pub type_ann: Box<TsType>,
}

#[ast_node("TsNonNullExpression")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsNonNullExpr {
    pub span: Span,

    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub enum Accessibility {
    Public,
    Protected,
    Private,
}

#[ast_node("TsConstAssertion")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct TsConstAssertion {
    pub span: Span,
    pub expr: Box<Expr>,
}
