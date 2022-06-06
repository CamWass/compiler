use crate::{
    expr::Expr,
    function::{Function, ParamOrTsParamProp},
    ident::PrivateName,
    prop::PropName,
    stmt::BlockStmt,
    typescript::{
        Accessibility, TsExprWithTypeArgs, TsIndexSignature, TsTypeAnn, TsTypeParamInstantiation,
    },
    EmptyStmt, TsTypeParamDecl,
};
use global_common::{ast_node, EqIgnoreSpan, Span};
use serde::{Deserialize, Serialize};

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Class {
    pub span: Span,

    #[serde(default)]
    pub decorators: Vec<Decorator>,

    #[serde(default)]
    pub body: Vec<ClassMember>,

    #[serde(default)]
    pub is_abstract: bool,

    #[serde(default)]
    pub type_params: Option<Vec<TsTypeParamDecl>>,

    #[serde(default)]
    pub extends: Option<ExtendsClause>,

    /// Typescript extension.
    #[serde(default)]
    pub implements: Vec<TsExprWithTypeArgs>,
}

#[ast_node("ExtendsClause")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExtendsClause {
    pub span: Span,
    pub super_class: Box<Expr>,
    /// Typescript extension.
    #[serde(default)]
    pub super_type_params: Option<TsTypeParamInstantiation>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ClassMember {
    #[tag("Constructor")]
    Constructor(Constructor),
    /// `es2015`
    #[tag("ClassMethod")]
    Method(ClassMethod),
    #[tag("PrivateMethod")]
    PrivateMethod(PrivateMethod),
    /// stage 0 / Typescript
    #[tag("ClassProperty")]
    ClassProp(ClassProp),
    #[tag("PrivateProperty")]
    PrivateProp(PrivateProp),
    #[tag("TsIndexSignature")]
    TsIndexSignature(TsIndexSignature),
    #[tag("EmptyStatement")]
    Empty(EmptyStmt),
}

#[ast_node("ClassProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ClassProp {
    #[serde(default)]
    pub span: Span,

    pub key: PropName,

    #[serde(default)]
    pub value: Option<Box<Expr>>,

    #[serde(default, rename = "typeAnnotation")]
    pub type_ann: Option<TsTypeAnn>,

    #[serde(default)]
    pub is_static: bool,

    #[serde(default)]
    pub decorators: Vec<Decorator>,

    /// Typescript extension.
    #[serde(default)]
    pub accessibility: Option<Accessibility>,

    /// Typescript extension.
    #[serde(default)]
    pub is_abstract: bool,

    #[serde(default)]
    pub is_optional: bool,

    #[serde(default)]
    pub is_override: bool,

    #[serde(default)]
    pub readonly: bool,

    #[serde(default)]
    pub declare: bool,

    #[serde(default)]
    pub definite: bool,
}

#[ast_node("PrivateProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct PrivateProp {
    #[serde(default)]
    pub span: Span,

    pub key: PrivateName,

    #[serde(default)]
    pub value: Option<Box<Expr>>,

    #[serde(default, rename = "typeAnnotation")]
    pub type_ann: Option<TsTypeAnn>,

    #[serde(default)]
    pub is_static: bool,

    #[serde(default)]
    pub decorators: Vec<Decorator>,

    /// Typescript extension.
    #[serde(default)]
    pub accessibility: Option<Accessibility>,

    /// Typescript extension.
    #[serde(default)]
    pub is_abstract: bool,

    #[serde(default)]
    pub is_optional: bool,

    #[serde(default)]
    pub is_override: bool,

    #[serde(default)]
    pub readonly: bool,

    #[serde(default)]
    pub definite: bool,
}

macro_rules! method {
    ($name:ident, $ty:literal, $KEY:ty) => {
        #[ast_node($ty)]
        #[derive(Eq, Hash, EqIgnoreSpan)]
        #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
        pub struct $name {
            #[serde(default)]
            pub span: Span,

            pub key: $KEY,

            pub function: Function,

            pub kind: MethodKind,

            #[serde(default)]
            pub is_static: bool,

            /// Typescript extension.
            #[serde(default)]
            pub accessibility: Option<Accessibility>,

            /// Typescript extension.
            #[serde(default)]
            pub is_abstract: bool,

            #[serde(default)]
            pub is_optional: bool,

            #[serde(default)]
            pub is_override: bool,
        }
    };
}

method!(ClassMethod, "ClassMethod", PropName);
method!(PrivateMethod, "PrivateMethod", PrivateName);

#[ast_node("Constructor")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Constructor {
    pub span: Span,

    pub params: Vec<ParamOrTsParamProp>,

    #[serde(default)]
    pub body: Option<BlockStmt>,

    #[serde(default)]
    pub accessibility: Option<Accessibility>,

    #[serde(default)]
    pub is_optional: bool,
}

#[ast_node("Decorator")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Decorator {
    pub span: Span,

    #[serde(rename = "expression")]
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, EqIgnoreSpan)]
pub enum MethodKind {
    #[serde(rename = "method")]
    Method,
    #[serde(rename = "getter")]
    Getter,
    #[serde(rename = "setter")]
    Setter,
}
