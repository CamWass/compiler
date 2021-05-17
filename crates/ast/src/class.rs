use super::{
    expr::Expr,
    function::{Function, ParamOrTsParamProp},
    ident::PrivateName,
    prop::PropName,
    stmt::BlockStmt,
    typescript::{
        Accessibility, TsExprWithTypeArgs, TsIndexSignature, TsTypeAnn, TsTypeParamDecl,
        TsTypeParamInstantiation,
    },
    EmptyStmt,
};
use global_common::{ast_node, EqIgnoreSpan, Span};

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Class {
    pub span: Span,
    pub decorators: Vec<Decorator>,

    pub body: Vec<ClassMember>,

    pub super_class: Option<Box<Expr>>,

    pub is_abstract: bool,

    pub type_params: Option<TsTypeParamDecl>,

    pub super_type_params: Option<TsTypeParamInstantiation>,

    /// Typescript extension.
    pub implements: Vec<TsExprWithTypeArgs>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub enum ClassMember {
    Constructor(Constructor),
    /// `es2015`
    Method(ClassMethod),
    PrivateMethod(PrivateMethod),
    /// stage 0 / Typescript
    ClassProp(ClassProp),
    PrivateProp(PrivateProp),
    TsIndexSignature(TsIndexSignature),
    Empty(EmptyStmt),
}

#[ast_node("ClassProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ClassProp {
    pub span: Span,
    pub key: Box<Expr>,

    pub value: Option<Box<Expr>>,

    pub type_ann: Option<TsTypeAnn>,

    pub is_static: bool,

    pub decorators: Vec<Decorator>,

    pub computed: bool,

    /// Typescript extension.
    pub accessibility: Option<Accessibility>,

    /// Typescript extension.
    pub is_abstract: bool,

    pub is_optional: bool,

    pub is_override: bool,

    pub readonly: bool,

    pub declare: bool,

    pub definite: bool,
}

#[ast_node("PrivateProperty")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct PrivateProp {
    pub span: Span,
    pub key: PrivateName,

    pub value: Option<Box<Expr>>,

    pub type_ann: Option<TsTypeAnn>,

    pub is_static: bool,

    pub decorators: Vec<Decorator>,

    pub computed: bool,

    /// Typescript extension.
    pub accessibility: Option<Accessibility>,

    /// Typescript extension.
    pub is_abstract: bool,

    pub is_optional: bool,

    pub is_override: bool,

    pub readonly: bool,

    pub definite: bool,
}

#[ast_node("ClassMethod")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ClassMethod {
    pub span: Span,

    pub key: PropName,

    pub function: Function,

    pub kind: MethodKind,

    pub is_static: bool,

    /// Typescript extension.
    pub accessibility: Option<Accessibility>,

    /// Typescript extension.
    pub is_abstract: bool,

    pub is_optional: bool,

    pub is_override: bool,
}

#[ast_node("PrivateMethod")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct PrivateMethod {
    pub span: Span,

    pub key: PrivateName,

    pub function: Function,

    pub kind: MethodKind,

    pub is_static: bool,

    /// Typescript extension.
    pub accessibility: Option<Accessibility>,

    /// Typescript extension.
    pub is_abstract: bool,

    pub is_optional: bool,

    pub is_override: bool,
}

#[ast_node("Constructor")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Constructor {
    pub span: Span,
    pub key: PropName,

    pub params: Vec<ParamOrTsParamProp>,

    pub body: Option<BlockStmt>,

    pub accessibility: Option<Accessibility>,

    pub is_optional: bool,
}

#[ast_node("Decorator")]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Decorator {
    pub span: Span,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub enum MethodKind {
    Method,
    Getter,
    Setter,
}
