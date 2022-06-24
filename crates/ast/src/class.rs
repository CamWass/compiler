use crate::{
    expr::Expr,
    function::{Function, ParamOrTsParamProp},
    ident::PrivateName,
    prop::PropName,
    stmt::BlockStmt,
    typescript::{
        Accessibility, TsExprWithTypeArgs, TsIndexSignature, TsTypeAnn, TsTypeParamInstantiation,
    },
    EmptyStmt, GetNodeId, NodeId, TsTypeParamDecl,
};
use ast_node::ast_node;
use global_common::{EqIgnoreSpan, Span};

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Class {
    pub node_id: NodeId,

    pub span: Span,

    pub decorators: Vec<Decorator>,

    pub body: Vec<ClassMember>,

    pub is_abstract: bool,

    pub type_params: Option<Vec<TsTypeParamDecl>>,

    pub extends: Option<ExtendsClause>,

    /// Typescript extension.
    pub implements: Vec<TsExprWithTypeArgs>,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ExtendsClause {
    pub node_id: NodeId,

    pub span: Span,
    pub super_class: Box<Expr>,
    /// Typescript extension.
    pub super_type_params: Option<TsTypeParamInstantiation>,
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

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ClassProp {
    pub node_id: NodeId,

    pub span: Span,

    pub key: PropName,

    pub value: Option<Box<Expr>>,

    pub type_ann: Option<TsTypeAnn>,

    pub is_static: bool,

    pub decorators: Vec<Decorator>,

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

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct PrivateProp {
    pub node_id: NodeId,

    pub span: Span,

    pub key: PrivateName,

    pub value: Option<Box<Expr>>,

    pub type_ann: Option<TsTypeAnn>,

    pub is_static: bool,

    pub decorators: Vec<Decorator>,

    /// Typescript extension.
    pub accessibility: Option<Accessibility>,

    /// Typescript extension.
    pub is_abstract: bool,

    pub is_optional: bool,

    pub is_override: bool,

    pub readonly: bool,

    pub definite: bool,
}

macro_rules! method {
    ($name:ident, $KEY:ty) => {
        #[ast_node]
        #[derive(Eq, Hash, EqIgnoreSpan)]
        #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
        pub struct $name {
            pub node_id: NodeId,

            pub span: Span,

            pub key: $KEY,

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
    };
}

method!(ClassMethod, PropName);
method!(PrivateMethod, PrivateName);

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Constructor {
    pub node_id: NodeId,

    pub span: Span,

    pub params: Vec<ParamOrTsParamProp>,

    pub body: Option<BlockStmt>,

    pub accessibility: Option<Accessibility>,

    pub is_optional: bool,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Decorator {
    pub node_id: NodeId,

    pub span: Span,

    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub enum MethodKind {
    Method,
    Getter,
    Setter,
}
