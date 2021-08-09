use crate::{
    expr::Expr,
    function::{Function, ParamOrTsParamProp},
    ident::PrivateName,
    node::NodeId,
    prop::PropName,
    stmt::BlockStmt,
    typescript::{
        Accessibility, TsExprWithTypeArgs, TsIndexSignature, TsTypeAnn, TsTypeParamDecl,
        TsTypeParamInstantiation,
    },
    EmptyStmt,
};
use ast_node2::ast_node;
use global_common::{EqIgnoreSpan, Span};

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Class<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub decorators: Vec<&'ast Decorator<'ast>>,
    pub body: Vec<ClassMember<'ast>>,
    pub super_class: Option<Expr<'ast>>,
    pub is_abstract: bool,
    pub type_params: Option<&'ast TsTypeParamDecl<'ast>>,
    pub super_type_params: Option<&'ast TsTypeParamInstantiation<'ast>>,
    /// Typescript extension.
    pub implements: Vec<&'ast TsExprWithTypeArgs<'ast>>,
}

#[ast_node]
#[derive(Eq, Hash, Copy, EqIgnoreSpan)]
pub enum ClassMember<'ast> {
    Constructor(&'ast Constructor<'ast>),
    /// `es2015`
    Method(&'ast ClassMethod<'ast>),
    PrivateMethod(&'ast PrivateMethod<'ast>),
    /// stage 0 / Typescript
    ClassProp(&'ast ClassProp<'ast>),
    PrivateProp(&'ast PrivateProp<'ast>),
    TsIndexSignature(&'ast TsIndexSignature<'ast>),
    Empty(&'ast EmptyStmt),
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ClassProp<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub key: Expr<'ast>,
    pub value: Option<Expr<'ast>>,
    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,
    pub is_static: bool,
    pub decorators: Vec<&'ast Decorator<'ast>>,
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

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct PrivateProp<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub key: &'ast PrivateName<'ast>,
    pub value: Option<Expr<'ast>>,
    pub type_ann: Option<&'ast TsTypeAnn<'ast>>,
    pub is_static: bool,
    pub decorators: Vec<&'ast Decorator<'ast>>,
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

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct ClassMethod<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub key: PropName<'ast>,
    pub function: &'ast Function<'ast>,
    pub kind: MethodKind,
    pub is_static: bool,
    /// Typescript extension.
    pub accessibility: Option<Accessibility>,
    /// Typescript extension.
    pub is_abstract: bool,
    pub is_optional: bool,
    pub is_override: bool,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct PrivateMethod<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub key: &'ast PrivateName<'ast>,
    pub function: &'ast Function<'ast>,
    pub kind: MethodKind,
    pub is_static: bool,
    /// Typescript extension.
    pub accessibility: Option<Accessibility>,
    /// Typescript extension.
    pub is_abstract: bool,
    pub is_optional: bool,
    pub is_override: bool,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Constructor<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub key: PropName<'ast>,
    pub params: Vec<ParamOrTsParamProp<'ast>>,
    pub body: Option<&'ast BlockStmt<'ast>>,
    pub accessibility: Option<Accessibility>,
    pub is_optional: bool,
}

#[ast_node]
#[derive(Eq, Hash, EqIgnoreSpan)]
pub struct Decorator<'ast> {
    pub node_id: NodeId,
    pub span: Span,
    pub expr: Expr<'ast>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EqIgnoreSpan)]
pub enum MethodKind {
    Method,
    Getter,
    Setter,
}
