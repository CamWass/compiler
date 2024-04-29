use crate::{
    expr::Expr,
    function::{Function, Param},
    ident::PrivateName,
    prop::PropName,
    stmt::BlockStmt,
    EmptyStmt, GetNodeId, NodeId,
};
use ast_node::ast_node;
use clone_node::CloneNode;

#[ast_node]
#[derive(Eq, Hash)]
pub struct Class {
    pub node_id: NodeId,

    pub decorators: Vec<Decorator>,

    pub extends: Option<ExtendsClause>,

    pub body: Vec<ClassMember>,
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct ExtendsClause {
    pub node_id: NodeId,
    pub super_class: Box<Expr>,
}

#[ast_node]
#[derive(Eq, Hash)]
pub enum ClassMember {
    Constructor(Constructor),
    /// `es2015`
    Method(ClassMethod),
    PrivateMethod(PrivateMethod),
    /// stage 0 / Typescript
    ClassProp(ClassProp),
    PrivateProp(PrivateProp),
    Empty(EmptyStmt),
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct ClassProp {
    pub node_id: NodeId,

    pub key: PropName,

    pub value: Option<Box<Expr>>,

    pub is_static: bool,

    pub decorators: Vec<Decorator>,
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct PrivateProp {
    pub node_id: NodeId,

    pub key: PrivateName,

    pub value: Option<Box<Expr>>,

    pub is_static: bool,

    pub decorators: Vec<Decorator>,
}

macro_rules! method {
    ($name:ident, $KEY:ty) => {
        #[ast_node]
        #[derive(Eq, Hash)]
        #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
        pub struct $name {
            pub node_id: NodeId,

            pub key: $KEY,

            pub function: Function,

            pub kind: MethodKind,

            pub is_static: bool,
        }
    };
}

method!(ClassMethod, PropName);
method!(PrivateMethod, PrivateName);

#[ast_node]
#[derive(Eq, Hash)]
pub struct Constructor {
    pub node_id: NodeId,

    pub params: Vec<Param>,

    pub body: BlockStmt,
}

#[ast_node]
#[derive(Eq, Hash)]
pub struct Decorator {
    pub node_id: NodeId,

    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, CloneNode)]
pub enum MethodKind {
    Method,
    Getter,
    Setter,
}
