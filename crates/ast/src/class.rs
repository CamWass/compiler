use crate::{
    expr::Expr,
    function::{Function, Param},
    ident::PrivateName,
    prop::PropName,
    stmt::BlockStmt,
    EmptyStmt, GetNodeId, NodeId,
};
use clone_node::CloneNode;
use node_id::GetNodeIdMacro;

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Class {
    pub node_id: NodeId,

    pub decorators: Vec<Decorator>,

    pub extends: Option<ExtendsClause>,

    pub body: Vec<ClassMember>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ExtendsClause {
    pub node_id: NodeId,
    pub super_class: Box<Expr>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
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

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ClassProp {
    pub node_id: NodeId,

    pub key: PropName,

    pub value: Option<Box<Expr>>,

    pub is_static: bool,

    pub decorators: Vec<Decorator>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct PrivateProp {
    pub node_id: NodeId,

    pub key: PrivateName,

    pub value: Option<Box<Expr>>,

    pub is_static: bool,

    pub decorators: Vec<Decorator>,
}

macro_rules! method {
    ($name:ident, $KEY:ty) => {
        #[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
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

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Constructor {
    pub node_id: NodeId,

    pub params: Vec<Param>,

    pub body: BlockStmt,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
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
