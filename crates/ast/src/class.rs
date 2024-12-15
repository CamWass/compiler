use crate::{
    expr::Expr,
    function::{Function, Param},
    ident::PrivateName,
    prop::PropName,
    stmt::BlockStmt,
    GetNodeId, NodeId,
};
use clone_node::CloneNode;
use node_id::GetNodeIdMacro;

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Class {
    pub node_id: NodeId,

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
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ClassProp {
    pub node_id: NodeId,

    pub key: PropName,
    pub value: Option<Box<Expr>>,
    pub is_static: bool,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct PrivateProp {
    pub node_id: NodeId,

    pub key: PrivateName,
    pub value: Option<Box<Expr>>,
    pub is_static: bool,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ClassMethod {
    pub node_id: NodeId,

    pub key: PropName,
    pub function: Function,
    pub kind: MethodKind,
    pub is_static: bool,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct PrivateMethod {
    pub node_id: NodeId,

    pub key: PrivateName,
    pub function: Function,
    pub kind: MethodKind,
    pub is_static: bool,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Constructor {
    pub node_id: NodeId,

    pub params: Vec<Param>,
    pub body: BlockStmt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, CloneNode)]
pub enum MethodKind {
    Method,
    Getter,
    Setter,
}
