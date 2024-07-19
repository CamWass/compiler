use crate::{
    expr::Expr,
    ident::{BindingIdent, Ident},
    prop::PropName,
    GetNodeId, Invalid, NodeId,
};
use clone_node::CloneNode;
use global_common::util::take::Take;
use node_id::GetNodeIdMacro;

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum Pat {
    Ident(BindingIdent),

    Array(ArrayPat),

    Rest(RestPat),

    Object(ObjectPat),

    Assign(AssignPat),

    Invalid(Invalid),

    /// Only for for-in / for-of loops. This is *syntactically* valid.
    Expr(Box<Expr>),
}

impl Take for Pat {
    fn dummy() -> Self {
        Pat::Invalid(Invalid {
            node_id: NodeId::DUMMY,
        })
    }
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ArrayPat {
    pub node_id: NodeId,

    pub elems: Vec<Option<Pat>>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct ObjectPat {
    pub node_id: NodeId,

    pub props: Vec<ObjectPatProp>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct AssignPat {
    pub node_id: NodeId,

    pub left: Box<Pat>,

    pub right: Box<Expr>,
}

/// EsTree `RestElement`
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct RestPat {
    pub node_id: NodeId,

    pub arg: Box<Pat>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum ObjectPatProp {
    KeyValue(KeyValuePatProp),

    Assign(AssignPatProp),

    Rest(RestPat),
}

impl Take for ObjectPatProp {
    fn dummy() -> Self {
        ObjectPatProp::Assign(AssignPatProp {
            node_id: NodeId::DUMMY,
            key: Ident::dummy(),
            value: None,
        })
    }
}

/// `{key: value}`
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct KeyValuePatProp {
    pub node_id: NodeId,
    pub key: PropName,
    pub value: Box<Pat>,
}
/// `{key}` or `{key = value}`
#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct AssignPatProp {
    pub node_id: NodeId,
    pub key: Ident,

    pub value: Option<Box<Expr>>,
}
