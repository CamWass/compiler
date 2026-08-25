use crate::{GetNodeId, NodeId};
use clone_node::CloneNode;
use node_eq::NodeEq;
use node_id::GetNodeIdMacro;
use num_bigint::BigUint;
use serde::Serialize;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub enum Lit {
    Str(Str),

    Bool(Bool),

    Null(Null),

    Num(Number),

    BigInt(BigInt),

    Regex(Regex),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct BigInt {
    pub node_id: NodeId,
    pub value: Box<BigUint>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct Str {
    pub node_id: NodeId,

    pub value: Box<String>,
}

impl Str {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.value.is_empty()
    }
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct Bool {
    pub node_id: NodeId,
    pub value: bool,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct Null {
    pub node_id: NodeId,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct Regex {
    pub node_id: NodeId,
    pub raw: Box<String>,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct Number {
    pub node_id: NodeId,
    pub value: f64,
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.value.is_infinite() {
            if self.value.is_sign_positive() {
                Display::fmt("Infinity", f)
            } else {
                Display::fmt("-Infinity", f)
            }
        } else {
            Display::fmt(&self.value, f)
        }
    }
}
