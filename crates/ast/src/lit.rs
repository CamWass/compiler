use crate::{GetNodeId, NodeId, ProgramData};
use atoms::JsWord;
use bitflags::bitflags;
use clone_node::CloneNode;
use global_common::integer_decode::integer_decode;
use node_id::GetNodeIdMacro;
use num_bigint::BigUint;
use std::{
    fmt::{self, Display, Formatter, Write},
    hash::{Hash, Hasher},
};

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub enum Lit {
    Str(Str),

    Bool(Bool),

    Null(Null),

    Num(Number),

    BigInt(BigInt),

    Regex(Regex),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct BigInt {
    pub node_id: NodeId,
    pub value: BigUint,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Str {
    pub node_id: NodeId,

    pub value: JsWord,

    /// This includes line escape.
    pub has_escape: bool,
}

impl Str {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.value.is_empty()
    }
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Bool {
    pub node_id: NodeId,
    pub value: bool,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Null {
    pub node_id: NodeId,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, Eq, Hash)]
pub struct Regex {
    pub node_id: NodeId,

    pub exp: JsWord,

    pub flags: RegexFlags,
}

bitflags! {
    #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    pub struct RegexFlags: u8 {
        const D = 1 << 0;
        const G = 1 << 1;
        const I = 1 << 2;
        const M = 1 << 3;
        const S = 1 << 4;
        const U = 1 << 5;
        const V = 1 << 6;
        const Y = 1 << 7;
    }
}

impl Display for RegexFlags {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.contains(Self::D) {
            f.write_char('d')?
        }
        if self.contains(Self::G) {
            f.write_char('g')?
        }
        if self.contains(Self::I) {
            f.write_char('i')?
        }
        if self.contains(Self::M) {
            f.write_char('m')?
        }
        if self.contains(Self::S) {
            f.write_char('s')?
        }
        if self.contains(Self::U) {
            f.write_char('u')?
        }
        if self.contains(Self::V) {
            f.write_char('v')?
        }
        if self.contains(Self::Y) {
            f.write_char('y')?
        }
        Ok(())
    }
}

impl crate::CloneNode for RegexFlags {
    fn clone_node(&self, _: &mut ProgramData) -> Self {
        *self
    }
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode)]
pub struct Number {
    pub node_id: NodeId,
    /// **Note**: This should not be `NaN`. Use [crate::Ident] to represent NaN.
    ///
    /// If you store `NaN` in this field, a hash map will behave strangely.
    pub value: f64,
}

impl Eq for Number {}

#[allow(clippy::derived_hash_with_manual_eq)]
impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        integer_decode(self.value).hash(state);
    }
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
