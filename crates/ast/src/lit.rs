use crate::{GetNodeId, NodeId, ProgramData};
use atoms::JsWord;
use bitflags::bitflags;
use clone_node::CloneNode;
use node_eq::NodeEq;
use node_id::GetNodeIdMacro;
use num_bigint::BigUint;
use serde::Serialize;
use std::{
    fmt::{self, Display, Formatter, Write},
    hash::{Hash, Hasher},
};

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize, Eq, Hash)]
pub enum Lit {
    Str(Str),

    Bool(Bool),

    Null(Null),

    Num(Number),

    BigInt(BigInt),

    Regex(Regex),
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize, Eq, Hash)]
pub struct BigInt {
    pub node_id: NodeId,
    pub value: BigUint,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize, Eq, Hash)]
pub struct Str {
    pub node_id: NodeId,

    pub value: JsWord,
}

impl Str {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.value.is_empty()
    }
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize, Eq, Hash)]
pub struct Bool {
    pub node_id: NodeId,
    pub value: bool,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize, Eq, Hash)]
pub struct Null {
    pub node_id: NodeId,
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize, Eq, Hash)]
pub struct Regex {
    pub node_id: NodeId,

    pub exp: JsWord,

    pub flags: RegexFlags,
}

bitflags! {
    #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Serialize)]
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
            f.write_char('d')?;
        }
        if self.contains(Self::G) {
            f.write_char('g')?;
        }
        if self.contains(Self::I) {
            f.write_char('i')?;
        }
        if self.contains(Self::M) {
            f.write_char('m')?;
        }
        if self.contains(Self::S) {
            f.write_char('s')?;
        }
        if self.contains(Self::U) {
            f.write_char('u')?;
        }
        if self.contains(Self::V) {
            f.write_char('v')?;
        }
        if self.contains(Self::Y) {
            f.write_char('y')?;
        }
        Ok(())
    }
}

impl crate::CloneNode for RegexFlags {
    fn clone_node(&self, _: &mut ProgramData) -> Self {
        *self
    }
}

impl crate::NodeEq for RegexFlags {
    fn eq_ignoring_node_id(&self, other: &Self) -> bool {
        self == other
    }
}

#[derive(Debug, PartialEq, GetNodeIdMacro, CloneNode, NodeEq, Serialize)]
pub struct Number {
    pub node_id: NodeId,
    /// **Note**: This should not be `NaN`. Use [crate::Ident] to represent NaN.
    ///
    /// If you store `NaN` in this field, a hash map will behave strangely.
    pub value: f64,
}

impl Eq for Number {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct DecodedF64(u64, i16, i8);

// See: https://stackoverflow.com/a/39639200/
fn integer_decode(val: f64) -> DecodedF64 {
    let bits = val.to_bits();
    let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
    let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
    let mantissa = if exponent == 0 {
        (bits & 0xfffffffffffff) << 1
    } else {
        (bits & 0xfffffffffffff) | 0x10000000000000
    };

    exponent -= 1023 + 52;
    DecodedF64(mantissa, exponent, sign)
}

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
