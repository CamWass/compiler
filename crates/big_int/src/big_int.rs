use std::{
    cmp::Ordering,
    ops::{Neg, Not},
};

use crate::BigUintValue;

/// A `Sign` is a [`BigInt`]'s composing element.
#[derive(PartialEq, PartialOrd, Eq, Ord, Copy, Clone, Debug, Hash)]
pub enum BigIntSign {
    /// The value of the [`BigInt`] is less than `0`.
    Minus,
    /// The value of the [`BigInt`] is equal to `0`.
    NoSign,
    /// The value of the [`BigInt`] is greater than `0`.
    Plus,
}

impl Neg for BigIntSign {
    type Output = BigIntSign;

    /// Negate `BigIntSign` value.
    #[inline]
    fn neg(self) -> BigIntSign {
        match self {
            Self::Minus => Self::Plus,
            Self::NoSign => Self::NoSign,
            Self::Plus => Self::Minus,
        }
    }
}

#[derive(Debug, Eq)]
pub struct BigIntValue {
    sign: BigIntSign,
    data: BigUintValue,
}

impl BigIntValue {
    pub fn zero() -> Self {
        Self {
            sign: BigIntSign::NoSign,
            data: BigUintValue::zero(),
        }
    }

    pub fn one() -> Self {
        Self {
            sign: BigIntSign::Plus,
            data: BigUintValue::one(),
        }
    }

    pub fn from_big_unint_and_sign(n: BigUintValue, sign: BigIntSign) -> Self {
        if n.is_zero() {
            Self::zero()
        } else {
            BigIntValue { sign, data: n }
        }
    }

    pub fn from_big_unint(n: BigUintValue) -> Self {
        if n.is_zero() {
            Self::zero()
        } else {
            BigIntValue {
                sign: BigIntSign::Plus,
                data: n,
            }
        }
    }

    pub fn is_zero(&self) -> bool {
        self.sign == BigIntSign::NoSign
    }

    pub fn from_f64(n: f64) -> Option<Self> {
        if n >= 0.0 {
            BigUintValue::from_f64(n).map(Self::from_big_unint)
        } else {
            let x = BigUintValue::from_f64(-n)?;
            Some(-Self::from_big_unint(x))
        }
    }

    pub fn from_u64(n: u64) -> Self {
        if n > 0 {
            Self {
                sign: BigIntSign::Plus,
                data: BigUintValue::from_u64(n),
            }
        } else {
            Self::zero()
        }
    }
}

impl Neg for BigIntValue {
    type Output = BigIntValue;

    #[inline]
    fn neg(mut self) -> BigIntValue {
        self.sign = -self.sign;
        self
    }
}

// !-2 = !...f fe = ...0 01 = +1
// !-1 = !...f ff = ...0 00 =  0
// ! 0 = !...0 00 = ...f ff = -1
// !+1 = !...0 01 = ...f fe = -2
impl Not for BigIntValue {
    type Output = BigIntValue;

    fn not(mut self) -> BigIntValue {
        match self.sign {
            BigIntSign::NoSign | BigIntSign::Plus => {
                self.data.add_one();
                self.sign = BigIntSign::Minus;
            }
            BigIntSign::Minus => {
                self.data.sub_one();
                self.sign = if self.data.is_zero() {
                    BigIntSign::NoSign
                } else {
                    BigIntSign::Plus
                };
            }
        }
        self
    }
}

impl PartialEq for BigIntValue {
    #[inline]
    fn eq(&self, other: &BigIntValue) -> bool {
        debug_assert!((self.sign != BigIntSign::NoSign) ^ self.data.is_zero());
        debug_assert!((other.sign != BigIntSign::NoSign) ^ other.data.is_zero());
        self.sign == other.sign && (self.sign == BigIntSign::NoSign || self.data == other.data)
    }
}

impl PartialOrd for BigIntValue {
    #[inline]
    fn partial_cmp(&self, other: &BigIntValue) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BigIntValue {
    #[inline]
    fn cmp(&self, other: &BigIntValue) -> Ordering {
        debug_assert!((self.sign != BigIntSign::NoSign) ^ self.data.is_zero());
        debug_assert!((other.sign != BigIntSign::NoSign) ^ other.data.is_zero());
        let scmp = self.sign.cmp(&other.sign);
        if scmp != Ordering::Equal {
            return scmp;
        }

        match self.sign {
            BigIntSign::NoSign => Ordering::Equal,
            BigIntSign::Plus => self.data.cmp(&other.data),
            BigIntSign::Minus => other.data.cmp(&self.data),
        }
    }
}
