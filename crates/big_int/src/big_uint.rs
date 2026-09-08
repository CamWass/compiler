use std::cmp::Ordering;

use serde::Serialize;

// TODO: copy tests from num-bigint

#[derive(Debug, PartialEq, Eq, Serialize, Clone)]
pub struct BigUintValue {
    raw: String,
}

impl BigUintValue {
    pub fn zero() -> Self {
        Self {
            raw: String::from("0"),
        }
    }

    pub fn one() -> Self {
        Self {
            raw: String::from("1"),
        }
    }

    pub fn from_str_radix(string: &str, radix: u32) -> Self {
        match radix {
            10 => {
                debug_assert!(string.bytes().all(|b| b.is_ascii_digit() || b == b'_'));

                if string.as_bytes().contains(&b'_') {
                    let raw: String = string.chars().filter(|&c| c != '_').collect();
                    assert!(!raw.is_empty());
                    return Self { raw };
                } else {
                    return Self { raw: string.into() };
                }
            }
            2 => {
                debug_assert!(string.bytes().all(|b| matches!(b, b'0' | b'1' | b'_')));
            }
            16 => {
                debug_assert!(string.bytes().all(|b| b.is_ascii_hexdigit() || b == b'_'));
            }
            8 => {
                debug_assert!(string.bytes().all(|b| matches!(b, b'0'..=b'7' | b'_')));
            }
            _ => unreachable!(),
        }

        // Note: this is a naive algorithm, but we're trading performance for
        // code size/simplicity, under the assumption that BigInt literals are
        // fairly rare and short.

        let mut digits = vec![b'0'];

        for byte in string.bytes().filter(|b| *b != b'_') {
            let digit_val = match byte {
                b'0'..=b'9' => (byte - b'0') as u32,
                b'a'..=b'f' => (byte - b'a' + 10) as u32,
                b'A'..=b'F' => (byte - b'A' + 10) as u32,
                _ => 0,
            };

            let mut carry = digit_val;

            // Multiply existing base-10 value by radix and add incoming digit.
            for digit in digits.iter_mut() {
                let val = (*digit - b'0') as u32 * radix + carry;
                *digit = b'0' + (val % 10) as u8;
                carry = val / 10;
            }

            while carry > 0 {
                digits.push(b'0' + (carry % 10) as u8);
                carry /= 10;
            }
        }

        digits.reverse();
        Self {
            raw: String::from_utf8(digits).unwrap(),
        }
    }

    pub fn is_zero(&self) -> bool {
        self.raw == "0"
    }

    pub fn as_str(&self) -> &str {
        &self.raw
    }

    pub(crate) fn add_one(&mut self) {
        let mut bytes = std::mem::take(&mut self.raw).into_bytes();

        for i in (0..bytes.len()).rev() {
            if bytes[i] == b'9' {
                bytes[i] = b'0';
            } else {
                bytes[i] += 1;
                self.raw = String::from_utf8(bytes).unwrap();
                return;
            }
        }

        // All digits were '9' (e.g. "99" became "00").
        // Re-purpose the buffer: set first byte to '1' and push '0' -> "100".
        // This turns an O(N) element shift into an O(1) push.
        bytes[0] = b'1';
        bytes.push(b'0');
        self.raw = String::from_utf8(bytes).unwrap();
    }

    pub(crate) fn sub_one(&mut self) {
        assert!(!self.is_zero());

        let mut bytes = std::mem::take(&mut self.raw).into_bytes();

        for i in (0..bytes.len()).rev() {
            if bytes[i] == b'0' {
                bytes[i] = b'9';
            } else {
                bytes[i] -= 1;
                break;
            }
        }

        // Strip leading zeros caused by borrows (e.g., "100" -> "099" -> "99")
        let start = bytes
            .iter()
            .position(|&b| b != b'0')
            .unwrap_or(bytes.len() - 1);
        bytes.drain(..start);

        self.raw = String::from_utf8(bytes).unwrap();
    }

    pub fn from_f64(mut n: f64) -> Option<Self> {
        // handle NAN, INFINITY, NEG_INFINITY
        if !n.is_finite() {
            return None;
        }

        // match the rounding of casting from float to int
        n = n.trunc();

        // handle 0.x, -0.x
        if n == 0.0 {
            return Some(Self::zero());
        }

        let (mantissa, exponent, sign) = integer_decode_f64(n);

        if sign == -1 {
            return None;
        }

        if exponent >= 0 {
            // Exponent >= 0: Append `exponent` zeros to binary mantissa, then convert
            let mut bin = format!("{:b}", mantissa);
            bin.reserve(exponent as usize);
            for _ in 0..exponent {
                bin.push('0');
            }
            Some(Self::from_str_radix(&bin, 2))
        } else {
            // Exponent < 0: Because `n` was `trunc()`'d, the shift stays within u64 bounds
            let shift = (-exponent) as usize;
            if shift >= 64 {
                Some(Self::zero())
            } else {
                Some(Self::from_u64(mantissa >> shift))
            }
        }
    }

    pub fn from_u64(n: u64) -> Self {
        Self { raw: n.to_string() }
    }
}

fn integer_decode_f64(f: f64) -> (u64, i16, i8) {
    let bits: u64 = f.to_bits();
    let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
    let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
    let mantissa = if exponent == 0 {
        (bits & 0xfffffffffffff) << 1
    } else {
        (bits & 0xfffffffffffff) | 0x10000000000000
    };
    // Exponent bias + mantissa shift
    exponent -= 1023 + 52;
    (mantissa, exponent, sign)
}

impl Ord for BigUintValue {
    fn cmp(&self, other: &Self) -> Ordering {
        // 1. A string with more digits is numerically larger/
        // 2. If length is equal, lexicographical comparison matches numerical comparison/
        self.raw
            .len()
            .cmp(&other.raw.len())
            .then_with(|| self.raw.cmp(&other.raw))
    }
}

impl PartialOrd for BigUintValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
