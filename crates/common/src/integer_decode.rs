#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DecodedF64(u64, i16, i8);

// See: https://stackoverflow.com/a/39639200/
pub fn integer_decode(val: f64) -> DecodedF64 {
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
