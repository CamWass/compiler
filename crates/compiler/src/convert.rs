use bitflags::bitflags;
use common::chars::char_literals;
use lexical::NumberFormatBuilder;
use num_bigint::BigInt;
use num_traits::Num;

// TODO: clean up, no unsafe

/// Converts a string to a number, according to the ECMAScript spec.
///
/// See spec:
/// - [7.1.4 ToNumber][https://tc39.es/ecma262/#sec-tonumber]
/// - [7.1.4.1 ToNumber Applied to the String Type][https://tc39.es/ecma262/#sec-tonumber-applied-to-the-string-type]
/// - [7.1.4.1.1 StringToNumber ][https://tc39.es/ecma262/#sec-stringtonumber]
/// - [7.1.4.1.2 Runtime Semantics: StringNumericValue][https://tc39.es/ecma262/#sec-runtime-semantics-stringnumericvalue]
///
/// Based on V8's implementation.
// Ported from (the bottom of): https://github.com/v8/v8/blob/62130792d1e439b4ccb4df6942ef17c26c1ca5bb/src/objects/string.cc#L690
pub fn ecma_string_to_number(string: &str) -> f64 {
    let flags =
        ConversionFlags::ALLOW_HEX | ConversionFlags::ALLOW_OCTAL | ConversionFlags::ALLOW_BINARY;
    parse_num(string, flags, 0.0)
}

/// Converts a number to a string, according to the ECMAScript spec.
///
/// See spec:
/// - [7.1.17 ToString][https://tc39.es/ecma262/#sec-tostring]
/// - [6.1.6.1.20 Number::toString][https://tc39.es/ecma262/#sec-numeric-types-number-tostring]
pub fn ecma_number_to_string(v: f64) -> String {
    const FORMAT: u128 = NumberFormatBuilder::new()
        .required_exponent_sign(true)
        .build();
    let options = lexical::WriteFloatOptions::builder()
        .trim_floats(true)
        .inf_string(Some(b"Infinity"))
        .build()
        .unwrap();
    lexical::to_string_with_options::<_, FORMAT>(v, &options)
}

// TODO: tests, doc comment
/// https://tc39.es/ecma262/multipage/numbers-and-dates.html#sec-bigint-constructor-number-value
/// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-tobigint
/// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-stringtobigint
pub fn ecma_string_to_big_int(string: &str) -> Option<BigInt> {
    let trimmed = string.trim_matches(|c| is_line_break(c) || is_whitespace(c));

    if trimmed.is_empty() {
        return Some(BigInt::ZERO);
    }

    // The String -> BigInt parsing used by the JS BigInt constructor is much
    // more strict than num_bigint::BigInt's parsing (e.g. numerical separators
    // aren't allowed), so we validate the string before passing it to
    // BigInt::from_str_radix.

    if matches!(trimmed.as_bytes().first(), Some(b'-' | b'+')) {
        let digits = &trimmed[1..];
        if digits.as_bytes().iter().all(|b| b.is_ascii_digit()) {
            return BigInt::from_str_radix(trimmed, 10).ok();
        }

        return None;
    }

    if matches!(trimmed.as_bytes(), [b'0', b'x' | b'X', ..]) {
        let digits = &trimmed[2..];
        if digits.as_bytes().iter().all(|b| b.is_ascii_hexdigit()) {
            return BigInt::from_str_radix(digits, 16).ok();
        }

        return None;
    }

    if matches!(trimmed.as_bytes(), [b'0', b'o' | b'O', ..]) {
        let digits = &trimmed[2..];
        if digits.as_bytes().iter().all(|b| matches!(b, b'0'..=b'7')) {
            return BigInt::from_str_radix(digits, 8).ok();
        }

        return None;
    }

    if matches!(trimmed.as_bytes(), [b'0', b'b' | b'B', ..]) {
        let digits = &trimmed[2..];
        if digits.as_bytes().iter().all(|b| matches!(b, b'0' | b'1')) {
            return BigInt::from_str_radix(digits, 2).ok();
        }

        return None;
    }

    if trimmed.as_bytes().iter().all(|b| b.is_ascii_digit()) {
        return BigInt::from_str_radix(trimmed, 10).ok();
    }

    None
}

// TODO: tests, doc comment
/// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-toint32
/// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-tointegerorinfinity
/// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-tofixedsizeinteger
pub fn ecma_number_to_int_32(x: f64) -> i32 {
    if !x.is_finite() {
        return 0; // Steps 7.1.5 (2-4) & 7.1.6 (1)
    }

    // 7.1.5 Step 5 (truncate) + 7.1.6 Step 2 (modulo 2^32)
    let fixed_int = x.trunc().rem_euclid(4_294_967_296.0);

    // 7.1.6 Step 4 (Two's complement signed conversion)
    (fixed_int as u32) as i32
}

// ------------------------------------------------------------------------------------
// The following is based on the lexer:
// ------------------------------------------------------------------------------------

/// See https://tc39.github.io/ecma262/#sec-line-terminators
fn is_line_break(ch: char) -> bool {
    matches!(
        ch,
        char_literals::LINE_FEED
            | char_literals::CARRIAGE_RETURN
            | char_literals::LINE_SEPARATOR
            | char_literals::PARAGRAPH_SEPARATOR
    )
}

// https://tc39.github.io/ecma262/#sec-white-space
fn is_whitespace(ch: char) -> bool {
    matches!(
        ch,
        char_literals::CHARACTER_TABULATION
            | char_literals::LINE_TABULATION
            | char_literals::FORM_FEED
            | char_literals::SPACE
            | char_literals::NON_BREAKING_SPACE
            | char_literals::OGHAM_SPACE_MARK
            | char_literals::EN_QUAD
            | char_literals::EM_QUAD
            | char_literals::EN_SPACE
            | char_literals::EM_SPACE
            | char_literals::THREE_PER_EM_SPACE
            | char_literals::FOUR_PER_EM_SPACE
            | char_literals::SIX_PER_EM_SPACE
            | char_literals::FIGURE_SPACE
            | char_literals::PUNCTUATION_SPACE
            | char_literals::THIN_SPACE
            | char_literals::HAIR_SPACE
            | char_literals::NARROW_NO_BREAK_SPACE
            | char_literals::MEDIUM_MATHEMATICAL_SPACE
            | char_literals::IDEOGRAPHIC_SPACE
            | char_literals::ZERO_WIDTH_NO_BREAK_SPACE
    )
}

#[derive(Clone)]
struct Input<'src> {
    cur: usize,
    bytes: &'src [u8],
}

impl<'src> Input<'src> {
    /// Returns the remaining portion of the input as a str.
    #[inline(always)]
    fn as_str(&self) -> &str {
        debug_assert!(unsafe {
            std::str::from_utf8(&self.bytes.get_unchecked(self.cur..)).is_ok()
        });

        // Safety: We know this is safe because we require the input to be valid utf8 and
        // cur always points to a character boundary.
        unsafe { std::str::from_utf8_unchecked(&self.bytes.get_unchecked(self.cur..)) }
    }

    /// Gets the current char in the input.
    fn cur(&self) -> Option<char> {
        self.as_str().chars().next()
    }

    fn cur_byte(&self) -> Option<u8> {
        self.bytes.get(self.cur).copied()
    }

    fn advance(&mut self, amount: usize) {
        self.cur += amount;
    }

    fn bump(&mut self) {
        let char = self.cur().unwrap();

        self.cur += char.len_utf8();
    }

    fn is(&self, c: u8) -> bool {
        match self.bytes.get(self.cur).copied() {
            Some(ch) if ch == c => true,
            _ => false,
        }
    }

    fn eat(&mut self, c: u8) -> bool {
        if self.is(c) {
            self.cur += 1;
            true
        } else {
            false
        }
    }

    /// Returns true if a nonspace character has been found and false if the
    /// end was been reached before finding a nonspace character.
    // Ported from: https://github.com/v8/v8/blob/b25addd/src/numbers/conversions.cc#L172
    fn advance_to_non_space(&mut self) -> bool {
        while let Some(ch) = self.cur() {
            if !is_whitespace(ch) && !is_line_break(ch) {
                return true;
            }
            self.bump();
        }
        false
    }

    fn finished(&self) -> bool {
        self.cur == self.bytes.len()
    }

    fn is_cur_ascii_digit(&self) -> bool {
        matches!(self.cur_byte(), Some(b'0'..=b'9'))
    }

    /// Eats as many bytes as it can and returns whether all test bytes were eaten.
    fn eat_bytes(&mut self, test_bytes: &[u8]) -> bool {
        debug_assert!(test_bytes.len() > 0);
        for &test_byte in test_bytes {
            if self.finished() || self.cur_byte() != Some(test_byte) {
                return false;
            }
            self.advance(1);
        }
        true
    }
}

// ------------------------------------------------------------------------------------
// The rest is ported from V8.
// ------------------------------------------------------------------------------------

// Ported from: https://github.com/v8/v8/blob/b25adddb903f6ff7707091065ff74aa51c002f8b/src/numbers/conversions.h#L81
bitflags! {
    /// Options for allowing octals and ignoring junk when converting
    /// strings to numbers.
    pub struct ConversionFlags: u8 {
        const NO_CONVERSION_FLAGS  = 0;
        const ALLOW_HEX            = 1 << 1;
        const ALLOW_OCTAL          = 1 << 2;
        const ALLOW_IMPLICIT_OCTAL = 1 << 3;
        const ALLOW_BINARY         = 1 << 4;
        const ALLOW_TRAILING_JUNK  = 1 << 5;
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum Sign {
    None,
    Negative,
    Positive,
}

// Ported from: https://github.com/v8/v8/blob/b25adddb903f6ff7707091065ff74aa51c002f8b/src/numbers/conversions.cc#L636
fn signed_zero(sign: Sign) -> f64 {
    if sign == Sign::Negative { -0.0 } else { 0.0 }
}

// Ported from: https://github.com/v8/v8/blob/b25adddb903f6ff7707091065ff74aa51c002f8b/src/numbers/conversions.cc#L636
fn is_digit(x: u8, radix: u8) -> bool {
    assert!(matches!(radix, 2 | 8 | 10 | 16));
    (x >= b'0' && x <= b'9' && x < b'0' + radix)
        || (radix > 10 && x >= b'a' && x < b'a' + radix - 10)
        || (radix > 10 && x >= b'A' && x < b'A' + radix - 10)
}

fn parse_int_from_string(string: &str, radix: u8) -> f64 {
    assert!(matches!(radix, 2 | 8 | 10 | 16));
    i64::from_str_radix(string, radix as u32)
        .map(|num| num as f64)
        .unwrap_or(f64::NAN)
}

// TODO: can use lexical flags such as NumberFormatBuilder::new().required_exponent_digits to make `parse`
// closure more resiliant to trailing junk, preventing special handling involving input.cur.
// See the JAVASCRIPT_LITERAL/JAVASCRIPT_STRING presets

/// Parses number from a string according to the ECMAScript spec.
///
/// This implementation is based on V8's `InternalStringToDouble` method, which V8 uses for many
/// things (e.g. parsing / lexing / runtime conversion). We only use it to emulate javascript's
/// string-to-number conversion.
// TODO: link to relevant spec section?
// Ported from: https://github.com/v8/v8/blob/b25adddb903f6ff7707091065ff74aa51c002f8b/src/numbers/conversions.cc#L636
fn parse_num(string: &str, flags: ConversionFlags, empty_string_val: f64) -> f64 {
    let mut input = Input {
        cur: 0,
        bytes: string.as_bytes(),
    };

    if !input.advance_to_non_space() {
        return empty_string_val;
    }

    let allow_trailing_junk = flags.intersects(ConversionFlags::ALLOW_TRAILING_JUNK);

    let mut sign = Sign::None;

    if input.eat(b'+') {
        // Ignore leading sign.
        if input.finished() {
            return f64::NAN;
        }
        sign = Sign::Positive;
    } else if input.eat(b'-') {
        if input.finished() {
            return f64::NAN;
        }
        sign = Sign::Negative;
    }

    const INFINITY: &'static str = "Infinity";
    if input.is(INFINITY.as_bytes()[0]) {
        if !input.eat_bytes(INFINITY.as_bytes()) {
            return f64::NAN;
        }

        if !allow_trailing_junk && input.advance_to_non_space() {
            return f64::NAN;
        }

        return if sign == Sign::Negative {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        };
    }

    let start = input.cur;

    let parse = |octal, end, input: Input| {
        let slice = &input.bytes[start..end];

        let radix = if octal { 8 } else { 10 };
        const RADIX_FORMAT: u128 = lexical::NumberFormatBuilder::new().radix(8).build();
        let options = lexical::ParseFloatOptions::from_radix(radix);

        let res = if octal {
            lexical::parse_with_options::<f64, _, RADIX_FORMAT>(slice, &options).unwrap_or(f64::NAN)
        } else {
            lexical::parse::<f64, _>(slice).unwrap_or(f64::NAN)
        };

        if sign == Sign::Negative { -res } else { res }
    };

    let mut leading_zero = false;
    if input.eat(b'0') {
        if input.finished() {
            return signed_zero(sign);
        }

        leading_zero = true;

        // It could be hexadecimal value.
        if flags.intersects(ConversionFlags::ALLOW_HEX) && (input.eat(b'x') || input.eat(b'X')) {
            if input.finished() || !is_digit(input.cur_byte().unwrap(), 16) || sign != Sign::None {
                return f64::NAN; // "0x".
            }

            // TODO: allow_trailing_junk
            return parse_int_from_string(input.as_str(), 16);

            // It could be an explicit octal value.
        } else if flags.intersects(ConversionFlags::ALLOW_OCTAL)
            && (input.eat(b'o') || input.eat(b'O'))
        {
            if input.finished() || !is_digit(input.cur_byte().unwrap(), 8) || sign != Sign::None {
                return f64::NAN; // "0o".
            }

            // TODO: allow_trailing_junk
            return parse_int_from_string(input.as_str(), 8);

            // It could be a binary value.
        } else if flags.intersects(ConversionFlags::ALLOW_BINARY)
            && (input.eat(b'b') || input.eat(b'B'))
        {
            if input.finished() || !is_digit(input.cur_byte().unwrap(), 2) || sign != Sign::None {
                return f64::NAN; // "0b".
            }

            // TODO: allow_trailing_junk
            return parse_int_from_string(input.as_str(), 2);
        }

        // Ignore leading zeros in the integer part.
        while input.eat(b'0') {
            if input.finished() {
                return signed_zero(sign);
            }
        }
    }

    let mut octal = leading_zero && flags.intersects(ConversionFlags::ALLOW_IMPLICIT_OCTAL);

    let mut has_significant_digits = false;

    // Copy significant digits of the integer part (if any) to the buffer.
    while input.is_cur_ascii_digit() {
        has_significant_digits = true;
        octal = octal && input.cur_byte().unwrap() < b'8';
        input.advance(1);
        if input.finished() {
            return parse(octal, input.cur, input);
        }
    }

    if !has_significant_digits {
        octal = false;
    }

    if input.eat(b'.') {
        if octal && !allow_trailing_junk {
            return f64::NAN;
        }
        if octal {
            // TODO: input.cur might not be the correct end point
            return parse(octal, input.cur, input);
        }

        if input.finished() {
            if !has_significant_digits && !leading_zero {
                return f64::NAN;
            } else {
                // TODO: input.cur might not be the correct end point
                return parse(octal, input.cur, input);
            }
        }

        if !has_significant_digits {
            // octal = false;
            // Integer part consists of 0 or is absent. Significant digits start after
            // leading zeros (if any).
            while input.eat(b'0') {
                if input.finished() {
                    return signed_zero(sign);
                }
            }
        }

        // There is a fractional part.  We don't emit a '.', but adjust the exponent
        // instead.
        while input.is_cur_ascii_digit() {
            has_significant_digits = true;
            input.advance(1);
            if input.finished() {
                return parse(octal, input.cur, input);
            }
        }
    }

    if !leading_zero && !has_significant_digits {
        // If leading_zeros is true then the string contains zeros.
        // If exponent < 0 then string was [+-]\.0*...
        // If significant_digits != 0 the string is not equal to 0.
        // Otherwise there are no digits in the string.
        return f64::NAN;
    }

    let exponent_start = input.cur;

    // Parse exponential part.
    if input.eat(b'e') || input.eat(b'E') {
        if octal {
            return f64::NAN;
        }
        if input.finished() {
            if allow_trailing_junk {
                // -1 so we don't include e/E (which `parse` can't handle)
                return parse(octal, input.cur - 1, input);
            } else {
                return f64::NAN;
            }
        }
        if input.eat(b'+') || input.eat(b'-') {
            if input.finished() {
                if allow_trailing_junk {
                    // -2 so we don't include e/E and +/- (which `parse` can't handle)
                    return parse(octal, input.cur - 2, input);
                } else {
                    return f64::NAN;
                }
            }
        }
        if !input.is_cur_ascii_digit() {
            if allow_trailing_junk {
                // -3 so we don't include e/E, +/-, and the non-ascii char (which `parse` can't handle)
                return parse(octal, input.cur - 3, input);
            } else {
                return f64::NAN;
            }
        }
        let mut empty_exponent = true;

        while let Some(b'0'..=b'9') = input.cur_byte() {
            if input.cur_byte().unwrap() != b'0' {
                empty_exponent = false;
            }
            input.advance(1);
        }

        if empty_exponent {
            return parse(octal, exponent_start, input);
        }
    }

    // This is the end of any valid input, before any trailing jank.
    let end = input.cur;

    if !allow_trailing_junk && input.advance_to_non_space() {
        return f64::NAN;
    }

    // We use `end` instead of `input.cur` since `parse` can't handle trailing junk.
    parse(octal, end, input)
}

/// Ported from: https://github.com/v8/v8/blob/b25adddb903f6ff7707091065ff74aa51c002f8b/test/cctest/test-conversions.cc
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn convert_string_to_number() {
        assert_eq!(ecma_string_to_number("123"), 123_f64);
        assert_eq!(ecma_string_to_number("-123"), -123_f64);
        assert_eq!(ecma_string_to_number("+123"), 123_f64);

        assert_eq!(ecma_string_to_number("0xF00D"), 0xF00D as f64);
        assert_eq!(ecma_string_to_number("61453"), 0xF00D as f64);

        assert!(ecma_string_to_number("NaN").is_nan());
        assert!(ecma_string_to_number("-NaN").is_nan());
        assert!(ecma_string_to_number("+NaN").is_nan());

        assert!(ecma_string_to_number("abc").is_nan());
        assert!(ecma_string_to_number("12ffff").is_nan());
        assert!(ecma_string_to_number("123n").is_nan());

        assert_eq!(ecma_string_to_number("Infinity"), f64::INFINITY);
        assert_eq!(ecma_string_to_number("+Infinity"), f64::INFINITY);
        assert_eq!(ecma_string_to_number("-Infinity"), f64::NEG_INFINITY);

        assert_eq!(ecma_string_to_number("  123 \n\r\t"), 123_f64);
        assert_eq!(ecma_string_to_number("  000123 \n\r\t"), 123_f64);
        assert_eq!(ecma_string_to_number("000123"), 123_f64);

        assert!(ecma_string_to_number("123_111").is_nan());
    }

    #[test]
    fn convert_number_to_string() {
        assert_eq!("123", ecma_number_to_string(123_f64));
        assert_eq!("-123", ecma_number_to_string(-123_f64));

        assert_eq!("61453", ecma_number_to_string(f64::from(0xF00D)));

        assert_eq!("NaN", ecma_number_to_string(f64::NAN));

        assert_eq!("Infinity", ecma_number_to_string(f64::INFINITY));
        assert_eq!("-Infinity", ecma_number_to_string(f64::NEG_INFINITY));

        assert_eq!(ecma_number_to_string(0.0), "0");
        assert_eq!(ecma_number_to_string(f64::from(i32::MIN)), "-2147483648");
        assert_eq!(ecma_number_to_string(f64::from(i32::MAX)), "2147483647");
        // ES section 7.1.12.1 #sec-tostring-applied-to-the-number-type:
        // -0.0 is stringified to "0".
        assert_eq!(ecma_number_to_string(-0.0), "0");
        assert_eq!(ecma_number_to_string(1.1), "1.1");
        assert_eq!(ecma_number_to_string(0.1), "0.1");
    }

    macro_rules! eq {
        ($num:expr, $str:expr, $( $flag:ident $(|)? )*) => {
            assert_eq!($num, parse_num($str, $( ConversionFlags::$flag | )* ConversionFlags::empty(), 0.0));
        };
    }

    #[test]
    fn test_parse_num_hex() {
        eq!(0.0, "0x0", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(0.0, "0X0", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(1.0, "0x1", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(16.0, "0x10", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(255.0, "0xFF", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(175.0, "0xAF", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);

        eq!(0.0, "0x0", ALLOW_HEX);
        eq!(0.0, "0X0", ALLOW_HEX);
        eq!(1.0, "0x1", ALLOW_HEX);
        eq!(16.0, "0x10", ALLOW_HEX);
        eq!(255.0, "0xFF", ALLOW_HEX);
        eq!(175.0, "0xAF", ALLOW_HEX);
    }

    #[test]
    fn test_parse_num_octal() {
        eq!(0.0, "0o0", ALLOW_OCTAL | ALLOW_IMPLICIT_OCTAL);
        eq!(0.0, "0O0", ALLOW_OCTAL | ALLOW_IMPLICIT_OCTAL);
        eq!(1.0, "0o1", ALLOW_OCTAL | ALLOW_IMPLICIT_OCTAL);
        eq!(7.0, "0o7", ALLOW_OCTAL | ALLOW_IMPLICIT_OCTAL);
        eq!(8.0, "0o10", ALLOW_OCTAL | ALLOW_IMPLICIT_OCTAL);
        eq!(63.0, "0o77", ALLOW_OCTAL | ALLOW_IMPLICIT_OCTAL);

        eq!(0.0, "0o0", ALLOW_OCTAL);
        eq!(0.0, "0O0", ALLOW_OCTAL);
        eq!(1.0, "0o1", ALLOW_OCTAL);
        eq!(7.0, "0o7", ALLOW_OCTAL);
        eq!(8.0, "0o10", ALLOW_OCTAL);
        eq!(63.0, "0o77", ALLOW_OCTAL);
    }

    #[test]
    fn test_parse_num_implicit_octal() {
        eq!(0.0, "0", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(0.0, "00", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(1.0, "01", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(7.0, "07", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(8.0, "010", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(63.0, "077", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);

        eq!(0.0, "0", ALLOW_HEX);
        eq!(0.0, "00", ALLOW_HEX);
        eq!(1.0, "01", ALLOW_HEX);
        eq!(7.0, "07", ALLOW_HEX);
        eq!(10.0, "010", ALLOW_HEX);
        eq!(77.0, "077", ALLOW_HEX);

        let x = 1073741824_f64; // Power of 2, no rounding errors.
        eq!(
            x * x * x * x * x,
            "01\
            0000000000\
            0000000000\
            0000000000\
            0000000000\
            0000000000",
            ALLOW_IMPLICIT_OCTAL
        );
    }

    #[test]
    fn test_parse_num_binary() {
        eq!(0.0, "0b0", ALLOW_BINARY | ALLOW_IMPLICIT_OCTAL);
        eq!(0.0, "0B0", ALLOW_BINARY | ALLOW_IMPLICIT_OCTAL);
        eq!(1.0, "0b1", ALLOW_BINARY | ALLOW_IMPLICIT_OCTAL);
        eq!(2.0, "0b10", ALLOW_BINARY | ALLOW_IMPLICIT_OCTAL);
        eq!(3.0, "0b11", ALLOW_BINARY | ALLOW_IMPLICIT_OCTAL);

        eq!(0.0, "0b0", ALLOW_BINARY);
        eq!(0.0, "0B0", ALLOW_BINARY);
        eq!(1.0, "0b1", ALLOW_BINARY);
        eq!(2.0, "0b10", ALLOW_BINARY);
        eq!(3.0, "0b11", ALLOW_BINARY);
    }

    #[test]
    fn test_parse_num_malformed_octal() {
        eq!(8.0, "08", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(81.0, "081", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(78.0, "078", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);

        assert!(
            parse_num(
                "07.7",
                ConversionFlags::ALLOW_HEX | ConversionFlags::ALLOW_IMPLICIT_OCTAL,
                0.0
            )
            .is_nan()
        );
        assert!(
            parse_num(
                "07.8",
                ConversionFlags::ALLOW_HEX | ConversionFlags::ALLOW_IMPLICIT_OCTAL,
                0.0
            )
            .is_nan()
        );
        assert!(
            parse_num(
                "07e8",
                ConversionFlags::ALLOW_HEX | ConversionFlags::ALLOW_IMPLICIT_OCTAL,
                0.0
            )
            .is_nan()
        );
        assert!(
            parse_num(
                "07e7",
                ConversionFlags::ALLOW_HEX | ConversionFlags::ALLOW_IMPLICIT_OCTAL,
                0.0
            )
            .is_nan()
        );

        eq!(8.7, "08.7", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(8e7, "08e7", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);

        eq!(0.001, "0.001", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(0.713, "0.713", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);

        eq!(8.0, "08", ALLOW_HEX);
        eq!(81.0, "081", ALLOW_HEX);
        eq!(78.0, "078", ALLOW_HEX);

        eq!(7.7, "07.7", ALLOW_HEX);
        eq!(7.8, "07.8", ALLOW_HEX);
        eq!(7e8, "07e8", ALLOW_HEX);
        eq!(7e7, "07e7", ALLOW_HEX);

        eq!(8.7, "08.7", ALLOW_HEX);
        eq!(8e7, "08e7", ALLOW_HEX);

        eq!(0.001, "0.001", ALLOW_HEX);
        eq!(0.713, "0.713", ALLOW_HEX);
    }

    #[test]
    fn test_parse_num_trailing_junk() {
        eq!(8.0, "8q", ALLOW_TRAILING_JUNK);
        eq!(63.0, "077qqq", ALLOW_IMPLICIT_OCTAL | ALLOW_TRAILING_JUNK);
        eq!(10.0, "10e", ALLOW_IMPLICIT_OCTAL | ALLOW_TRAILING_JUNK);
        eq!(10.0, "10e-", ALLOW_IMPLICIT_OCTAL | ALLOW_TRAILING_JUNK);
    }

    #[test]
    fn test_parse_num_non_str_decimal_literal() {
        assert!(parse_num(" ", ConversionFlags::NO_CONVERSION_FLAGS, f64::NAN).is_nan());
        assert!(parse_num("", ConversionFlags::NO_CONVERSION_FLAGS, f64::NAN).is_nan());
        eq!(0.0, "", NO_CONVERSION_FLAGS);
        eq!(0.0, " ", NO_CONVERSION_FLAGS);
    }

    #[test]
    fn test_parse_num_integer_str_literal() {
        eq!(0.0, "0.0", NO_CONVERSION_FLAGS);
        eq!(0.0, "0", NO_CONVERSION_FLAGS);
        eq!(0.0, "00", NO_CONVERSION_FLAGS);
        eq!(0.0, "000", NO_CONVERSION_FLAGS);
        eq!(1.0, "1", NO_CONVERSION_FLAGS);
        eq!(-1.0, "-1", NO_CONVERSION_FLAGS);
        eq!(-1.0, "  -1  ", NO_CONVERSION_FLAGS);
        eq!(1.0, "  +1  ", NO_CONVERSION_FLAGS);
        assert!(parse_num("  -  1  ", ConversionFlags::NO_CONVERSION_FLAGS, 0.0).is_nan());
        assert!(parse_num("  +  1  ", ConversionFlags::NO_CONVERSION_FLAGS, 0.0).is_nan());

        eq!(0.0, "0e0", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(0.0, "0e1", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(0.0, "0e-1", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(0.0, "0e-100000", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(0.0, "0e+100000", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
        eq!(0.0, "0.", ALLOW_HEX | ALLOW_IMPLICIT_OCTAL);
    }

    #[test]
    fn test_parse_num_long_number_str() {
        eq!(
            1e10,
            "1\
            0000000000",
            NO_CONVERSION_FLAGS
        );
        eq!(
            1e20,
            "1\
            0000000000\
            0000000000",
            NO_CONVERSION_FLAGS
        );

        eq!(
            1e60,
            "1\
            0000000000\
            0000000000\
            0000000000\
            0000000000\
            0000000000\
            0000000000",
            NO_CONVERSION_FLAGS
        );

        eq!(
            1e-2,
            ".\
            0\
            1",
            NO_CONVERSION_FLAGS
        );
        eq!(
            1e-11,
            ".\
            0000000000\
            1",
            NO_CONVERSION_FLAGS
        );
        eq!(
            1e-21,
            ".\
            0000000000\
            0000000000\
            1",
            NO_CONVERSION_FLAGS
        );

        eq!(
            1e-61,
            ".\
            0000000000\
            0000000000\
            0000000000\
            0000000000\
            0000000000\
            0000000000\
            1",
            NO_CONVERSION_FLAGS
        );

        // x = 24414062505131248.0 and y = 24414062505131252.0 are representable in
        // double. Check chat z = (x + y) / 2 is rounded to x...
        eq!(
            24414062505131248.0,
            "24414062505131250.0",
            NO_CONVERSION_FLAGS
        );

        // ... and z = (x + y) / 2 + delta is rounded to y.
        eq!(
            24414062505131252.0,
            "24414062505131250.000000001",
            NO_CONVERSION_FLAGS
        );
    }

    #[test]
    fn test_parse_num_maximum_significant_digits() {
        let mut num = String::from(
            "4.4501477170144020250819966727949918635852426585926051135169509\
            122872622312493126406953054127118942431783801370080830523154578\
            251545303238277269592368457430440993619708911874715081505094180\
            604803751173783204118519353387964161152051487413083163272520124\
            606023105869053620631175265621765214646643181420505164043632222\
            668006474326056011713528291579642227455489682133472873831754840\
            341397809846934151055619529382191981473003234105366170879223151\
            087335413188049110555339027884856781219017754500629806224571029\
            581637117459456877330110324211689177656713705497387108207822477\
            584250967061891687062782163335299376138075114200886249979505279\
            101870966346394401564490729731565935244123171539810221213221201\
            847003580761626016356864581135848683152156368691976240370422601\
            6998291015625000000000000000000000000000000000e-308",
        );

        eq!(4.4501477170144017780491e-308, &num, NO_CONVERSION_FLAGS);

        unsafe {
            let idx = num.len() - 8;
            num.as_bytes_mut()[idx] = b'1';
        }

        eq!(4.4501477170144022721148e-308, &num, NO_CONVERSION_FLAGS);
    }

    #[test]
    fn test_parse_num_minimum_exponent() {
        // Same as `maximum_significant_digits` but with different point-position.
        let mut num = String::from(
            "445014771701440202508199667279499186358524265859260511351695091\
            228726223124931264069530541271189424317838013700808305231545782\
            515453032382772695923684574304409936197089118747150815050941806\
            048037511737832041185193533879641611520514874130831632725201246\
            060231058690536206311752656217652146466431814205051640436322226\
            680064743260560117135282915796422274554896821334728738317548403\
            413978098469341510556195293821919814730032341053661708792231510\
            873354131880491105553390278848567812190177545006298062245710295\
            816371174594568773301103242116891776567137054973871082078224775\
            842509670618916870627821633352993761380751142008862499795052791\
            018709663463944015644907297315659352441231715398102212132212018\
            470035807616260163568645811358486831521563686919762403704226016\
            998291015625000000000000000000000000000000000e-1108",
        );

        eq!(4.4501477170144017780491e-308, &num, NO_CONVERSION_FLAGS);

        unsafe {
            let idx = num.len() - 8;
            num.as_bytes_mut()[idx] = b'1';
        }

        eq!(4.4501477170144022721148e-308, &num, NO_CONVERSION_FLAGS);
    }

    #[test]
    fn test_parse_num_maximum_exponent() {
        eq!(1.59999999999999997765e+308, "0.16e309", NO_CONVERSION_FLAGS);
    }

    #[test]
    fn test_parse_num_exponent_number_str() {
        eq!(1e1, "1e1", NO_CONVERSION_FLAGS);
        eq!(1e1, "1e+1", NO_CONVERSION_FLAGS);
        eq!(1e-1, "1e-1", NO_CONVERSION_FLAGS);
        eq!(1e100, "1e+100", NO_CONVERSION_FLAGS);
        eq!(1e-100, "1e-100", NO_CONVERSION_FLAGS);
        eq!(1e-106, ".000001e-100", NO_CONVERSION_FLAGS);
    }

    #[test]
    fn test_parse_num_invalid_exponent() {
        eq!(0.0, "0e01", NO_CONVERSION_FLAGS);
    }
}
