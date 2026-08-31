use common::chars::{is_js_line_break, is_js_whitespace};
use num_bigint::BigInt;
use num_traits::Num;

/// Converts a string to a number, according to the ECMAScript spec.
///
/// See spec:
/// - [7.1.4 ToNumber][https://tc39.es/ecma262/#sec-tonumber]
/// - [7.1.4.1 ToNumber Applied to the String Type][https://tc39.es/ecma262/#sec-tonumber-applied-to-the-string-type]
pub fn ecma_string_to_number(string: &str) -> f64 {
    let trimmed = string.trim_matches(|c| is_js_line_break(c) || is_js_whitespace(c));

    if trimmed.is_empty() {
        return 0.0;
    }

    match trimmed {
        "Infinity" | "+Infinity" => return f64::INFINITY,
        "-Infinity" => return f64::NEG_INFINITY,
        _ => {}
    }

    if matches!(trimmed.as_bytes(), [b'0', b'x' | b'X', ..,]) {
        let digits = &trimmed[2..];
        return parse_int_from_string(digits, 16);
    }

    if matches!(trimmed.as_bytes(), [b'0', b'o' | b'O', ..]) {
        let digits = &trimmed[2..];
        return parse_int_from_string(digits, 8);
    }

    if matches!(trimmed.as_bytes(), [b'0', b'b' | b'B', ..]) {
        let digits = &trimmed[2..];
        return parse_int_from_string(digits, 2);
    }

    // Rust's string->f64 below will accept e.g. "inf", so we need to check if
    // the string starts with a numeric char (digit or dot), or a sign (+/-)
    // followed by a numeric char.
    let looks_like_decimal = matches!(
        trimmed.as_bytes(),
        [b'0'..=b'9' | b'.', ..] | [b'+' | b'-', b'0'..=b'9' | b'.', ..]
    );

    if looks_like_decimal {
        trimmed.parse::<f64>().unwrap_or(f64::NAN)
    } else {
        f64::NAN
    }
}

fn parse_int_from_string(digits: &str, radix: u32) -> f64 {
    if digits.is_empty() {
        return f64::NAN;
    }

    // Fast, exact path: fits in u128 -> single correctly-rounded cast to f64.
    // Covers up to ~38-39 significant digits, well past the spec's 20-digit
    // exactness requirement, so this is always exact where exactness matters.
    if let Ok(n) = u128::from_str_radix(digits, radix) {
        return n as f64;
    }

    // Only reached for literals long enough to overflow u128 - already in
    // RoundMVResult's "implementation-defined" territory, so an approximate
    // (if imperfectly-rounded) accumulation is spec-conformant here.
    let mut acc = 0f64;
    for c in digits.chars() {
        match c.to_digit(radix) {
            Some(d) => acc = acc * radix as f64 + d as f64,
            None => return f64::NAN,
        }
    }
    acc
}

/// Converts a number to a decimal string according to the ECMAScript spec.
///
/// See spec:
/// - [7.1.17 ToString][https://tc39.es/ecma262/#sec-tostring]
/// - [6.1.6.1.20 Number::toString][https://tc39.es/ecma262/#sec-numeric-types-number-tostring]
pub fn ecma_number_to_string(x: f64) -> String {
    // 1. If x is NaN, return "NaN".
    if x.is_nan() {
        return "NaN".to_string();
    }

    // 2. If x is +0 or -0, return "0".
    if x == 0.0 {
        return "0".to_string();
    }

    // 3. If x < 0, return "-" + ToString(-x).
    if x < 0.0 {
        return format!("-{}", ecma_number_to_string(-x));
    }

    // 4. If x is +Infinity, return "Infinity".
    if x.is_infinite() {
        return "Infinity".to_string();
    }

    // x is finite, positive, and non-zero from here on.

    // 5. Let n, k, and s be integers such that k ≥ 1,
    //    10^(k-1) ≤ s < 10^k, s * 10^(n-k) = x, and k is as small
    //    as possible.
    //
    // `{:e}` (no explicit precision) computes the shortest round-trip decimal
    // digits in the form:
    //     x = d.ddd...eEXP [1]
    // where the mantissa has k digits (one before the decimal point and k-1
    // after it). The spec uses:
    //     x = s * 10^(n-k) [2]
    // where s is the k digits of our mantissa, so:
    //     s = d.ddd...*10^(k-1) [3]
    // (k-1 to move the decimal point to the right past the k-1 digits).
    // Substituting [3] into [2] gives:
    //     x = d.ddd...*10^(k-1) * 10^(n-k) = d.ddd...*10^(n-1) [4]
    // Finally, comparing [1] and [4], we have:
    //     x = `d.ddd...eEXP` = d.ddd...*10^(n-1)
    // so EXP = n-1 and n = EXP+1
    let sci = format!("{:e}", x);
    let (mantissa, exp_str) = sci.split_once('e').unwrap();
    let exp: i64 = exp_str.parse().unwrap();

    let s: String = mantissa.chars().filter(|&c| c != '.').collect();
    let k = s.len() as i64;
    let n = exp + 1;

    // 6. If radix ≠ 10 or n is in the inclusive interval from -5 to 21, then:
    if -5 <= n && n <= 21 {
        // 6a. If n ≥ k, return s followed by n - k occurrences of "0".
        if n >= k {
            return format!("{s}{}", "0".repeat((n - k) as usize));
        }

        // 6b. If n > 0, return the first n digits of s, followed by ".",
        //     followed by the remaining k - n digits of s.
        if 0 < n {
            let (int_part, frac_part) = s.split_at(n as usize);
            return format!("{int_part}.{frac_part}");
        }

        // 6c. Assert n <= 0.
        // 6d. Return "0.", followed by -n occurrences of "0", followed by s.
        return format!("0.{}{s}", "0".repeat((-n) as usize));
    }

    // 7. NOTE: In this case, the input will be represented using scientific E
    // notation, such as 1.2e+3.
    // 8. Assert radix is 10.

    let exponent_sign = if n < 0 {
        // 9a. If n < 0, then let exponentSign be "-".
        "-"
    } else {
        // 10a. Else, let exponentSign be "+".
        "+"
    };

    // 11. If k = 1, then:
    if k == 1 {
        // 11a. Return the digit s, followed by "e", followed by exponentSign,
        // followed by abs(n-1).
        return format!("{s}e{exponent_sign}{}", (n - 1).abs());
    }

    // 12. Return the most significant digit of s, followed by ".", followed by
    // the remaining k-1 digits of s, followed by "e", followed by,
    // exponentSign, followed by abs(n-1).
    let (first, rest) = s.split_at(1);
    format!("{first}.{rest}e{exponent_sign}{}", (n - 1).abs())
}

// TODO: tests, doc comment
/// https://tc39.es/ecma262/multipage/numbers-and-dates.html#sec-bigint-constructor-number-value
/// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-tobigint
/// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-stringtobigint
pub fn ecma_string_to_big_int(string: &str) -> Option<BigInt> {
    let trimmed = string.trim_matches(|c| is_js_line_break(c) || is_js_whitespace(c));

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

#[cfg(test)]
mod tests {
    use common::chars::char_literals;

    use super::*;

    fn white_space_string() -> String {
        [
            char_literals::LINE_FEED,
            char_literals::CARRIAGE_RETURN,
            char_literals::LINE_SEPARATOR,
            char_literals::PARAGRAPH_SEPARATOR,
            char_literals::CHARACTER_TABULATION,
            char_literals::LINE_TABULATION,
            char_literals::FORM_FEED,
            char_literals::SPACE,
            char_literals::NON_BREAKING_SPACE,
            char_literals::OGHAM_SPACE_MARK,
            char_literals::EN_QUAD,
            char_literals::EM_QUAD,
            char_literals::EN_SPACE,
            char_literals::EM_SPACE,
            char_literals::THREE_PER_EM_SPACE,
            char_literals::FOUR_PER_EM_SPACE,
            char_literals::SIX_PER_EM_SPACE,
            char_literals::FIGURE_SPACE,
            char_literals::PUNCTUATION_SPACE,
            char_literals::THIN_SPACE,
            char_literals::HAIR_SPACE,
            char_literals::NARROW_NO_BREAK_SPACE,
            char_literals::MEDIUM_MATHEMATICAL_SPACE,
            char_literals::IDEOGRAPHIC_SPACE,
            char_literals::ZERO_WIDTH_NO_BREAK_SPACE,
        ]
        .into_iter()
        .collect()
    }

    #[test]
    fn string_to_big_int() {
        assert_eq!(
            ecma_string_to_big_int(&format!(
                "{}12345{}",
                white_space_string(),
                white_space_string()
            )),
            Some(BigInt::from(12345))
        );
    }

    #[test]
    fn number_to_string() {
        assert_eq!(ecma_number_to_string(123_f64), "123");
        assert_eq!(ecma_number_to_string(-123_f64), "-123");

        assert_eq!(ecma_number_to_string(f64::from(0xF00D)), "61453");

        assert_eq!(ecma_number_to_string(f64::NAN), "NaN");

        assert_eq!(ecma_number_to_string(f64::INFINITY), "Infinity");
        assert_eq!(ecma_number_to_string(f64::NEG_INFINITY), "-Infinity");

        assert_eq!(ecma_number_to_string(0.0), "0");
        assert_eq!(ecma_number_to_string(f64::from(i32::MIN)), "-2147483648");
        assert_eq!(ecma_number_to_string(f64::from(i32::MAX)), "2147483647");
        // ES section 7.1.12.1 #sec-tostring-applied-to-the-number-type:
        // -0.0 is stringified to "0".
        assert_eq!(ecma_number_to_string(-0.0), "0");
        assert_eq!(ecma_number_to_string(1.1), "1.1");
        assert_eq!(ecma_number_to_string(0.1), "0.1");

        assert_eq!(ecma_number_to_string(100.0), "100");
        assert_eq!(ecma_number_to_string(123.456), "123.456");
        assert_eq!(ecma_number_to_string(0.0001), "0.0001");
        assert_eq!(ecma_number_to_string(1e-6), "0.000001");
        assert_eq!(ecma_number_to_string(1e-7), "1e-7");
        assert_eq!(ecma_number_to_string(1e21), "1e+21");
        assert_eq!(ecma_number_to_string(-42.5), "-42.5");
        assert_eq!(ecma_number_to_string(5.0), "5");

        assert_eq!(ecma_number_to_string(1.234e30), "1.234e+30");
        assert_eq!(ecma_number_to_string(1.234e-10), "1.234e-10");

        // n=21
        assert_eq!(ecma_number_to_string(1e20), "100000000000000000000");
        // n=21
        assert_eq!(
            ecma_number_to_string(123456789012345680000.0),
            "123456789012345680000"
        );
        // n=22
        assert_eq!(
            ecma_number_to_string(1234567890123456800000.0),
            "1.2345678901234568e+21"
        );

        // Negative output in exponential form.
        assert_eq!(ecma_number_to_string(-1e21), "-1e+21");
        assert_eq!(ecma_number_to_string(-1e-7), "-1e-7");

        // Sanity check.
        assert_eq!(ecma_number_to_string(0.1 + 0.2), "0.30000000000000004");
        assert_eq!(ecma_number_to_string(1.0 / 3.0), "0.3333333333333333");

        assert_eq!(ecma_number_to_string(f64::MAX), "1.7976931348623157e+308");
        assert_eq!(
            ecma_number_to_string(f64::MIN_POSITIVE),
            "2.2250738585072014e-308"
        );
        // Smallest positive subnormal.
        assert_eq!(ecma_number_to_string(5e-324), "5e-324");

        // 2^53 - 1
        assert_eq!(
            ecma_number_to_string(9007199254740991.0),
            "9007199254740991"
        );
        // 2^53
        assert_eq!(
            ecma_number_to_string(9007199254740992.0),
            "9007199254740992"
        );
    }

    #[test]
    fn string_to_number() {
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

        assert_eq!(
            ecma_string_to_number(&format!(
                "{}12345{}",
                white_space_string(),
                white_space_string()
            )),
            12345_f64
        );

        assert_eq!(ecma_string_to_number("00000001"), 1.0);

        assert_eq!(ecma_string_to_number(""), 0.0);
        assert_eq!(ecma_string_to_number(&white_space_string()), 0.0);
        assert!(ecma_string_to_number("inf").is_nan());
        assert!(ecma_string_to_number("-inf").is_nan());
        assert!(ecma_string_to_number("+inf").is_nan());

        // Sign not allowed on hex.
        assert!(ecma_string_to_number("+0x1F").is_nan());
        assert_eq!(ecma_string_to_number(".5"), 0.5);
        assert_eq!(ecma_string_to_number("-.5"), -0.5);
        assert_eq!(ecma_string_to_number("+.5"), 0.5);
        assert_eq!(ecma_string_to_number("5."), 5.0);
        assert!(ecma_string_to_number("5e").is_nan());
        // Separators not allowed.
        assert!(ecma_string_to_number("1_000").is_nan());

        assert_eq!(ecma_string_to_number("1"), 1.0);
        assert_eq!(ecma_string_to_number("-1"), -1.0);
        assert_eq!(ecma_string_to_number("  -1  "), -1.0);
        assert_eq!(ecma_string_to_number("  +1  "), 1.0);
        assert!(ecma_string_to_number("  -  1  ").is_nan());
        assert!(ecma_string_to_number("  +  1  ").is_nan());

        assert_eq!(ecma_string_to_number("1e1"), 1e1);
        assert_eq!(ecma_string_to_number("1e+1"), 1e1);
        assert_eq!(ecma_string_to_number("1e-1"), 1e-1);
        assert_eq!(ecma_string_to_number("1e+100"), 1e100);
        assert_eq!(ecma_string_to_number("1e-100"), 1e-100);
        assert_eq!(ecma_string_to_number(".000001e-100"), 1e-106);

        assert_eq!(ecma_string_to_number("0e01"), 0.0);
    }

    #[test]
    fn string_to_number_hex() {
        assert_eq!(ecma_string_to_number("0x0"), 0.0);
        assert_eq!(ecma_string_to_number("0X0"), 0.0);
        assert_eq!(ecma_string_to_number("0x1"), 1.0);
        assert_eq!(ecma_string_to_number("0x10"), 16.0);
        assert_eq!(ecma_string_to_number("0xFF"), 255.0);
        assert_eq!(ecma_string_to_number("0xAF"), 175.0);

        assert_eq!(ecma_string_to_number("0x0"), 0.0);
        assert_eq!(ecma_string_to_number("0X0"), 0.0);
        assert_eq!(ecma_string_to_number("0x1"), 1.0);
        assert_eq!(ecma_string_to_number("0x10"), 16.0);
        assert_eq!(ecma_string_to_number("0xFF"), 255.0);
        assert_eq!(ecma_string_to_number("0xAF"), 175.0);
    }

    #[test]
    fn string_to_number_octal() {
        assert_eq!(ecma_string_to_number("0o0"), 0.0);
        assert_eq!(ecma_string_to_number("0O0"), 0.0);
        assert_eq!(ecma_string_to_number("0o1"), 1.0);
        assert_eq!(ecma_string_to_number("0o7"), 7.0);
        assert_eq!(ecma_string_to_number("0o10"), 8.0);
        assert_eq!(ecma_string_to_number("0o77"), 63.0);

        assert_eq!(ecma_string_to_number("0o0"), 0.0);
        assert_eq!(ecma_string_to_number("0O0"), 0.0);
        assert_eq!(ecma_string_to_number("0o1"), 1.0);
        assert_eq!(ecma_string_to_number("0o7"), 7.0);
        assert_eq!(ecma_string_to_number("0o10"), 8.0);
        assert_eq!(ecma_string_to_number("0o77"), 63.0);
    }

    #[test]
    fn string_to_number_binary() {
        assert_eq!(ecma_string_to_number("0b0"), 0.0);
        assert_eq!(ecma_string_to_number("0B0"), 0.0);
        assert_eq!(ecma_string_to_number("0b1"), 1.0);
        assert_eq!(ecma_string_to_number("0b10"), 2.0);
        assert_eq!(ecma_string_to_number("0b11"), 3.0);

        assert_eq!(ecma_string_to_number("0b0"), 0.0);
        assert_eq!(ecma_string_to_number("0B0"), 0.0);
        assert_eq!(ecma_string_to_number("0b1"), 1.0);
        assert_eq!(ecma_string_to_number("0b10"), 2.0);
        assert_eq!(ecma_string_to_number("0b11"), 3.0);
    }

    #[test]
    fn test_parse_num_zero() {
        assert_eq!(ecma_string_to_number("0.0"), 0.0);
        assert_eq!(ecma_string_to_number("0"), 0.0);
        assert_eq!(ecma_string_to_number("00"), 0.0);
        assert_eq!(ecma_string_to_number("000"), 0.0);

        assert_eq!(ecma_string_to_number("0e0"), 0.0);
        assert_eq!(ecma_string_to_number("0e1"), 0.0);
        assert_eq!(ecma_string_to_number("0e-1"), 0.0);
        assert_eq!(ecma_string_to_number("0e-100000"), 0.0);
        assert_eq!(ecma_string_to_number("0e+100000"), 0.0);
        assert_eq!(ecma_string_to_number("0."), 0.0);

        assert_eq!(ecma_string_to_number("-0"), -0.0);
    }

    #[test]
    fn test_parse_num_long_number_str() {
        assert_eq!(
            ecma_string_to_number(
                "1\
                0000000000"
            ),
            1e10
        );
        assert_eq!(
            ecma_string_to_number(
                "1\
                0000000000\
                0000000000"
            ),
            1e20
        );

        assert_eq!(
            ecma_string_to_number(
                "1\
                0000000000\
                0000000000\
                0000000000\
                0000000000\
                0000000000\
                0000000000"
            ),
            1e60
        );

        assert_eq!(
            ecma_string_to_number(
                ".\
                0\
                1"
            ),
            1e-2
        );
        assert_eq!(
            ecma_string_to_number(
                ".\
                0000000000\
                1"
            ),
            1e-11
        );
        assert_eq!(
            ecma_string_to_number(
                ".\
                0000000000\
                0000000000\
                1"
            ),
            1e-21
        );

        assert_eq!(
            ecma_string_to_number(
                ".\
                0000000000\
                0000000000\
                0000000000\
                0000000000\
                0000000000\
                0000000000\
                1"
            ),
            1e-61
        );

        // x = 24414062505131248.0 and y = 24414062505131252.0 are representable in
        // double. Check chat z = (x + y) / 2 is rounded to x...
        assert_eq!(
            ecma_string_to_number("24414062505131250.0"),
            24414062505131248.0
        );

        // ... and z = (x + y) / 2 + delta is rounded to y.
        assert_eq!(
            ecma_string_to_number("24414062505131250.000000001"),
            24414062505131252.0
        );
    }

    #[test]
    fn string_to_number_maximum_significant_digits() {
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

        assert_eq!(ecma_string_to_number(&num), 4.4501477170144017780491e-308);

        unsafe {
            let idx = num.len() - 8;
            num.as_bytes_mut()[idx] = b'1';
        }

        assert_eq!(ecma_string_to_number(&num), 4.4501477170144022721148e-308);
    }

    #[test]
    fn string_to_number_minimum_exponent() {
        // Same as `string_to_number_maximum_significant_digits` but with
        // different point-position.
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

        assert_eq!(ecma_string_to_number(&num), 4.4501477170144017780491e-308);

        unsafe {
            let idx = num.len() - 8;
            num.as_bytes_mut()[idx] = b'1';
        }

        assert_eq!(ecma_string_to_number(&num), 4.4501477170144022721148e-308);
    }

    #[test]
    fn string_to_number_maximum_exponent() {
        assert_eq!(
            ecma_string_to_number("0.16e309"),
            1.59999999999999997765e+308
        );
    }
}
