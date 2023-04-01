mod parse_num;

use lexical::NumberFormatBuilder;
use parse_num::{parse_num, ConversionFlags};

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

/// The intent of numeric names is that:
/// - they are names with text in a numeric form, and that
/// - setting properties/indexing with them is always equivalent to doing so with the numeric literal `numLit`,
///   acquired by applying the abstract `ToNumber` operation on the name's text.
///
/// The subtlety is in the latter portion, as we cannot reliably say that anything that looks like a numeric literal is a numeric name.
/// In fact, it is the case that the text of the name must be equal to `ToString(numLit)` for this to hold.
///
/// Consider the property name `"0xF00D"`. When one indexes with `0xF00D`, they are actually indexing with the value of `ToString(0xF00D)`
/// according to the ECMAScript specification, so it is actually as if the user indexed with the string `"61453"`.
/// Thus, the text of all numeric literals equivalent to `61543` such as `0xF00D`, `0xf00D`, `0170015`, etc. are not valid numeric names
/// because their `ToString` representation is not equal to their original text.
/// This is motivated by ECMA-262 5.1 sections:
/// - [9.3.1 ToNumber Applied to the String Type][https://262.ecma-international.org/5.1/#sec-9.3.1]
/// - [9.8.1 ToString Applied to the Number Type][https://262.ecma-international.org/5.1/#sec-9.8.1]
/// - [11.1.5 Object Initialiser][https://262.ecma-international.org/5.1/#sec-11.1.5]
/// - [11.2.1 Property Accessors][https://262.ecma-international.org/5.1/#sec-11.2.1]
///
/// Here, we test whether `ToString(ToNumber(name))` is exactly equal to `name`.
///
/// Note that this accepts the values `Infinity`, `-Infinity`, and `NaN`, and that this is intentional.
/// This is desired behavior, because when indexing with them as numeric entities, you are indexing
/// with the strings `"Infinity"`, `"-Infinity"`, and `"NaN"` respectively.
///
/// This implementation is ported from V8's `IsSpecialIndex` method, which includes various fast paths to avoid computing
/// `ToString(ToNumber(name))` where possible.
// https://github.com/v8/v8/blob/b25adddb903f6ff7707091065ff74aa51c002f8b/src/numbers/conversions.cc#L1434
pub fn is_numeric_literal_name(string: &str) -> bool {
    // Max length of canonical double: -X.XXXXXXXXXXXXXXXXX-eXXX
    let buffer_size = 24;
    let length = string.len();
    if length == 0 || length > buffer_size {
        return false;
    };
    let bytes = string.as_bytes();
    // If the first char is not a digit or a '-' or we can't match 'NaN' or
    // '(-)Infinity', bailout immediately.
    let mut offset = 0;
    if !bytes[0].is_ascii_digit() {
        if bytes[0] == b'-' {
            if length == 1 {
                return false; // Just '-' is bad.
            }
            if !bytes[1].is_ascii_digit() {
                if bytes[1] == b'I' && length == 9 {
                    // Allow matching of '-Infinity' below.
                } else {
                    return false;
                }
            }
            offset += 1;
        } else if bytes[0] == b'I' && length == 8 {
            // Allow matching of 'Infinity' below.
        } else if bytes[0] == b'N' && length == 3 {
            // Match NaN.
            return bytes[1] == b'a' && bytes[2] == b'N';
        } else {
            return false;
        }
    }
    // Expected fast path: key is an integer.
    let representable_integer_length = 15; // (-)XXXXXXXXXXXXXXX
    if length - offset <= representable_integer_length {
        let initial_offset = offset;
        let mut matches = true;
        while offset < length {
            matches &= bytes[offset].is_ascii_digit();
            offset += 1;
        }
        if matches {
            // Match 0 and -0.
            if bytes[initial_offset] == b'0' {
                return initial_offset == length - 1;
            }
            return true;
        }
    }
    // Slow path: test `ToString(ToNumber(string)) == string`.

    // Note: we don't use any conversion flags here since we're only interested in well formed decimal numbers.
    // Decimal numbers are the canonical representaion, so we always return false for non-decimal numbers,
    // saving us from doing expensive number formatting and string comparison. With no conversion flags,
    // parse_num will return NaN early if it encounters non-decimal characters.
    let d = parse_num(string, ConversionFlags::NO_CONVERSION_FLAGS, 0.0);
    if d.is_nan() {
        return false;
    }
    let parsed_string = ecma_number_to_string(d);
    parsed_string == string
}

#[cfg(test)]
mod tests {

    use super::{ecma_number_to_string, is_numeric_literal_name};

    // use super::ecma_string_to_number;
    // #[test]
    // fn convert_string_to_number() {
    //     assert_eq!(ecma_string_to_number("123"), 123_f64);
    //     assert_eq!(ecma_string_to_number("-123"), -123_f64);
    //     assert_eq!(ecma_string_to_number("+123"), 123_f64);

    //     assert_eq!(ecma_string_to_number("0xF00D"), 0xF00D as f64);
    //     assert_eq!(ecma_string_to_number("61453"), 0xF00D as f64);

    //     assert!(ecma_string_to_number("NaN").is_nan());
    //     assert!(ecma_string_to_number("-NaN").is_nan());
    //     assert!(ecma_string_to_number("+NaN").is_nan());

    //     assert!(ecma_string_to_number("abc").is_nan());
    //     assert!(ecma_string_to_number("12ffff").is_nan());
    //     assert!(ecma_string_to_number("123n").is_nan());

    //     assert_eq!(ecma_string_to_number("Infinity"), f64::INFINITY);
    //     assert_eq!(ecma_string_to_number("+Infinity"), f64::INFINITY);
    //     assert_eq!(ecma_string_to_number("-Infinity"), f64::NEG_INFINITY);

    //     assert_eq!(ecma_string_to_number("  123 \n\r\t"), 123_f64);
    //     assert_eq!(ecma_string_to_number("  000123 \n\r\t"), 123_f64);
    //     assert_eq!(ecma_string_to_number("000123"), 123_f64);

    //     assert!(ecma_string_to_number("123_111").is_nan());
    // }

    #[test]
    fn convert_number_to_string() {
        assert_eq!("123", ecma_number_to_string(123_f64));
        assert_eq!("-123", ecma_number_to_string(-123_f64));

        assert_eq!("61453", ecma_number_to_string(0xF00D as f64));

        assert_eq!("NaN", ecma_number_to_string(f64::NAN));

        assert_eq!("Infinity", ecma_number_to_string(f64::INFINITY));
        assert_eq!("-Infinity", ecma_number_to_string(f64::NEG_INFINITY));

        assert_eq!(ecma_number_to_string(0.0), "0");
        assert_eq!(ecma_number_to_string(i32::MIN as f64), "-2147483648");
        assert_eq!(ecma_number_to_string(i32::MAX as f64), "2147483647");
        // ES section 7.1.12.1 #sec-tostring-applied-to-the-number-type:
        // -0.0 is stringified to "0".
        assert_eq!(ecma_number_to_string(-0.0), "0");
        assert_eq!(ecma_number_to_string(1.1), "1.1");
        assert_eq!(ecma_number_to_string(0.1), "0.1");
    }

    fn CheckNonArrayIndex(expected: bool, string: &str) {
        assert_eq!(expected, is_numeric_literal_name(string));
    }

    // Ported from: https://github.com/v8/v8/blob/b25adddb903f6ff7707091065ff74aa51c002f8b/test/cctest/test-conversions.cc#L383
    #[test]
    fn SpecialIndexParsing() {
        CheckNonArrayIndex(false, "");
        CheckNonArrayIndex(false, "-");
        CheckNonArrayIndex(true, "0");
        CheckNonArrayIndex(true, "-0");
        CheckNonArrayIndex(false, "01");
        CheckNonArrayIndex(false, "-01");
        CheckNonArrayIndex(true, "0.5");
        CheckNonArrayIndex(true, "-0.5");
        CheckNonArrayIndex(true, "1");
        CheckNonArrayIndex(true, "-1");
        CheckNonArrayIndex(true, "10");
        CheckNonArrayIndex(true, "-10");
        CheckNonArrayIndex(true, "NaN");
        CheckNonArrayIndex(true, "Infinity");
        CheckNonArrayIndex(true, "-Infinity");
        CheckNonArrayIndex(true, "4294967295");
        CheckNonArrayIndex(true, "429496.7295");
        CheckNonArrayIndex(true, "1.3333333333333333");
        CheckNonArrayIndex(false, "1.3333333333333339");
        CheckNonArrayIndex(true, "1.333333333333331e+222");
        CheckNonArrayIndex(true, "-1.3333333333333211e+222");
        CheckNonArrayIndex(false, "-1.3333333333333311e+222");
        CheckNonArrayIndex(true, "429496.7295");
        CheckNonArrayIndex(false, "43s3");
        CheckNonArrayIndex(true, "4294967296");
        CheckNonArrayIndex(true, "-4294967296");
        CheckNonArrayIndex(true, "999999999999999");
        CheckNonArrayIndex(false, "9999999999999999");
        CheckNonArrayIndex(true, "-999999999999999");
        CheckNonArrayIndex(false, "-9999999999999999");
        CheckNonArrayIndex(false, "42949672964294967296429496729694966");
    }
}
