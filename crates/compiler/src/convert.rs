use lexical::NumberFormatBuilder;

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

#[cfg(test)]
mod tests {
    use super::ecma_number_to_string;

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
}
