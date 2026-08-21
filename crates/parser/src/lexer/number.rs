//! Lexer methods related to reading numbers.
//!
//!
//! See https://tc39.github.io/ecma262/#sec-literals-numeric-literals

use super::{LexResult, Lexer, pos_span};
use crate::{JscTarget, error::SyntaxError, token::Token};
use common::BytePos;
use num_bigint::BigUint;
use num_traits::Num as _;
use std::{fmt::Write, iter::FusedIterator};

fn is_forbidden_numeric_separator_sibling(b: Option<u8>, radix: Radix) -> bool {
    let Some(b) = b else {
        return false;
    };

    match radix {
        Radix::Hex => {
            // These characters are forbidden from being an immediate sibling of
            // a NumericLiteralSeparator '_' in hex numbers.
            matches!(b, b'.' | b'X' | b'_' | b'x')
        }
        Radix::Bin | Radix::Oct | Radix::Dec => {
            // These characters are forbidden from being an immediate sibling of
            // a NumericLiteralSeparator '_' in decimal, binary, and octal numbers.
            matches!(b, b'.' | b'B' | b'E' | b'O' | b'_' | b'b' | b'e' | b'o')
        }
    }
}

fn is_allowed_numeric_separator_siblings(b: Option<u8>, radix: Radix) -> bool {
    let Some(b) = b else {
        return false;
    };

    // Only valid digits in the given radix are valid numeric separator siblings.
    match radix {
        Radix::Dec => {
            matches!(b, b'0'..=b'9')
        }
        Radix::Bin => {
            matches!(b, b'0' | b'1')
        }
        Radix::Oct => {
            matches!(b, b'0'..=b'7')
        }
        Radix::Hex => {
            matches!(b, b'0'..=b'F')
        }
    }
}

fn digits(value: u64, radix: u64) -> impl Iterator<Item = u64> + Clone + 'static {
    debug_assert!(radix > 0);

    #[derive(Clone, Copy)]
    struct Digits {
        n: u64,
        divisor: u64,
    }

    impl Digits {
        fn new(n: u64, radix: u64) -> Self {
            let mut divisor = 1;
            while n >= divisor * radix {
                divisor *= radix;
            }

            Digits { n, divisor }
        }
    }

    impl Iterator for Digits {
        type Item = u64;

        fn next(&mut self) -> Option<u64> {
            if self.divisor == 0 {
                None
            } else {
                let v = Some(self.n / self.divisor);
                self.n %= self.divisor;
                self.divisor /= 10;
                v
            }
        }
    }

    impl FusedIterator for Digits {}

    Digits::new(value, radix)
}

impl Lexer<'_> {
    fn read_digits(
        &mut self,
        radix: Radix,
        max_len: Option<usize>,
        raw: &mut String,
        allow_num_separator: bool,
    ) {
        let start = self.cur_pos();

        while let Some(c) = self.cur() {
            if c == '_' {
                let next = self.peek_nth(1);

                let prev = self.prev_byte();

                if !is_allowed_numeric_separator_siblings(next, radix)
                    || is_forbidden_numeric_separator_sibling(prev, radix)
                    || is_forbidden_numeric_separator_sibling(next, radix)
                {
                    // TODO: possibly use babel's error
                    // self.raise(self.state.pos, Errors.UnexpectedNumericSeparator);
                    self.emit_error(
                        start,
                        SyntaxError::NumericSeparatorIsAllowedOnlyBetweenTwoDigits,
                    );
                }

                if !allow_num_separator {
                    // TODO: possibly use babel's error
                    // self.raise(self.state.pos, Errors.NumericSeparatorInEscapeSequence);
                    self.emit_error(
                        start,
                        SyntaxError::NumericSeparatorIsAllowedOnlyBetweenTwoDigits,
                    );
                }

                // Ignore this '_' character.
                self.advance(1);
                continue;
            }

            if !c.is_digit(radix as u32) {
                return;
            }

            raw.push(c);

            self.bump();

            if let Some(max_len) = max_len
                && max_len == raw.len()
            {
                return;
            }
        }
    }

    /// This can read long integers like
    /// "13612536612375123612312312312312312312312".
    fn read_number_no_dot(&mut self, radix: Radix) -> LexResult<f64> {
        let start = self.cur_pos();

        let mut raw = String::new();

        self.read_digits(radix, None, &mut raw, true);

        if raw.is_empty() {
            self.error(start, SyntaxError::ExpectedDigit { radix: radix as u8 })?;
            Ok(0.0)
        } else {
            Ok(parse_float_from_str(&raw, radix))
        }
    }

    /// Ensure that an identifier does not directly follow a number.
    fn ensure_not_ident(&mut self) -> LexResult<()> {
        match self.cur() {
            Some(ch) if ast::Ident::is_valid_start(ch) => {
                let span = pos_span(self.cur_pos());
                Lexer::error_span(span, SyntaxError::IdentAfterNum)?
            }
            _ => Ok(()),
        }
    }

    pub(super) fn read_radix_number(&mut self, radix: NonDecRadix) -> LexResult<Token> {
        debug_assert!(self.is(b'0'));

        self.advance(2); // 0 followed by one of x, X, o, O, b, B

        let raw_start = self.cur_pos();

        let value = self.read_number_no_dot(radix.into())?;

        let tok = if self.is(b'n') {
            let raw = self.slice_to_cur(raw_start);
            let b = BigUint::from_str_radix(raw, radix as _)
                .expect("failed to parse string as a bigint");
            self.bump(); // 'n'
            self.make_big_int_token(Box::new(b))
        } else {
            self.make_num_token(value)
        };

        self.ensure_not_ident()?;

        Ok(tok)
    }

    /// Read an integer in the given radix. Returns the integer value, or `None`
    /// if zero digits were read.
    /// When `len` is not zero, this will return `None` unless the integer has
    /// exactly `len` digits.
    pub(super) fn read_int(
        &mut self,
        radix: Radix,
        len: usize,
        raw: &mut String,
        allow_num_separator: bool,
    ) -> Option<f64> {
        self.read_digits(radix, Some(len), raw, allow_num_separator);

        if raw.is_empty() || len != 0 && raw.len() != len {
            None
        } else {
            Some(parse_float_from_str(raw, radix))
        }
    }

    /// See documentation for `read_int`.
    pub(super) fn read_int_u32(
        &mut self,
        radix: Radix,
        len: usize,
        allow_num_separator: bool,
    ) -> Option<u32> {
        let mut raw = String::new();

        self.read_digits(radix, Some(len), &mut raw, allow_num_separator);

        if raw.is_empty() || len != 0 && raw.len() != len {
            None
        } else {
            Some(u32::from_str_radix(&raw, radix as u32).unwrap())
        }
    }

    fn make_legacy_octal(&mut self, start: BytePos, val: f64) -> LexResult<f64> {
        self.ensure_not_ident()?;

        if self.syntax.typescript() && self.target >= JscTarget::Es5 {
            self.emit_error(start, SyntaxError::TS1085);
        }
        self.emit_strict_mode_error(start, SyntaxError::LegacyOctal);

        Ok(val)
    }

    /// Reads an integer, octal integer, or floating-point number
    pub(super) fn read_number(&mut self, starts_with_dot: bool) -> LexResult<Token> {
        debug_assert!(self.cur().is_some());
        if starts_with_dot {
            debug_assert!(
                self.is(b'.'),
                "read_number(starts_with_dot = true) expects current char to be '.'"
            );
        }
        let start = self.cur_pos();

        let val = if starts_with_dot {
            // first char is '.'
            0f64
        } else {
            let starts_with_zero = self.is(b'0');

            // Use read_number_no_dot to support long numbers.
            let val = self.read_number_no_dot(Radix::Dec)?;

            if self.is(b'n') {
                let raw = self.slice_to_cur(start);
                let b =
                    BigUint::from_str_radix(raw, 10).expect("failed to parse string as a bigint");
                self.bump(); // 'n'

                // TODO: do we need to check ensure_not_ident()?
                return Ok(self.make_big_int_token(Box::new(b)));
            }

            if starts_with_zero {
                if val == 0.0f64 {
                    // If only one zero is used, it's decimal.
                    // And if multiple zero is used, it's octal.
                    //
                    // e.g. `0` is decimal (so it can be part of float)
                    //
                    // e.g. `000` is octal
                    if start.0 != self.cur_pos().0 - 1 {
                        // `-1` is utf 8 length of `0`

                        return self
                            .make_legacy_octal(start, 0f64)
                            .map(|v| self.make_num_token(v));
                    }
                } else {
                    // strict mode hates non-zero decimals starting with zero.
                    // e.g. 08.1 is strict mode violation but 0.1 is valid float.

                    if val.fract() < 1e-10 {
                        let mut d = digits(val.round() as u64, 10);

                        // TODO: should we be checking the string representation
                        // rather than the digits of the parsed decimal value?
                        // if it contains '8' or '9', it's decimal.
                        if d.any(|v| v == 8 || v == 9) {
                            // Continue parsing
                            self.emit_strict_mode_error(start, SyntaxError::LegacyDecimal);
                        } else {
                            // It's Legacy octal, and we should reinterpret value.
                            let val = u64::from_str_radix(&val.to_string(), 8)
                                .expect("Does this can really happen?");
                            let val = val
                                .to_string()
                                .parse()
                                .expect("failed to parse numeric value as f64");
                            return self
                                .make_legacy_octal(start, val)
                                .map(|v| self.make_num_token(v));
                        }
                    }
                }
            }

            val
        };

        // At this point, number cannot be an octal literal.

        let mut val: f64 = val;

        //  `0.a`, `08.a`, `102.a` are invalid.
        //
        // `.1.a`, `.1e-4.a` are valid,
        if self.eat(b'.') {
            if starts_with_dot {
                debug_assert!(self.cur().is_some());
                debug_assert!(self.cur().unwrap().is_ascii_digit());
            }

            let mut raw = String::new();
            // Read numbers after dot
            let dec_val = self.read_int(Radix::Dec, 0, &mut raw, true);

            val = {
                // TODO: is it possible/worthwhile to pre-allocate this using
                // String::with_capacity()?
                let mut buffer = String::new();

                write!(buffer, "{val}.").unwrap();

                if dec_val.is_some() {
                    buffer.push_str(&raw);
                }

                buffer
                    .parse()
                    .expect("failed to parse float using rust's impl")
            };
        }

        // Handle 'e' and 'E'
        //
        // .5e1 = 5
        // 1e2 = 100
        // 1e+2 = 100
        // 1e-2 = 0.01
        if self.is(b'e') || self.is(b'E') {
            self.advance(1); // 'e' or 'E'

            let next = if let Some(next) = self.cur_byte() {
                next
            } else {
                let pos = self.cur_pos();
                self.error(pos, SyntaxError::NumLitTerminatedWithExp)?
            };

            let positive = if next == b'+' || next == b'-' {
                self.advance(1); // remove '+', '-'
                next == b'+'
            } else {
                true
            };

            let exp = self.read_number_no_dot(Radix::Dec)?;
            let flag = if positive { '+' } else { '-' };
            // TODO:
            val = format!("{val}e{flag}{exp}")
                .parse()
                .expect("failed to parse float literal");
        }

        self.ensure_not_ident()?;

        Ok(self.make_num_token(val))
    }
}

fn parse_float_from_str(str: &str, radix: Radix) -> f64 {
    debug_assert!(!str.is_empty());
    debug_assert!(str.chars().all(|c| c.is_digit(radix as u32)));

    match radix {
        Radix::Bin | Radix::Oct | Radix::Hex => str.as_bytes().iter().fold(0.0, |result, &cur| {
            result.mul_add(
                radix as u8 as f64,
                char::from_u32(cur as u32)
                    .unwrap()
                    .to_digit(radix as u32)
                    .unwrap() as f64,
            )
        }),

        // The above method is not exact when the radix not a power of two. For
        // example, 1e+30 is parsed as 9.999999999999999e29 when the radix is
        // 10.
        Radix::Dec => str.parse::<f64>().unwrap(),
    }
}

#[derive(Clone, Copy)]
pub(super) enum Radix {
    Bin = 2,
    Oct = 8,
    Dec = 10,
    Hex = 16,
}

#[derive(Clone, Copy)]
pub(super) enum NonDecRadix {
    Bin = 2,
    Oct = 8,
    Hex = 16,
}

impl From<NonDecRadix> for Radix {
    fn from(radix: NonDecRadix) -> Self {
        match radix {
            NonDecRadix::Bin => Radix::Bin,
            NonDecRadix::Oct => Radix::Oct,
            NonDecRadix::Hex => Radix::Hex,
        }
    }
}
