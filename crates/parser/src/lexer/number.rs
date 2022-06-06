//! Lexer methods related to reading numbers.
//!
//!
//! See https://tc39.github.io/ecma262/#sec-literals-numeric-literals

use super::{is_ident_start, pos_span, LexResult, Lexer};
use crate::{
    error::SyntaxError,
    token::{BigInt, Num, Token},
    JscTarget,
};
use global_common::BytePos;
use num_bigint::BigInt as BigIntValue;
use std::{fmt::Write, iter::FusedIterator};

fn is_forbidden_numeric_separator_sibling(b: Option<u8>, radix: u8) -> bool {
    match b {
        Some(b) => {
            if radix == 16 {
                // These characters are forbidden from being an immediate sibling of
                // a NumericLiteralSeparator '_' in hex numbers.
                matches!(b, b'.' | b'X' | b'_' | b'x')
            } else {
                // These characters are forbidden from being an immediate sibling of
                // a NumericLiteralSeparator '_' in decimal, binary, and octal numbers.
                matches!(b, b'.' | b'B' | b'E' | b'O' | b'_' | b'b' | b'e' | b'o')
            }
        }
        None => false,
    }
}

fn is_allowed_numeric_separator_siblings(b: Option<u8>, radix: u8) -> bool {
    debug_assert!(
        radix == 2 || radix == 8 || radix == 10 || radix == 16,
        "radix should be one of 2, 8, 10, 16, but got {}",
        radix
    );

    match b {
        Some(b) => {
            // Only valid digits in the given radix are valid numeric separator siblings.

            match radix {
                10 => {
                    matches!(b, b'0'..=b'9')
                }
                2 => {
                    matches!(b, b'0' | b'1')
                }
                8 => {
                    matches!(b, b'0'..=b'7')
                }
                16 => {
                    matches!(b, b'0'..=b'F')
                }
                _ => unsafe {
                    // Safety: This function is only called with a fixed set of
                    // radices, as checked above.
                    core::hint::unreachable_unchecked();
                },
            }
        }
        None => false,
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
    /// `op`- |total, radix, value| -> (total * radix + value, continue)
    fn read_digits<F, Ret>(
        &mut self,
        radix: u8,
        mut op: F,
        // TODO: explore making callers of this function use a slice of the
        // source to get the number as a string, rather than passing a String
        // to be populated.
        raw: &mut String,
        allow_num_separator: bool,
    ) -> Ret
    where
        F: FnMut(Ret, u8, u32) -> (Ret, bool),
        Ret: Copy + Default,
    {
        debug_assert!(
            radix == 2 || radix == 8 || radix == 10 || radix == 16,
            "radix for read_int should be one of 2, 8, 10, 16, but got {}",
            radix
        );

        let start = self.cur_pos();

        let mut total: Ret = Default::default();

        while let Some(c) = self.cur() {
            if c == '_' {
                let next = self.peek_nth(1);

                let prev = self.prev_byte();

                if !is_allowed_numeric_separator_siblings(next, radix)
                    || is_forbidden_numeric_separator_sibling(prev, radix)
                    || is_forbidden_numeric_separator_sibling(next, radix)
                {
                    // TODO: possible use babel's error
                    // self.raise(self.state.pos, Errors.UnexpectedNumericSeparator);
                    self.emit_error(
                        start,
                        SyntaxError::NumericSeparatorIsAllowedOnlyBetweenTwoDigits,
                    );
                }

                if !allow_num_separator {
                    // TODO: possible use babel's error
                    // self.raise(self.state.pos, Errors.NumericSeparatorInEscapeSequence);
                    self.emit_error(
                        start,
                        SyntaxError::NumericSeparatorIsAllowedOnlyBetweenTwoDigits,
                    );
                }

                // Ignore this '_' character
                self.advance(1);
                continue;
            }

            let val = if let Some(val) = c.to_digit(radix as u32) {
                val
            } else {
                return total;
            };

            raw.push(c);

            self.bump();
            let (t, cont) = op(total, radix, val);
            total = t;
            if !cont {
                return total;
            }
        }

        total
    }

    /// This can read long integers like
    /// "13612536612375123612312312312312312312312".
    fn read_number_no_dot(&mut self, radix: u8, raw: &mut String) -> LexResult<f64> {
        debug_assert!(
            radix == 2 || radix == 8 || radix == 10 || radix == 16,
            "radix for read_number_no_dot should be one of 2, 8, 10, 16, but got {}",
            radix
        );

        let start = self.cur_pos();

        let mut read_any = false;

        let res = self.read_digits(
            radix,
            |total, radix, v| {
                read_any = true;
                (f64::mul_add(total, radix as f64, v as f64), true)
            },
            raw,
            true,
        );

        if !read_any {
            self.error(start, SyntaxError::ExpectedDigit { radix })?;
        }
        Ok(res)
    }

    /// This can read long integers like
    /// "13612536612375123612312312312312312312312".
    fn read_number_no_dot_as_str(&mut self, radix: u8) -> LexResult<(f64, BigIntValue, String)> {
        debug_assert!(
            radix == 2 || radix == 8 || radix == 10 || radix == 16,
            "radix for read_number_no_dot should be one of 2, 8, 10, 16, but got {}",
            radix
        );
        let start = self.cur_pos();

        let mut read_any = false;

        let mut raw = String::new();

        let val = self.read_digits(
            radix,
            |total, radix, v| {
                read_any = true;
                (f64::mul_add(total, radix as f64, v as f64), true)
            },
            &mut raw,
            true,
        );

        if !read_any {
            self.error(start, SyntaxError::ExpectedDigit { radix })?;
        }

        Ok((
            val,
            // TODO: this seems inefficient; we have a string, convert it to
            // bytes, and then pass it to BigIntValue::parse_bytes which converts
            // it back into a string. Look into a more direct way to create a
            // Bigint from a string.
            BigIntValue::parse_bytes(&raw.as_bytes(), radix as _)
                .expect("failed to parse string as a bigint"),
            raw,
        ))
    }

    /// Ensure that an identifier does not directly follow a number.
    fn ensure_not_ident(&mut self) -> LexResult<()> {
        match self.cur() {
            Some(ch) if is_ident_start(ch) => {
                let span = pos_span(self.cur_pos());
                self.error_span(span, SyntaxError::IdentAfterNum)?
            }
            _ => Ok(()),
        }
    }

    pub(super) fn read_radix_number(&mut self, radix: u8) -> LexResult<Token> {
        debug_assert!(
            radix == 2 || radix == 8 || radix == 16,
            "radix should be one of 2, 8, 16, but got {}",
            radix
        );
        debug_assert!(self.is(b'0'));

        let start = self.cur_pos();

        self.advance(2); // 0 followed by one of x, X, o, O, b, B

        let mut buffer = self.slice_to_cur(start).to_string();

        let (value, s, raw) = self.read_number_no_dot_as_str(radix)?;

        let is_big_int = self.eat(b'n');

        self.ensure_not_ident()?;

        if is_big_int {
            Ok(BigInt(s))
        } else {
            buffer.push_str(&raw);
            Ok(Num {
                value,
                raw: buffer.into(),
            })
        }
    }

    /// Read an integer in the given radix. Returns the integer value, or `None`
    /// if zero digits were read.
    /// When `len` is not zero, this will return `None` unless the integer has
    /// exactly `len` digits.
    pub(super) fn read_int(
        &mut self,
        radix: u8,
        len: u8,
        raw: &mut String,
        allow_num_separator: bool,
    ) -> Option<f64> {
        let mut count = 0;
        let v = self.read_digits(
            radix,
            |opt: Option<f64>, radix, val| {
                count += 1;
                let total = opt.unwrap_or_default() * radix as f64 + val as f64;
                (Some(total), count != len)
            },
            raw,
            allow_num_separator,
        );
        if len != 0 && count != len {
            None
        } else {
            v
        }
    }

    /// See documentation for `read_int`.
    pub(super) fn read_int_u32(
        &mut self,
        radix: u8,
        len: u8,
        allow_num_separator: bool,
    ) -> Option<u32> {
        let mut count = 0;
        let v = self.read_digits(
            radix,
            |opt: Option<u32>, radix, val| {
                count += 1;
                let total = opt.unwrap_or_default() * radix as u32 + val as u32;
                (Some(total), count != len)
            },
            &mut String::new(),
            allow_num_separator,
        );
        if len != 0 && count != len {
            None
        } else {
            v
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

        let mut raw_str = String::new();

        let val = if starts_with_dot {
            // first char is '.'
            0f64
        } else {
            let starts_with_zero = self.is(b'0');

            // Use read_number_no_dot to support long numbers.
            let (val, s, raw) = self.read_number_no_dot_as_str(10)?;

            if self.eat(b'n') {
                // TODO: do we need to check ensure_not_ident()?
                return Ok(BigInt(s));
            }

            raw_str.push_str(&raw);

            if starts_with_zero {
                // TODO(swc): I guess it would be okay if I don't use -ffast-math
                // (or something like that), but needs review.

                if val == 0.0f64 {
                    // If only one zero is used, it's decimal.
                    // And if multiple zero is used, it's octal.
                    //
                    // e.g. `0` is decimal (so it can be part of float)
                    //
                    // e.g. `000` is octal
                    if start.0 != self.cur_pos().0 - 1 {
                        // `-1` is utf 8 length of `0`

                        return self.make_legacy_octal(start, 0f64).map(|v| Num {
                            value: v,
                            raw: raw.into(),
                        });
                    }
                } else {
                    // strict mode hates non-zero decimals starting with zero.
                    // e.g. 08.1 is strict mode violation but 0.1 is valid float.

                    if val.fract() < 1e-10 {
                        let mut d = digits(val.round() as u64, 10);

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
                            return self.make_legacy_octal(start, val).map(|v| Num {
                                value: v,
                                raw: raw.into(),
                            });
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
            raw_str.push('.');

            if starts_with_dot {
                debug_assert!(self.cur().is_some());
                debug_assert!(self.cur().unwrap().is_digit(10));
            }

            let mut raw = String::new();
            // Read numbers after dot
            let dec_val = self.read_int(10, 0, &mut raw, true);

            raw_str.push_str(&raw);

            val = {
                // TODO: is it possible/worthwhile to pre-allocate this using
                // String::with_capacity()?
                let mut buffer = String::new();

                write!(buffer, "{}.", val).unwrap();

                if let Some(..) = dec_val {
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
        if let Some(e @ b'e' | e @ b'E') = self.cur_byte() {
            self.advance(1); // 'e' or 'E'

            let next = match self.cur_byte() {
                Some(next) => next,
                None => {
                    let pos = self.cur_pos();
                    self.error(pos, SyntaxError::NumLitTerminatedWithExp)?
                }
            };

            raw_str.push(e as char);

            let positive = if next == b'+' || next == b'-' {
                self.advance(1); // remove '+', '-'
                raw_str.push(next as char);
                next == b'+'
            } else {
                true
            };

            let exp = self.read_number_no_dot(10, &mut raw_str)?;
            let flag = if positive { '+' } else { '-' };
            // TODO(swc):
            val = format!("{}e{}{}", val, flag, exp)
                .parse()
                .expect("failed to parse float literal");
        }

        self.ensure_not_ident()?;

        Ok(Num {
            value: val,
            raw: raw_str.into(),
        })
    }
}
