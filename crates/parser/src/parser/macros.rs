macro_rules! span {
    ($parser:expr, $start:expr) => {{
        let start: ::global_common::BytePos = $start;
        let end: ::global_common::BytePos = $parser.input.prev_span().hi;

        debug_assert!(
            start <= end,
            "assertion failed: (span.start <= span.end). start = {}, end = {}",
            start.0,
            end.0
        );
        ::global_common::Span::new(start, end, ::global_common::SyntaxContext::empty())
    }};
}

/// cur!($parser, required:bool)
macro_rules! cur {
    ($parser:expr, $required:expr) => {{
        let pos = $parser.input.last_pos();
        let last = Span::new(pos, pos, Default::default());
        let is_err_token = match $parser.input.cur() {
            Some(&$crate::token::Token::Error(..)) => true,
            _ => false,
        };
        if is_err_token {
            match $parser.input.bump() {
                $crate::token::Token::Error(e) => {
                    return Err(e);
                }
                _ => unreachable!(),
            }
        }

        match $parser.input.cur() {
            Some(c) => Ok(c),
            None => {
                if $required {
                    let err = crate::error::Error {
                        error: Box::new((last, crate::error::SyntaxError::Eof)),
                    };
                    return Err(err);
                }
                Err(crate::error::Error {
                    error: Box::new((last, crate::error::SyntaxError::Eof)),
                })
            }
        }
    }};
}

/// This handles automatic semicolon insertion.
///
/// Returns bool.
macro_rules! is {
    ($parser:expr, BindingIdent) => {{
        let ctx = $parser.ctx();
        match $parser.input.cur() {
            Some(&Word(ref w)) => !ctx.is_reserved_word(&w.cow()),
            _ => false,
        }
    }};

    ($parser:expr, IdentRef) => {{
        let ctx = $parser.ctx();
        match $parser.input.cur() {
            Some(&Word(ref w)) => !ctx.is_reserved_word(&w.cow()),
            _ => false,
        }
    }};

    ($parser:expr,IdentName) => {{
        match $parser.input.cur() {
            Some(&Word(..)) => true,
            _ => false,
        }
    }};

    ($parser:expr,';') => {{
        match $parser.input.cur() {
            Some(&Token::Semi) | None | Some(&tok!('}')) => true,
            _ => $parser.input.had_line_break_before_cur(),
        }
    }};

    ($parser:expr, $t:tt) => {
        is_exact!($parser, $t)
    };
}

macro_rules! is_exact {
    ($parser:expr, $t:tt) => {{
        $parser.input.is(&tok!($t))
    }};
}

macro_rules! is_one_of {
    ($parser:expr, $($t:tt),+) => {{
        false
        $(
            || is!($parser, $t)
        )*
    }};
}

/// This handles automatic semicolon insertion.
///
/// Returns bool if token is static, and Option<Token>
///     if token has data like string.
macro_rules! eat {
    ($parser:expr, ';') => {{
        match $parser.input.cur() {
            Some(&Token::Semi) => {
                $parser.input.bump();
                true
            }
            None | Some(&tok!('}')) => true,
            _ => $parser.input.had_line_break_before_cur(),
        }
    }};

    ($parser:expr, $t:tt) => {{
        if is!($parser, $t) {
            $parser.input.bump();
            true
        } else {
            false
        }
    }};
}

macro_rules! eat_exact {
    ($parser:expr, $t:tt) => {{
        if is_exact!($parser, $t) {
            $parser.input.bump();
            true
        } else {
            false
        }
    }};
}

macro_rules! peek {
    ($parser:expr) => {{
        debug_assert!(
            $parser.input.knows_cur(),
            "parser should not call peek() without knowing current token.
Current token is {:?}",
            cur!($parser, false),
        );

        let pos = $parser.input.cur_pos();
        let last = Span::new(pos, pos, Default::default());
        match $parser.input.peek() {
            Some(c) => Ok(c),
            None => {
                let err = crate::error::Error {
                    error: Box::new((last, crate::error::SyntaxError::Eof)),
                };
                Err(err)
            }
        }
    }};
}
/// Returns true on eof.
macro_rules! eof {
    ($parser:expr) => {
        $parser.input.cur().is_none()
    };
}

macro_rules! unexpected {
    ($parser:expr, $expected:literal) => {{
        let got = $parser.input.dump_cur();
        syntax_error!(
            $parser,
            $parser.input.cur_span(),
            SyntaxError::Unexpected {
                got,
                expected: $expected
            }
        )
    }};
}

/// This handles automatic semicolon insertion.
macro_rules! expect {
    ($parser:expr, $t:tt) => {{
        const TOKEN: &Token = &tok!($t);
        if !eat!($parser, $t) {
            let cur = $parser.input.dump_cur();
            dbg!($t);
            syntax_error!(
                $parser,
                $parser.input.cur_span(),
                SyntaxError::Expected(TOKEN, cur)
            )
        }
    }};
}

macro_rules! expect_exact {
    ($parser:expr, $t:tt) => {{
        const TOKEN: &Token = &tok!($t);
        if !eat_exact!($parser, $t) {
            let cur = $parser.input.dump_cur();
            syntax_error!(
                $parser,
                $parser.input.cur_span(),
                SyntaxError::Expected(TOKEN, cur)
            )
        }
    }};
}

macro_rules! return_if_arrow {
    ($parser:expr, $expr:expr) => {{
        match $parser.state.potential_arrow_start {
            Some(start) if $expr.span().lo == start && matches!(*$expr, Expr::Arrow { .. }) => {
                return Ok($expr)
            }
            _ => {}
        };
    }};
}

macro_rules! make_error {
    ($parser:expr, $span:expr, $err:expr) => {{
        crate::error::Error {
            error: Box::new(($span, $err)),
        }
    }};
}

macro_rules! syntax_error {
    ($parser:expr, $err:expr) => {
        syntax_error!($parser, $parser.input.cur_span(), $err)
    };

    ($parser:expr, $span:expr, $err:expr) => {{
        let err = make_error!($parser, $span, $err);
        return Err(err.into());
    }};
}
