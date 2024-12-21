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
        ::global_common::Span::new(start, end)
    }};
}

/// cur!($parser, required:bool)
macro_rules! cur {
    ($parser:expr, $required:expr) => {{
        let pos = $parser.input.last_pos();
        let last = Span::new(pos, pos);
        let is_err_token = match $parser.input.cur() {
            Some($crate::token::Token::Error(..)) => true,
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
            Some(Word(w)) => !ctx.is_reserved_word(&w.cow()),
            _ => false,
        }
    }};

    ($parser:expr, IdentRef) => {{
        let ctx = $parser.ctx();
        match $parser.input.cur() {
            Some(Word(w)) => !ctx.is_reserved_word(&w.cow()),
            _ => false,
        }
    }};

    ($parser:expr, IdentName) => {{
        match $parser.input.cur() {
            Some(Word(..)) => true,
            _ => false,
        }
    }};

    ($parser:expr, Str) => {{
        match $parser.input.cur() {
            Some(Token::Str { .. }) => true,
            _ => false,
        }
    }};

    ($parser:expr, Num) => {{
        match $parser.input.cur() {
            Some(Token::Num { .. }) => true,
            _ => false,
        }
    }};

    ($parser:expr,';') => {{
        match $parser.input.cur() {
            Some(Token::Semi) | None | Some(tok!('}')) => true,
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

macro_rules! peeked_is {
    ($parser:expr, BindingIdent) => {{
        let ctx = $parser.ctx();
        match peek!($parser) {
            Ok(Word(w)) => !ctx.is_reserved_word(&w.cow()),
            _ => false,
        }
    }};

    ($parser:expr, IdentRef) => {{
        let ctx = $parser.ctx();
        match peek!($parser) {
            Ok(Word(w)) => !ctx.is_reserved_word(&w.cow()),
            _ => false,
        }
    }};

    ($parser:expr, IdentName) => {{
        match peek!($parser) {
            Ok(Word(..)) => true,
            _ => false,
        }
    }};

    ($parser:expr, ';') => {{
        compile_error!("peeked_is!(self, ';') is invalid");
    }};

    ($parser:expr, $t:tt) => {
        match peek!($parser).ok() {
            Some(tok!($t)) => true,
            _ => false,
        }
    };
}

/// This handles automatic semicolon insertion.
///
/// Returns bool if token is static, and Option<Token>
///     if token has data like string.
macro_rules! eat {
    ($parser:expr, ';') => {{
        match $parser.input.cur() {
            Some(Token::Semi) => {
                $parser.input.bump();
                true
            }
            None | Some(tok!('}')) => true,
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
        let last = Span::new(pos, pos);
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

macro_rules! store {
    ($parser:expr, $t:tt) => {{
        const TOKEN: Token = tok!($t);

        $parser.input.store(TOKEN);
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
    ($parser:expr, $potential_arrow_start:expr, $expr:expr) => {{
        if let Some(start) = $potential_arrow_start {
            if let MaybeParen::Expr(e) = &$expr {
                if matches!(e.as_ref(), Expr::Arrow { .. })
                    && get_span!($parser, $expr.node_id()).lo == start
                {
                    return Ok($expr);
                }
            }
        }
    }};
}

macro_rules! trace_cur {
    ($parser:expr, $name:ident) => {{
        // println!("{}: {:?}", stringify!($name), $parser.input.cur());
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

macro_rules! node_id {
    ($parser:expr, $span:expr) => {{
        let span = $span;
        let n = $parser.program_data.new_id(span);
        n
    }};
}

macro_rules! node_id_from {
    ($parser:expr, $other:expr) => {{
        let other = $other;
        let n = $parser.program_data.new_id_from(other);
        n
    }};
}

macro_rules! program_data {
    ($parser:expr) => {
        &mut $parser.program_data
    };
}

macro_rules! get_span {
    ($parser:expr, $node:expr) => {{
        let id = $node;
        let s = $parser.program_data.get_span(id);
        s
    }};
}

macro_rules! set_span {
    ($parser:expr, $node:expr, $span:expr) => {
        let id = $node;
        $parser.program_data.set_span(id, $span);
    };
}
