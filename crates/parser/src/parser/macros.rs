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
            Some(Word(w)) => !ctx.is_reserved_word(w.get_name_id()),
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

    ($parser:expr, ';') => {{
        match $parser.input.cur() {
            Some(Token::Semi) | None | Some(tok!('}')) => true,
            _ => $parser.input.had_line_break_before_cur(),
        }
    }};
}

macro_rules! peeked_is {
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

macro_rules! expect {
    ($parser:expr, $t:tt) => {{
        if !$parser.eat(tok!($t)) {
            let cur = $parser.input.dump_cur();
            syntax_error!(
                $parser,
                $parser.input.cur_span(),
                SyntaxError::Expected(tok!($t), cur)
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

macro_rules! syntax_error {
    ($parser:expr, $err:expr) => {
        syntax_error!($parser, $parser.input.cur_span(), $err)
    };

    ($parser:expr, $span:expr, $err:expr) => {{
        let err = crate::error::Error {
            error: Box::new(($span, $err)),
        };
        return Err(err.into());
    }};
}

macro_rules! node_id {
    ($parser:expr, $span:expr) => {{
        let span = $span;
        let n = $parser.input.program_data_mut().new_id(span);
        n
    }};
}

macro_rules! node_id_from {
    ($parser:expr, $other:expr) => {{
        let other = $other;
        let n = $parser.input.program_data_mut().new_id_from(other);
        n
    }};
}

macro_rules! program_data {
    ($parser:expr) => {
        &mut $parser.input.program_data_mut()
    };
}

macro_rules! get_span {
    ($parser:expr, $node:expr) => {{
        let id = $node;
        let s = $parser.input.program_data().get_span(id);
        s
    }};
}

macro_rules! set_span {
    ($parser:expr, $node:expr, $span:expr) => {
        let id = $node;
        $parser.input.program_data_mut().set_span(id, $span);
    };
}
