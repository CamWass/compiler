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
    ($parser:expr, true) => {{
        match $parser.input.cur() {
            Some(c) => c,
            None => {
                panic!("Unexpected eof at {:?}", $parser.input.last_pos());
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
        // log::trace!("eat(';'): cur={:?}", cur!($parser, false));
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

macro_rules! peek_or_panic {
    ($parser:expr) => {{
        let pos = $parser.input.cur_pos();
        let last = Span::new(pos, pos);
        match $parser.input.peek() {
            Some(c) => c,
            None => {
                panic!("Unexpected eof at {:?}", last);
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
    ($parser:expr, $expected:expr) => {{
        panic!(
            "Unexpected token at {:?}. Expected '{:?}', found '{:?}'",
            $parser.input.cur_span(),
            $expected,
            $parser.input.dump_cur()
        );
    }};
    ($parser:expr) => {{
        panic!(
            "Unexpected token '{:?}' at {:?}",
            $parser.input.dump_cur(),
            $parser.input.cur_span()
        );
    }};
}

/// This handles automatic semicolon insertion.
macro_rules! expect {
    ($parser:expr, $t:tt) => {{
        if !eat!($parser, $t) {
            unexpected!($parser, &tok!($t));
        }
    }};
}

macro_rules! expect_exact {
    ($parser:expr, $t:tt) => {{
        const TOKEN: &Token = &tok!($t);
        if !$parser.input.eat(TOKEN) {
            unexpected!($parser, TOKEN);
        }
    }};
}

macro_rules! return_if_arrow {
    ($parser:expr, $expr:expr) => {{
        match $parser.state.potential_arrow_start {
            Some(start) if $expr.span().lo == start && matches!(*$expr, Expr::Arrow { .. }) => {
                return $expr
            }
            _ => {}
        };
    }};
}

/// Consume a semicolon, or, failing that, see if we are allowed to
/// pretend that there is a semicolon at self position.
macro_rules! semicolon {
    ($parser:expr, $allow_asi:expr) => {{
        if ($allow_asi && !$parser.is_line_terminator())
            || (!$allow_asi && !$parser.input.eat(&tok!(';')))
        {
            // self.raise(self.lexer.state.last_tok_end, Errors.MissingSemicolon);
            panic!("MissingSemicolon at {:?}", $parser.input.cur_span());
        }
    }};
    ($parser:expr) => {{
        if !$parser.is_line_terminator() {
            // self.raise(self.lexer.state.last_tok_end, Errors.MissingSemicolon);
            panic!("MissingSemicolon at {:?}", $parser.input.cur_span());
        }
    }};
}
