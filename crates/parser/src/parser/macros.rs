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

        if let $crate::token::Token::Error(_) = $parser.input.cur() {
            if let $crate::token::Token::Error(e) = $parser.input.bump() {
                $parser.emit_error(e);
            } else {
                unreachable!();
            }
        }

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
        $parser.input.program_data_mut()
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
