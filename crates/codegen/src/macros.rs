macro_rules! opt_leading_space {
    ($emitter:expr, $func:ident, $e:expr) => {
        if let Some(ref e) = $e {
            formatting_space!($emitter);
            $emitter.$func(e)?;
        }
    };
}

macro_rules! opt {
    ($emitter:expr, $func:ident, $e:expr) => {{
        if let Some(ref expr) = $e {
            $emitter.$func(expr)?;
        }
    }};
}

macro_rules! keyword {
    ($emitter:expr, $span:expr, $s:expr) => {
        $emitter.wr.write_keyword(Some($span), $s)?
    };
    ($emitter:expr, $s:expr) => {
        $emitter.wr.write_keyword(None, $s)?
    };
}

macro_rules! punct {
    ($emitter:expr, ";") => {
        $emitter.wr.write_semi(None);
    };
    ($emitter:expr, "[") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::LBracket)?
    };
    ($emitter:expr, "!") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::Bang)?
    };
    ($emitter:expr, "{") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::LBrace)?
    };
    ($emitter:expr, "(") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::LParen)?
    };
    ($emitter:expr, "~") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::Tilde)?
    };
    ($emitter:expr, "-") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::Minus)?
    };
    ($emitter:expr, "+") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::Plus)?
    };
    ($emitter:expr, "#") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::Hash)?
    };
    ($emitter:expr, "`") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::BackTick)?
    };
    ($emitter:expr, "*") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::Asterisk)?
    };
    ($emitter:expr, "=") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::Eq)?
    };
    ($emitter:expr, "}") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::RBrace)?
    };
    ($emitter:expr, ")") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::RParen)?
    };
    ($emitter:expr, "]") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::RBracket)?
    };
    ($emitter:expr, ":") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::Colon)?
    };
    ($emitter:expr, ".") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::Dot)?
    };
    ($emitter:expr, ",") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::Comma)?
    };
    ($emitter:expr, "?") => {
        $emitter
            .wr
            .write_punct(None, $crate::text_writer::Punct::QuestionMark)?
    };
    ($emitter:expr, "?.") => {
        $emitter.wr.write_multi_byte_punct(&[
            $crate::text_writer::Punct::QuestionMark,
            $crate::text_writer::Punct::Dot,
        ])?
    };
    ($emitter:expr, "=>") => {
        $emitter.wr.write_multi_byte_punct(&[
            $crate::text_writer::Punct::Eq,
            $crate::text_writer::Punct::Gt,
        ])?
    };
    ($emitter:expr, "${") => {
        $emitter.wr.write_multi_byte_punct(&[
            $crate::text_writer::Punct::Dollar,
            $crate::text_writer::Punct::LBrace,
        ])?
    };
    ($emitter:expr, "...") => {
        $emitter.wr.write_multi_byte_punct(&[
            $crate::text_writer::Punct::Dot,
            $crate::text_writer::Punct::Dot,
            $crate::text_writer::Punct::Dot,
        ])?
    };
}

macro_rules! operator {
    ($emitter:expr, $s:expr) => {
        $emitter.wr.write_operator(None, $s)?;
    };
}

macro_rules! space {
    ($emitter:expr) => {
        $emitter.wr.write_space()?;
    };
}

macro_rules! formatting_space {
    ($emitter:expr) => {
        if !$emitter.cfg.minify {
            $emitter.wr.write_space()?;
        }
    };
}

/// This macro *may* emit a semicolon, if it's required in this context.
macro_rules! formatting_semi {
    ($emitter:expr) => {
        punct!($emitter, ";")
    };
}

/// This macro *always* emits a semicolon, as it's required by the structure we
/// emit.
macro_rules! semi {
    ($emitter:expr) => {
        $emitter.wr.write_punct(None, Punct::Semi)?;
    };
}

macro_rules! get_span {
    ($emitter:expr, $node:expr) => {
        $emitter.program_data.get_span($node)
    };
}
