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
    ($emitter:expr, $sp:expr, ";") => {
        $emitter.wr.write_semi(Some($sp))?;
    };
    ($emitter:expr, $sp:expr, $s:expr) => {
        $emitter.wr.write_punct(Some($sp), $s)?;
    };

    ($emitter:expr, ";") => {
        $emitter.wr.write_semi(None)?;
    };
    ($emitter:expr, $s:expr) => {
        $emitter.wr.write_punct(None, $s)?
    };
}

macro_rules! operator {
    ($emitter:expr, $sp:expr, $s:expr) => {
        $emitter.wr.write_operator(Some($sp), $s)?;
    };

    ($emitter:expr, $s:expr) => {
        $emitter.wr.write_operator(None, $s)?;
    };
}

macro_rules! space {
    ($emitter:expr) => {
        $emitter.wr.write_space()?;
    };
    ($emitter:expr,) => {
        space!($emitter)
    };
}

macro_rules! formatting_space {
    ($emitter:expr) => {
        if !$emitter.cfg.minify {
            $emitter.wr.write_space()?;
        }
    };
    ($emitter:expr,) => {
        formatting_space!($emitter)
    };
}

/// This macro *may* emit a semicolon, if it's required in this context.
macro_rules! formatting_semi {
    ($emitter:expr) => {
        punct!($emitter, ";")
    };
    ($emitter:expr, ) => {
        punct!($emitter, ";")
    };
}

/// This macro *always* emits a semicolon, as it's required by the structure we
/// emit.
macro_rules! semi {
    ($emitter:expr, $sp:expr) => {
        $emitter.wr.write_punct(Some($sp), ";")?;
    };
    ($emitter:expr) => {
        $emitter.wr.write_punct(None, ";")?;
    };
}

macro_rules! get_span {
    ($emitter:expr, $node:expr) => {
        $emitter.program_data.get_span($node)
    };
}
