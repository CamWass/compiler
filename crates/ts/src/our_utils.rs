macro_rules! unwrap_as {
    ($expr:expr, $pat:pat, $res:expr) => {
        match $expr {
            $pat => $res,
            _ => unreachable!(),
        }
    };
}

pub(crate) use unwrap_as;
