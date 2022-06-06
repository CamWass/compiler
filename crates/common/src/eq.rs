use crate::{BytePos, Span, SyntaxContext};
use num_bigint::BigInt;
use std::{
    cell::{Cell, RefCell},
    cmp::PartialEq,
    rc::Rc,
    sync::Arc,
};
use string_cache::Atom;

/// Derive with `#[derive(EqIgnoreSpan)]`.
pub trait EqIgnoreSpan {
    fn eq_ignore_span(&self, other: &Self) -> bool;
}

impl EqIgnoreSpan for Span {
    /// Always returns true
    #[inline]
    fn eq_ignore_span(&self, _: &Self) -> bool {
        true
    }
}

impl<T> EqIgnoreSpan for Option<T>
where
    T: EqIgnoreSpan,
{
    fn eq_ignore_span(&self, other: &Self) -> bool {
        match (self, other) {
            (Some(l), Some(r)) => l.eq_ignore_span(r),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<T> EqIgnoreSpan for Vec<T>
where
    T: EqIgnoreSpan,
{
    fn eq_ignore_span(&self, other: &Self) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(a, b)| a.eq_ignore_span(b))
    }
}

impl<T> EqIgnoreSpan for Cell<T>
where
    T: EqIgnoreSpan + Copy,
{
    fn eq_ignore_span(&self, other: &Self) -> bool {
        self.get().eq_ignore_span(&other.get())
    }
}

/// Implement traits using PartialEq
macro_rules! eq {
    ($T:ty) => {
        impl EqIgnoreSpan for $T {
            #[inline]
            fn eq_ignore_span(&self, other: &Self) -> bool {
                self == other
            }
        }
    };

    (
        $(
            $T:ty
        ),*
    ) => {
        $(
            eq!($T);
        )*
    };
}

eq!(SyntaxContext, BytePos);
eq!(bool);
eq!(usize, u8, u16, u32, u64, u128);
eq!(isize, i8, i16, i32, i64, i128);
eq!(f32, f64);
eq!(char, str, String);

impl<S: PartialEq> EqIgnoreSpan for Atom<S> {
    #[inline]
    fn eq_ignore_span(&self, other: &Self) -> bool {
        self == other
    }
}

macro_rules! deref {
    ($T:ident) => {
        impl<N> EqIgnoreSpan for $T<N>
        where
            N: EqIgnoreSpan,
        {
            #[inline]
            fn eq_ignore_span(&self, other: &Self) -> bool {
                (**self).eq_ignore_span(&**other)
            }
        }
    };

    (
        $(
            $T:ident
        ),*
    ) => {
        $(
            deref!($T);
        )*
    };
}

deref!(Box, Rc, Arc);

impl<'a, N> EqIgnoreSpan for &'a N
where
    N: EqIgnoreSpan,
{
    #[inline]
    fn eq_ignore_span(&self, other: &Self) -> bool {
        (**self).eq_ignore_span(&**other)
    }
}

impl<N> EqIgnoreSpan for RefCell<N>
where
    N: EqIgnoreSpan,
{
    fn eq_ignore_span(&self, other: &Self) -> bool {
        self.borrow().eq_ignore_span(&*other.borrow())
    }
}

impl EqIgnoreSpan for BigInt {
    fn eq_ignore_span(&self, other: &Self) -> bool {
        self == other
    }
}
