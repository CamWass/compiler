use std::{
    borrow::Cow,
    cmp,
    hash::Hash,
    ops::{Add, Sub},
    rc::Rc,
    sync::Arc,
};

/// Spans represent a region of code.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct Span {
    pub lo: BytePos,
    pub hi: BytePos,
}

impl Span {
    #[inline]
    pub fn lo(self) -> BytePos {
        self.lo
    }
    #[inline]
    pub fn new(mut lo: BytePos, mut hi: BytePos) -> Self {
        if lo > hi {
            std::mem::swap(&mut lo, &mut hi);
        }

        Span { lo, hi }
    }

    #[inline]
    pub fn with_lo(&self, lo: BytePos) -> Span {
        Span::new(lo, self.hi)
    }
    #[inline]
    pub fn hi(self) -> BytePos {
        self.hi
    }

    #[inline]
    pub fn with_hi(&self, hi: BytePos) -> Span {
        Span::new(self.lo, hi)
    }

    /// Returns `true` if this is a dummy span with any hygienic context.
    #[inline]
    pub fn is_dummy(self) -> bool {
        self.lo.0 == 0 && self.hi.0 == 0
    }

    /// Returns a new span representing an empty span at the beginning of this
    /// span
    #[inline]
    pub fn shrink_to_lo(self) -> Span {
        self.with_hi(self.lo)
    }
    /// Returns a new span representing an empty span at the end of this span
    #[inline]
    pub fn shrink_to_hi(self) -> Span {
        self.with_lo(self.hi)
    }

    /// Returns `self` if `self` is not the dummy span, and `other` otherwise.
    pub fn substitute_dummy(self, other: Span) -> Span {
        if self.is_dummy() {
            other
        } else {
            self
        }
    }

    /// Return true if `self` fully encloses `other`.
    pub fn contains(self, other: Span) -> bool {
        self.lo <= other.lo && other.hi <= self.hi
    }

    /// Return true if the spans are equal with regards to the source text.
    ///
    /// Use this instead of `==` when either span could be generated code,
    /// and you only care that they point to the same bytes of source text.
    pub fn source_equal(self, other: Span) -> bool {
        self.lo == other.lo && self.hi == other.hi
    }

    /// Returns `Some(span)`, where the start is trimmed by the end of `other`
    pub fn trim_start(self, other: Span) -> Option<Span> {
        if self.hi > other.hi {
            Some(self.with_lo(cmp::max(self.lo, other.hi)))
        } else {
            None
        }
    }
}

/// Dummy span, both position and length are zero.
pub const DUMMY_SP: Span = Span {
    lo: BytePos(0),
    hi: BytePos(0),
};

impl Default for Span {
    fn default() -> Self {
        DUMMY_SP
    }
}

// _____________________________________________________________________________
// Pos, BytePos, CharPos
//

pub trait Pos {
    fn from_usize(n: usize) -> Self;
    fn to_usize(&self) -> usize;
    fn from_u32(n: u32) -> Self;
    fn to_u32(&self) -> u32;
}

/// A byte offset. Keep this small (currently 32-bits), as AST contains
/// a lot of them.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct BytePos(pub u32);

/// A character offset. Because of multi-byte utf8 characters, a byte offset
/// is not equivalent to a character offset.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct CharPos(pub usize);

impl Pos for BytePos {
    #[inline(always)]
    fn from_usize(n: usize) -> BytePos {
        BytePos(n as u32)
    }

    #[inline(always)]
    fn to_usize(&self) -> usize {
        self.0 as usize
    }

    #[inline(always)]
    fn from_u32(n: u32) -> BytePos {
        BytePos(n)
    }

    #[inline(always)]
    fn to_u32(&self) -> u32 {
        self.0
    }
}

impl Add for BytePos {
    type Output = BytePos;

    #[inline(always)]
    fn add(self, rhs: BytePos) -> BytePos {
        BytePos((self.to_usize() + rhs.to_usize()) as u32)
    }
}

impl Sub for BytePos {
    type Output = BytePos;

    #[inline(always)]
    fn sub(self, rhs: BytePos) -> BytePos {
        BytePos((self.to_usize() - rhs.to_usize()) as u32)
    }
}

impl PartialEq<u32> for BytePos {
    fn eq(&self, other: &u32) -> bool {
        self.0 == *other
    }
}

impl Pos for CharPos {
    #[inline(always)]
    fn from_usize(n: usize) -> CharPos {
        CharPos(n)
    }

    #[inline(always)]
    fn to_usize(&self) -> usize {
        self.0
    }

    #[inline(always)]
    fn from_u32(n: u32) -> CharPos {
        CharPos(n as usize)
    }

    #[inline(always)]
    fn to_u32(&self) -> u32 {
        self.0 as u32
    }
}

impl Add for CharPos {
    type Output = CharPos;

    #[inline(always)]
    fn add(self, rhs: CharPos) -> CharPos {
        CharPos(self.to_usize() + rhs.to_usize())
    }
}

impl Sub for CharPos {
    type Output = CharPos;

    #[inline(always)]
    fn sub(self, rhs: CharPos) -> CharPos {
        CharPos(self.to_usize() - rhs.to_usize())
    }
}

///
/// # Derive
/// This trait can be derived with `#[derive(Spanned)]`.
pub trait Spanned {
    /// Get span of `self`.
    fn span(&self) -> Span;
}

impl<'a, T> Spanned for Cow<'a, T>
where
    T: Spanned + Clone,
{
    #[inline(always)]
    fn span(&self) -> Span {
        (**self).span()
    }
}

impl Spanned for Span {
    #[inline(always)]
    fn span(&self) -> Span {
        *self
    }
}

impl Spanned for BytePos {
    /// Creates a new single-byte span.
    #[inline(always)]
    fn span(&self) -> Span {
        Span::new(*self, *self)
    }
}

impl<S> Spanned for Option<S>
where
    S: Spanned,
{
    fn span(&self) -> Span {
        match *self {
            Some(ref s) => s.span(),
            None => DUMMY_SP,
        }
    }
}

impl<S> Spanned for Rc<S>
where
    S: ?Sized + Spanned,
{
    fn span(&self) -> Span {
        <S as Spanned>::span(&*self)
    }
}

impl<S> Spanned for Arc<S>
where
    S: ?Sized + Spanned,
{
    fn span(&self) -> Span {
        <S as Spanned>::span(&*self)
    }
}

impl<S> Spanned for Box<S>
where
    S: ?Sized + Spanned,
{
    fn span(&self) -> Span {
        <S as Spanned>::span(&*self)
    }
}

impl<'a, S> Spanned for &'a S
where
    S: ?Sized + Spanned,
{
    fn span(&self) -> Span {
        <S as Spanned>::span(&*self)
    }
}

impl<A, B> Spanned for ::either::Either<A, B>
where
    A: Spanned,
    B: Spanned,
{
    fn span(&self) -> Span {
        match *self {
            ::either::Either::Left(ref n) => n.span(),
            ::either::Either::Right(ref n) => n.span(),
        }
    }
}
