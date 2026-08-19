use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut, RangeBounds};
use std::slice;
use std::vec;

/// Represents some newtyped `usize` wrapper.
///
/// Purpose: avoid mixing indexes for different bitvector domains.
pub trait Idx: Copy + 'static + Ord + Debug + Hash {
    fn new(idx: usize) -> Self;

    fn index(self) -> usize;

    fn increment_by(&mut self, amount: usize) {
        *self = self.plus(amount);
    }

    fn plus(self, amount: usize) -> Self {
        Self::new(self.index() + amount)
    }
}

impl Idx for usize {
    #[inline(always)]
    fn new(idx: usize) -> Self {
        idx
    }
    #[inline(always)]
    fn index(self) -> usize {
        self
    }
}

impl Idx for u32 {
    #[inline(always)]
    fn new(idx: usize) -> Self {
        assert!(idx <= u32::MAX as usize);
        idx as u32
    }
    #[inline(always)]
    fn index(self) -> usize {
        self as usize
    }
}

/// Creates a struct type `S` that can be used as an index with
/// `IndexVec` and so on.
///
/// There are two ways of interacting with these indices:
///
/// - The `From` impls are the preferred way. So you can do
///   `S::from(v)` with a `usize` or `u32`. And you can convert back
///   to an integer with `u32::from(s)`.
///
/// - Alternatively, you can use the methods `S::new(v)` and `s.index()`
///   to create/return a value.
///
/// Internally, the index uses a u32, so the index must not exceed
/// `u32::MAX`.
#[macro_export]
macro_rules! newtype_index {
    ($v:vis $type:ident $(, $extra:path)*) => (
        #[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord $(, $extra)*)]
        $v struct $type(u32);

        impl $type {
            pub const MAX: Self = Self::from_u32(u32::MAX);

            #[inline(always)]
            pub const fn from_usize(value: usize) -> Self {
                assert!(value <= u32::MAX as usize);
                Self(value as u32)
            }

            #[inline(always)]
            pub const fn from_u32(value: u32) -> Self {
                Self(value)
            }

            /// Extracts the value of this index as an integer.
            #[inline(always)]
            pub const fn index(self) -> usize {
                self.as_usize()
            }

            /// Extracts the value of this index as a `u32`.
            #[inline(always)]
            pub const fn as_u32(self) -> u32 {
                self.0
            }

            /// Extracts the value of this index as a `usize`.
            #[inline(always)]
            pub const fn as_usize(self) -> usize {
                self.as_u32() as usize
            }
        }

        impl $crate::vec::Idx for $type {
            #[inline(always)]
            fn new(value: usize) -> Self {
                Self::from_usize(value)
            }

            #[inline(always)]
            fn index(self) -> usize {
                self.as_usize()
            }
        }

        impl ::std::fmt::Debug for $type {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                write!(fmt, "{}({})", stringify!($type), self.as_u32())
            }
        }
    );
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IndexVec<I: Idx, T> {
    pub raw: Vec<T>,
    _marker: PhantomData<fn(&I)>,
}

// Whether `IndexVec` is `Send` depends only on the data,
// not the phantom data.
unsafe impl<I: Idx, T> Send for IndexVec<I, T> where T: Send {}

impl<I: Idx, T: fmt::Debug> fmt::Debug for IndexVec<I, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.raw, fmt)
    }
}

impl<I: Idx, T> IndexVec<I, T> {
    #[inline]
    pub fn new() -> Self {
        IndexVec {
            raw: Vec::new(),
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn from_raw(raw: Vec<T>) -> Self {
        IndexVec {
            raw,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        IndexVec {
            raw: Vec::with_capacity(capacity),
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn from_elem<S>(elem: T, universe: &IndexVec<I, S>) -> Self
    where
        T: Clone,
    {
        IndexVec {
            raw: vec![elem; universe.len()],
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn from_elem_n(elem: T, n: usize) -> Self
    where
        T: Clone,
    {
        IndexVec {
            raw: vec![elem; n],
            _marker: PhantomData,
        }
    }

    /// Create an `IndexVec` with `n` elements, where the value of each
    /// element is the result of `func(i)`. (The underlying vector will
    /// be allocated only once, with a capacity of at least `n`.)
    #[inline]
    pub fn from_fn_n(func: impl FnMut(I) -> T, n: usize) -> Self {
        let indices = (0..n).map(I::new);
        Self::from_raw(indices.map(func).collect())
    }

    #[inline]
    pub fn push(&mut self, d: T) -> I {
        let idx = I::new(self.len());
        self.raw.push(d);
        idx
    }

    #[inline]
    pub fn pop(&mut self) -> Option<T> {
        self.raw.pop()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.raw.len()
    }

    /// Gives the next index that will be assigned when `push` is
    /// called.
    #[inline]
    pub fn next_index(&self) -> I {
        I::new(self.len())
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    #[inline]
    pub fn into_iter(self) -> vec::IntoIter<T> {
        self.raw.into_iter()
    }

    #[inline]
    pub fn into_iter_enumerated(self) -> impl Iterator<Item = (I, T)> {
        self.raw
            .into_iter()
            .enumerate()
            .map(|(i, v)| (I::new(i), v))
    }

    #[inline]
    pub fn iter(&self) -> slice::Iter<'_, T> {
        self.raw.iter()
    }

    #[inline]
    pub fn iter_enumerated(&self) -> impl DoubleEndedIterator<Item = (I, &T)> {
        self.raw.iter().enumerate().map(|(i, c)| (I::new(i), c))
    }

    #[inline]
    pub fn indices(&self) -> impl Iterator<Item = I> + use<I, T> {
        (0..self.len()).map(I::new)
    }

    #[inline]
    pub fn iter_mut(&mut self) -> slice::IterMut<'_, T> {
        self.raw.iter_mut()
    }

    #[inline]
    pub fn iter_enumerated_mut(&mut self) -> impl Iterator<Item = (I, &mut T)> {
        self.raw.iter_mut().enumerate().map(|(i, v)| (I::new(i), v))
    }

    #[inline]
    pub fn drain<R: RangeBounds<usize>>(&mut self, range: R) -> impl Iterator<Item = T> + '_ {
        self.raw.drain(range)
    }

    #[inline]
    pub fn drain_enumerated<R: RangeBounds<usize>>(
        &mut self,
        range: R,
    ) -> impl Iterator<Item = (I, T)> + '_ {
        self.raw
            .drain(range)
            .enumerate()
            .map(|(i, v)| (I::new(i), v))
    }

    #[inline]
    pub fn last(&self) -> Option<I> {
        self.len().checked_sub(1).map(I::new)
    }

    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.raw.shrink_to_fit();
    }

    #[inline]
    pub fn swap(&mut self, a: I, b: I) {
        self.raw.swap(a.index(), b.index());
    }

    #[inline]
    pub fn truncate(&mut self, a: usize) {
        self.raw.truncate(a);
    }

    #[inline]
    pub fn get(&self, index: I) -> Option<&T> {
        self.raw.get(index.index())
    }

    #[inline]
    pub fn get_mut(&mut self, index: I) -> Option<&mut T> {
        self.raw.get_mut(index.index())
    }

    /// Returns mutable references to two distinct elements, a and b. Panics if a == b.
    #[inline]
    pub fn pick2_mut(&mut self, a: I, b: I) -> (&mut T, &mut T) {
        let (ai, bi) = (a.index(), b.index());
        assert!(ai != bi);

        if ai < bi {
            let (c1, c2) = self.raw.split_at_mut(bi);
            (&mut c1[ai], &mut c2[0])
        } else {
            let (c2, c1) = self.pick2_mut(b, a);
            (c1, c2)
        }
    }

    /// Returns mutable references to three distinct elements or panics otherwise.
    #[inline]
    pub fn pick3_mut(&mut self, a: I, b: I, c: I) -> (&mut T, &mut T, &mut T) {
        let (ai, bi, ci) = (a.index(), b.index(), c.index());
        assert!(ai != bi && bi != ci && ci != ai);
        let len = self.raw.len();
        assert!(ai < len && bi < len && ci < len);
        let ptr = self.raw.as_mut_ptr();
        unsafe { (&mut *ptr.add(ai), &mut *ptr.add(bi), &mut *ptr.add(ci)) }
    }

    pub fn convert_index_type<Ix: Idx>(self) -> IndexVec<Ix, T> {
        IndexVec {
            raw: self.raw,
            _marker: PhantomData,
        }
    }

    /// Grows the index vector so that it contains an entry for
    /// `elem`; if that is already true, then has no
    /// effect. Otherwise, inserts new values as needed by invoking
    /// `fill_value`.
    #[inline]
    pub fn ensure_contains_elem(&mut self, elem: I, fill_value: impl FnMut() -> T) {
        let min_new_len = elem.index() + 1;
        if self.len() < min_new_len {
            self.raw.resize_with(min_new_len, fill_value);
        }
    }

    #[inline]
    pub fn resize_to_elem(&mut self, elem: I, fill_value: impl FnMut() -> T) {
        let min_new_len = elem.index() + 1;
        self.raw.resize_with(min_new_len, fill_value);
    }
}

impl<I: Idx, T: Clone> IndexVec<I, T> {
    #[inline]
    pub fn resize(&mut self, new_len: usize, value: T) {
        self.raw.resize(new_len, value);
    }
}

impl<I: Idx, T: Ord> IndexVec<I, T> {
    #[inline]
    pub fn binary_search(&self, value: &T) -> Result<I, I> {
        match self.raw.binary_search(value) {
            Ok(i) => Ok(Idx::new(i)),
            Err(i) => Err(Idx::new(i)),
        }
    }
}

impl<I: Idx, T> Index<I> for IndexVec<I, T> {
    type Output = T;

    #[inline]
    fn index(&self, index: I) -> &T {
        &self.raw[index.index()]
    }
}

impl<I: Idx, T> IndexMut<I> for IndexVec<I, T> {
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut T {
        &mut self.raw[index.index()]
    }
}

impl<I: Idx, T> Default for IndexVec<I, T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Idx, T> Extend<T> for IndexVec<I, T> {
    #[inline]
    fn extend<J: IntoIterator<Item = T>>(&mut self, iter: J) {
        self.raw.extend(iter);
    }
}

impl<I: Idx, T> FromIterator<T> for IndexVec<I, T> {
    #[inline]
    fn from_iter<J>(iter: J) -> Self
    where
        J: IntoIterator<Item = T>,
    {
        IndexVec {
            raw: FromIterator::from_iter(iter),
            _marker: PhantomData,
        }
    }
}

impl<I: Idx, T> IntoIterator for IndexVec<I, T> {
    type Item = T;
    type IntoIter = vec::IntoIter<T>;

    #[inline]
    fn into_iter(self) -> vec::IntoIter<T> {
        self.raw.into_iter()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a IndexVec<I, T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> slice::Iter<'a, T> {
        self.raw.iter()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a mut IndexVec<I, T> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;

    #[inline]
    fn into_iter(self) -> slice::IterMut<'a, T> {
        self.raw.iter_mut()
    }
}
