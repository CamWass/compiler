use std::{hash::BuildHasherDefault, iter::FusedIterator, num::NonZeroU32, ops::Index};

use indexmap::IndexSet;
use rustc_hash::FxHasher;

use super::{ObjectId, Pointer};

#[derive(Debug, Default)]
pub(super) struct UnionStore {
    inner: IndexSet<Union, BuildHasherDefault<FxHasher>>,
}

impl UnionStore {
    pub fn build_union(&mut self, builder: UnionBuilder) -> Option<Pointer> {
        if let Ok(res) = builder.try_build() {
            return res;
        }

        let mut idx = UnionId::from_usize(self.inner.insert_full(builder.union).0);
        if builder.invalid {
            idx.0 |= UnionId::INVALID_FLAG;
        }

        Some(Pointer::Union(idx))
    }
}

impl Index<UnionId> for UnionStore {
    type Output = Union;

    fn index(&self, index: UnionId) -> &Self::Output {
        &self.inner[index.as_usize()]
    }
}

#[derive(Clone, Debug, Default)]
pub(super) struct UnionBuilder {
    union: Union,
    has_null_or_void: bool,
    invalid: bool,
}

impl UnionBuilder {
    /// Tries to build a union without consulting a [`UnionStore`].
    /// This is only possible for simple unions, but helps to avoid acquiring a
    /// write lock if the [`UnionStore`] is within a [`RwLock`][std::sync::RwLock].
    pub fn try_build(&self) -> Result<Option<Pointer>, ()> {
        if let Union::Inline(inline) = &self.union {
            if inline[0].is_none() {
                if self.has_null_or_void && !self.invalid {
                    // NullOrVoid is ignored unless it is the only constituent.
                    return Ok(Some(Pointer::NullOrVoid));
                }
                // Empty union.
                return Ok(None);
            }

            if inline[1].is_none() && !self.invalid {
                // Only one constituent. If the union is valid then we flatten it.
                // If it is invalid, we must still create a union to persist the
                // fact that the constituent was in a union with something invalid.
                return Ok(Some(Pointer::Object(inline[0].unwrap())));
            }
        }
        Err(())
    }

    pub fn add_null_or_void(&mut self) {
        self.has_null_or_void = true;
    }

    pub fn add_object(&mut self, constituent: Option<ObjectId>) {
        match constituent {
            Some(constituent) => {
                match &mut self.union {
                    Union::Heap(heap) => {
                        match heap.binary_search(&constituent) {
                            Ok(_) => {
                                // Already in union
                            }
                            Err(insert_idx) => {
                                heap.insert(insert_idx, constituent);
                            }
                        }
                    }
                    Union::Inline(inline) => {
                        let mut insert_idx = 0;
                        while insert_idx <= MAX_INLINE_SIZE {
                            if insert_idx == MAX_INLINE_SIZE {
                                break;
                            }
                            if inline[insert_idx].is_none() {
                                inline[insert_idx] = Some(constituent);
                                return;
                            }

                            if inline[insert_idx] == Some(constituent) {
                                return;
                            }

                            if inline[insert_idx].unwrap() > constituent {
                                // insert
                                break;
                            }

                            insert_idx += 1;
                        }

                        if inline[MAX_INLINE_SIZE - 1].is_some() || insert_idx == MAX_INLINE_SIZE {
                            // Inline capacity exhausted; swap to heap.
                            let mut heap = Vec::new();
                            heap.reserve_exact(MAX_INLINE_SIZE + 1);
                            for i in 0..insert_idx {
                                heap.push(inline[i].unwrap());
                            }
                            heap.push(constituent);
                            for i in insert_idx..MAX_INLINE_SIZE {
                                heap.push(inline[i].unwrap());
                            }
                            self.union = Union::Heap(heap);
                        } else {
                            inline.copy_within(insert_idx..MAX_INLINE_SIZE - 1, insert_idx + 1);
                            inline[insert_idx] = Some(constituent);
                        }
                    }
                }
            }
            None => {
                self.invalid = true;
            }
        }
    }

    pub fn add(&mut self, constituent: Option<Pointer>, store: &UnionStore) {
        match constituent {
            Some(Pointer::Object(constituent)) => {
                self.add_object(Some(constituent));
            }
            Some(Pointer::Union(union)) => {
                if union.invalid() {
                    self.invalid = true;
                }

                // TODO: this could be smarter/more efficient if necessary.
                for constituent in store[union].constituents() {
                    self.add_object(Some(constituent));
                }
            }
            Some(Pointer::Fn(_)) | None => {
                self.invalid = true;
            }
            Some(Pointer::NullOrVoid) => {
                self.has_null_or_void = true;
            }
        }
    }
}

// Chosen so inline `Union` is same size as its `Heap` variant.
const MAX_INLINE_SIZE: usize = 4;

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub(super) enum Union {
    Heap(Vec<ObjectId>),
    Inline([Option<ObjectId>; MAX_INLINE_SIZE]),
}

impl Default for Union {
    fn default() -> Self {
        Union::Inline([None, None, None, None])
    }
}

// TODO: this is true in rust playground, may require newer nightly compiler.
// See https://play.rust-lang.org/?version=nightly&mode=debug&edition=2021&gist=f869b77427f67327c5ea6ebf82b20e0a
// assert_eq_size!(Union, Vec<ObjectId>);

impl Union {
    pub fn len(&self) -> usize {
        match self {
            Union::Heap(heap) => heap.len(),
            Union::Inline(inline) => {
                let mut len = 0;
                for _ in inline.iter() {
                    len += 1;
                }
                len
            }
        }
    }

    pub fn constituents(&self) -> impl Iterator<Item = ObjectId> + '_ {
        Constituents {
            union: self,
            index: 0,
            len: self.len(),
        }
    }

    pub fn contains(&self, value: ObjectId) -> bool {
        match self {
            Union::Heap(heap) => heap.binary_search(&value).is_ok(),
            Union::Inline(inline) => inline.contains(&Some(value)),
        }
    }
}

pub(super) struct Constituents<'a> {
    union: &'a Union,
    index: usize,
    len: usize,
}

impl<'a> Iterator for Constituents<'a> {
    type Item = ObjectId;
    fn next(&mut self) -> Option<ObjectId> {
        if self.index < self.len {
            match self.union {
                Union::Heap(heap) => {
                    let r = heap.get(self.index);
                    self.index += 1;
                    r.copied()
                }
                Union::Inline(inline) => {
                    let r = inline.get(self.index);
                    self.index += 1;
                    *r.unwrap()
                }
            }
        } else {
            None
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.len))
    }
}

impl FusedIterator for Constituents<'_> {}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub(super) struct UnionId(NonZeroU32);

impl UnionId {
    const INVALID_FLAG: u32 = u32::MAX ^ (u32::MAX >> 1);
    const VALID_BITS_MASK: u32 = !Self::INVALID_FLAG;
    #[inline]
    const fn from_usize(mut value: usize) -> Self {
        value += 1;
        assert!(value & Self::INVALID_FLAG as usize == 0);
        assert!(value != 0);
        unsafe { Self(NonZeroU32::new_unchecked(value as u32)) }
    }

    #[doc = " Extracts the value of this index as a `u32`."]
    #[inline]
    const fn as_u32(self) -> u32 {
        self.0.get() - 1
    }
    #[doc = " Extracts the value of this index as a `usize`."]
    #[inline]
    const fn as_usize(self) -> usize {
        (self.as_u32() & Self::VALID_BITS_MASK) as usize
    }
    #[inline]
    pub const fn invalid(&self) -> bool {
        self.as_u32() & Self::INVALID_FLAG != 0
    }
}
