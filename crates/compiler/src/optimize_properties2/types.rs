use std::{hash::BuildHasherDefault, iter::FusedIterator, num::NonZeroU32, ops::Index};

use atoms::{js_word, JsWord};
use index::{bit_set::GrowableBitSet, vec::Idx};
use indexmap::IndexSet;
use rustc_hash::FxHasher;

use super::Pointer;

index::newtype_index!(pub(super) struct ObjectId { .. });

impl ObjectId {
    pub fn is_built_in(&self) -> bool {
        self.private < ObjectStore::BUILT_INS.len() as u32
    }
}

#[derive(Debug)]
pub(super) struct ObjectStore {
    cur_object_id: ObjectId,
}

pub(super) struct BuiltIn {
    pub id: ObjectId,
    pub properties: &'static [JsWord],
}

macro_rules! create_constants {
    [$id:expr,] => {};
    [$id:expr, $(#[$attr:meta])* $name:ident, $($tail:tt)*] => {
        $(#[$attr])*
        pub const $name: ObjectId = ObjectId::from_u32($id);
        create_constants![$id + 1u32, $($tail)*];
    };
}

macro_rules! create_impl {
    [$($(#[$attr:meta])* $name:ident : [$($prop:expr$(,)?)*] $(,)?)*] => {
        impl ObjectStore {
            pub const BUILT_INS: &[BuiltIn] = &[
                $(
                    BuiltIn { id: ObjectStore::$name, properties: &[$($prop,)*] },
                )*
            ];
            create_constants![0u32, $($(#[$attr])* $name,)*];
        }
    };
}

create_impl![
    /// Placeholder for an unresolved call during call resolution.
    RESOLVING_CALL: [],
    NUMBER: [
        js_word!("constructor"),
        js_word!("toExponential"),
        js_word!("toFixed"),
        js_word!("toLocaleString"),
        js_word!("toPrecision"),
        js_word!("toString"),
        js_word!("valueOf"),
    ],
    STRING: [
        js_word!("constructor"),
        js_word!("length"),
        js_word!("at"),
        js_word!("charAt"),
        js_word!("charCodeAt"),
        js_word!("codePointAt"),
        js_word!("concat"),
        js_word!("endsWith"),
        js_word!("includes"),
        js_word!("indexOf"),
        js_word!("isWellFormed"),
        js_word!("lastIndexOf"),
        js_word!("localeCompare"),
        js_word!("match"),
        js_word!("matchAll"),
        js_word!("normalize"),
        js_word!("padEnd"),
        js_word!("padStart"),
        js_word!("repeat"),
        js_word!("replace"),
        js_word!("replaceAll"),
        js_word!("search"),
        js_word!("slice"),
        js_word!("split"),
        js_word!("startsWith"),
        js_word!("substr"),
        js_word!("substring"),
        js_word!("toLocaleLowerCase"),
        js_word!("toLocaleUpperCase"),
        js_word!("toLowerCase"),
        js_word!("toString"),
        js_word!("toUpperCase"),
        js_word!("toWellFormed"),
        js_word!("trim"),
        js_word!("trimEnd"),
        js_word!("trimStart"),
        js_word!("valueOf"),
    ],
    BOOL: [
        js_word!("constructor"),
        js_word!("toString"),
        js_word!("valueOf"),
    ],
    BIG_INT: [
        js_word!("constructor"),
        js_word!("toLocaleString"),
        js_word!("toString"),
        js_word!("valueOf"),
    ],
];

impl ObjectStore {
    pub fn new() -> Self {
        Self {
            cur_object_id: ObjectId::from_u32(Self::BUILT_INS.len() as u32),
        }
    }

    pub fn next_object_id(&mut self) -> ObjectId {
        let id = self.cur_object_id;
        self.cur_object_id.increment_by(1);
        id
    }
}

#[derive(Debug, Default)]
pub(super) struct UnionStore {
    inner: IndexSet<Union, BuildHasherDefault<FxHasher>>,
}

impl UnionStore {
    pub fn build_union(&mut self, builder: UnionBuilder) -> Option<Pointer> {
        if let Union::Inline(inline) = &builder.union {
            if inline[0].is_none() {
                if builder.has_null_or_void && !builder.invalid {
                    // NullOrVoid is ignored unless it is the only constituent.
                    return Some(Pointer::NullOrVoid);
                }
                // Empty union.
                return None;
            }

            if inline[1].is_none() {
                // Only one constituent.
                if !builder.invalid {
                    // Union is valid; flatten it.
                    return Some(Pointer::Object(inline[0].unwrap()));
                }
                // Invalid; we must still create a union to persist the fact that the
                // constituent was in a union with something invalid.
            }

            if builder.invalid
                && inline.iter().all(|c| match c {
                    Some(c) => c.is_built_in(),
                    None => true,
                })
            {
                // Invalid, but the constituents are all primitive (whose properties
                // are invalid by default), so no need to record their union with an
                // invalid object.
                return None;
            }
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
    pub fn add_null_or_void(&mut self) {
        self.has_null_or_void = true;
    }

    pub fn add_object(
        &mut self,
        constituent: Option<ObjectId>,
        invalid_objects: &GrowableBitSet<ObjectId>,
    ) {
        match constituent {
            Some(constituent) => {
                if invalid_objects.contains(constituent) {
                    self.invalid = true;
                    return;
                }
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

    pub fn add(
        &mut self,
        constituent: Option<Pointer>,
        store: &UnionStore,
        invalid_objects: &GrowableBitSet<ObjectId>,
    ) {
        match constituent {
            Some(Pointer::Object(constituent)) => {
                self.add_object(Some(constituent), invalid_objects);
            }
            Some(Pointer::Union(union)) => {
                if union.invalid() {
                    self.invalid = true;
                }

                // TODO: this could be smarter/more efficient if necessary.
                for constituent in store[union].constituents() {
                    self.add_object(Some(constituent), invalid_objects);
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
            Union::Inline(inline) => inline.iter().filter(|c| c.is_some()).count(),
        }
    }

    // TODO: return slice
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

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(super) struct UnionId(NonZeroU32);

impl std::fmt::Debug for UnionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("UnionId").field(&self.as_usize()).finish()
    }
}

impl UnionId {
    const INVALID_FLAG: u32 = u32::MAX ^ (u32::MAX >> 1);
    const VALID_BITS_MASK: u32 = !Self::INVALID_FLAG;
    // TODO: temp pub
    #[inline]
    pub const fn from_usize(mut value: usize) -> Self {
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
