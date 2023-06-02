use std::convert::TryFrom;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};

use rustc_hash::{FxHashMap, FxHasher};

#[derive(Debug)]
pub struct HashableHashMap<K, V>(FxHashMap<K, V>);

impl<K, V> PartialEq<HashableHashMap<K, V>> for HashableHashMap<K, V>
where
    K: Eq + Hash,
    V: PartialEq,
{
    fn eq(&self, other: &HashableHashMap<K, V>) -> bool {
        self.0 == other.0
    }
}

impl<K, V> Eq for HashableHashMap<K, V>
where
    K: Eq + Hash,
    V: Eq,
{
}

impl<K, V> Default for HashableHashMap<K, V> {
    fn default() -> Self {
        Self(FxHashMap::default())
    }
}

impl<K, V> Clone for HashableHashMap<K, V>
where
    K: Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

// Based on https://github.com/rust-lang/rust/pull/48366/
// See link for discussion/design.
impl<K, V> Hash for HashableHashMap<K, V>
where
    K: Hash,
    V: Hash,
{
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        // This hash isn't particularly good, but we only use it to reduce Eq
        // comparisons in set searches, so a few collisions is still an
        // improvement over comparing against all elements in the set.

        // Hash elements
        let mut hash = self
            .0
            .iter()
            .map(|kv| {
                let mut h = FxHasher::default();
                kv.hash(&mut h);
                h.finish()
            })
            .fold(0, u64::wrapping_add);
        // Factor in number of entries.
        // From https://github.com/python/cpython/blob/fa7880604191f81cbdddc191216f7b1e39a74d8d/Objects/setobject.c#L746-L807
        hash ^= u64::try_from(self.0.len()).unwrap() * 1927868237;
        hasher.write_u64(hash);
    }
}

impl<K, V> AsRef<FxHashMap<K, V>> for HashableHashMap<K, V> {
    fn as_ref(&self) -> &FxHashMap<K, V> {
        &self.0
    }
}

impl<K, V> AsMut<FxHashMap<K, V>> for HashableHashMap<K, V> {
    fn as_mut(&mut self) -> &mut FxHashMap<K, V> {
        &mut self.0
    }
}

impl<K, V> Deref for HashableHashMap<K, V> {
    type Target = FxHashMap<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K, V> DerefMut for HashableHashMap<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
