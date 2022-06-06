use ahash::AHashMap;
use std::hash::Hash;

pub struct IndexSet<T> {
    map: AHashMap<T, usize>,
    vec: Vec<T>,
}

impl<T> IndexSet<T>
where
    T: Eq + Hash + Copy,
{
    /// Insert the value into the set.
    ///
    /// If an equivalent item already exists in the set, the original item and its order are unchanged.
    /// Otherwise, it inserts the new item.
    ///
    /// Computes in O(1) time (amortized average).
    pub fn insert(&mut self, value: T) {
        match self.map.entry(value) {
            std::collections::hash_map::Entry::Occupied(slot) => {
                let idx = *slot.get();
                // TODO: can probably use get_unchecked since we create all of the indices and only allow insertions, not removals.
                // Therefore, we can garentee that all indices are valid.
                self.vec[idx] = value
            }
            std::collections::hash_map::Entry::Vacant(slot) => {
                let idx = self.vec.len();
                self.vec.push(value);
                slot.insert(idx);
            }
        }
    }

    /// Return `true` if an equivalent to `value` exists in the set.
    ///
    /// Computes in O(1) time (average).
    pub fn contains(&self, value: &T) -> bool {
        self.map.contains_key(value)
    }

    pub fn into_ordered_vec(self) -> Vec<T> {
        self.vec
    }
}

impl<T> Default for IndexSet<T> {
    fn default() -> Self {
        Self {
            map: AHashMap::default(),
            vec: Vec::default(),
        }
    }
}
