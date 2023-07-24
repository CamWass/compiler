use core::hash::BuildHasherDefault;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Index;

use index::vec::Idx;
use indexmap::set::Iter;
use indexmap::IndexSet as ISet;
use rustc_hash::FxHasher;

use super::utils::ReusableState;

#[derive(Debug)]
pub struct IndexSet<I: Idx, T> {
    inner: ISet<T, BuildHasherDefault<FxHasher>>,
    _idx: PhantomData<I>,
}

impl<I: Idx, T> Default for IndexSet<I, T> {
    fn default() -> Self {
        Self {
            inner: ISet::default(),
            _idx: PhantomData,
        }
    }
}

impl<I, T> IndexSet<I, T>
where
    I: Idx,
    T: Eq + Hash,
{
    pub fn insert(&mut self, value: T) -> I {
        I::new(self.inner.insert_full(value).0)
    }

    pub fn contains(&self, value: &T) -> bool {
        self.inner.contains(value)
    }

    pub fn get_index(&self, value: &T) -> Option<I> {
        self.inner.get_index_of(value).map(I::new)
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a IndexSet<I, T> {
    type Item = &'a T;

    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<I: Idx, T> Index<I> for IndexSet<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.inner[index.index()]
    }
}

impl<I, T> ReusableState for IndexSet<I, T>
where
    I: Idx,
    T: Eq + Hash,
{
    fn reset(&mut self) {
        self.inner.clear();
    }
}
