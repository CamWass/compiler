use std::mem::replace;

use crate::{Span, DUMMY_SP};

pub trait Take: Sized {
    fn take(&mut self) -> Self {
        replace(self, Self::dummy())
    }

    /// Create a dummy value of this type.
    fn dummy() -> Self;

    /// Mutate `self` using `op`, which accepts owned data.
    #[inline]
    fn map_with_mut<F>(&mut self, op: F)
    where
        F: FnOnce(Self) -> Self,
    {
        let dummy = Self::dummy();
        let cur_val = replace(self, dummy);
        let new_val = op(cur_val);
        let _dummy = replace(self, new_val);
    }
}

impl<T> Take for Option<T> {
    fn dummy() -> Self {
        None
    }
}

impl<T> Take for Box<T>
where
    T: Take,
{
    fn dummy() -> Self {
        Box::new(T::dummy())
    }
}

impl<T> Take for Vec<T> {
    fn dummy() -> Self {
        vec![]
    }
}

impl Take for Span {
    fn dummy() -> Self {
        DUMMY_SP
    }
}
