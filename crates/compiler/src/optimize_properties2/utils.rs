use std::collections::HashSet;

/// Implemented for data structures that can be reused. Generally, this is
/// implemented for types that perform allocations as a way to reuse them.
/// e.g. a for a struct that contains a vec of items, the vec can be cleared to
/// reuse the struct and its allocations.
pub trait ReusableState {
    /// Reset back to initial state, ready for reuse.
    fn reset(&mut self);
}

impl<T, S> ReusableState for HashSet<T, S> {
    fn reset(&mut self) {
        self.clear();
    }
}

/// A stack of [`ReusableState`]s that itself can be reused.
#[derive(Debug, Default)]
pub struct ReusableStateStack<T>
where
    T: ReusableState + Default,
{
    stack: Vec<T>,
    index: Option<usize>,
}

impl<T> ReusableStateStack<T>
where
    T: ReusableState + Default,
{
    /// Create a new entry in the stack, initialised to a default value.
    pub fn push(&mut self) {
        let new_index = self.index.map(|i| i + 1).unwrap_or(0);
        if let Some(old) = self.stack.get_mut(new_index) {
            old.reset();
        } else {
            self.stack.push(T::default());
        }
        self.index = Some(new_index);
    }

    /// Returns a reference to the element on the top of the stack.
    pub fn cur(&mut self) -> &mut T {
        &mut self.stack[self.index.unwrap()]
    }

    /// Updates the head of the stack to point the the previous head.
    pub fn pop(&mut self) {
        self.index = self.index.unwrap().checked_sub(1);
    }
}

impl<T> ReusableState for ReusableStateStack<T>
where
    T: ReusableState + Default,
{
    fn reset(&mut self) {
        let ReusableStateStack { stack, index } = self;
        stack.clear();
        *index = None;
    }
}
