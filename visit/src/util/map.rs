/// Copied from `syntax::ptr::P` of rustc.
pub trait Map<T> {
    /// Produce a new `P<T>` from `self` without reallocating.
    fn map<F>(self, f: F) -> Self
    where
        F: FnOnce(T) -> T;
}

impl<T> Map<T> for Box<T> {
    fn map<F>(mut self, f: F) -> Self
    where
        F: FnOnce(T) -> T,
    {
        *self = f(*self);
        self
    }
}
