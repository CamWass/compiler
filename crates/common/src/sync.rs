// Copyright 2017 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::cell::{Ref, RefCell, RefMut};
use std::fmt;
use std::{
    cmp::Ordering,
    fmt::{Debug, Formatter},
};

#[derive(Debug)]
pub struct Lock<T>(RefCell<T>);

impl<T> Lock<T> {
    #[inline(always)]
    pub fn new(inner: T) -> Self {
        Lock(RefCell::new(inner))
    }

    #[inline(always)]
    pub fn lock(&self) -> RefMut<'_, T> {
        self.0.borrow_mut()
    }

    #[inline(always)]
    pub fn borrow(&self) -> RefMut<'_, T> {
        self.lock()
    }

    #[inline(always)]
    pub fn borrow_mut(&self) -> RefMut<'_, T> {
        self.lock()
    }
}

impl<T: Default> Default for Lock<T> {
    #[inline]
    fn default() -> Self {
        Lock::new(T::default())
    }
}

impl<T> LockCell<T> {
    #[inline(always)]
    pub fn new(inner: T) -> Self {
        LockCell(Lock::new(inner))
    }

    #[inline(always)]
    pub fn set(&self, new_inner: T) {
        *self.0.lock() = new_inner;
    }

    #[inline(always)]
    pub fn get(&self) -> T
    where
        T: Copy,
    {
        *self.0.lock()
    }
}

impl<T: Copy + Debug> Debug for LockCell<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("LockCell")
            .field("value", &self.get())
            .finish()
    }
}

impl<T: Default> Default for LockCell<T> {
    /// Creates a `LockCell<T>`, with the `Default` value for T.
    #[inline]
    fn default() -> LockCell<T> {
        LockCell::new(Default::default())
    }
}

impl<T: PartialEq + Copy> PartialEq for LockCell<T> {
    #[inline]
    fn eq(&self, other: &LockCell<T>) -> bool {
        self.get() == other.get()
    }
}

impl<T: Eq + Copy> Eq for LockCell<T> {}

impl<T: PartialOrd + Copy> PartialOrd for LockCell<T> {
    #[inline]
    fn partial_cmp(&self, other: &LockCell<T>) -> Option<Ordering> {
        self.get().partial_cmp(&other.get())
    }

    #[inline]
    fn lt(&self, other: &LockCell<T>) -> bool {
        self.get() < other.get()
    }

    #[inline]
    fn le(&self, other: &LockCell<T>) -> bool {
        self.get() <= other.get()
    }

    #[inline]
    fn gt(&self, other: &LockCell<T>) -> bool {
        self.get() > other.get()
    }

    #[inline]
    fn ge(&self, other: &LockCell<T>) -> bool {
        self.get() >= other.get()
    }
}

impl<T: Ord + Copy> Ord for LockCell<T> {
    #[inline]
    fn cmp(&self, other: &LockCell<T>) -> Ordering {
        self.get().cmp(&other.get())
    }
}

#[derive(Debug, Default)]
pub struct RwLock<T>(RefCell<T>);

impl<T> RwLock<T> {
    #[inline(always)]
    pub fn new(inner: T) -> Self {
        RwLock(RefCell::new(inner))
    }

    #[inline(always)]
    pub fn read(&self) -> Ref<'_, T> {
        self.0.borrow()
    }

    #[inline(always)]
    pub fn borrow(&self) -> Ref<'_, T> {
        self.read()
    }

    #[inline(always)]
    pub fn get_mut(&mut self) -> &mut T {
        self.0.get_mut()
    }

    #[inline(always)]
    pub fn with_read_lock<F: FnOnce(&T) -> R, R>(&self, f: F) -> R {
        f(&*self.read())
    }

    #[inline(always)]
    pub fn try_write(&self) -> Result<RefMut<'_, T>, ()> {
        self.0.try_borrow_mut().map_err(|_| ())
    }

    #[inline(always)]
    pub fn write(&self) -> RefMut<'_, T> {
        self.0.borrow_mut()
    }

    #[inline(always)]
    pub fn with_write_lock<F: FnOnce(&mut T) -> R, R>(&self, f: F) -> R {
        f(&mut *self.write())
    }

    #[inline(always)]
    pub fn borrow_mut(&self) -> RefMut<'_, T> {
        self.write()
    }
}

pub struct LockCell<T>(Lock<T>);
