use core::fmt;
use std::{fmt::Display, ops::Deref, rc::Rc};

/// A smart pointer for garbage collected object.
/// Note: This will later be turned into a tracing GC.
/// We now use reference counting for simplicity.
#[derive(Debug)]
pub struct Gc<T: ?Sized> {
    raw: Rc<T>,
}

impl<T> Gc<T> {
    pub fn new(value: T) -> Self {
        Self {
            raw: Rc::new(value),
        }
    }
}

impl<T: ?Sized> Gc<T> {
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        Rc::ptr_eq(&this.raw, &other.raw)
    }
}

impl<T: ?Sized> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        self
    }
}

impl<T: ?Sized> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.raw.deref()
    }
}

impl<T: ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self {
            raw: Rc::clone(&self.raw),
        }
    }
}

impl<T: ?Sized + Eq> Eq for Gc<T> {}

impl<T: ?Sized + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        (self.raw).eq(&other.raw)
    }
}

impl<T: ?Sized + Display> Display for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.raw, f)
    }
}

impl From<String> for Gc<str> {
    fn from(value: String) -> Self {
        Self {
            raw: Rc::from(value),
        }
    }
}
