use core::fmt;
use std::{cell::Cell, fmt::Display, ops::Deref, ptr::NonNull};

/// The on-heap data of `Gc<T>`.
#[derive(Debug, PartialEq)]
#[repr(C)]
struct GcRaw<T: ?Sized> {
    count: Cell<usize>,
    value: T,
}

/// A smart pointer for garbage collected object.
/// Note: This will later be turned into a tracing GC.
/// We now use reference counting for simplicity.
#[derive(Debug)]
pub struct Gc<T: ?Sized> {
    raw: NonNull<GcRaw<T>>,
}

impl<T> Gc<T> {
    pub fn new(value: T) -> Self {
        let value = Box::new(GcRaw {
            count: Cell::new(1),
            value,
        });
        Self {
            raw: NonNull::new(Box::leak(value)).expect("Pointer from Box<T> must be non-null."),
        }
    }
}

impl<T: ?Sized> Gc<T> {
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        this.raw.as_ptr() == other.raw.as_ptr()
    }
}

impl<T: ?Sized> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // SAFETY: self.inner is a raw pointer to a `Box` that is deallocated when the last `Gc`
        // goes away, dereference the shared poninter here is fine since we are having an `Gc`.
        let raw = unsafe { self.raw.as_ref() };
        &raw.value
    }
}

impl<T: ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Self {
        // SAFETY: self.inner is a raw pointer to a `Box` that is deallocated when the last `Gc`
        // goes away, dereference the shared poninter here is fine since we are having an `Gc`.
        let raw = unsafe { self.raw.as_ref() };
        raw.count.set(raw.count.get() + 1);
        Self { raw: self.raw }
    }
}

impl<T: ?Sized> Drop for Gc<T> {
    fn drop(&mut self) {
        // SAFETY: self.inner is a raw pointer to a `Box` that is deallocated when the last `Gc`
        // goes away, dereference the shared poninter here is fine since we are having an `Gc`.
        let raw = unsafe { self.raw.as_ref() };
        let count = raw.count.get();
        if count == 1 {
            // SAFETY: We are dropping the only `Gc` left, after being dropped, there is no more
            // reference to `T`. Hence, deallocating the heap memory is safe.
            unsafe { Box::from_raw(self.raw.as_ptr()) };
        } else {
            raw.count.set(count - 1);
        }
    }
}

impl<T: ?Sized + Eq> Eq for Gc<T> {}

impl<T: ?Sized + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        (**self).eq(&**other)
    }
}

impl<T: ?Sized + Display> Display for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}
