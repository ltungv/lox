use std::cell::RefCell;

use string_interner::{symbol::SymbolU32, DefaultBackend, DefaultHashBuilder};

use crate::MAX_STACK;

/// Default string interner
pub type StringInterner<B = DefaultBackend<StrId>, H = DefaultHashBuilder> =
    string_interner::StringInterner<StrId, B, H>;

/// Interned string id
pub type StrId = SymbolU32;

// This idea was taken from
// https://github.com/anellie/cloxrs/blob/main/src/interner.rs
thread_local! {
    static INTERN: RefCell<StringInterner> = RefCell::new(StringInterner::with_capacity(MAX_STACK));
}

/// Intern a string if it has not been allocated by the global interner,
/// otherwise, returning the existing reference for that string.
pub fn id<S: AsRef<str>>(s: S) -> StrId {
    INTERN.with(|intern| intern.borrow_mut().get_or_intern(s))
}

/// Get the string reference from the global interner using its id.
pub fn str(id: StrId) -> String {
    INTERN.with(|intern| {
        intern
            .borrow_mut()
            .resolve(id)
            .expect("Preallocated")
            .to_string()
    })
}
