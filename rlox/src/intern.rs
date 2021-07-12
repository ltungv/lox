use std::cell::RefCell;

use string_interner::{symbol::SymbolU32, DefaultBackend, DefaultHashBuilder};

use crate::MAX_STACK;

/// Default string interner
pub type StringInterner<B = DefaultBackend<StringId>, H = DefaultHashBuilder> =
    string_interner::StringInterner<StringId, B, H>;

/// Interned string id
pub type StringId = SymbolU32;

thread_local! {
    static INTERN: RefCell<StringInterner> = RefCell::new(StringInterner::with_capacity(MAX_STACK));
}

/// Intern a string if it has not been allocated by the global interner,
/// otherwise, returning the existing reference for that string.
pub fn id<S: AsRef<str>>(s: S) -> StringId {
    INTERN.with(|intern| intern.borrow_mut().get_or_intern(s))
}

/// Get the string reference from the global interner using its id.
pub fn str(id: StringId) -> String {
    INTERN.with(|intern| {
        intern
            .borrow_mut()
            .resolve(id)
            .expect("String must be allocated prior to access.")
            .to_string()
    })
}
