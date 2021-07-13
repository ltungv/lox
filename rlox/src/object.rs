use std::{fmt, rc::Rc};

use crate::{intern, Chunk, StringId, Value};

/// A structure for managing closed-over value
#[derive(Debug)]
pub struct ObjUpvalue {
    /// This field stores a slot offset which points to a value that was closed over
    pub location: usize,
}

/// A function that capter its surrounding environemnt,
#[derive(Debug)]
pub struct ObjClosure {
    /// The base function of this closure
    pub fun: Rc<ObjFun>,
    /// Upvalues for indirect access to closed-over variables
    pub upvalues: Vec<Rc<ObjUpvalue>>,
}

/// A function object that holds the bytecode of the function along with other metadata
#[derive(Debug)]
pub struct ObjFun {
    /// The name of the function
    pub name: StringId,
    /// Number of parameters the function has
    pub arity: u8,
    /// The bytecode chunk of this function
    pub chunk: Chunk,
}

impl fmt::Display for ObjFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        let name_str = intern::str(self.name);
        if name_str.is_empty() {
            write!(f, "<script>")
        } else {
            write!(f, "<fn {}>", name_str)
        }
    }
}

impl Default for ObjFun {
    fn default() -> Self {
        Self {
            name: intern::id(""),
            arity: 0,
            chunk: Chunk::default(),
        }
    }
}

/// A native function
#[derive(Clone)]
pub struct ObjNativeFun {
    /// Number of parameters
    pub arity: u8,
    /// Native function reference
    pub call: fn(&[Value]) -> Value,
}

impl fmt::Display for ObjNativeFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "<native fn>")
    }
}

impl fmt::Debug for ObjNativeFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "<native fn>")
    }
}
