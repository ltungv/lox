use std::{cell::RefCell, fmt, rc::Rc};

use crate::{intern, Chunk, StrId, Value};

/// A structure for managing closed-over value
#[derive(Debug)]
pub enum ObjUpvalue {
    /// This field stores a slot offset which points to a value that was captured
    Open(usize),
    /// This stores the closed over value
    Closed(Value),
}

/// A function that capture its surrounding environemnt,
#[derive(Debug)]
pub struct ObjClosure {
    /// The base function of this closure
    pub fun: Rc<ObjFun>,
    /// Upvalues for indirect access to closed-over variables
    pub upvalues: Vec<Rc<RefCell<ObjUpvalue>>>,
}
impl fmt::Display for ObjClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{}", self.fun)
    }
}

/// A function object that holds the bytecode of the function along with other metadata
#[derive(Debug)]
pub struct ObjFun {
    /// The name of the function
    pub name: StrId,
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
