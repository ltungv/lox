use std::{cell::RefCell, fmt, rc::Rc};

use rustc_hash::FxHashMap;

use crate::{intern, Chunk, StrId, Value};

/// Enumeration of heap-allocated object type.
#[derive(Debug, Clone)]
pub enum Object {
    /// A heap allocated string
    String(Rc<str>),
    /// A closure that can captured surrounding variables
    Closure(Rc<ObjClosure>),
    /// A function object
    Fun(Rc<ObjFun>),
    /// A class object
    Class(Rc<RefCell<ObjClass>>),
    /// A class instance
    Instance(Rc<RefCell<ObjInstance>>),
    /// A class instance
    BoundMethod(Rc<ObjBoundMethod>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::String(s) => write!(f, "{s}"),
            Self::Closure(c) => write!(f, "{c}"),
            Self::Fun(fun) => write!(f, "{fun}"),
            Self::Class(c) => write!(f, "{}", c.borrow()),
            Self::Instance(i) => write!(f, "{}", i.borrow()),
            Self::BoundMethod(m) => write!(f, "{m}"),
        }
    }
}

/// A structure for class instance information
#[derive(Debug)]
pub struct ObjInstance {
    /// The class type of this instance
    pub class: Rc<RefCell<ObjClass>>,
    /// The fields that this instance stores
    pub fields: FxHashMap<StrId, Value>,
}

impl ObjInstance {
    /// Create a new instance of the given class.
    pub fn new(class: Rc<RefCell<ObjClass>>) -> Self {
        Self {
            class,
            fields: FxHashMap::default(),
        }
    }
}

impl fmt::Display for ObjInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{} instance", intern::str(self.class.borrow().name))
    }
}

/// A structure for holding class information
#[derive(Debug)]
pub struct ObjClass {
    /// Class name
    pub name: StrId,
    /// Mapping of all methods defined on the class
    pub methods: FxHashMap<StrId, Value>,
}

impl ObjClass {
    /// Create a new class with the given name
    pub fn new(name: StrId) -> Self {
        Self {
            name,
            methods: FxHashMap::default(),
        }
    }
}

impl fmt::Display for ObjClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{}", intern::str(self.name))
    }
}

/// A class method that is bound to the instance that it was called on
#[derive(Debug)]
pub struct ObjBoundMethod {
    /// Bound instance
    pub receiver: Value,
    /// The closure object of the method
    pub method: Rc<ObjClosure>,
}

impl ObjBoundMethod {
    /// Create a new method bound to the given receiver
    pub fn new(receiver: Value, method: Rc<ObjClosure>) -> Self {
        Self { receiver, method }
    }
}

impl fmt::Display for ObjBoundMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{}", self.method)
    }
}

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

impl ObjClosure {
    /// Create a new closure of the function that captures the variables specified in the list of upvalues
    pub fn new(fun: Rc<ObjFun>, upvalues: Vec<Rc<RefCell<ObjUpvalue>>>) -> Self {
        Self { fun, upvalues }
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

impl ObjFun {
    /// Create a new function of the given name, with its arity set to 0 and its chunk set to the
    /// default value
    pub fn new(name: StrId) -> Self {
        Self {
            name,
            arity: 0,
            chunk: Chunk::default(),
        }
    }
}

impl fmt::Display for ObjFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        let name_str = intern::str(self.name);
        if name_str.is_empty() {
            write!(f, "<script>")
        } else {
            write!(f, "<fn {name_str}>")
        }
    }
}
