use std::{cell::RefCell, fmt, rc::Rc};

use crate::{intern, ObjBoundMethod, ObjClass, ObjClosure, ObjFun, ObjInstance, StrId};

/// This represents a Lox type and its data at.
#[derive(Debug, Clone)]
pub enum Value {
    /// A nothing value in Lox
    Nil,
    /// A boolean value in Lox
    Bool(bool),
    /// A number value in Lox
    Number(f64),
    /// A constant hashed string
    Str(StrId),
    /// A heap allocated string
    String(Rc<str>),
    /// A native function reference
    NativeFun(NativeFun),
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Number(n) => {
                if n.trunc().eq(n) {
                    write!(f, "{:.0?}", n)
                } else {
                    write!(f, "{:?}", n)
                }
            }
            Self::Str(s) => write!(f, "{}", intern::str(*s)),
            Self::String(s) => write!(f, "{}", s),
            Self::NativeFun(fun) => write!(f, "{}", fun),
            Self::Closure(c) => write!(f, "{}", c),
            Self::Fun(fun) => write!(f, "{}", fun),
            Self::Class(c) => write!(f, "{}", c.borrow()),
            Self::Instance(i) => write!(f, "{}", i.borrow()),
            Self::BoundMethod(m) => write!(f, "{}", m),
        }
    }
}

impl Value {
    /// Return true if the value is `nil` or `false`. Otherwise, return false.
    pub fn is_falsey(&self) -> bool {
        match self {
            Self::Bool(b) => !b,
            Self::Nil => true,
            _ => false,
        }
    }

    /// Check for equality between two values of the same type. If the operands are of different
    /// types, return `false`.
    pub fn equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Bool(v1), Self::Bool(v2)) => v1 == v2,
            (Self::Number(v1), Self::Number(v2)) => (v1 - v2).abs() < f64::EPSILON,
            (Self::Str(s1), Self::Str(s2)) => s1 == s2,
            (Self::String(s1), Self::Str(s2)) => s1.as_ref() == intern::str(*s2),
            (Self::Str(s1), Self::String(s2)) => intern::str(*s1) == s2.as_ref(),
            (Self::String(s1), Self::String(s2)) => s1 == s2,
            (Self::NativeFun(f1), Self::NativeFun(f2)) => f1.name == f2.name,
            (Self::Closure(c1), Self::Closure(c2)) => Rc::ptr_eq(c1, c2),
            (Self::Fun(f1), Self::Fun(f2)) => Rc::ptr_eq(f1, f2),
            (Self::Class(c1), Self::Class(c2)) => Rc::ptr_eq(c1, c2),
            (Self::Instance(i1), Self::Instance(i2)) => {
                let i1 = i1.borrow();
                let i2 = i2.borrow();
                if !Rc::ptr_eq(&i1.class, &i2.class) {
                    return false;
                }
                if i1.fields.len() != i2.fields.len() {
                    return false;
                }
                for k in i1.fields.keys() {
                    if !i2.fields.contains_key(k) {
                        return false;
                    }
                    if !i1.fields[k].equal(&i2.fields[k]) {
                        return false;
                    }
                }
                true
            }
            (Self::BoundMethod(b1), Self::BoundMethod(b2)) => Rc::ptr_eq(b1, b2),
            _ => false,
        }
    }

    /// Cast the value as a constant string
    pub fn as_str(&self) -> &StrId {
        if let Value::Str(str) = self {
            str
        } else {
            panic!("Invalid cast")
        }
    }
}

/// A native function
#[derive(Clone)]
pub struct NativeFun {
    /// Function's name
    pub name: StrId,
    /// Number of parameters
    pub arity: u8,
    /// Native function reference
    pub call: fn(&[Value]) -> Value,
}

impl fmt::Display for NativeFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "<native fn>")
    }
}

impl fmt::Debug for NativeFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "<native fn>")
    }
}
