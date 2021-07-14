use std::{fmt, rc::Rc};

use crate::{intern, ObjClass, ObjClosure, ObjFun, StrId};

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
    Class(Rc<ObjClass>),
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
            Self::Class(c) => write!(f, "{}", c),
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
            _ => false,
        }
    }
}

/// A native function
#[derive(Clone)]
pub struct NativeFun {
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
