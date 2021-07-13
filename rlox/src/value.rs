use std::{fmt, rc::Rc};

use crate::{intern, ObjClosure, ObjFun, ObjNativeFun, StringId};

/// This represents a Lox type and its data at.
#[derive(Debug, Clone)]
pub enum Value {
    /// A nothing value in Lox
    Nil,
    /// A boolean value in Lox
    Bool(bool),
    /// A number value in Lox
    Number(f64),
    /// A heap allocated string
    ///
    /// # Notes
    ///
    /// To improve memory usage, we should separated string into 2 types, one that owns its
    /// character array and one that is "constant" such that it points to the original source
    /// or some non-freeable location.
    String(StringId),
    /// A native function object
    NativeFun(ObjNativeFun),
    /// A function object
    Closure(Rc<ObjClosure>),
    /// A function object
    Fun(Rc<ObjFun>),
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
            Self::String(id) => write!(f, "{}", intern::str(*id)),
            Self::Closure(c) => write!(f, "{}", c.fun),
            Self::Fun(obj) => write!(f, "{}", obj),
            Self::NativeFun(n) => write!(f, "{}", n),
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
            (Self::String(s1), Self::String(s2)) => s1 == s2,
            _ => false,
        }
    }
}
