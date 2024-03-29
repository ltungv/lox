use std::{cell::RefCell, fmt, ops};

use crate::{
    gc::Gc, intern, ObjClass, ObjClosure, ObjFun, ObjInstance, Object, RuntimeError, StrId,
};

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
    /// A native function reference
    NativeFun(NativeFun),
    /// A heap-allocated object
    Object(Object),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Number(n) => {
                if n.trunc().eq(n) {
                    write!(f, "{n:.0?}")
                } else {
                    write!(f, "{n:?}")
                }
            }
            Self::Str(s) => write!(f, "{}", intern::str(*s)),
            Self::NativeFun(fun) => write!(f, "{fun}"),
            Self::Object(o) => write!(f, "{o}"),
        }
    }
}

impl ops::Add for &Value {
    type Output = Result<Value, RuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
            (Value::Str(s1), Value::Str(s2)) => {
                let res = intern::str(*s1) + intern::str(*s2).as_str();
                Ok(Value::Object(Object::String(Gc::from(res))))
            }
            (Value::Object(Object::String(s1)), Value::Str(s2)) => {
                let res = s1.to_string() + intern::str(*s2).as_str();
                Ok(Value::Object(Object::String(Gc::from(res))))
            }
            (Value::Str(s1), Value::Object(Object::String(s2))) => {
                let res = intern::str(*s1) + s2;
                Ok(Value::Object(Object::String(Gc::from(res))))
            }
            (Value::Object(Object::String(s1)), Value::Object(Object::String(s2))) => {
                let res = s1.to_string() + s2;
                Ok(Value::Object(Object::String(Gc::from(res))))
            }
            _ => Err(RuntimeError(
                "Operands must be two numbers or two strings".to_string(),
            )),
        }
    }
}

impl ops::Sub for &Value {
    type Output = Result<Value, RuntimeError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 - n2)),
            _ => Err(RuntimeError("Operands must be numbers".to_string())),
        }
    }
}

impl ops::Mul for &Value {
    type Output = Result<Value, RuntimeError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 * n2)),
            _ => Err(RuntimeError("Operands must be numbers".to_string())),
        }
    }
}

impl ops::Div for &Value {
    type Output = Result<Value, RuntimeError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 / n2)),
            _ => Err(RuntimeError("Operands must be numbers".to_string())),
        }
    }
}

impl ops::Not for &Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        Value::Bool(match self {
            Value::Bool(b) => !b,
            Value::Nil => true,
            _ => false,
        })
    }
}

impl ops::Neg for &Value {
    type Output = Result<Value, RuntimeError>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(n) => Ok(Value::Number(-n)),
            _ => Err(RuntimeError("Operand must be a number".to_string())),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Bool(v1), Self::Bool(v2)) => v1 == v2,
            (Self::Number(v1), Self::Number(v2)) => (v1 - v2).abs() < f64::EPSILON,
            (Self::Str(s1), Self::Str(s2)) => s1 == s2,
            (Self::Str(s1), Self::Object(Object::String(s2))) => intern::str(*s1) == s2.as_ref(),
            (Self::NativeFun(f1), Self::NativeFun(f2)) => f1.name == f2.name,
            (Self::Object(Object::String(s1)), Self::Str(s2)) => s1.as_ref() == intern::str(*s2),
            (Self::Object(Object::String(s1)), Self::Object(Object::String(s2))) => s1 == s2,
            (Self::Object(Object::Closure(c1)), Self::Object(Object::Closure(c2))) => {
                Gc::ptr_eq(c1, c2)
            }
            (Self::Object(Object::Fun(f1)), Self::Object(Object::Fun(f2))) => Gc::ptr_eq(f1, f2),
            (Self::Object(Object::Class(c1)), Self::Object(Object::Class(c2))) => {
                Gc::ptr_eq(c1, c2)
            }
            (Self::Object(Object::Instance(i1)), Self::Object(Object::Instance(i2))) => {
                let i1 = i1.borrow();
                let i2 = i2.borrow();
                if !Gc::ptr_eq(&i1.class, &i2.class) {
                    return false;
                }
                if i1.fields.len() != i2.fields.len() {
                    return false;
                }
                for k in i1.fields.keys() {
                    if !i2.fields.contains_key(k) {
                        return false;
                    }
                    if i1.fields[k] != i2.fields[k] {
                        return false;
                    }
                }
                true
            }
            (Self::Object(Object::BoundMethod(b1)), Self::Object(Object::BoundMethod(b2))) => {
                Gc::ptr_eq(b1, b2)
            }
            _ => false,
        }
    }
}

impl Value {
    /// Return true if the value is holding a closure object
    pub fn is_closure(&self) -> bool {
        matches!(self, Value::Object(Object::Closure(_)))
    }

    /// Return true if the value is holding a class object
    pub fn is_class(&self) -> bool {
        matches!(self, Value::Object(Object::Class(_)))
    }

    /// Return true if the value is holding an instance object
    pub fn is_instance(&self) -> bool {
        matches!(self, Value::Object(Object::Instance(_)))
    }

    /// Cast the value as a boolean
    pub fn as_bool(&self) -> bool {
        if let Value::Bool(bool) = self {
            *bool
        } else {
            panic!("Invalid cast")
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

    /// Cast the value as a closure object
    pub fn as_closure(&self) -> &Gc<ObjClosure> {
        if let Value::Object(Object::Closure(closure)) = self {
            closure
        } else {
            panic!("Invalid cast")
        }
    }

    /// Cast the value as a function object
    pub fn as_fun(&self) -> &Gc<ObjFun> {
        if let Value::Object(Object::Fun(fun)) = self {
            fun
        } else {
            panic!("Invalid cast")
        }
    }

    /// Cast the value as a class object
    pub fn as_class(&self) -> &Gc<RefCell<ObjClass>> {
        if let Value::Object(Object::Class(class)) = self {
            class
        } else {
            panic!("Invalid cast")
        }
    }

    /// Cast the value as a instance object
    pub fn as_instance(&self) -> &Gc<RefCell<ObjInstance>> {
        if let Value::Object(Object::Instance(instance)) = self {
            instance
        } else {
            panic!("Invalid cast")
        }
    }

    /// Check if the current value is less than the given value
    pub fn lt(&self, rhs: &Value) -> Result<Value, RuntimeError> {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Bool(n1 < n2)),
            _ => Err(RuntimeError("Operands must be numbers".to_string())),
        }
    }

    /// Check if the current value is greater than the given value
    pub fn gt(&self, rhs: &Value) -> Result<Value, RuntimeError> {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Bool(n1 > n2)),
            _ => Err(RuntimeError("Operands must be numbers".to_string())),
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
