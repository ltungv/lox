use string_interner::{symbol::SymbolU32, DefaultBackend, DefaultHashBuilder};

/// Default string interner
pub type StringInterner<B = DefaultBackend<StringId>, H = DefaultHashBuilder> =
    string_interner::StringInterner<StringId, B, H>;

/// Interned string id
pub type StringId = SymbolU32;

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
}

impl Value {
    /// Return true if this is a Lox nil value
    pub fn is_nil(&self) -> bool {
        matches!(self, Self::Nil)
    }

    /// Return true if this is a Lox boolean value
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(_))
    }

    /// Return true if this is a Lox number value
    pub fn is_number(&self) -> bool {
        matches!(self, Self::Number(_))
    }

    /// Return true if this is a heap-allocated Lox value
    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(_))
    }

    /// Return true if the value is `nil` or `false`. Otherwise, return false.
    pub fn is_falsey(&self) -> bool {
        match self {
            Self::Bool(b) => !b,
            Self::Nil => true,
            _ => false,
        }
    }

    /// Get the string representation as this value.
    pub fn as_string(&self, strings: &StringInterner) -> String {
        match self {
            Self::Nil => "nil".to_string(),
            Self::Bool(b) => format!("{}", b),
            Self::Number(n) => format!("{}", n),
            Value::String(id) => strings
                .resolve(*id)
                .expect("String must be allocated before access.")
                .to_string(),
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

    /// Check if the current value is greater than the other one.
    ///
    /// # Error
    ///
    /// If this value and the other value are not both numbers, a runtime error is return
    pub fn greater(&self, other: &Value) -> bool {
        match (self, other) {
            (Self::Number(v1), Self::Number(v2)) => v1 > v2,
            _ => panic!("Check values' type before applying the operation."),
        }
    }

    /// Check if the current value is less than the other one.
    ///
    /// # Error
    ///
    /// If this value and the other value are not both numbers, a runtime error is return
    pub fn less(&self, other: &Value) -> bool {
        match (self, other) {
            (Self::Number(v1), Self::Number(v2)) => v1 < v2,
            _ => panic!("Check values' type before applying the operation."),
        }
    }

    /// Concatenate two strings.
    ///
    /// # Error
    ///
    /// If this value and the other value are not both numbers or both strings,
    /// a runtime error is return
    pub fn concat(&mut self, other: &Value, strings: &mut StringInterner) {
        match (self, other) {
            (Self::String(v1), Self::String(v2)) => {
                let s1 = strings
                    .resolve(*v1)
                    .expect("String must be allocated before access.")
                    .to_string();
                let s2 = strings
                    .resolve(*v2)
                    .expect("String must be allocated before access.")
                    .to_string();
                *v1 = strings.get_or_intern(s1 + s2.as_str());
            }
            _ => panic!("Check values' type before applying the operation."),
        }
    }

    /// Add two values.
    ///
    /// # Error
    ///
    /// If this value and the other value are not both numbers or both strings,
    /// a runtime error is return
    pub fn add(&mut self, other: &Value) {
        match (self, other) {
            (Self::Number(v1), Self::Number(v2)) => {
                *v1 += v2;
            }
            _ => panic!("Check values' type before applying the operation."),
        }
    }

    /// Subtract two values.
    ///
    /// # Error
    ///
    /// If this value and the other value are not both numbers, a runtime error is return
    pub fn subtract(&mut self, other: &Value) {
        match (self, other) {
            (Self::Number(v1), Self::Number(v2)) => {
                *v1 -= v2;
            }
            _ => panic!("Check values' type before applying the operation."),
        }
    }

    /// Multiply two values.
    ///
    /// # Error
    ///
    /// If this value and the other value are not both numbers, a runtime error is return
    pub fn multiply(&mut self, other: &Value) {
        match (self, other) {
            (Self::Number(v1), Self::Number(v2)) => {
                *v1 *= v2;
            }
            _ => panic!("Check values' type before applying the operation."),
        }
    }

    /// Divide two values.
    ///
    /// # Error
    ///
    /// If this value and the other value are not both numbers, a runtime error is return
    pub fn divide(&mut self, other: &Value) {
        match (self, other) {
            (Self::Number(v1), Self::Number(v2)) => {
                *v1 /= v2;
            }
            _ => panic!("Check values' type before applying the operation."),
        }
    }

    /// Negate the current value
    ///
    /// # Error
    ///
    /// If this value is not a number, a runtime error is return
    pub fn negate(&mut self) {
        match self {
            Self::Number(v) => {
                *v = -*v;
            }
            _ => panic!("Check values' type before applying the operation."),
        }
    }
}
