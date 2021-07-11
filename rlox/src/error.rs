use std::fmt::{self, Debug};

use crate::Position;

/// Virtual machine errors
#[derive(Debug)]
pub enum RuntimeError {
    /// Pus on an full stack
    StackOverflow,
    /// Pop on an empty stack
    StackUnderflow,
    /// Wrong arguments given to binary add operators that only accept two numbers
    /// or two strings
    InvalidAddOperands(Position),
    /// Wrong arguments given to binary operators that only accept numbers
    ExpectedTwoNumbers(Position),
    /// Wrong arguments given to unary operators that only accept a numbers
    ExpectedOneNumber(Position),
    /// Accessing an undefined variable
    UndefinedVariable(Position, String),
}

/// Error while parsing Lox tokens
#[derive(Debug)]
pub enum ParseError {
    /// Loop body exceeds u16::MAX bytes
    LoopTooLarge(Position, String),
    /// Range to jump over exceeds u16
    JumpTooLarge(Position, String),
    /// Can not use variable name in its initializer
    SelfReferencingInitializer(Position, String),
    /// A named can only be declared as variable once in local scope
    VariableRedeclaration(Position, String),
    /// The number of local variables can not exceed the maximum stack size
    TooManyLocalVariables(Position, String),
    /// Can not assign a value to the LHS
    InvalidAssignTarget(Position, String),
    /// Current token is not supposed to be there
    UnexpectedToken(Position, String, String),
    /// Reached EOF abruptly
    UnexpectedEof,
}

/// Error while scanning Lox source code
#[derive(Debug, Clone)]
pub enum ScanError {
    /// A string literal is unterminated
    UnterminatedString(Position),
    /// Invalid character
    UnexpectedCharacter(Position, char),
}

/// Lox virtual machine errors
#[derive(Debug)]
pub enum Error {
    /// A runtime error happened
    Runtime(RuntimeError),
    /// A compilation error happened
    Compile,
}

impl std::error::Error for RuntimeError {}
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::StackOverflow => {
                write!(f, "Virtual machine's stack overflows.")
            }
            Self::StackUnderflow => {
                write!(f, "Virtual machine's stack underflows.")
            }
            Self::InvalidAddOperands(p) => {
                write!(
                    f,
                    "Operands must be two numbers or two strings.\n{} in script.",
                    p
                )
            }
            Self::ExpectedTwoNumbers(p) => {
                write!(f, "Operands must be numbers.\n{} in script.", p)
            }
            Self::ExpectedOneNumber(p) => {
                write!(f, "Operand must be a number.\n{} in script.", p)
            }
            Self::UndefinedVariable(p, name) => {
                write!(f, "Undefined variable '{}'.\n{} in script.", name, p)
            }
        }
    }
}

impl std::error::Error for ParseError {}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let at = |s: &str| {
            if s.is_empty() {
                "end".to_string()
            } else {
                format!("'{}'", s)
            }
        };
        match self {
            Self::LoopTooLarge(p, lexeme) => {
                write!(
                    f,
                    "{} Error at {}: Too much code to jump over.",
                    p,
                    at(lexeme)
                )
            }
            Self::JumpTooLarge(p, lexeme) => {
                write!(
                    f,
                    "{} Error at {}: Too much code to jump over.",
                    p,
                    at(lexeme)
                )
            }
            Self::SelfReferencingInitializer(p, lexeme) => {
                write!(
                    f,
                    "{} Error at {}: Can't read local variable in its own initializer.",
                    p,
                    at(lexeme)
                )
            }
            Self::VariableRedeclaration(p, lexeme) => {
                write!(
                    f,
                    "{} Error at {}: Already variable with this name in this scope.",
                    p,
                    at(lexeme)
                )
            }
            Self::TooManyLocalVariables(p, lexeme) => {
                write!(
                    f,
                    "{} Error at {}: Too many local variables in function.",
                    p,
                    at(lexeme),
                )
            }
            Self::InvalidAssignTarget(p, lexeme) => {
                write!(
                    f,
                    "{} Error at {}: Invalid assignment target.",
                    p,
                    at(lexeme)
                )
            }
            Self::UnexpectedToken(ref pos, ref lexeme, ref msg) => {
                write!(f, "{} Error at {}: {}.", pos, at(lexeme), msg)
            }
            Self::UnexpectedEof => write!(f, "Error: Unexpected end of file."),
        }
    }
}

impl std::error::Error for ScanError {}
impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnterminatedString(pos) => write!(f, "{} Error: Unterminated string.", pos),
            Self::UnexpectedCharacter(pos, c) => {
                write!(f, "{} Error: Unexpected character '{}'.", pos, c)
            }
        }
    }
}

impl std::error::Error for Error {}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Runtime(err) => write!(f, "{}", err),
            Self::Compile => write!(f, "Compilation errors."),
        }
    }
}

impl From<RuntimeError> for Error {
    fn from(err: RuntimeError) -> Self {
        Self::Runtime(err)
    }
}
