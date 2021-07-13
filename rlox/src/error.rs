use std::fmt::{self, Debug};

use crate::Position;

/// Lox virtual machine errors
#[derive(Debug)]
pub enum Error {
    /// A runtime error happened
    Runtime,
    /// A compilation error happened
    Compile,
}

/// Virtual machine errors
#[derive(Debug)]
pub enum RuntimeError {
    /// Push on an full stack
    StackOverflow,
    /// Make call on unsupported object
    InvalidCall(String),
    /// Operand(s) given to an opcode is invalid
    InvalidOperand(String),
    /// Accessing an undefined variable
    UndefinedVariable(String),
}

/// Error while scanning Lox source code
#[derive(Debug, Clone)]
pub enum ScanError {
    /// A string literal is unterminated
    UnterminatedString(Position),
    /// Invalid character
    UnexpectedCharacter(Position, char),
}

impl std::error::Error for Error {}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Runtime => write!(f, "Runtime error(s) occured"),
            Self::Compile => write!(f, "Compilation error(s) occured."),
        }
    }
}

impl std::error::Error for RuntimeError {}
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::StackOverflow => {
                write!(f, "Stack overflow.")
            }
            Self::InvalidCall(ref msg) => {
                write!(f, "{}.", msg)
            }
            Self::InvalidOperand(ref msg) => {
                write!(f, "{}.", msg)
            }
            Self::UndefinedVariable(ref name) => {
                write!(f, "Undefined variable '{}'.", name)
            }
        }
    }
}

impl std::error::Error for ScanError {}
impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnterminatedString(ref pos) => write!(f, "{} Error: Unterminated string.", pos),
            Self::UnexpectedCharacter(ref pos, ref c) => {
                write!(f, "{} Error: Unexpected character '{}'.", pos, c)
            }
        }
    }
}
