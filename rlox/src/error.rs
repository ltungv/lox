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
    BinaryNumberOperands(Position),
    /// Wrong arguments given to unary operators that only accept a numbers
    UnaryNumberOperand(Position),
    /// Accessing an undefined variable
    UndefinedVariable(Position, String),
}

/// Error while parsing Lox tokens
#[derive(Debug)]
pub enum ParseError {
    /// Current token is not supposed to be there
    UnexpectedToken(Position, Option<String>, String),
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
                writeln!(f, "Virtual machine's stack overflows.")
            }
            Self::StackUnderflow => {
                writeln!(f, "Virtual machine's stack underflows.")
            }
            Self::InvalidAddOperands(p) => {
                writeln!(
                    f,
                    "Operands must be two numbers or two strings.\n{} in script.",
                    p
                )
            }
            Self::BinaryNumberOperands(p) => {
                writeln!(f, "Operands must be numbers.\n{} in script.", p)
            }
            Self::UnaryNumberOperand(p) => {
                writeln!(f, "Operand must be a number.\n{} in script.", p)
            }
            Self::UndefinedVariable(p, name) => {
                writeln!(f, "Undefined variable '{}'.\n{} in script.", name, p)
            }
        }
    }
}

impl std::error::Error for ParseError {}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken(ref pos, ref lexeme, ref msg) => {
                let at = match lexeme {
                    Some(s) => format!("'{}'", s),
                    None => "end".to_string(),
                };
                write!(f, "{} Error at {}: {}.", pos, at, msg)
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
