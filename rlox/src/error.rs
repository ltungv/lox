use std::fmt::{self, Debug};

use crate::Position;

/// Virtual machine errors
#[derive(Debug)]
pub enum RuntimeError {
    /// Push on an full stack
    StackOverflow,
    /// Pop on an empty stack
    StackUnderflow,
    /// Operand(s) given to an opcode is invalid
    InvalidOperand(Position, &'static str),
    /// Accessing an undefined variable
    UndefinedVariable(Position, String),
}

/// Error while parsing Lox tokens
#[derive(Debug)]
pub enum ParseError {
    /// Exceeds limits set by Lox specifications
    JumpTooLarge(Position, String, &'static str),
    /// Violations of declaration semantics
    InvalidDeclaration(Position, String, &'static str),
    /// Current token is not supposed to be there
    UnexpectedToken(Position, String, &'static str),
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
            Self::InvalidOperand(ref p, ref msg) => {
                write!(f, "{}.\n{} in script.", msg, p)
            }
            Self::UndefinedVariable(ref p, ref name) => {
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
            Self::JumpTooLarge(ref p, ref lexeme, ref msg) => {
                write!(f, "{} Error at {}: {}.", p, at(lexeme), msg,)
            }
            Self::InvalidDeclaration(ref p, ref lexeme, ref msg) => {
                write!(f, "{} Error at {}: {}.", p, at(lexeme), msg,)
            }
            Self::UnexpectedToken(ref pos, ref lexeme, ref msg) => {
                write!(f, "{} Error at {}: {}.", pos, at(lexeme), msg)
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
