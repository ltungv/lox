//! Infrastructure for a bytecode virtual machine for the Lox programming language.

#![warn(missing_debug_implementations)]
#![deny(missing_docs)]

mod chunk;
mod compile;
mod scan;
mod token;
mod vm;

use std::fmt;

pub use chunk::*;
pub use compile::*;
pub use scan::*;
pub use token::*;
pub use vm::*;

/// Result type for the interpreter
pub type Result<T> = std::result::Result<T, Error>;

/// Error type for the interpreter
#[derive(Debug)]
pub enum Error {
    /// Compile error
    Compile(CompileError),
    /// Runtime error
    Runtime(RuntimeError),
}

impl Error {
    /// Create a compile error
    pub fn compile(line: usize, message: String) -> Self {
        Self::Compile(CompileError { line, message })
    }

    /// Create a runtime error
    pub fn runtime(line: usize, message: String) -> Self {
        Self::Runtime(RuntimeError { line, message })
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Compile(e) => write!(f, "{}", e),
            Self::Runtime(e) => write!(f, "{}", e),
        }
    }
}

/// Diagnostic data for reporting a compile error
#[derive(Debug)]
pub struct CompileError {
    line: usize,
    message: String,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "[line {}] Error: {}", self.line, self.message)
    }
}

/// Diagnostic data for reporting a runtime error
#[derive(Debug)]
pub struct RuntimeError {
    line: usize,
    message: String,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "[line {}] Error: {}", self.line, self.message)
    }
}
