//! Infrastructure for a bytecode virtual machine for the Lox programming language.

#![warn(missing_debug_implementations)]
#![deny(missing_docs)]

use std::fmt::{self, Debug};

mod chunk;
mod compile;
mod scan;
mod token;
mod vm;

pub use chunk::*;
pub use compile::*;
pub use scan::*;
pub use token::*;
pub use vm::*;

/// Lox virtual machine errors
#[derive(Debug)]
pub enum Error {
    /// A runtime error happened
    Runtime(RuntimeError),
    /// A compilation error happened
    Compile(CompileError),
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Runtime(err) => write!(f, "{}", err),
            Self::Compile(err) => write!(f, "{}", err),
        }
    }
}

impl From<CompileError> for Error {
    fn from(err: CompileError) -> Self {
        Self::Compile(err)
    }
}
impl From<RuntimeError> for Error {
    fn from(err: RuntimeError) -> Self {
        Self::Runtime(err)
    }
}
