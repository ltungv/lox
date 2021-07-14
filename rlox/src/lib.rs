//! Infrastructure for a bytecode virtual machine for the Lox programming language.

#![warn(missing_debug_implementations)]
#![deny(missing_docs)]

mod chunk;
mod compile;
mod error;
mod object;
mod scan;
mod token;
mod value;
mod vm;

mod intern;

pub use chunk::*;
pub use compile::*;
pub use error::*;
pub use intern::*;
pub use object::*;
pub use scan::*;
pub use token::*;
pub use value::*;
pub use vm::*;

/// We're limiting the frames's size to be in specification with clox
pub const MAX_FRAMES: usize = 64;

/// We're limiting the stack's size to be in specification with clox
pub const MAX_STACK: usize = u8::MAX as usize * MAX_FRAMES;

/// Maximum number of parameters a function can take, this is u8::MAX - 1 so "this" can be accomodated in bound function
pub const MAX_PARAMS: usize = 255;

/// Maximum number of parameters a function can take
pub const MAX_LOCAL_VARIABLES: usize = 256;

/// Maximum number of parameters a function can take
pub const MAX_CHUNK_CONSTANTS: usize = 256;

/// Maximum number of upvalues a closure can have
pub const MAX_UPVALUES: usize = 256;
