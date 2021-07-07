//! Infrastructure for a bytecode virtual machine for the Lox programming language.

#![warn(missing_debug_implementations)]
#![deny(missing_docs)]

use crate::scan::{ScanError, Scanner};

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

/// Load and run the virtual machine on the given chunk
pub fn interpret(src: &str) {
    compile(src);
}

/// Compile the given source code in to bytecodes that can be read
/// by the virtual machine
pub fn compile(src: &str) {
    let mut s = Scanner::new(src);
    loop {
        match s.scan() {
            Ok(t) => {
                if t.is_none() {
                    break;
                }
                let t = t.unwrap();
                println!(
                    "{:4},{:<4} | {:?} '{:}'",
                    t.pos.line, t.pos.column, t.typ, t.lexeme
                );
            }
            Err(err) => match err {
                ScanError::UnterminatedString(pos) => {
                    eprintln!("[{},{}] Error: Unterminated string.", pos.line, pos.column)
                }
                ScanError::UnexpectedCharacter(pos, c) => {
                    eprintln!(
                        "[{},{}] Error: Unexpected character '{}'.",
                        pos.line, pos.column, c
                    )
                }
            },
        }
    }
}
