//! This module deals with chunks of bytecodes.

use std::collections::HashMap;
use std::fmt;
use std::io;

/// Chunk is a sequence of bytecode.
#[derive(Default, Debug)]
pub struct Chunk {
    codes: Vec<OpCode>,
    constants: Vec<Value>,
    line_run_length: HashMap<usize, usize>,
}

impl Chunk {
    /// Add a new instruction to the chunk.
    pub fn write(&mut self, code: OpCode, line: usize) {
        self.codes.push(code);
        *self.line_run_length.entry(line).or_default() += 1;
    }

    /// Add a constant value to the chunk and return it position in the Vec
    pub fn add_const(&mut self, val: Value) -> u8 {
        self.constants.push(val);
        self.constants.len() as u8 - 1
    }

    /// Determine the line where the instruction at the given index occured.
    pub fn get_line(&self, code_idx: usize) -> usize {
        let mut lines: Vec<_> = self.line_run_length.keys().copied().collect();
        lines.sort_unstable();

        let mut offset = 0;
        for line in &lines {
            offset += self.line_run_length[line];
            if code_idx < offset {
                return *line;
            }
        }
        return lines.last().copied().unwrap();
    }

    /// Go through the currently held bytecodes and display them in human-readable format.
    pub fn disassemble<W: io::Write>(&self, name: &str, mut w: W) -> io::Result<()> {
        writeln!(w, "== {} ==", name)?;
        let mut offset = 0;
        for (i, code) in self.codes.iter().enumerate() {
            write!(w, "{:04} ", offset)?;
            if i > 0 && self.get_line(i) == self.get_line(i - 1) {
                print!("   | ");
            } else {
                print!("{:4} ", self.get_line(i));
            }

            match *code {
                OpCode::Return => writeln!(w, "OP_RETURN")?,
                OpCode::Constant(idx) => writeln!(
                    w,
                    "{:-16} {:4} {}",
                    "OP_CONSTANT", idx, self.constants[idx as usize]
                )?,
            }
            offset += std::mem::size_of_val(code);
        }
        Ok(())
    }
}

/// OpCode is a number that specifies the type of the instruction.
#[derive(Debug)]
#[repr(u8)]
pub enum OpCode {
    /// Return from the current function.
    Return,
    /// Loads a constants
    Constant(u8),
}

/// This represents a Lox type and its data at.
#[derive(Debug)]
pub enum Value {
    /// A number value in Lox
    Number(f64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match *self {
            Self::Number(n) => write!(f, "{}", n),
        }
    }
}
