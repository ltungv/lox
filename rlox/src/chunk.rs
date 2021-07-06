//! This module deals with chunks of bytecodes.

use std::collections::HashMap;
use std::fmt;

/// Chunk is a sequence of bytecode.
#[derive(Default, Debug)]
pub struct Chunk {
    instructions: Vec<OpCode>,
    constants: Vec<Value>,
    line_run_length: HashMap<usize, usize>,
}

impl Chunk {
    /// Add a new instruction to the chunk.
    pub fn write_instruction(&mut self, code: OpCode, line: usize) {
        self.instructions.push(code);
        *self.line_run_length.entry(line).or_default() += 1;
    }

    /// Read the instruction at the index.
    pub fn read_instruction(&self, idx: usize) -> &OpCode {
        &self.instructions[idx]
    }

    /// Determine the line where the instruction at the given index occured.
    pub fn instruction_line(&self, code_idx: usize) -> usize {
        let mut lines: Vec<_> = self.line_run_length.keys().copied().collect();
        lines.sort_unstable();

        let mut offset = 0;
        for line in &lines {
            offset += self.line_run_length[line];
            if code_idx < offset {
                return *line;
            }
        }
        return lines.last().copied().unwrap_or(0);
    }

    /// Add a constant value to the chunk and return it position in the Vec
    pub fn write_const(&mut self, val: Value) -> u8 {
        self.constants.push(val);
        self.constants.len() as u8 - 1
    }

    /// Read the constant at the given index
    pub fn read_const(&self, idx: u8) -> &Value {
        &self.constants[idx as usize]
    }
}

/// OpCode is a number that specifies the type of the instruction.
#[derive(Debug, Clone)]
#[repr(u8)]
pub enum OpCode {
    /// Load a constant
    Constant(u8),
    /// Return from the current function.
    Return,
    /// Operator that has one operand
    Unary(UnaryOp),
    /// Operator that has two operands
    Binary(BinaryOp),
}

/// All operators that have one operand
#[derive(Debug, Clone)]
#[repr(u8)]
pub enum UnaryOp {
    /// Negate the single operand
    Negate,
}

/// All operators that have two operands
#[derive(Debug, Clone)]
#[repr(u8)]
pub enum BinaryOp {
    /// Add two operands
    Add,
    /// Subtract the first operand with the second operand
    Subtract,
    /// Multiply two operands
    Multiply,
    /// Divide the first operand with the second operand
    Divide,
}

/// This represents a Lox type and its data at.
#[derive(Debug, Clone)]
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

/// Go through the instructions in the chunk and display them in human-readable format.
#[cfg(debug_assertions)]
pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    for i in 0..chunk.instructions.len() {
        disassemble_instruction(chunk, i);
    }
}

/// Display an instruction in human readable format.
#[cfg(debug_assertions)]
pub fn disassemble_instruction(chunk: &Chunk, idx: usize) {
    print!("{:04} ", idx);
    if idx > 0 && chunk.instruction_line(idx) == chunk.instruction_line(idx - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.instruction_line(idx));
    }

    match chunk.instructions[idx] {
        OpCode::Constant(ref idx) => {
            println!("{:-16} {:4} {}", "OP_CONSTANT", idx, chunk.read_const(*idx))
        }
        OpCode::Return => println!("OP_RETURN"),
        OpCode::Unary(ref op) => match op {
            UnaryOp::Negate => println!("OP_NEGATE"),
        },
        OpCode::Binary(ref op) => match op {
            BinaryOp::Add => println!("OP_ADD"),
            BinaryOp::Subtract => println!("OP_SUBTRACT"),
            BinaryOp::Multiply => println!("OP_MULTIPLY"),
            BinaryOp::Divide => println!("OP_DIVIDE"),
        },
    }
}
