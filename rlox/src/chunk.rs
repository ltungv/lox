//! This module deals with chunks of bytecodes.

use std::fmt;

use crate::Position;

/// Chunk is a sequence of instructions and data that will be written to by the compiler
/// and later run by the virtual-machine.
///
/// # Examples
///
/// ```
/// use rlox::{Chunk, OpCode, Position, Value};
///
/// let mut chunk = Chunk::default();
/// let const_id = chunk.write_const(Value::Number(1.0));
/// assert!(matches!(chunk.read_const(const_id), &Value::Number(1.0)));
///
/// chunk.write_instruction(OpCode::Constant(const_id), Position::default());
/// assert!(matches!(
///     chunk.read_instruction(0),
///     (&OpCode::Constant(cost_id), &Position { line: 1, column : 1 }),
/// ));
/// ```
#[derive(Default, Debug)]
pub struct Chunk {
    instructions: Vec<OpCode>,
    constants: Vec<Value>,
    positions: Vec<Position>,
}

impl Chunk {
    /// Add a new instruction to the chunk.
    pub fn write_instruction(&mut self, code: OpCode, pos: Position) {
        self.instructions.push(code);
        self.positions.push(pos);
    }

    /// Read the instruction at the index.
    pub fn read_instruction(&self, idx: usize) -> (&OpCode, &Position) {
        (&self.instructions[idx], &self.positions[idx])
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
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    /// Load a constant
    Constant(u8),
    /// Load a `nil` value
    Nil,
    /// Load a `true` value
    True,
    /// Load a `false` value
    False,
    /// Return from the current function.
    Return,
    /// Negate a single number operand
    Negate,
    /// Add two number operands
    Add,
    /// Subtract the first operand with the second operand, both operands
    /// must be numbers
    Subtract,
    /// Multiply two number operands
    Multiply,
    /// Divide the first operand with the second operand, both operands
    /// must be numbers
    Divide,
}

/// This represents a Lox type and its data at.
#[derive(Debug, Clone, Copy)]
pub enum Value {
    /// A nothing value in Lox
    Nil,
    /// A boolean value in Lox
    Bool(bool),
    /// A number value in Lox
    Number(f64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match *self {
            Self::Nil => write!(f, "nil"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Number(n) => write!(f, "{}", n),
        }
    }
}

impl Value {
    /// Return true if this is a Lox nil value
    pub fn is_nil(&self) -> bool {
        matches!(self, Self::Nil)
    }

    /// Return true if this is a Lox boolean value
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(_))
    }

    /// Return true if this is a Lox number value
    pub fn is_number(&self) -> bool {
        matches!(self, Self::Number(_))
    }

    /// Add two values.
    ///
    /// # Error
    ///
    /// If this value and the other value are not both numbers or both strings,
    /// a runtime error is return
    pub fn add(&mut self, other: &Value) {
        match (self, other) {
            (Self::Number(v1), Self::Number(v2)) => {
                *v1 += v2;
            }
            _ => panic!("Check values' type before applying the operation."),
        }
    }

    /// Subtract two values.
    ///
    /// # Error
    ///
    /// If this value and the other value are not both numbers, a runtime error is return
    pub fn subtract(&mut self, other: &Value) {
        match (self, other) {
            (Self::Number(v1), Self::Number(v2)) => {
                *v1 -= v2;
            }
            _ => panic!("Check values' type before applying the operation."),
        }
    }

    /// Multiply two values.
    ///
    /// # Error
    ///
    /// If this value and the other value are not both numbers, a runtime error is return
    pub fn multiply(&mut self, other: &Value) {
        match (self, other) {
            (Self::Number(v1), Self::Number(v2)) => {
                *v1 *= v2;
            }
            _ => panic!("Check values' type before applying the operation."),
        }
    }

    /// Divide two values.
    ///
    /// # Error
    ///
    /// If this value and the other value are not both numbers, a runtime error is return
    pub fn divide(&mut self, other: &Value) {
        match (self, other) {
            (Self::Number(v1), Self::Number(v2)) => {
                *v1 /= v2;
            }
            _ => panic!("Check values' type before applying the operation."),
        }
    }

    /// Negate the current value
    ///
    /// # Error
    ///
    /// If this value is not a number, a runtime error is return
    pub fn negate(&mut self) {
        match self {
            Self::Number(v) => {
                *v = -*v;
            }
            _ => panic!("Check values' type before applying the operation."),
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
    if idx > 0 && chunk.positions[idx].line == chunk.positions[idx - 1].line {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.positions[idx].line);
    }

    match chunk.instructions[idx] {
        OpCode::Constant(ref idx) => {
            println!("{:-16} {:4} {}", "OP_CONSTANT", idx, chunk.read_const(*idx))
        }
        OpCode::Nil => println!("OP_NIL"),
        OpCode::True => println!("OP_TRUE"),
        OpCode::False => println!("OP_FALSE"),
        OpCode::Return => println!("OP_RETURN"),
        OpCode::Negate => println!("OP_NEGATE"),
        OpCode::Add => println!("OP_ADD"),
        OpCode::Subtract => println!("OP_SUBTRACT"),
        OpCode::Multiply => println!("OP_MULTIPLY"),
        OpCode::Divide => println!("OP_DIVIDE"),
    }
}
