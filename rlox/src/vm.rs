use std::fmt;

use crate::{
    compile, disassemble_chunk, disassemble_instruction, Chunk, Error, OpCode, Position, Value,
};

/// Virtual machine errors
#[derive(Debug)]
pub enum RuntimeError {
    /// Pop on an empty stack
    StackUnderflow,
    /// Wrong arguments given to binary operators that only accept numbers
    BinaryNumberOperands(Position),
    /// Wrong arguments given to unary operators that only accept a numbers
    UnaryNumberOperand(Position),
}
impl std::error::Error for RuntimeError {}
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match *self {
            Self::StackUnderflow => {
                writeln!(f, "Virtual machine's stack underflow.")
            }
            Self::BinaryNumberOperands(p) => {
                writeln!(f, "Operands must be numbers.\n{} in script.", p)
            }
            Self::UnaryNumberOperand(p) => {
                writeln!(f, "Operand must be a number.\n{} in script.", p)
            }
        }
    }
}

/// A bytecode virtual machine for the Lox programming language
#[derive(Debug, Default)]
pub struct VM {
    ip: usize,
    stack: Vec<Value>,
}

impl VM {
    /// Load and run the virtual machine on the given chunk
    pub fn interpret(&mut self, src: &str) -> Result<(), Error> {
        let mut chunk = compile(src).ok_or(Error::Compile)?;
        chunk.write_instruction(OpCode::Return, Position::default());

        self.ip = 0;
        self.run(&chunk)?;
        Ok(())
    }

    fn peek(&self, steps: usize) -> Result<&Value, RuntimeError> {
        self.stack
            .get(self.stack.len() - 1 - steps)
            .ok_or(RuntimeError::StackUnderflow)
    }

    fn peek_mut(&mut self, steps: usize) -> Result<&mut Value, RuntimeError> {
        let idx = self.stack.len() - 1 - steps;
        self.stack.get_mut(idx).ok_or(RuntimeError::StackUnderflow)
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.stack.pop().ok_or(RuntimeError::StackUnderflow)
    }

    /// Run the virtual machine with it currently given chunk.
    fn run(&mut self, chunk: &Chunk) -> Result<(), RuntimeError> {
        if cfg!(debug_assertions) {
            disassemble_chunk(&chunk, "code");
            print!("\n\n== execution ==");
        }

        loop {
            if cfg!(debug_assertions) {
                print_stack_trace(&self.stack);
                disassemble_instruction(&chunk, self.ip);
            }

            let (opcode, pos) = chunk.read_instruction(self.ip);
            self.ip += 1;
            match opcode {
                OpCode::Constant(ref idx) => {
                    let val = chunk.read_const(*idx);
                    self.stack.push(val.clone());
                }
                OpCode::Return => {
                    let v = self.pop()?;
                    println!("{}", v);
                    return Ok(());
                }
                OpCode::Negate => {
                    let v = self.peek_mut(0)?;
                    if !v.is_number() {
                        return Err(RuntimeError::UnaryNumberOperand(*pos));
                    }
                    v.negate();
                }
                OpCode::Add => {
                    if !self.peek(0)?.is_number() || !self.peek(1)?.is_number() {
                        return Err(RuntimeError::UnaryNumberOperand(*pos));
                    }
                    let v2 = self.pop()?;
                    let v1 = self.peek_mut(0)?;
                    v1.add(&v2);
                }
                OpCode::Subtract => {
                    if !self.peek(0)?.is_number() || !self.peek(1)?.is_number() {
                        return Err(RuntimeError::UnaryNumberOperand(*pos));
                    }
                    let v2 = self.pop()?;
                    let v1 = self.peek_mut(0)?;
                    v1.subtract(&v2);
                }
                OpCode::Multiply => {
                    if !self.peek(0)?.is_number() || !self.peek(1)?.is_number() {
                        return Err(RuntimeError::UnaryNumberOperand(*pos));
                    }
                    let v2 = self.pop()?;
                    let v1 = self.peek_mut(0)?;
                    v1.multiply(&v2);
                }
                OpCode::Divide => {
                    if !self.peek(0)?.is_number() || !self.peek(1)?.is_number() {
                        return Err(RuntimeError::UnaryNumberOperand(*pos));
                    }
                    let v2 = self.pop()?;
                    let v1 = self.peek_mut(0)?;
                    v1.divide(&v2);
                }
            }
        }
    }
}

#[cfg(debug_assertions)]
fn print_stack_trace(stack: &[Value]) {
    // print stack trace
    print!("          ");
    for val in stack {
        print!("[ {} ]", val);
    }
    println!();
}
