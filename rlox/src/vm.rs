use std::fmt;

use crate::{
    compile, disassemble_chunk, disassemble_instruction, BinaryOp, Chunk, Error, OpCode, Position,
    UnaryOp, Value,
};

/// Virtual machine errors
#[derive(Debug)]
pub enum RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "RuntimeError")
    }
}

/// A bytecode virtual machine for the Lox programming language
#[derive(Debug, Default)]
pub struct VM {
    chunk: Option<Chunk>,
    ip: usize,
    stack: Vec<Value>,
}

impl VM {
    /// Load and run the virtual machine on the given chunk
    pub fn interpret(&mut self, src: &str) -> Result<(), Error> {
        let mut chunk = compile(src).ok_or(Error::Compile)?;
        chunk.write_instruction(OpCode::Return, Position::default());

        self.chunk = Some(chunk);
        self.ip = 0;
        self.run()?;
        Ok(())
    }

    /// Run the virtual machine with it currently given chunk.
    fn run(&mut self) -> Result<(), RuntimeError> {
        let chunk = match self.chunk {
            Some(ref c) => c,
            None => return Ok(()),
        };

        if cfg!(debug_assertions) {
            disassemble_chunk(&chunk, "code");
        }

        loop {
            if cfg!(debug_assertions) {
                print_stack_trace(&self.stack);
                disassemble_instruction(&chunk, self.ip);
            }

            let opcode = chunk.read_instruction(self.ip);
            self.ip += 1;
            match opcode {
                OpCode::Constant(ref idx) => {
                    let val = chunk.read_const(*idx);
                    self.stack.push(val.clone());
                }
                OpCode::Return => {
                    if let Some(val) = self.stack.pop() {
                        println!("{}", val);
                    }
                    return Ok(());
                }
                OpCode::Unary(ref op) => {
                    if let Some(val) = self.stack.pop() {
                        match (op, val) {
                            (UnaryOp::Negate, Value::Number(n)) => {
                                self.stack.push(Value::Number(-n))
                            }
                        }
                    }
                }
                OpCode::Binary(ref op) => {
                    if let (Some(v2), Some(v1)) = (self.stack.pop(), self.stack.pop()) {
                        // TODO: match on values when there's more value types
                        let (Value::Number(n1), Value::Number(n2)) = (v1, v2);
                        match op {
                            BinaryOp::Add => self.stack.push(Value::Number(n1 + n2)),
                            BinaryOp::Subtract => self.stack.push(Value::Number(n1 - n2)),
                            BinaryOp::Multiply => self.stack.push(Value::Number(n1 * n2)),
                            BinaryOp::Divide => self.stack.push(Value::Number(n1 / n2)),
                        }
                    }
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
