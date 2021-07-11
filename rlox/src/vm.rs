use std::collections::HashMap;

use crate::{intern, Chunk, Compiler, Error, OpCode, Position, RuntimeError, StringId, Value};

#[cfg(debug_assertions)]
use crate::{disassemble_chunk, disassemble_instruction};

/// We're limiting the stack's size to be in specification with clox
pub const MAX_STACK_SIZE: usize = 256;

/// A bytecode virtual machine for the Lox programming language
#[derive(Debug)]
pub struct VM {
    ip: usize,
    stack: Vec<Value>,
    globals: HashMap<StringId, Value>,
}

impl Default for VM {
    fn default() -> Self {
        Self {
            ip: 0,
            stack: Vec::with_capacity(MAX_STACK_SIZE),
            globals: HashMap::default(),
        }
    }
}

impl VM {
    /// Load and run the virtual machine on the given chunk
    pub fn interpret(&mut self, src: &str) -> Result<(), Error> {
        let mut compiler = Compiler::new(src);
        compiler.compile();
        let mut chunk = compiler.finish().ok_or(Error::Compile)?;
        chunk.write_instruction(OpCode::Return, Position::default());

        #[cfg(debug_assertions)]
        disassemble_chunk(&chunk, "code");

        self.ip = 0;
        self.run(&chunk)?;
        Ok(())
    }

    /// Run the virtual machine with it currently given chunk.
    fn run(&mut self, chunk: &Chunk) -> Result<(), RuntimeError> {
        loop {
            #[cfg(debug_assertions)]
            {
                print_stack_trace(&self.stack);
                disassemble_instruction(&chunk, self.ip);
            }

            let (opcode, pos) = chunk.read_instruction(self.ip);
            self.ip += 1;
            match opcode {
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::Jump(offset) => {
                    self.ip += *offset as usize;
                }
                OpCode::JumpIfFalse(offset) => {
                    if self.peek(0)?.is_falsey() {
                        self.ip += *offset as usize;
                    }
                }
                OpCode::Return => {
                    // exit the interpreter
                    return Ok(());
                }
                OpCode::Print => {
                    let v = self.pop()?;
                    println!("{}", v);
                }
                OpCode::GetLocal(ref slot) => {
                    let local = self.stack[*slot as usize].clone();
                    self.push(local)?;
                }
                OpCode::SetLocal(ref slot) => {
                    let val = self.peek(0)?;
                    self.stack[*slot as usize] = val.clone();
                }
                OpCode::DefineGlobal(ref const_id) => {
                    let name = chunk.read_const(*const_id);
                    if let Value::String(name) = name {
                        let val = self.peek(0)?.clone();
                        self.globals.insert(*name, val);
                        self.pop()?;
                    } else {
                        unreachable!("Constant for the variable name must have been added.");
                    }
                }
                OpCode::GetGlobal(ref const_id) => {
                    let name = chunk.read_const(*const_id);
                    if let Value::String(name) = name {
                        let val = self
                            .globals
                            .get(name)
                            .ok_or_else(|| {
                                RuntimeError::UndefinedVariable(*pos, intern::str(*name))
                            })?
                            .clone();
                        self.push(val)?;
                    } else {
                        unreachable!("Constant for the variable name must have been added.");
                    }
                }
                OpCode::SetGlobal(ref const_id) => {
                    let name = chunk.read_const(*const_id);
                    if let Value::String(name) = name {
                        let val = self.peek(0)?.clone();
                        if !self.globals.contains_key(name) {
                            return Err(RuntimeError::UndefinedVariable(*pos, intern::str(*name)));
                        }
                        self.globals.insert(*name, val);
                    } else {
                        unreachable!("Constant for the variable name must have been added.");
                    }
                }
                OpCode::Constant(ref const_id) => {
                    let val = chunk.read_const(*const_id);
                    self.push(val.clone())?;
                }
                OpCode::Nil => self.push(Value::Nil)?,
                OpCode::True => self.push(Value::Bool(true))?,
                OpCode::False => self.push(Value::Bool(false))?,
                OpCode::Not => {
                    let v = self.peek_mut(0)?;
                    *v = Value::Bool(v.is_falsey());
                }
                OpCode::Negate => match self.peek_mut(0)? {
                    Value::Number(v) => {
                        *v = -*v;
                    }
                    _ => return Err(RuntimeError::ExpectedTwoNumbers(*pos)),
                },
                OpCode::Equal => {
                    let v2 = self.pop()?;
                    let v1 = self.peek_mut(0)?;
                    *v1 = Value::Bool(v1.equal(&v2));
                }
                OpCode::Greater => match (self.peek(0)?, self.peek(1)?) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop()?;
                        let v1 = self.peek_mut(0)?;
                        *v1 = Value::Bool(n1 > n2);
                    }
                    _ => return Err(RuntimeError::ExpectedTwoNumbers(*pos)),
                },
                OpCode::Less => match (self.peek(0)?, self.peek(1)?) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop()?;
                        let v1 = self.peek_mut(0)?;
                        *v1 = Value::Bool(n1 < n2);
                    }
                    _ => return Err(RuntimeError::ExpectedTwoNumbers(*pos)),
                },
                OpCode::Add => match (self.peek(0)?, self.peek(1)?) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop()?;
                        let v1 = self.peek_mut(0)?;
                        *v1 = Value::Number(n1 + n2);
                    }
                    (&Value::String(s2), &Value::String(s1)) => {
                        let mut res = intern::str(s1);
                        res += intern::str(s2).as_str();
                        self.pop()?;

                        let v1 = self.peek_mut(0)?;
                        *v1 = Value::String(intern::id(res));
                    }
                    _ => return Err(RuntimeError::InvalidAddOperands(*pos)),
                },
                OpCode::Subtract => match (self.peek(0)?, self.peek(1)?) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop()?;
                        let v1 = self.peek_mut(0)?;
                        *v1 = Value::Number(n1 - n2);
                    }
                    _ => return Err(RuntimeError::ExpectedTwoNumbers(*pos)),
                },
                OpCode::Multiply => match (self.peek(0)?, self.peek(1)?) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop()?;
                        let v1 = self.peek_mut(0)?;
                        *v1 = Value::Number(n1 * n2);
                    }
                    _ => return Err(RuntimeError::ExpectedTwoNumbers(*pos)),
                },
                OpCode::Divide => match (self.peek(0)?, self.peek(1)?) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop()?;
                        let v1 = self.peek_mut(0)?;
                        *v1 = Value::Number(n1 / n2);
                    }
                    _ => return Err(RuntimeError::ExpectedTwoNumbers(*pos)),
                },
            }
        }
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

    fn push(&mut self, val: Value) -> Result<(), RuntimeError> {
        if self.stack.len() == MAX_STACK_SIZE {
            return Err(RuntimeError::StackOverflow);
        }
        self.stack.push(val);
        Ok(())
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.stack.pop().ok_or(RuntimeError::StackUnderflow)
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
