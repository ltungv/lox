use std::collections::HashMap;

use crate::{
    compile, Chunk, Error, OpCode, Position, RuntimeError, StringId, StringInterner, Value,
};

#[cfg(debug_assertions)]
use crate::{disassemble_chunk, disassemble_instruction};

/// We're limiting the stack's size to be in specification with clox
const MAX_STACK_SIZE: usize = 256;

/// A bytecode virtual machine for the Lox programming language
#[derive(Debug)]
pub struct VM {
    ip: usize,
    stack: Vec<Value>,
    globals: HashMap<StringId, Value>,
    strings: StringInterner,
}

impl Default for VM {
    fn default() -> Self {
        Self {
            ip: 0,
            stack: Vec::with_capacity(MAX_STACK_SIZE),
            globals: HashMap::default(),
            strings: StringInterner::default(),
        }
    }
}

impl VM {
    /// Load and run the virtual machine on the given chunk
    pub fn interpret(&mut self, src: &str) -> Result<(), Error> {
        let mut chunk = compile(src, &mut self.strings).ok_or(Error::Compile)?;
        chunk.write_instruction(OpCode::Return, Position::default());

        #[cfg(debug_assertions)]
        disassemble_chunk(&chunk, "code", &self.strings);

        self.ip = 0;
        self.run(&chunk)?;
        Ok(())
    }

    /// Run the virtual machine with it currently given chunk.
    fn run(&mut self, chunk: &Chunk) -> Result<(), RuntimeError> {
        loop {
            #[cfg(debug_assertions)]
            {
                print_stack_trace(&self.stack, &self.strings);
                disassemble_instruction(&chunk, self.ip, &self.strings);
            }

            let (opcode, pos) = chunk.read_instruction(self.ip);
            self.ip += 1;
            match opcode {
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::Print => {
                    let v = self.pop()?;
                    println!("{}", v.as_string(&self.strings));
                }
                OpCode::Return => {
                    // exit the interpreter
                    return Ok(());
                }
                OpCode::DefineGlobal(ref idx) => {
                    let name = chunk.read_const(*idx);
                    if let Value::String(name) = name {
                        let val = self.peek(0)?.clone();
                        self.globals.insert(*name, val);
                        self.pop()?;
                    } else {
                        unreachable!("Constant for the variable name must have been added.");
                    }
                }
                OpCode::GetGlobal(ref idx) => {
                    let name = chunk.read_const(*idx);
                    if let Value::String(name) = name {
                        let val = self
                            .globals
                            .get(name)
                            .ok_or_else(|| {
                                let name = self
                                    .strings
                                    .resolve(*name)
                                    .expect("String for variable name must have been allocated.")
                                    .to_string();
                                RuntimeError::UndefinedVariable(*pos, name)
                            })?
                            .clone();
                        self.push(val)?;
                    } else {
                        unreachable!("Constant for the variable name must have been added.");
                    }
                }
                OpCode::Constant(ref idx) => {
                    let val = chunk.read_const(*idx);
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
                    _ => return Err(RuntimeError::BinaryNumberOperands(*pos)),
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
                    _ => return Err(RuntimeError::BinaryNumberOperands(*pos)),
                },
                OpCode::Less => match (self.peek(0)?, self.peek(1)?) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop()?;
                        let v1 = self.peek_mut(0)?;
                        *v1 = Value::Bool(n1 < n2);
                    }
                    _ => return Err(RuntimeError::BinaryNumberOperands(*pos)),
                },
                OpCode::Add => match (self.peek(0)?, self.peek(1)?) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop()?;
                        let v1 = self.peek_mut(0)?;
                        *v1 = Value::Number(n1 + n2);
                    }
                    (&Value::String(s2), &Value::String(s1)) => {
                        let mut res = String::new();
                        res += self
                            .strings
                            .resolve(s1)
                            .expect("String must be allocated before access.");
                        res += self
                            .strings
                            .resolve(s2)
                            .expect("String must be allocated before access.");
                        let res_id = self.strings.get_or_intern(res);

                        self.pop()?;
                        let v1 = self.peek_mut(0)?;
                        *v1 = Value::String(res_id);
                    }
                    _ => return Err(RuntimeError::InvalidAddOperands(*pos)),
                },
                OpCode::Subtract => match (self.peek(0)?, self.peek(1)?) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop()?;
                        let v1 = self.peek_mut(0)?;
                        *v1 = Value::Number(n1 - n2);
                    }
                    _ => return Err(RuntimeError::BinaryNumberOperands(*pos)),
                },
                OpCode::Multiply => match (self.peek(0)?, self.peek(1)?) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop()?;
                        let v1 = self.peek_mut(0)?;
                        *v1 = Value::Number(n1 * n2);
                    }
                    _ => return Err(RuntimeError::BinaryNumberOperands(*pos)),
                },
                OpCode::Divide => match (self.peek(0)?, self.peek(1)?) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop()?;
                        let v1 = self.peek_mut(0)?;
                        *v1 = Value::Number(n1 / n2);
                    }
                    _ => return Err(RuntimeError::BinaryNumberOperands(*pos)),
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
fn print_stack_trace(stack: &[Value], strings: &StringInterner) {
    // print stack trace
    print!("          ");
    for val in stack {
        print!("[ {} ]", val.as_string(strings));
    }
    println!();
}
