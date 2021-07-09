use crate::{
    compile, disassemble_chunk, disassemble_instruction, Chunk, Error, OpCode, Position,
    RuntimeError, StringInterner, Value,
};

/// We're limiting the stack's size to be in specification with clox
pub const MAX_STACK_SIZE: usize = 256;

/// A bytecode virtual machine for the Lox programming language
#[derive(Debug)]
pub struct VM {
    ip: usize,
    stack: Vec<Value>,
}

impl Default for VM {
    fn default() -> Self {
        Self {
            ip: 0,
            stack: Vec::with_capacity(MAX_STACK_SIZE),
        }
    }
}

impl VM {
    /// Load and run the virtual machine on the given chunk
    pub fn interpret(&mut self, src: &str) -> Result<(), Error> {
        let (mut chunk, mut strings) = compile(src).ok_or(Error::Compile)?;
        chunk.write_instruction(OpCode::Return, Position::default());

        self.ip = 0;
        self.run(&chunk, &mut strings)?;
        Ok(())
    }

    /// Run the virtual machine with it currently given chunk.
    fn run(&mut self, chunk: &Chunk, strings: &mut StringInterner) -> Result<(), RuntimeError> {
        if cfg!(debug_assertions) {
            disassemble_chunk(&chunk, "code", strings);
            print!("\n\n== execution ==");
        }

        loop {
            if cfg!(debug_assertions) {
                print_stack_trace(&self.stack, strings);
                disassemble_instruction(&chunk, self.ip, strings);
            }

            let (opcode, pos) = chunk.read_instruction(self.ip);
            self.ip += 1;
            match opcode {
                OpCode::Constant(ref idx) => {
                    let val = chunk.read_const(*idx);
                    self.push(val.clone())?;
                }
                OpCode::Nil => self.push(Value::Nil)?,
                OpCode::True => self.push(Value::Bool(true))?,
                OpCode::False => self.push(Value::Bool(false))?,
                OpCode::Return => {
                    let v = self.pop()?;
                    println!("{}", v.as_string(strings));
                    return Ok(());
                }
                OpCode::Not => {
                    self.apply_unary_op(
                        pos,
                        |v| {
                            *v = Value::Bool(v.is_falsey());
                        },
                        |_| true,
                        |_| unreachable!(),
                    )?;
                }
                OpCode::Negate => {
                    self.apply_unary_op(
                        pos,
                        Value::negate,
                        Value::is_number,
                        RuntimeError::UnaryNumberOperand,
                    )?;
                }
                OpCode::Equal => {
                    self.apply_binary_op(
                        pos,
                        |v1, v2| {
                            *v1 = Value::Bool(v1.equal(v2));
                        },
                        |_| true,
                        |_| unreachable!(),
                    )?;
                }
                OpCode::Greater => {
                    self.apply_binary_op(
                        pos,
                        |v1, v2| {
                            *v1 = Value::Bool(v1.greater(v2));
                        },
                        Value::is_number,
                        RuntimeError::BinaryNumberOperands,
                    )?;
                }
                OpCode::Less => {
                    self.apply_binary_op(
                        pos,
                        |v1, v2| {
                            *v1 = Value::Bool(v1.less(v2));
                        },
                        Value::is_number,
                        RuntimeError::BinaryNumberOperands,
                    )?;
                }
                OpCode::Add => {
                    self.apply_binary_op(
                        pos,
                        Value::add,
                        |v| v.is_number(),
                        RuntimeError::InvalidAddOperands,
                    )
                    .or_else(|_| {
                        self.apply_binary_op(
                            pos,
                            |v1, v2| v1.concat(v2, strings),
                            |v| v.is_string(),
                            RuntimeError::InvalidAddOperands,
                        )
                    })?;
                }
                OpCode::Subtract => {
                    self.apply_binary_op(
                        pos,
                        Value::subtract,
                        Value::is_number,
                        RuntimeError::BinaryNumberOperands,
                    )?;
                }
                OpCode::Multiply => {
                    self.apply_binary_op(
                        pos,
                        Value::multiply,
                        Value::is_number,
                        RuntimeError::BinaryNumberOperands,
                    )?;
                }
                OpCode::Divide => {
                    self.apply_binary_op(
                        pos,
                        Value::divide,
                        Value::is_number,
                        RuntimeError::BinaryNumberOperands,
                    )?;
                }
            }
        }
    }

    fn apply_unary_op<
        F: FnMut(&mut Value),
        P: Fn(&Value) -> bool,
        E: Fn(Position) -> RuntimeError,
    >(
        &mut self,
        pos: &Position,
        mut op: F,
        check: P,
        err: E,
    ) -> Result<(), RuntimeError> {
        let v = self.peek_mut(0)?;
        if !check(v) {
            return Err(err(*pos));
        }
        op(v);
        Ok(())
    }

    fn apply_binary_op<
        F: FnMut(&mut Value, &Value),
        P: Fn(&Value) -> bool,
        E: Fn(Position) -> RuntimeError,
    >(
        &mut self,
        pos: &Position,
        mut op: F,
        check: P,
        err: E,
    ) -> Result<(), RuntimeError> {
        if !check(self.peek(0)?) || !check(self.peek(1)?) {
            return Err(err(*pos));
        }
        let v2 = self.pop()?;
        let v1 = self.peek_mut(0)?;
        op(v1, &v2);
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
