use std::{collections::HashMap, rc::Rc};

use crate::{
    intern, Chunk, Compiler, Error, ObjFun, ObjNativeFun, OpCode, RuntimeError, StringId, Value,
    MAX_FRAMES, MAX_STACK,
};

#[cfg(debug_assertions)]
use crate::disassemble_instruction;

#[derive(Debug)]
struct CallFrame {
    fun: Rc<ObjFun>,
    ip: usize,
    slot: usize,
}

/// A bytecode virtual machine for the Lox programming language
#[derive(Debug)]
pub struct VM {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    globals: HashMap<StringId, Value>,
}

impl Default for VM {
    fn default() -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity(MAX_STACK),
            frames: Vec::with_capacity(MAX_FRAMES),
            globals: HashMap::default(),
        };
        vm.define_native("clock", 0, clock_native)
            .expect("Unreachable");
        vm
    }
}

impl VM {
    /// Load and run the virtual machine on the given chunk
    pub fn interpret(&mut self, src: &str) -> Result<(), Error> {
        let mut compiler = Compiler::new(src);
        compiler.compile();

        let fun = compiler.finish().ok_or(Error::Compile)?;
        let fun = Rc::new(fun);

        || -> Result<(), RuntimeError> {
            self.stack.push(Value::Fun(Rc::clone(&fun)));
            self.call(fun, 0)?;
            self.run()
        }()
        .map_err(|err| {
            eprintln!("{}", err);
            self.print_stack_trace();
            self.reset_stack();
            Error::Runtime
        })
    }

    /// Run the virtual machine with it currently given chunk.
    fn run(&mut self) -> Result<(), RuntimeError> {
        loop {
            #[cfg(debug_assertions)]
            {
                print_stack(&self.stack);
                disassemble_instruction(&self.chunk(), self.frame().ip as usize);
            }

            let opcode = self.next_instruction();
            match opcode {
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::Loop(offset) => {
                    self.frame_mut().ip -= offset as usize;
                }
                OpCode::Jump(offset) => {
                    self.frame_mut().ip += offset as usize;
                }
                OpCode::JumpIfFalse(offset) => {
                    if self.peek(0).is_falsey() {
                        self.frame_mut().ip += offset as usize;
                    }
                }
                OpCode::Call(argc) => {
                    self.call_value(self.peek(argc as usize).clone(), argc)?;
                }
                OpCode::Return => {
                    let val = self.pop();
                    let frame = self.frames.pop().expect("Cannot be empty");
                    if self.frames.is_empty() {
                        self.pop();
                        return Ok(());
                    }
                    while self.stack.len() != frame.slot {
                        self.pop();
                    }
                    self.push(val)?;
                }
                OpCode::Print => {
                    let v = self.pop();
                    println!("{}", v);
                }
                OpCode::GetLocal(ref slot) => {
                    let local = self.stack[self.frame().slot + *slot as usize].clone();
                    self.push(local)?;
                }
                OpCode::SetLocal(ref slot) => {
                    let val = self.peek(0);
                    let offset = self.frame().slot + *slot as usize;
                    self.stack[offset] = val.clone();
                }
                OpCode::DefineGlobal(ref const_id) => {
                    let name =
                        if let Value::String(name) = self.chunk().read_const(*const_id as usize) {
                            *name
                        } else {
                            unreachable!("Constant for the variable name must have been added.");
                        };
                    let val = self.peek(0).clone();
                    self.globals.insert(name, val);
                    self.pop();
                }
                OpCode::GetGlobal(ref const_id) => {
                    let name = self.chunk().read_const(*const_id as usize);
                    if let Value::String(name) = name {
                        let val = self
                            .globals
                            .get(&name)
                            .ok_or_else(|| RuntimeError::UndefinedVariable(intern::str(*name)))?
                            .clone();
                        self.push(val)?;
                    } else {
                        unreachable!("Constant for the variable name must have been added.");
                    }
                }
                OpCode::SetGlobal(ref const_id) => {
                    let name =
                        if let Value::String(name) = self.chunk().read_const(*const_id as usize) {
                            *name
                        } else {
                            unreachable!("Constant for the variable name must have been added.");
                        };

                    let val = self.peek(0).clone();
                    if !self.globals.contains_key(&name) {
                        return Err(RuntimeError::UndefinedVariable(intern::str(name)));
                    }
                    self.globals.insert(name, val);
                }
                OpCode::Constant(ref const_id) => {
                    let val = self.chunk().read_const(*const_id as usize).clone();
                    self.push(val)?;
                }
                OpCode::Nil => self.push(Value::Nil)?,
                OpCode::True => self.push(Value::Bool(true))?,
                OpCode::False => self.push(Value::Bool(false))?,
                OpCode::Not => {
                    let v = self.peek_mut(0);
                    *v = Value::Bool(v.is_falsey());
                }
                OpCode::Negate => match self.peek_mut(0) {
                    Value::Number(v) => {
                        *v = -*v;
                    }
                    _ => {
                        return Err(RuntimeError::InvalidOperand(
                            "Operand must be a number".to_string(),
                        ))
                    }
                },
                OpCode::Equal => {
                    let v2 = self.pop();
                    let v1 = self.peek_mut(0);
                    *v1 = Value::Bool(v1.equal(&v2));
                }
                OpCode::Greater => match (self.peek(0), self.peek(1)) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop();
                        let v1 = self.peek_mut(0);
                        *v1 = Value::Bool(n1 > n2);
                    }
                    _ => {
                        return Err(RuntimeError::InvalidOperand(
                            "Operands must be numbers".to_string(),
                        ))
                    }
                },
                OpCode::Less => match (self.peek(0), self.peek(1)) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop();
                        let v1 = self.peek_mut(0);
                        *v1 = Value::Bool(n1 < n2);
                    }
                    _ => {
                        return Err(RuntimeError::InvalidOperand(
                            "Operands must be numbers".to_string(),
                        ))
                    }
                },
                OpCode::Add => match (self.peek(0), self.peek(1)) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop();
                        let v1 = self.peek_mut(0);
                        *v1 = Value::Number(n1 + n2);
                    }
                    (&Value::String(s2), &Value::String(s1)) => {
                        let mut res = intern::str(s1);
                        res += intern::str(s2).as_str();
                        self.pop();

                        let v1 = self.peek_mut(0);
                        *v1 = Value::String(intern::id(res));
                    }
                    _ => {
                        return Err(RuntimeError::InvalidOperand(
                            "Operands must be two numbers or two strings".to_string(),
                        ))
                    }
                },
                OpCode::Subtract => match (self.peek(0), self.peek(1)) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop();
                        let v1 = self.peek_mut(0);
                        *v1 = Value::Number(n1 - n2);
                    }
                    _ => {
                        return Err(RuntimeError::InvalidOperand(
                            "Operands must be numbers".to_string(),
                        ))
                    }
                },
                OpCode::Multiply => match (self.peek(0), self.peek(1)) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop();
                        let v1 = self.peek_mut(0);
                        *v1 = Value::Number(n1 * n2);
                    }
                    _ => {
                        return Err(RuntimeError::InvalidOperand(
                            "Operands must be numbers".to_string(),
                        ))
                    }
                },
                OpCode::Divide => match (self.peek(0), self.peek(1)) {
                    (&Value::Number(n2), &Value::Number(n1)) => {
                        self.pop();
                        let v1 = self.peek_mut(0);
                        *v1 = Value::Number(n1 / n2);
                    }
                    _ => {
                        return Err(RuntimeError::InvalidOperand(
                            "Operands must be numbers".to_string(),
                        ))
                    }
                },
            }
        }
    }

    fn call_value(&mut self, callee: Value, argc: u8) -> Result<(), RuntimeError> {
        match callee {
            Value::Fun(f) => self.call(f, argc),
            Value::NativeFun(f) => self.call_native(f, argc),
            _ => Err(RuntimeError::InvalidCall(
                "Can only call functions and classes".to_string(),
            )),
        }
    }

    fn call(&mut self, fun: Rc<ObjFun>, argc: u8) -> Result<(), RuntimeError> {
        if argc != fun.arity {
            return Err(RuntimeError::InvalidCall(format!(
                "Expected {} arguments but got {}",
                fun.arity, argc
            )));
        }

        if self.frames.len() == MAX_FRAMES {
            return Err(RuntimeError::StackOverflow);
        }

        let frame = CallFrame {
            fun,
            ip: 0,
            slot: self.stack.len() - argc as usize - 1,
        };
        self.frames.push(frame);
        Ok(())
    }

    fn call_native(&mut self, fun: ObjNativeFun, argc: u8) -> Result<(), RuntimeError> {
        if argc != fun.arity {
            return Err(RuntimeError::InvalidCall(format!(
                "Expected {} arguments but got {}",
                fun.arity, argc
            )));
        }
        let argc = argc as usize;
        let args = &self.stack[self.stack.len() - argc..];
        let call = fun.call;
        let res = call(args);
        for _ in 0..argc {
            self.pop();
        }
        self.push(res)
    }

    fn define_native(
        &mut self,
        name: &str,
        arity: u8,
        call: fn(&[Value]) -> Value,
    ) -> Result<(), RuntimeError> {
        self.push(Value::String(intern::id(name)))?;
        self.push(Value::NativeFun(ObjNativeFun { arity, call }))?;
        if let Value::String(name) = self.stack[0] {
            self.globals.insert(name, self.stack[1].clone());
        }
        self.pop();
        self.pop();
        Ok(())
    }

    fn next_instruction(&mut self) -> OpCode {
        let frame = self.frame_mut();
        let (opcode, _) = frame.fun.chunk.read_instruction(frame.ip);
        frame.ip += 1;
        opcode
    }

    fn chunk(&self) -> &Chunk {
        &self.frame().fun.chunk
    }

    fn frame(&self) -> &CallFrame {
        self.frames.last().expect("Cannot be empty")
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().expect("Cannot be empty")
    }

    fn peek(&self, steps: usize) -> &Value {
        self.stack
            .get(self.stack.len() - 1 - steps)
            .expect("Invalid bytecodes")
    }

    fn peek_mut(&mut self, steps: usize) -> &mut Value {
        let idx = self.stack.len() - 1 - steps;
        self.stack.get_mut(idx).expect("Invalid bytecodes")
    }

    fn push(&mut self, val: Value) -> Result<(), RuntimeError> {
        if self.stack.len() == MAX_STACK {
            return Err(RuntimeError::StackOverflow);
        }
        self.stack.push(val);
        Ok(())
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Invalid bytecodes")
    }

    /// Clear current stack and frames
    fn reset_stack(&mut self) {
        self.stack.clear();
        self.frames.clear();
    }

    /// Print out where execution stop right before the error
    fn print_stack_trace(&self) {
        for frame in self.frames.iter().rev() {
            let (_, pos) = frame.fun.chunk.read_instruction(frame.ip - 1);
            let fname = intern::str(frame.fun.name);
            if fname.is_empty() {
                eprintln!("{} in script.", pos);
            } else {
                eprintln!("{} in {}().", pos, fname);
            }
        }
    }
}

fn clock_native(_args: &[Value]) -> Value {
    let start = std::time::SystemTime::now();
    let since_epoch = start
        .duration_since(std::time::UNIX_EPOCH)
        .expect("Time went backwards");
    Value::Number(since_epoch.as_secs_f64())
}

#[cfg(debug_assertions)]
fn print_stack(stack: &[Value]) {
    // print stack trace
    print!("          ");
    for val in stack {
        print!("[ {} ]", val);
    }
    println!();
}
