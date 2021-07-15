use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    intern, Chunk, Compiler, Error, NativeFun, ObjBoundMethod, ObjClass, ObjClosure, ObjInstance,
    ObjUpvalue, RuntimeError, StrId, Upvalue, Value, MAX_FRAMES, MAX_STACK,
};

#[cfg(debug_assertions)]
use crate::disassemble_instruction;

#[cfg(debug_assertions)]
fn print_stack(stack: &[Value]) {
    // print stack trace
    print!("          ");
    for val in stack {
        print!("[ {} ]", val);
    }
    println!();
}

/// OpCode is a number that specifies the type of the instruction.
///
/// # Notes
///
/// If it was for performances purposes, the following options could be considered:
/// + Having the opcode designed to be as close as possbible to existing lower-level instructions
/// + Having specialized opcode for constant
///
/// We don't have a `OpCode::NotEqual` because we will transform `a != b` to `!(a == b)` to demonstrated
/// that bytecode can deviate from the actual user's code as long as they behave similarly. This is
/// also applied for operator `<=` and operator `>=`.
///
/// `a <= b` does not equals equivalent to `!(a > b)`, similarly with greater and greater or equal.
/// According to [IEEE 754] all comparison operators return `false` when an operand is `NaN`. These
/// are implementation details that we should keep in mind when making a real language.
///
/// [IEEE 754]: https://en.wikipedia.org/wiki/IEEE_754
#[derive(Debug, Clone)]
pub enum OpCode {
    /// Load a constant
    Constant(u8),
    /// Load a `nil` value
    Nil,
    /// Load a `true` value
    True,
    /// Load a `false` value
    False,
    /// Pop the top of the stack
    Pop,
    /// Set the value of a global variable
    GetLocal(u8),
    /// Set the value of a local variable
    SetLocal(u8),
    /// Get the value of a global variable
    GetGlobal(u8),
    /// Pop the top of the stack and define a variable initialized with that value.
    DefineGlobal(u8),
    /// Set the value of a global variable
    SetGlobal(u8),
    /// Get a variable through its upvalue
    GetUpvalue(u8),
    /// Set a variable through its upvalue
    SetUpvalue(u8),
    /// Get the value of a property on the class instance
    GetProperty(u8),
    /// Set the value of a property on the class instance
    SetProperty(u8),
    /// Get the super class instance of the current class
    GetSuper(u8),
    /// Check for equality between 2 operands.
    Equal,
    /// Compare if the first operand is greater than the second
    Greater,
    /// Compare if the first operand is less than the second
    Less,
    /// Add two number operands or two string operands
    Add,
    /// Subtract two number operands
    Subtract,
    /// Multiply two number operands
    Multiply,
    /// Divide two number operands
    Divide,
    /// Apply logical `not` to a single boolean operand
    Not,
    /// Negate a single number operand
    Negate,
    /// Print an expression in human readable format
    Print,
    /// Jump forward for n instructions
    Jump(u16),
    /// Jump forward for n instructions if current stack top is falsey
    JumpIfFalse(u16),
    /// Jump backward for n instructions
    Loop(u16),
    /// Make a function call
    Call(u8),
    /// Invoke method call directly without going though an access operation
    Invoke(u8, u8),
    /// Invoke super call directly without going though an access operation
    SuperInvoke(u8, u8),
    /// Add a new closure
    Closure(u8, Vec<Upvalue>),
    /// Move captured value to the heap
    CloseUpvalue,
    /// Return from the current function
    Return,
    /// Create a class and bind it to a name
    Class(u8),
    /// Create a inheritance relation between two classes
    Inherit,
    /// Define a method
    Method(u8),
}

fn clock_native(_args: &[Value]) -> Value {
    let start = std::time::SystemTime::now();
    let since_epoch = start
        .duration_since(std::time::UNIX_EPOCH)
        .expect("Time went backwards");
    Value::Number(since_epoch.as_secs_f64())
}

#[derive(Debug)]
struct CallFrame {
    closure: Rc<ObjClosure>,
    ip: usize,
    slot: usize,
}

/// A bytecode virtual machine for the Lox programming language
#[derive(Debug)]
pub struct VM {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    open_upvalues: Vec<Rc<RefCell<ObjUpvalue>>>,
    globals: HashMap<StrId, Value>,
    init_string: StrId,
}

impl Default for VM {
    fn default() -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity(MAX_STACK),
            frames: Vec::with_capacity(MAX_FRAMES),
            open_upvalues: Vec::new(),
            globals: HashMap::default(),
            init_string: intern::id("init"),
        };
        vm.define_native("clock", 0, clock_native);
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
            let closure = Rc::new(ObjClosure::new(fun, Vec::new()));
            self.push(Value::Closure(Rc::clone(&closure)))?;
            self.call_closure(closure, 0)?;
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

            let opcode = self.next_instruction().clone();
            match opcode {
                OpCode::Constant(ref const_id) => {
                    let val = self.chunk().read_const(*const_id as usize).clone();
                    self.push(val)?;
                }
                OpCode::Nil => self.push(Value::Nil)?,
                OpCode::True => self.push(Value::Bool(true))?,
                OpCode::False => self.push(Value::Bool(false))?,
                OpCode::Pop => {
                    self.pop();
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
                OpCode::GetGlobal(ref const_id) => {
                    let name = self.chunk().read_const(*const_id as usize).as_str();
                    let val = self
                        .globals
                        .get(name)
                        .ok_or_else(|| {
                            RuntimeError(format!("Undefined variable '{}'", intern::str(*name)))
                        })?
                        .clone();
                    self.push(val)?;
                }
                OpCode::DefineGlobal(ref const_id) => {
                    let name = *self.chunk().read_const(*const_id as usize).as_str();
                    let val = self.pop();
                    self.globals.insert(name, val);
                }
                OpCode::SetGlobal(ref const_id) => {
                    let name = *self.chunk().read_const(*const_id as usize).as_str();
                    let val = self.peek(0).clone();
                    if !self.globals.contains_key(&name) {
                        return Err(RuntimeError(format!(
                            "Undefined variable '{}'",
                            intern::str(name)
                        )));
                    }
                    self.globals.insert(name, val);
                }
                OpCode::GetUpvalue(ref slot) => {
                    let slot = *slot as usize;
                    let upvalue = Rc::clone(&self.frame().closure.upvalues[slot]);
                    let value = match &*upvalue.borrow() {
                        ObjUpvalue::Open(loc) => self.stack[*loc as usize].clone(),
                        ObjUpvalue::Closed(val) => val.clone(),
                    };
                    self.push(value)?;
                }
                OpCode::SetUpvalue(ref slot) => {
                    let value = self.peek(0).clone();
                    let slot = *slot as usize;
                    let upvalue = Rc::clone(&self.frame().closure.upvalues[slot]);
                    match &mut *upvalue.borrow_mut() {
                        ObjUpvalue::Open(loc) => self.stack[*loc as usize] = value,
                        ObjUpvalue::Closed(val) => *val = value,
                    };
                }
                OpCode::GetProperty(ref const_id) => match self.peek(0) {
                    Value::Instance(instance) => {
                        let instance = Rc::clone(instance);
                        let prop_name = *self.chunk().read_const(*const_id as usize).as_str();
                        if let Some(val) = instance.borrow().fields.get(&prop_name) {
                            self.pop();
                            self.push(val.clone())?;
                        } else {
                            self.bind_method(Rc::clone(&instance.borrow().class), prop_name)?
                        };
                    }
                    _ => return Err(RuntimeError("Only instances have properties".to_string())),
                },
                OpCode::SetProperty(ref const_id) => {
                    let value = self.pop();
                    match self.pop() {
                        Value::Instance(instance) => {
                            let prop_name = *self.chunk().read_const(*const_id as usize).as_str();
                            instance
                                .borrow_mut()
                                .fields
                                .insert(prop_name, value.clone());
                            self.push(value)?;
                        }
                        _ => return Err(RuntimeError("Only instances have fields".to_string())),
                    }
                }
                OpCode::GetSuper(ref const_id) => {
                    let name = *self.chunk().read_const(*const_id as usize).as_str();
                    match self.pop() {
                        Value::Class(superclass) => {
                            self.bind_method(superclass, name)?;
                        }
                        _ => unreachable!(),
                    }
                }
                OpCode::Equal => {
                    let v2 = self.pop();
                    let v1 = self.peek_mut(0);
                    *v1 = Value::Bool(v1.equal(&v2));
                }
                OpCode::Greater => match (self.pop(), self.peek(0)) {
                    (Value::Number(n2), &Value::Number(n1)) => {
                        let v1 = self.peek_mut(0);
                        *v1 = Value::Bool(n1 > n2);
                    }
                    _ => return Err(RuntimeError("Operands must be numbers".to_string())),
                },
                OpCode::Less => match (self.pop(), self.peek(0)) {
                    (Value::Number(n2), &Value::Number(n1)) => {
                        let v1 = self.peek_mut(0);
                        *v1 = Value::Bool(n1 < n2);
                    }
                    _ => return Err(RuntimeError("Operands must be numbers".to_string())),
                },
                OpCode::Add => match (self.pop(), self.peek(0)) {
                    (Value::Number(n2), &Value::Number(n1)) => {
                        let v1 = self.peek_mut(0);
                        *v1 = Value::Number(n1 + n2);
                    }
                    (Value::Str(s2), Value::Str(s1)) => {
                        let res = Rc::from(intern::str(*s1) + intern::str(s2).as_str());
                        let v1 = self.peek_mut(0);
                        *v1 = Value::String(res);
                    }
                    (Value::String(s2), &Value::Str(s1)) => {
                        let res = Rc::from(intern::str(s1) + s2.as_ref());
                        let v1 = self.peek_mut(0);
                        *v1 = Value::String(res);
                    }
                    (Value::Str(s2), Value::String(s1)) => {
                        let res = Rc::from(s1.as_ref().to_string() + intern::str(s2).as_str());
                        let v1 = self.peek_mut(0);
                        *v1 = Value::String(res);
                    }
                    (Value::String(s2), Value::String(s1)) => {
                        let res = Rc::from(s1.as_ref().to_string() + s2.as_ref());
                        let v1 = self.peek_mut(0);
                        *v1 = Value::String(res);
                    }
                    _ => {
                        return Err(RuntimeError(
                            "Operands must be two numbers or two strings".to_string(),
                        ))
                    }
                },
                OpCode::Subtract => match (self.pop(), self.peek_mut(0)) {
                    (Value::Number(n2), Value::Number(n1)) => *n1 -= n2,
                    _ => return Err(RuntimeError("Operands must be numbers".to_string())),
                },
                OpCode::Multiply => match (self.pop(), self.peek_mut(0)) {
                    (Value::Number(n2), Value::Number(n1)) => *n1 *= n2,
                    _ => return Err(RuntimeError("Operands must be numbers".to_string())),
                },
                OpCode::Divide => match (self.pop(), self.peek_mut(0)) {
                    (Value::Number(n2), Value::Number(n1)) => *n1 /= n2,
                    _ => return Err(RuntimeError("Operands must be numbers".to_string())),
                },
                OpCode::Not => {
                    let v = self.peek_mut(0);
                    *v = Value::Bool(v.is_falsey());
                }
                OpCode::Negate => match self.peek_mut(0) {
                    Value::Number(v) => {
                        *v = -*v;
                    }
                    _ => return Err(RuntimeError("Operand must be a number".to_string())),
                },
                OpCode::Print => {
                    let v = self.pop();
                    println!("{}", v);
                }
                OpCode::Jump(ref offset) => {
                    self.frame_mut().ip += *offset as usize;
                }
                OpCode::JumpIfFalse(ref offset) => {
                    if self.peek(0).is_falsey() {
                        self.frame_mut().ip += *offset as usize;
                    }
                }
                OpCode::Loop(ref offset) => {
                    self.frame_mut().ip -= *offset as usize;
                }
                OpCode::Call(ref argc) => {
                    self.call_value(self.peek(*argc as usize).clone(), *argc)?;
                }
                OpCode::Invoke(ref const_id, ref argc) => {
                    let name = *self.chunk().read_const(*const_id as usize).as_str();
                    self.invoke(name, *argc)?;
                }
                OpCode::SuperInvoke(ref const_id, ref argc) => {
                    let method = *self.chunk().read_const(*const_id as usize).as_str();
                    match self.pop() {
                        Value::Class(superclass) => {
                            self.invoke_from_class(superclass, method, *argc)?;
                        }
                        _ => unreachable! {},
                    }
                }
                OpCode::Closure(ref fun_idx, ref upvalues) => {
                    if let Value::Fun(fun) = self.chunk().read_const(*fun_idx as usize) {
                        let fun = Rc::clone(fun);
                        let upvalues = upvalues.iter().map(|upvalue| {
                            if upvalue.is_local {
                                self.capture_upvalue(self.frame().slot + upvalue.index as usize)
                            } else {
                                Rc::clone(&self.frame().closure.upvalues[upvalue.index as usize])
                            }
                        });
                        let closure = Rc::new(ObjClosure::new(fun, upvalues.collect()));
                        self.push(Value::Closure(closure))?;
                    };
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1);
                    self.pop();
                }
                OpCode::Return => {
                    let val = self.pop();
                    self.close_upvalues(self.frame().slot);
                    let frame = self.frames.pop().expect("Frames empty");
                    if self.frames.is_empty() {
                        self.pop();
                        return Ok(());
                    }
                    while self.stack.len() != frame.slot {
                        self.pop();
                    }
                    self.push(val)?;
                }
                OpCode::Class(ref const_id) => {
                    let name = self.chunk().read_const(*const_id as usize).as_str();
                    let class = Rc::new(RefCell::new(ObjClass::new(*name)));
                    self.push(Value::Class(class))?;
                }
                OpCode::Inherit => {
                    let (subclass, superclass) = match (self.pop(), self.peek(0)) {
                        (Value::Class(sub), Value::Class(sup)) => (sub, Rc::clone(sup)),
                        (Value::Class(_), _) => {
                            return Err(RuntimeError("Superclass must be a class".to_string()))
                        }
                        (_, _) => unreachable!(),
                    };
                    // Upon inheritance, we copy all method references from the superclass
                    // to the subclass. This technique does not work in languages that support
                    // "monkey patching" lik Python or Ruby, where user can change the behaviors
                    // of a class at runtme.
                    subclass.borrow_mut().methods.extend(
                        superclass
                            .borrow()
                            .methods
                            .iter()
                            .map(|(k, v)| (*k, v.clone())),
                    );
                }
                OpCode::Method(ref const_id) => {
                    let name = *self.chunk().read_const(*const_id as usize).as_str();
                    self.define_method(name);
                }
            }
        }
    }

    fn invoke(&mut self, name: StrId, argc: u8) -> Result<(), RuntimeError> {
        let receiver = if let Value::Instance(r) = self.peek(argc as usize) {
            Rc::clone(r)
        } else {
            unreachable!();
        };

        let receiver = receiver.borrow();
        if let Some(value) = receiver.fields.get(&name) {
            *self.peek_mut(argc as usize) = value.clone();
            self.call_value(value.clone(), argc)
        } else {
            self.invoke_from_class(Rc::clone(&receiver.class), name, argc)
        }
    }

    fn invoke_from_class(
        &mut self,
        class: Rc<RefCell<ObjClass>>,
        name: StrId,
        argc: u8,
    ) -> Result<(), RuntimeError> {
        let method = match class.borrow().methods.get(&name) {
            Some(Value::Closure(c)) => Rc::clone(c),
            _ => {
                return Err(RuntimeError(format!(
                    "Undefined property '{}'",
                    intern::str(name)
                )))
            }
        };
        self.call_closure(method, argc)
    }

    fn call_value(&mut self, callee: Value, argc: u8) -> Result<(), RuntimeError> {
        match callee {
            Value::Closure(c) => self.call_closure(c, argc),
            Value::NativeFun(f) => self.call_native(f, argc),
            Value::Class(c) => self.call_class(c, argc),
            Value::BoundMethod(m) => self.call_bound_method(Rc::clone(&m), argc),
            _ => Err(RuntimeError(
                "Can only call functions and classes".to_string(),
            )),
        }
    }

    fn capture_upvalue(&mut self, location: usize) -> Rc<RefCell<ObjUpvalue>> {
        for upvalue in self.open_upvalues.iter() {
            if let ObjUpvalue::Open(loc) = *upvalue.borrow() {
                if loc == location {
                    return Rc::clone(upvalue);
                }
            }
        }
        let upvalue = Rc::new(RefCell::new(ObjUpvalue::Open(location)));
        self.open_upvalues.push(Rc::clone(&upvalue));
        upvalue
    }

    fn close_upvalues(&mut self, last: usize) {
        for upvalue in self.open_upvalues.iter() {
            let loc = if let ObjUpvalue::Open(loc) = *upvalue.borrow() {
                loc
            } else {
                continue;
            };
            if loc >= last {
                upvalue.replace(ObjUpvalue::Closed(self.stack[loc].clone()));
            }
        }
        // remove closed upvalues from list of open upvalues
        self.open_upvalues
            .retain(|v| matches!(*v.borrow(), ObjUpvalue::Open(_)))
    }

    fn call_closure(&mut self, closure: Rc<ObjClosure>, argc: u8) -> Result<(), RuntimeError> {
        if argc != closure.fun.arity {
            return Err(RuntimeError(format!(
                "Expected {} arguments but got {}",
                closure.fun.arity, argc
            )));
        }

        if self.frames.len() == MAX_FRAMES {
            return Err(RuntimeError("Stack overflow".to_string()));
        }

        let frame = CallFrame {
            closure,
            ip: 0,
            slot: self.stack.len() - argc as usize - 1,
        };
        self.frames.push(frame);
        Ok(())
    }

    fn call_native(&mut self, fun: NativeFun, argc: u8) -> Result<(), RuntimeError> {
        if argc != fun.arity {
            return Err(RuntimeError(format!(
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

    fn define_native(&mut self, name: &str, arity: u8, call: fn(&[Value]) -> Value) {
        let name = intern::id(name);
        self.globals
            .insert(name, Value::NativeFun(NativeFun { name, arity, call }));
    }

    fn call_class(&mut self, class: Rc<RefCell<ObjClass>>, argc: u8) -> Result<(), RuntimeError> {
        *self.peek_mut(argc as usize) =
            Value::Instance(Rc::new(RefCell::new(ObjInstance::new(Rc::clone(&class)))));
        if let Some(Value::Closure(initializer)) = class.borrow().methods.get(&self.init_string) {
            return self.call_closure(Rc::clone(initializer), argc);
        } else if argc != 0 {
            return Err(RuntimeError(format!(
                "Expected 0 arguments but got {}",
                argc
            )));
        }
        Ok(())
    }

    fn call_bound_method(
        &mut self,
        bound: Rc<ObjBoundMethod>,
        argc: u8,
    ) -> Result<(), RuntimeError> {
        *self.peek_mut(argc as usize) = bound.receiver.clone();
        self.call_closure(Rc::clone(&bound.method), argc)
    }

    fn define_method(&mut self, name: StrId) {
        let method = self.pop();
        if let Value::Class(class) = self.peek(0) {
            let class = Rc::clone(class);
            class.borrow_mut().methods.insert(name, method);
        }
    }

    fn bind_method(
        &mut self,
        class: Rc<RefCell<ObjClass>>,
        name: StrId,
    ) -> Result<(), RuntimeError> {
        match class.borrow().methods.get(&name) {
            Some(Value::Closure(method)) => {
                let instance = match self.pop() {
                    Value::Instance(instance) => instance,
                    _ => unimplemented!(),
                };
                let bound = Rc::new(ObjBoundMethod::new(
                    Value::Instance(Rc::clone(&instance)),
                    Rc::clone(method),
                ));
                self.push(Value::BoundMethod(bound))?;
                Ok(())
            }
            None => Err(RuntimeError(format!(
                "Undefined property '{}'",
                intern::str(name)
            ))),
            _ => unreachable!(),
        }
    }

    fn next_instruction(&mut self) -> &OpCode {
        let frame = self.frame_mut();
        let (opcode, _) = frame.closure.fun.chunk.read_instruction(frame.ip);
        frame.ip += 1;
        &opcode
    }

    fn chunk(&self) -> &Chunk {
        &self.frame().closure.fun.chunk
    }

    fn frame(&self) -> &CallFrame {
        self.frames.last().expect("Frames empty")
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().expect("Frames empty")
    }

    fn peek(&self, steps: usize) -> &Value {
        self.stack
            .get(self.stack.len() - 1 - steps)
            .expect("Stack empty")
    }

    fn peek_mut(&mut self, steps: usize) -> &mut Value {
        let idx = self.stack.len() - 1 - steps;
        self.stack.get_mut(idx).expect("Stack empty")
    }

    fn push(&mut self, val: Value) -> Result<(), RuntimeError> {
        if self.stack.len() == MAX_STACK {
            return Err(RuntimeError("Stack overflow".to_string()));
        }
        self.stack.push(val);
        Ok(())
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Stack empty")
    }

    /// Clear current stack and frames
    fn reset_stack(&mut self) {
        self.stack.clear();
        self.frames.clear();
        self.open_upvalues.clear();
    }

    /// Print out where execution stop right before the error
    fn print_stack_trace(&self) {
        for frame in self.frames.iter().rev() {
            let (_, pos) = frame.closure.fun.chunk.read_instruction(frame.ip - 1);
            let fname = intern::str(frame.closure.fun.name);
            if fname.is_empty() {
                eprintln!("{} in script.", pos);
            } else {
                eprintln!("{} in {}().", pos, fname);
            }
        }
    }
}
