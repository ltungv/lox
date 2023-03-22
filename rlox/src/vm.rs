use std::cell::RefCell;
use std::ops::{Add, Div, Mul, Neg, Not, Sub};

use rustc_hash::FxHashMap;

use crate::gc::Gc;
use crate::{
    intern, Compiler, Error, NativeFun, ObjBoundMethod, ObjClass, ObjClosure, ObjInstance,
    ObjUpvalue, Object, RuntimeError, StrId, Upvalue, Value, MAX_FRAMES, MAX_STACK,
};

#[cfg(debug_assertions)]
use crate::disassemble_instruction;

#[cfg(debug_assertions)]
fn print_stack(stack: &[Value]) {
    // print stack trace
    print!("          ");
    for val in stack {
        print!("[ {val} ]");
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
    closure: Gc<ObjClosure>,
    ip: usize,
    slot: usize,
}

/// A bytecode virtual machine for the Lox programming language
#[derive(Debug)]
pub struct VM {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    open_upvalues: Vec<Gc<RefCell<ObjUpvalue>>>,
    globals: FxHashMap<StrId, Value>,
    init_string: StrId,
}

impl Default for VM {
    fn default() -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity(MAX_STACK),
            frames: Vec::with_capacity(MAX_FRAMES),
            open_upvalues: Vec::new(),
            globals: FxHashMap::default(),
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
        let fun = Gc::new(fun);

        || -> Result<(), RuntimeError> {
            let closure = Gc::new(ObjClosure::new(fun, Vec::new()));
            self.push(Value::Object(Object::Closure(Gc::clone(&closure))))?;
            self.call_closure(closure, 0)?;
            self.run()
        }()
        .map_err(|err| {
            eprintln!("{err}");
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
                disassemble_instruction(&self.frame().closure.fun.chunk, self.frame().ip);
            }

            let opcode = self.next_instruction().clone();
            match opcode {
                OpCode::Constant(ref const_id) => {
                    let val = self.read_const(*const_id as usize).clone();
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
                    let name = self.read_const(*const_id as usize).as_str();
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
                    let name = *self.read_const(*const_id as usize).as_str();
                    let val = self.pop();
                    self.globals.insert(name, val);
                }
                OpCode::SetGlobal(ref const_id) => {
                    let name = *self.read_const(*const_id as usize).as_str();
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
                    let upvalue = Gc::clone(&self.frame().closure.upvalues[slot]);
                    let value = match &*upvalue.borrow() {
                        ObjUpvalue::Open(loc) => self.stack[*loc].clone(),
                        ObjUpvalue::Closed(val) => val.clone(),
                    };
                    self.push(value)?;
                }
                OpCode::SetUpvalue(ref slot) => {
                    let value = self.peek(0).clone();
                    let slot = *slot as usize;
                    let upvalue = Gc::clone(&self.frame().closure.upvalues[slot]);
                    match &mut *upvalue.borrow_mut() {
                        ObjUpvalue::Open(loc) => self.stack[*loc] = value,
                        ObjUpvalue::Closed(val) => *val = value,
                    };
                }
                OpCode::GetProperty(ref const_id) => {
                    let instance = self.peek(0);
                    if !instance.is_instance() {
                        return Err(RuntimeError("Only instances have properties".to_string()));
                    }
                    let instance = Gc::clone(instance.as_instance());
                    let prop_name = *self.read_const(*const_id as usize).as_str();
                    match instance.borrow().fields.get(&prop_name) {
                        Some(val) => {
                            self.pop();
                            self.push(val.clone())?;
                        }
                        None => self.bind_method(Gc::clone(&instance.borrow().class), prop_name)?,
                    };
                }
                OpCode::SetProperty(ref const_id) => {
                    let value = self.pop();
                    let instance = self.pop();
                    if !instance.is_instance() {
                        return Err(RuntimeError("Only instances have fields".to_string()));
                    }
                    let prop_name = *self.read_const(*const_id as usize).as_str();
                    instance
                        .as_instance()
                        .borrow_mut()
                        .fields
                        .insert(prop_name, value.clone());
                    self.push(value)?;
                }
                OpCode::GetSuper(ref const_id) => {
                    let name = *self.read_const(*const_id as usize).as_str();
                    let superclass = self.pop();
                    self.bind_method(Gc::clone(superclass.as_class()), name)?;
                }
                OpCode::Equal => {
                    let v2 = self.pop();
                    let v1 = self.peek_mut(0);
                    *v1 = Value::Bool(*v1 == v2);
                }
                OpCode::Greater => {
                    let v2 = self.pop();
                    let v1 = self.peek_mut(0);
                    *v1 = v1.gt(&v2)?;
                }
                OpCode::Less => {
                    let v2 = self.pop();
                    let v1 = self.peek_mut(0);
                    *v1 = v1.lt(&v2)?;
                }
                OpCode::Add => {
                    let v2 = self.pop();
                    let v1 = self.peek_mut(0);
                    *v1 = v1.add(&v2)?;
                }
                OpCode::Subtract => {
                    let v2 = self.pop();
                    let v1 = self.peek_mut(0);
                    *v1 = v1.sub(&v2)?;
                }
                OpCode::Multiply => {
                    let v2 = self.pop();
                    let v1 = self.peek_mut(0);
                    *v1 = v1.mul(&v2)?;
                }
                OpCode::Divide => {
                    let v2 = self.pop();
                    let v1 = self.peek_mut(0);
                    *v1 = v1.div(&v2)?;
                }
                OpCode::Not => {
                    let v = self.peek_mut(0);
                    *v = v.not();
                }
                OpCode::Negate => {
                    let v = self.peek_mut(0);
                    *v = v.neg()?;
                }
                OpCode::Print => {
                    let v = self.pop();
                    println!("{v}");
                }
                OpCode::Jump(ref offset) => {
                    self.frame_mut().ip += *offset as usize;
                }
                OpCode::JumpIfFalse(ref offset) => {
                    if self.peek(0).not().as_bool() {
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
                    let name = *self.read_const(*const_id as usize).as_str();
                    self.invoke(name, *argc)?;
                }
                OpCode::SuperInvoke(ref const_id, ref argc) => {
                    let method = *self.read_const(*const_id as usize).as_str();
                    let superclass = self.pop();
                    self.invoke_from_class(Gc::clone(superclass.as_class()), method, *argc)?;
                }
                OpCode::Closure(ref fun_idx, ref upvalues) => {
                    let fun = Gc::clone(self.read_const(*fun_idx as usize).as_fun());
                    let upvalues = upvalues.iter().map(|upvalue| {
                        if upvalue.is_local {
                            self.capture_upvalue(self.frame().slot + upvalue.index as usize)
                        } else {
                            Gc::clone(&self.frame().closure.upvalues[upvalue.index as usize])
                        }
                    });
                    let closure = Gc::new(ObjClosure::new(fun, upvalues.collect()));
                    self.push(Value::Object(Object::Closure(closure)))?;
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
                    self.popn(self.stack.len() - frame.slot);
                    self.push(val)?;
                }
                OpCode::Class(ref const_id) => {
                    let name = self.read_const(*const_id as usize).as_str();
                    let class = Gc::new(RefCell::new(ObjClass::new(*name)));
                    self.push(Value::Object(Object::Class(class)))?;
                }
                OpCode::Inherit => {
                    let subclass = self.pop();
                    let superclass = if self.peek(0).is_class() {
                        Gc::clone(self.peek(0).as_class())
                    } else {
                        return Err(RuntimeError("Superclass must be a class".to_string()));
                    };
                    // Upon inheritance, we copy all method references from the superclass
                    // to the subclass. This technique does not work in languages that support
                    // "monkey patching" lik Python or Ruby, where user can change the behaviors
                    // of a class at runtme.
                    subclass.as_class().borrow_mut().methods.extend(
                        superclass
                            .borrow()
                            .methods
                            .iter()
                            .map(|(k, v)| (*k, v.clone())),
                    );
                }
                OpCode::Method(ref const_id) => {
                    let name = *self.read_const(*const_id as usize).as_str();
                    self.define_method(name);
                }
            }
        }
    }

    fn invoke(&mut self, name: StrId, argc: u8) -> Result<(), RuntimeError> {
        let receiver = Gc::clone(self.peek(argc as usize).as_instance());
        let receiver = receiver.borrow();

        match receiver.fields.get(&name) {
            Some(value) => {
                *self.peek_mut(argc as usize) = value.clone();
                self.call_value(value.clone(), argc)
            }
            None => self.invoke_from_class(Gc::clone(&receiver.class), name, argc),
        }
    }

    fn invoke_from_class(
        &mut self,
        class: Gc<RefCell<ObjClass>>,
        name: StrId,
        argc: u8,
    ) -> Result<(), RuntimeError> {
        let class = class.borrow();
        let method = class
            .methods
            .get(&name)
            .ok_or_else(|| RuntimeError(format!("Undefined property '{}'", intern::str(name))))?;
        let method = Gc::clone(method.as_closure());
        self.call_closure(method, argc)
    }

    fn call_value(&mut self, callee: Value, argc: u8) -> Result<(), RuntimeError> {
        match callee {
            Value::NativeFun(f) => self.call_native(f, argc),
            Value::Object(Object::Closure(c)) => self.call_closure(c, argc),
            Value::Object(Object::Class(c)) => self.call_class(c, argc),
            Value::Object(Object::BoundMethod(m)) => self.call_bound_method(Gc::clone(&m), argc),
            _ => Err(RuntimeError(
                "Can only call functions and classes".to_string(),
            )),
        }
    }

    fn capture_upvalue(&mut self, location: usize) -> Gc<RefCell<ObjUpvalue>> {
        for upvalue in self.open_upvalues.iter() {
            if let ObjUpvalue::Open(loc) = *upvalue.borrow() {
                if loc == location {
                    return Gc::clone(upvalue);
                }
            }
        }
        let upvalue = Gc::new(RefCell::new(ObjUpvalue::Open(location)));
        self.open_upvalues.push(Gc::clone(&upvalue));
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

    fn call_closure(&mut self, closure: Gc<ObjClosure>, argc: u8) -> Result<(), RuntimeError> {
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
        self.popn(argc + 1);
        self.push(res)
    }

    fn define_native(&mut self, name: &str, arity: u8, call: fn(&[Value]) -> Value) {
        let name = intern::id(name);
        self.globals
            .insert(name, Value::NativeFun(NativeFun { name, arity, call }));
    }

    fn call_class(&mut self, class: Gc<RefCell<ObjClass>>, argc: u8) -> Result<(), RuntimeError> {
        *self.peek_mut(argc as usize) = Value::Object(Object::Instance(Gc::new(RefCell::new(
            ObjInstance::new(Gc::clone(&class)),
        ))));

        match class.borrow().methods.get(&self.init_string) {
            None if argc != 0 => Err(RuntimeError(format!("Expected 0 arguments but got {argc}"))),
            Some(init) => self.call_closure(Gc::clone(init.as_closure()), argc),
            _ => Ok(()),
        }
    }

    fn call_bound_method(
        &mut self,
        bound: Gc<ObjBoundMethod>,
        argc: u8,
    ) -> Result<(), RuntimeError> {
        *self.peek_mut(argc as usize) = bound.receiver.clone();
        self.call_closure(Gc::clone(&bound.method), argc)
    }

    fn define_method(&mut self, name: StrId) {
        let method = self.pop();
        let class = Gc::clone(self.peek(0).as_class());
        class.borrow_mut().methods.insert(name, method);
    }

    fn bind_method(
        &mut self,
        class: Gc<RefCell<ObjClass>>,
        name: StrId,
    ) -> Result<(), RuntimeError> {
        match class.borrow().methods.get(&name) {
            Some(val) => {
                let method = Gc::clone(val.as_closure());
                let instance = Gc::clone(self.pop().as_instance());
                let bound = Gc::new(ObjBoundMethod::new(
                    Value::Object(Object::Instance(instance)),
                    method,
                ));
                self.push(Value::Object(Object::BoundMethod(bound)))?;
                Ok(())
            }
            None => Err(RuntimeError(format!(
                "Undefined property '{}'",
                intern::str(name)
            ))),
        }
    }

    fn next_instruction(&mut self) -> &OpCode {
        let frame = self.frame_mut();
        let (opcode, _) = frame.closure.fun.chunk.read_instruction(frame.ip);
        frame.ip += 1;
        opcode
    }

    fn read_const(&self, idx: usize) -> &Value {
        self.frame().closure.fun.chunk.read_const(idx)
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

    fn popn(&mut self, n: usize) {
        self.stack.drain(self.stack.len() - n..);
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
                eprintln!("{pos} in script.");
            } else {
                eprintln!("{pos} in {fname}().");
            }
        }
    }
}
