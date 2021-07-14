use crate::{Position,OpCode,Value};

/// A chunk holds a sequence of instructions to be executes and their data
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

    /// Return the index of the last written instruction.
    pub fn instructions_count(&self) -> usize {
        self.instructions.len()
    }

    /// Replace the jump offset at the given jump instruction
    pub fn patch_jump_instruction(&mut self, jump: usize, offset: u16) {
        match self.instructions[jump] {
            OpCode::Jump(ref mut placeholder) | OpCode::JumpIfFalse(ref mut placeholder) => {
                *placeholder = offset;
            }
            _ => unreachable!("The given location must hold a jump instruction."),
        }
    }

    /// Add a constant value to the chunk and return it position in the Vec
    pub fn write_const(&mut self, val: Value) -> usize {
        self.constants.push(val);
        self.constants.len() - 1
    }

    /// Read the constant at the given index
    pub fn read_const(&self, idx: usize) -> &Value {
        &self.constants[idx]
    }

    /// Get the number of constants stored in the chunk
    pub fn const_count(&self) -> usize {
        self.constants.len()
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

    let constant_instruction = |op_repr: &str, const_id: u8| {
        println!(
            "{:-16} {:4} {}",
            op_repr,
            const_id,
            chunk.read_const(const_id as usize)
        );
    };
    let byte_instruction = |op_repr: &str, slot: u8| println!("{:-16} {:4}", op_repr, slot);
    let jump_instruction = |op_repr: &str, jump: usize, offset: u16, fwd: bool| {
        // +1 since the instruction pointer is increased right after we read an opcode
        let jump_target = if fwd {
            jump + 1 + offset as usize
        } else {
            jump + 1 - offset as usize
        };
        println!("{:-16} {:4} -> {}", op_repr, jump, jump_target);
    };

    match chunk.instructions[idx] {
        OpCode::Pop => println!("OP_POP"),
        OpCode::Print => println!("OP_PRINT"),
        OpCode::Return => println!("OP_RETURN"),
        OpCode::Loop(ref offset) => jump_instruction("OP_LOOP", idx, *offset, false),
        OpCode::Jump(ref offset) => jump_instruction("OP_JUMP", idx, *offset, true),
        OpCode::JumpIfFalse(ref offset) => jump_instruction("OP_JUMP_IF_FALSE", idx, *offset, true),
        OpCode::Call(ref idx) => byte_instruction("OP_CALL", *idx),
        OpCode::Closure(ref constant, ref upvalues) => {
            let value = chunk.read_const(idx as usize);
            println!("{:-16} {:4} {}", "OP_CLOSURE", constant, value);
            for upvalue in upvalues {
                println!(
                    "{:04}      |                     {} {}",
                    idx,
                    if upvalue.is_local { "local" } else { "upvalue" },
                    upvalue.index,
                )
            }
        }
        OpCode::Class(ref const_id) => constant_instruction("OP_CLASS", *const_id),
        OpCode::CloseUpvalue => println!("OP_CLOSE_UPVALUE"),
        OpCode::GetUpvalue(ref idx) => byte_instruction("OP_GET_UPVALUE", *idx),
        OpCode::SetUpvalue(ref idx) => byte_instruction("OP_SET_UPVALUE", *idx),
        OpCode::GetLocal(ref slot) => byte_instruction("OP_GET_LOCAL", *slot),
        OpCode::SetLocal(ref slot) => byte_instruction("OP_SET_LOCAL", *slot),
        OpCode::DefineGlobal(ref const_id) => constant_instruction("OP_DEFINE_GLOBAL", *const_id),
        OpCode::GetGlobal(ref const_id) => constant_instruction("OP_GET_GLOBAL", *const_id),
        OpCode::SetGlobal(ref const_id) => constant_instruction("OP_SET_GLOBAL", *const_id),
        OpCode::Constant(ref const_id) => constant_instruction("OP_CONSTANT", *const_id),
        OpCode::Nil => println!("OP_NIL"),
        OpCode::True => println!("OP_TRUE"),
        OpCode::False => println!("OP_FALSE"),
        OpCode::Not => println!("OP_NOT"),
        OpCode::Negate => println!("OP_NEGATE"),
        OpCode::Equal => println!("OP_EQUAL"),
        OpCode::Greater => println!("OP_GREATER"),
        OpCode::Less => println!("OP_LESS"),
        OpCode::Add => println!("OP_ADD"),
        OpCode::Subtract => println!("OP_SUBTRACT"),
        OpCode::Multiply => println!("OP_MULTIPLY"),
        OpCode::Divide => println!("OP_DIVIDE"),
    }
}
