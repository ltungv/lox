use rlox::{Chunk, OpCode, Value};

fn main() {
    let mut chunk = Chunk::default();
    let const_idx = chunk.add_const(Value::Number(1.2));
    chunk.write(OpCode::Constant(const_idx), 123);
    chunk.write(OpCode::Return, 123);
    chunk.disassemble("test chunk", std::io::stdout()).unwrap();
}
