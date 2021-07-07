use crate::{scan::Scanner, token, Result};

/// Compile the given source code in to bytecodes that can be read
/// by the virtual machine
pub fn compile(src: &str) -> Result<()> {
    let mut s = Scanner::new(src);
    let mut line = None;
    loop {
        let token = s.scan();
        if line.is_none() || token.pos.line != line.unwrap() {
            print!("{:4},{:<4}   ", token.pos.line, token.pos.column);
            line = Some(token.pos.line);
        } else {
            print!("     {:<4} | ", token.pos.column);
        }
        println!("{:?} '{:}'", token.typ, token.lexeme);

        if token.typ == token::Type::Eof {
            break Ok(());
        }
    }
}
