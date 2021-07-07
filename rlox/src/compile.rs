use crate::{token, Result, Scanner};

/// Compile the given source code in to bytecodes that can be read
/// by the virtual machine
pub fn compile(src: &str) -> Result<()> {
    let mut s = Scanner::new(src);
    let mut line = None;
    loop {
        let token = s.scan();
        if line.is_none() || token.line != line.unwrap() {
            print!("{:4} ", token.line);
            line = Some(token.line);
        } else {
            print!("   | ");
        }
        println!("{:?} '{:}'", token.typ, token.lexeme);

        if token.typ == token::Type::Eof {
            break Ok(());
        }
    }
}
