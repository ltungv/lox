use crate::{
    scan::{ScanError, Scanner},
    token, Result,
};

/// Compile the given source code in to bytecodes that can be read
/// by the virtual machine
pub fn compile(src: &str) -> Result<()> {
    let mut s = Scanner::new(src);
    loop {
        match s.scan() {
            Ok(t) => {
                println!(
                    "{:4},{:<4} | {:?} '{:}'",
                    t.pos.line, t.pos.column, t.typ, t.lexeme
                );

                if t.typ == token::Type::Eof {
                    return Ok(());
                }
            }
            Err(err) => match err {
                ScanError::UnterminatedString(pos) => {
                    eprintln!("[{},{}] Error: Unterminated string.", pos.line, pos.column)
                }
                ScanError::UnexpectedCharacter(pos, c) => {
                    eprintln!(
                        "[{},{}] Error: Unexpected character '{}'.",
                        pos.line, pos.column, c
                    )
                }
            },
        }
    }
}
