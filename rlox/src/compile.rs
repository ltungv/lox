use std::{fmt, iter::Peekable};

use crate::{scan, token, Chunk, ScanError, Scanner, Token};

/// Compile the given source code in to bytecodes that can be read
/// by the virtual machine
pub fn compile(src: &str) -> Result<(), ParseError> {
    let mut chunk = Chunk::default();
    let mut parser = Parser::new(&mut chunk, src);
    parser.advance();
    // parser.expression();
    Ok(())
}

/// Scan for tokens and emit corresponding bytecodes.
#[derive(Debug)]
pub struct Parser<'a> {
    chunk: &'a mut Chunk,
    tokens: Peekable<scan::Iter<'a>>,
    scan_errors: Vec<ScanError>,
}

impl<'a> Parser<'a> {
    /// Create a new parser
    pub fn new(chunk: &'a mut Chunk, src: &'a str) -> Self {
        Self {
            chunk,
            tokens: Scanner::new(src).into_iter().peekable(),
            scan_errors: Vec::new(),
        }
    }

    fn advance(&mut self) -> Option<Token> {
        while let Some(Err(_)) = self.tokens.peek() {
            self.scan_errors
                .push(self.tokens.next().unwrap().unwrap_err());
        }
        self.tokens.next().map(Result::unwrap)
    }

    fn consume(&mut self, typ: token::Type, msg: &str) -> Result<(), ParseError> {
        match self.tokens.peek() {
            None => Err(ParseError::UnexpectedEof),
            Some(Err(_)) => unreachable!("Invalid tokens should already be skipped."),
            Some(Ok(tok)) if tok.typ != typ => {
                Err(ParseError::UnexpectedToken(tok.clone(), msg.to_string()))
            }
            _ => {
                self.advance();
                Ok(())
            }
        }
    }
}

/// Error while parsing Lox tokens
#[derive(Debug)]
pub enum ParseError {
    /// Current token is not supposed to be there
    UnexpectedToken(Token, String),
    /// Reached EOF abruptly
    UnexpectedEof,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken(ref tok, ref msg) => write!(
                f,
                "[line {}] [column {}] Error at '{}': {}.",
                tok.pos.line, tok.pos.column, tok.lexeme, msg,
            ),
            Self::UnexpectedEof => write!(f, "Error: Unexpected end of file."),
        }
    }
}
