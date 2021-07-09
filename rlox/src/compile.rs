use std::{fmt, iter::Peekable};

use crate::{scan, token, Chunk, OpCode, Position, Scanner, StringInterner, Token, Value};

/// Error while parsing Lox tokens
#[derive(Debug)]
pub enum ParseError {
    /// Current token is not supposed to be there
    UnexpectedToken(Position, String, String),
    /// Reached EOF abruptly
    UnexpectedEof,
}
impl std::error::Error for ParseError {}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken(ref pos, ref lexeme, ref msg) => {
                write!(f, "{} Error at '{}': {}.", pos, lexeme, msg,)
            }
            Self::UnexpectedEof => write!(f, "Error: Unexpected end of file."),
        }
    }
}

/// Compile the given source code in to bytecodes that can be read
/// by the virtual machine
pub fn compile(src: &str) -> Option<(Chunk, StringInterner)> {
    let mut chunk = Chunk::default();
    let mut strings = StringInterner::default();

    let mut parser = Parser::new(&mut chunk, &mut strings, src);
    if let Err(err) = parser.expression() {
        eprintln!("{}", err);
    };

    if parser.had_scan_error {
        return None;
    }
    Some((chunk, strings))
}

/// Scan for tokens and emit corresponding bytecodes.
#[derive(Debug)]
pub struct Parser<'a> {
    chunk: &'a mut Chunk,
    strings: &'a mut StringInterner,
    tokens: Peekable<scan::Iter<'a>>,
    had_scan_error: bool,
}

impl<'a> Parser<'a> {
    /// Create a new parser
    pub fn new(chunk: &'a mut Chunk, strings: &'a mut StringInterner, src: &'a str) -> Self {
        Self {
            chunk,
            strings,
            tokens: Scanner::new(src).into_iter().peekable(),
            had_scan_error: false,
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), ParseError> {
        let tok = self.advance()?;
        self.prefix_fule(&tok)?;

        loop {
            match self.peek() {
                None => break,
                Some(tok) if precedence > Precedence::of(&tok.typ) => break,
                _ => {}
            }
            let tok = self.advance()?;
            self.infix_rule(&tok)?;
        }
        Ok(())
    }

    fn expression(&mut self) -> Result<(), ParseError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn binary(&mut self, operator: &Token) -> Result<(), ParseError> {
        self.parse_precedence(Precedence::of(&operator.typ).next())?;
        match operator.typ {
            token::Type::BangEqual => {
                self.chunk.write_instruction(OpCode::Equal, operator.pos);
                self.chunk.write_instruction(OpCode::Not, operator.pos);
            }
            token::Type::EqualEqual => self.chunk.write_instruction(OpCode::Equal, operator.pos),
            token::Type::Greater => self.chunk.write_instruction(OpCode::Greater, operator.pos),
            token::Type::GreaterEqual => {
                self.chunk.write_instruction(OpCode::Less, operator.pos);
                self.chunk.write_instruction(OpCode::Not, operator.pos);
            }
            token::Type::Less => self.chunk.write_instruction(OpCode::Less, operator.pos),
            token::Type::LessEqual => {
                self.chunk.write_instruction(OpCode::Greater, operator.pos);
                self.chunk.write_instruction(OpCode::Not, operator.pos);
            }
            token::Type::Plus => self.chunk.write_instruction(OpCode::Add, operator.pos),
            token::Type::Minus => self.chunk.write_instruction(OpCode::Subtract, operator.pos),
            token::Type::Star => self.chunk.write_instruction(OpCode::Multiply, operator.pos),
            token::Type::Slash => self.chunk.write_instruction(OpCode::Divide, operator.pos),
            _ => unreachable!("Rule table is wrong."),
        }
        Ok(())
    }

    fn unary(&mut self, operator: &Token) -> Result<(), ParseError> {
        self.parse_precedence(Precedence::Unary)?;
        match operator.typ {
            token::Type::Bang => self.chunk.write_instruction(OpCode::Not, operator.pos),
            token::Type::Minus => self.chunk.write_instruction(OpCode::Negate, operator.pos),
            _ => unreachable!("Rule table is wrong."),
        }
        Ok(())
    }

    fn grouping(&mut self) -> Result<(), ParseError> {
        self.expression()?;
        self.consume(token::Type::RParen, "Expect ')' after expression")
    }

    fn literal(&mut self, tok: &Token) -> Result<(), ParseError> {
        match tok.typ {
            token::Type::False => self.chunk.write_instruction(OpCode::False, tok.pos),
            token::Type::Nil => self.chunk.write_instruction(OpCode::Nil, tok.pos),
            token::Type::True => self.chunk.write_instruction(OpCode::True, tok.pos),
            _ => unreachable!("Rule table is wrong."),
        }
        Ok(())
    }

    fn string(&mut self, tok: &Token) -> Result<(), ParseError> {
        assert_eq!(tok.typ, token::Type::String);
        let value = tok.lexeme[1..tok.lexeme.len() - 1].to_string();
        let constant = self
            .chunk
            .write_const(Value::String(self.strings.get_or_intern(value)));
        self.chunk
            .write_instruction(OpCode::Constant(constant), tok.pos);
        Ok(())
    }

    fn number(&mut self, tok: &Token) -> Result<(), ParseError> {
        assert_eq!(tok.typ, token::Type::Number);
        let value = tok.lexeme.parse().unwrap();
        let constant = self.chunk.write_const(Value::Number(value));
        self.chunk
            .write_instruction(OpCode::Constant(constant), tok.pos);
        Ok(())
    }

    fn advance(&mut self) -> Result<Token<'a>, ParseError> {
        while let Some(Err(err)) = self.tokens.peek() {
            eprintln!("{}", err);
            self.had_scan_error = true;
            self.tokens.next();
        }
        self.tokens
            .next()
            .map(Result::unwrap)
            .ok_or(ParseError::UnexpectedEof)
    }

    fn peek(&mut self) -> Option<&Token> {
        while let Some(Err(err)) = self.tokens.peek() {
            eprintln!("{}", err);
            self.had_scan_error = true;
            self.tokens.next();
        }
        self.tokens.peek().map(|peeked| match peeked {
            Err(_) => unreachable!("Errors should have been skipped."),
            Ok(tok) => tok,
        })
    }

    fn consume(&mut self, typ: token::Type, msg: &str) -> Result<(), ParseError> {
        match self.tokens.peek() {
            Some(Ok(tok)) => {
                if tok.typ == typ {
                    self.advance()?;
                    Ok(())
                } else {
                    Err(ParseError::UnexpectedToken(
                        tok.pos,
                        tok.lexeme.to_string(),
                        msg.to_string(),
                    ))
                }
            }
            None => Err(ParseError::UnexpectedEof),
            Some(Err(_)) => unreachable!("Invalid tokens should already be skipped."),
        }
    }

    fn prefix_fule(&mut self, tok: &Token) -> Result<(), ParseError> {
        match tok.typ {
            token::Type::LParen => self.grouping(),
            token::Type::Minus | token::Type::Bang => self.unary(tok),
            token::Type::String => self.string(tok),
            token::Type::Number => self.number(tok),
            token::Type::False | token::Type::Nil | token::Type::True => self.literal(tok),
            _ => Err(ParseError::UnexpectedToken(
                tok.pos,
                tok.lexeme.to_string(),
                "Expect expression".to_string(),
            )),
        }
    }

    fn infix_rule(&mut self, tok: &Token) -> Result<(), ParseError> {
        match tok.typ {
            token::Type::Minus
            | token::Type::Plus
            | token::Type::Slash
            | token::Type::Star
            | token::Type::BangEqual
            | token::Type::EqualEqual
            | token::Type::Greater
            | token::Type::GreaterEqual
            | token::Type::Less
            | token::Type::LessEqual => self.binary(tok),
            _ => Err(ParseError::UnexpectedToken(
                tok.pos,
                tok.lexeme.to_string(),
                "Expect expression".to_string(),
            )),
        }
    }
}

/// All precedence levels in Lox
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    /// No precedence
    None,
    /// Operator `=`
    Assignment,
    /// Operator `or`
    Or,
    /// Operator `and`
    And,
    /// Operator `==` `!=`
    Equality,
    /// Operator `<` `>` `<=` `>=`
    Comparison,
    /// Operator `+` `-`
    Term,
    /// Operator `*` `/`
    Factor,
    /// Operator `!` `-`
    Unary,
    /// Operator `.` `()`
    Call,
    /// Literal and keywords
    Primary,
}

impl Precedence {
    /// Get the immediately higher precedence level
    fn next(&self) -> Self {
        match self {
            Self::None => Self::Assignment,
            Self::Assignment => Self::Or,
            Self::Or => Self::And,
            Self::And => Self::Equality,
            Self::Equality => Self::Comparison,
            Self::Comparison => Self::Term,
            Self::Term => Self::Factor,
            Self::Factor => Self::Unary,
            Self::Unary => Self::Call,
            Self::Call => Self::Primary,
            Self::Primary => Self::Primary,
        }
    }

    fn of(typ: &token::Type) -> Self {
        match typ {
            token::Type::BangEqual | token::Type::EqualEqual => (Precedence::Equality),
            token::Type::Greater
            | token::Type::GreaterEqual
            | token::Type::Less
            | token::Type::LessEqual => Precedence::Comparison,
            token::Type::Minus | token::Type::Plus => (Precedence::Term),
            token::Type::Slash | token::Type::Star => (Precedence::Factor),
            _ => Self::None,
        }
    }
}
