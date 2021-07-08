use std::{
    convert::{From, Into},
    fmt,
    iter::Peekable,
};

use crate::{scan, token, Chunk, OpCode, Position, Scanner, Token, Value};

/// Error while parsing Lox tokens
#[derive(Debug)]
pub enum ParseError {
    /// Current token is not supposed to be there
    UnexpectedToken(Token, String),
    /// Reached EOF abruptly
    UnexpectedEof,
}
impl std::error::Error for ParseError {}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken(ref tok, ref msg) => {
                write!(f, "{} Error at '{}': {}.", tok.pos, tok.lexeme, msg,)
            }
            Self::UnexpectedEof => write!(f, "Error: Unexpected end of file."),
        }
    }
}

/// Compile the given source code in to bytecodes that can be read
/// by the virtual machine
pub fn compile(src: &str) -> Option<Chunk> {
    let mut chunk = Chunk::default();
    let mut parser = Parser::new(&mut chunk, src);
    if let Err(err) = parser.expression() {
        eprintln!("{}", err);
    };
    if parser.had_error {
        return None;
    }
    Some(chunk)
}

/// Scan for tokens and emit corresponding bytecodes.
#[derive(Debug)]
pub struct Parser<'a> {
    chunk: &'a mut Chunk,
    tokens: Peekable<scan::Iter<'a>>,
    had_error: bool,
}

impl<'a> Parser<'a> {
    /// Create a new parser
    pub fn new(chunk: &'a mut Chunk, src: &'a str) -> Self {
        Self {
            chunk,
            tokens: Scanner::new(src).into_iter().peekable(),
            had_error: false,
        }
    }

    fn expression(&mut self) -> Result<(), ParseError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), ParseError> {
        let tok = self.peek().ok_or(ParseError::UnexpectedEof)?;
        match Self::get_rule(&tok.typ).prefix {
            None => {
                self.had_error = true;
                return Err(ParseError::UnexpectedToken(
                    tok,
                    "Expect expression".to_string(),
                ));
            }
            Some(rule) => rule(self)?,
        };

        while let Some(tok) = self.peek() {
            if precedence > Self::get_rule(&tok.typ).precedence {
                break;
            }

            if let Some(infix_rule) = Self::get_rule(&tok.typ).infix {
                infix_rule(self)?;
            } else {
                self.had_error = true;
                return Err(ParseError::UnexpectedToken(tok, "".to_string()));
            }
        }
        Ok(())
    }

    fn binary(&mut self) -> Result<(), ParseError> {
        let operator = self.advance().ok_or(ParseError::UnexpectedEof)?;
        let rule = Self::get_rule(&operator.typ);
        self.parse_precedence(rule.precedence.next())?;

        match operator.typ {
            token::Type::Plus => self.emit(OpCode::Add, operator.pos),
            token::Type::Minus => self.emit(OpCode::Subtract, operator.pos),
            token::Type::Star => self.emit(OpCode::Multiply, operator.pos),
            token::Type::Slash => self.emit(OpCode::Divide, operator.pos),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn unary(&mut self) -> Result<(), ParseError> {
        let operator = self.advance().ok_or(ParseError::UnexpectedEof)?;
        self.parse_precedence(Precedence::Unary)?;
        match operator.typ {
            token::Type::Minus => self.emit(OpCode::Negate, operator.pos),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn grouping(&mut self) -> Result<(), ParseError> {
        self.consume(token::Type::LParen, "Unexpected token.")?;
        self.expression()?;
        self.consume(token::Type::RParen, "Expect ')' after expression")
    }

    fn number(&mut self) -> Result<(), ParseError> {
        let tok = self.advance().ok_or(ParseError::UnexpectedEof)?;
        assert_eq!(tok.typ, token::Type::Number);

        let value = tok.lexeme.parse().unwrap();
        let constant = self.chunk.write_const(Value::Number(value));
        self.emit(OpCode::Constant(constant), tok.pos);
        Ok(())
    }

    fn advance(&mut self) -> Option<Token> {
        while let Some(Err(err)) = self.tokens.peek() {
            eprintln!("{}", err);
            self.had_error = true;
        }
        self.tokens.next().map(Result::unwrap)
    }

    fn peek(&mut self) -> Option<Token> {
        while let Some(Err(err)) = self.tokens.peek() {
            eprintln!("{}", err);
            self.had_error = true;
        }
        self.tokens.peek().map(|peeked| match peeked {
            Err(_) => unreachable!("Errors should have been skipped."),
            Ok(tok) => tok.clone(),
        })
    }

    fn consume(&mut self, typ: token::Type, msg: &str) -> Result<(), ParseError> {
        match self.tokens.peek() {
            None => {
                self.had_error = true;
                Err(ParseError::UnexpectedEof)
            }
            Some(Err(_)) => unreachable!("Invalid tokens should already be skipped."),
            Some(Ok(tok)) if tok.typ != typ => {
                self.had_error = true;
                Err(ParseError::UnexpectedToken(tok.clone(), msg.to_string()))
            }
            _ => {
                self.advance();
                Ok(())
            }
        }
    }

    fn emit(&mut self, code: OpCode, pos: Position) {
        self.chunk.write_instruction(code, pos);
    }

    fn get_rule(typ: &token::Type) -> ParseRule<'a> {
        let rule: (Precedence, Option<ParseFn<'a>>, Option<ParseFn<'a>>) = match typ {
            token::Type::LParen => (Precedence::None, Some(Self::grouping), None),
            token::Type::RParen => (Precedence::None, None, None),
            token::Type::LBrace => (Precedence::None, None, None),
            token::Type::RBrace => (Precedence::None, None, None),
            token::Type::Comma => (Precedence::None, None, None),
            token::Type::Dot => (Precedence::None, None, None),
            token::Type::Minus => (Precedence::Term, Some(Self::unary), Some(Self::binary)),
            token::Type::Plus => (Precedence::Term, None, Some(Self::binary)),
            token::Type::Semicolon => (Precedence::None, None, None),
            token::Type::Slash => (Precedence::Factor, None, Some(Self::binary)),
            token::Type::Star => (Precedence::Factor, None, Some(Self::binary)),
            token::Type::Bang => (Precedence::None, None, None),
            token::Type::BangEqual => (Precedence::None, None, None),
            token::Type::Equal => (Precedence::None, None, None),
            token::Type::EqualEqual => (Precedence::None, None, None),
            token::Type::Greater => (Precedence::None, None, None),
            token::Type::GreaterEqual => (Precedence::None, None, None),
            token::Type::Less => (Precedence::None, None, None),
            token::Type::LessEqual => (Precedence::None, None, None),
            token::Type::Ident => (Precedence::None, None, None),
            token::Type::String => (Precedence::None, None, None),
            token::Type::Number => (Precedence::None, Some(Self::number), None),
            token::Type::And => (Precedence::None, None, None),
            token::Type::Class => (Precedence::None, None, None),
            token::Type::Else => (Precedence::None, None, None),
            token::Type::False => (Precedence::None, None, None),
            token::Type::For => (Precedence::None, None, None),
            token::Type::Fun => (Precedence::None, None, None),
            token::Type::If => (Precedence::None, None, None),
            token::Type::Nil => (Precedence::None, None, None),
            token::Type::Or => (Precedence::None, None, None),
            token::Type::Print => (Precedence::None, None, None),
            token::Type::Return => (Precedence::None, None, None),
            token::Type::Super => (Precedence::None, None, None),
            token::Type::This => (Precedence::None, None, None),
            token::Type::True => (Precedence::None, None, None),
            token::Type::Var => (Precedence::None, None, None),
            token::Type::While => (Precedence::None, None, None),
            token::Type::Eof => (Precedence::None, None, None),
        };
        rule.into()
    }
}

type ParseFn<'a> = fn(&mut Parser<'a>) -> Result<(), ParseError>;

struct ParseRule<'a> {
    precedence: Precedence,
    prefix: Option<ParseFn<'a>>,
    infix: Option<ParseFn<'a>>,
}

impl<'a> From<(Precedence, Option<ParseFn<'a>>, Option<ParseFn<'a>>)> for ParseRule<'a> {
    fn from(
        (precedence, prefix, infix): (Precedence, Option<ParseFn<'a>>, Option<ParseFn<'a>>),
    ) -> Self {
        Self {
            precedence,
            prefix,
            infix,
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
}
