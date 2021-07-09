use std::{fmt, str::Chars};

use itertools::{self, MultiPeek};

use crate::{
    token::{self, Token},
    Position,
};

/// Error while scanning Lox source code
#[derive(Debug, Clone)]
pub enum ScanError {
    /// A string literal is unterminated
    UnterminatedString(Position),
    /// Invalid character
    UnexpectedCharacter(Position, char),
}
impl std::error::Error for ScanError {}
impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnterminatedString(pos) => write!(f, "{} Error: Unterminated string.", pos),
            Self::UnexpectedCharacter(pos, c) => {
                write!(f, "{} Error: Unexpected character '{}'.", pos, c)
            }
        }
    }
}

/// Scanner reads characters from the source code and groups them in to a sequence of tokens.
#[derive(Debug)]
pub struct Scanner<'a> {
    src: MultiPeek<Chars<'a>>,
    lexeme: String,
    pos: Position,
}

impl<'a> IntoIterator for Scanner<'a> {
    type Item = Result<Token, ScanError>;
    type IntoIter = Iter<'a>;
    fn into_iter(self) -> Self::IntoIter {
        Iter { scanner: self }
    }
}

impl<'a> Scanner<'a> {
    /// Create a new scanner
    pub fn new(src: &'a str) -> Self {
        let src = itertools::multipeek(src.chars());
        Self {
            src,
            lexeme: String::new(),
            pos: Default::default(),
        }
    }

    /// Consume and return the next token from source.
    pub fn scan(&mut self) -> Result<Option<Token>, ScanError> {
        self.skip_whitespace();
        self.lexeme.clear();
        let c = match self.advance() {
            None => return Ok(None),
            Some(c) => c,
        };

        Ok(Some(match c {
            '(' => self.make_token(token::Type::LParen),
            ')' => self.make_token(token::Type::RParen),
            '{' => self.make_token(token::Type::LBrace),
            '}' => self.make_token(token::Type::RBrace),
            ';' => self.make_token(token::Type::Semicolon),
            ',' => self.make_token(token::Type::Comma),
            '.' => self.make_token(token::Type::Dot),
            '-' => self.make_token(token::Type::Minus),
            '+' => self.make_token(token::Type::Plus),
            '/' => self.make_token(token::Type::Slash),
            '*' => self.make_token(token::Type::Star),
            '!' => {
                if self.consume('=') {
                    self.make_token(token::Type::BangEqual)
                } else {
                    self.make_token(token::Type::Bang)
                }
            }
            '=' => {
                if self.consume('=') {
                    self.make_token(token::Type::EqualEqual)
                } else {
                    self.make_token(token::Type::Equal)
                }
            }
            '<' => {
                if self.consume('=') {
                    self.make_token(token::Type::LessEqual)
                } else {
                    self.make_token(token::Type::Less)
                }
            }
            '>' => {
                if self.consume('=') {
                    self.make_token(token::Type::GreaterEqual)
                } else {
                    self.make_token(token::Type::Greater)
                }
            }
            '"' => self.string()?,
            n if is_digit(n) => self.number(),
            c if is_alpha(c) => self.identity(),
            c => {
                return Err(ScanError::UnexpectedCharacter(self.pos, c));
            }
        }))
    }

    fn identity(&mut self) -> Token {
        while self.peek_check(|c| is_alpha(c) || is_digit(c)) {
            self.advance();
        }
        self.make_token(match self.lexeme.as_ref() {
            "and" => token::Type::And,
            "class" => token::Type::Class,
            "else" => token::Type::Else,
            "if" => token::Type::If,
            "false" => token::Type::False,
            "for" => token::Type::For,
            "fun" => token::Type::Fun,
            "nil" => token::Type::Nil,
            "or" => token::Type::Or,
            "print" => token::Type::Print,
            "return" => token::Type::Return,
            "super" => token::Type::Super,
            "this" => token::Type::This,
            "true" => token::Type::True,
            "var" => token::Type::Var,
            "while" => token::Type::While,
            _ => token::Type::Ident,
        })
    }

    fn number(&mut self) -> Token {
        while self.peek_check(is_digit) {
            self.advance();
        }
        if self.peek_check(|c| c == '.') && self.peek_next_check(is_digit) {
            self.advance();
            while self.peek_check(is_digit) {
                self.advance();
            }
        }
        self.make_token(token::Type::Number)
    }

    fn string(&mut self) -> Result<Token, ScanError> {
        while self.peek_check(|c| c != '"') {
            self.advance();
        }
        if self.peek().is_none() {
            return Err(ScanError::UnterminatedString(self.pos));
        }
        self.advance();
        Ok(self.make_token(token::Type::String))
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                ' ' | '\r' | '\t' | '\n' => {
                    self.advance();
                }
                '/' => {
                    if !self.peek_check(|c| c == '/') {
                        return;
                    }
                    while self.peek_check(|c| c != '\n') {
                        self.advance();
                    }
                }
                _ => return,
            }
        }
    }

    fn peek_check<F: Fn(char) -> bool>(&mut self, check: F) -> bool {
        self.peek().map(check).unwrap_or(false)
    }

    fn peek_next_check<F: Fn(char) -> bool>(&mut self, check: F) -> bool {
        self.peek_next().map(check).unwrap_or(false)
    }

    fn peek(&mut self) -> Option<char> {
        self.src.reset_peek();
        self.src.peek().copied()
    }

    fn peek_next(&mut self) -> Option<char> {
        self.src.reset_peek();
        match self.src.peek() {
            None => None,
            Some(_) => self.src.peek().copied(),
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.src.next().map(|c| {
            self.lexeme.push(c);
            if c == '\n' {
                self.pos.next_line();
            } else {
                self.pos.next_column();
            }
            c
        })
    }

    fn consume(&mut self, expected: char) -> bool {
        match self.peek() {
            None => false,
            Some(c) if c != expected => false,
            _ => {
                self.advance();
                true
            }
        }
    }

    fn make_token(&mut self, typ: token::Type) -> Token {
        Token {
            typ,
            lexeme: self.lexeme.drain(..).collect(),
            pos: self.pos,
        }
    }
}

/// An interator for the tokens scanner
#[derive(Debug)]
pub struct Iter<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Iterator for Iter<'a> {
    type Item = Result<Token, ScanError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.scanner.scan().transpose()
    }
}

fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

fn is_alpha(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}
