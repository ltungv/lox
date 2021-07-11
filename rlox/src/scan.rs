use std::str::Chars;

use itertools::{self, MultiPeek};

use crate::{
    token::{self, Token},
    Position, ScanError,
};

/// Scanner reads characters from the source code and groups them in to a sequence of tokens.
#[derive(Debug)]
pub struct Scanner<'s> {
    src: &'s str,
    src_iter: MultiPeek<Chars<'s>>,
    pos: Position,

    finished: bool,
    lexeme_begin: usize,
    lexeme_end: usize,
}

impl<'s> Scanner<'s> {
    /// Create a new scanner
    pub fn new(src: &'s str) -> Self {
        let src_iter = itertools::multipeek(src.chars());
        Self {
            src,
            src_iter,
            pos: Default::default(),
            finished: false,
            lexeme_begin: 0,
            lexeme_end: 0,
        }
    }

    /// Consume and return the next token from source.
    fn scan(&mut self) -> Result<Option<Token<'s>>, ScanError> {
        if self.finished {
            return Ok(None);
        }

        self.skip_whitespace();
        self.lexeme_begin = self.lexeme_end;
        let c = match self.advance() {
            None => {
                self.finished = true;
                return Ok(Some(Token {
                    typ: token::Type::Eof,
                    lexeme: "",
                    pos: self.pos,
                }));
            }
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

    fn identity(&mut self) -> Token<'s> {
        while self.peek_check(|c| is_alpha(c) || is_digit(c)) {
            self.advance();
        }
        self.make_token(match &self.src[self.lexeme_begin..self.lexeme_end] {
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

    fn number(&mut self) -> Token<'s> {
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

    fn string(&mut self) -> Result<Token<'s>, ScanError> {
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
                    if !self.peek_next_check(|c| c == '/') {
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
        self.src_iter.reset_peek();
        self.src_iter.peek().copied()
    }

    fn peek_next(&mut self) -> Option<char> {
        self.src_iter.reset_peek();
        match self.src_iter.peek() {
            None => None,
            Some(_) => self.src_iter.peek().copied(),
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.src_iter.next().map(|c| {
            self.lexeme_end += c.len_utf8();
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

    fn make_token(&mut self, typ: token::Type) -> Token<'s> {
        Token {
            typ,
            lexeme: &self.src[self.lexeme_begin..self.lexeme_end],
            pos: self.pos,
        }
    }
}

impl<'s> IntoIterator for Scanner<'s> {
    type Item = Result<Token<'s>, ScanError>;
    type IntoIter = Iter<'s>;
    fn into_iter(self) -> Self::IntoIter {
        Iter { scanner: self }
    }
}

/// An interator for the tokens scanner
#[derive(Debug)]
pub struct Iter<'s> {
    scanner: Scanner<'s>,
}

impl<'s> Iterator for Iter<'s> {
    type Item = Result<Token<'s>, ScanError>;
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
