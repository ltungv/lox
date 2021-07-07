use crate::{
    token::{self, Token},
    Position,
};

/// Error while scanning Lox tokens
#[derive(Debug)]
pub enum ScanError {
    /// A string literal is unterminated
    UnterminatedString(Position),
    /// Invalid character
    UnexpectedCharacter(Position, char),
}

/// Scanner reads characters from the source code and groups them in to
/// a sequence of tokens.
#[derive(Debug)]
pub struct Scanner {
    src: Vec<char>,
    start: usize,
    current: usize,
    pos: Position,
}

impl Scanner {
    /// Create a new scanner
    pub fn new(src: &str) -> Self {
        let src = src.chars().collect();
        Self {
            src,
            start: 0,
            current: 0,
            pos: Default::default(),
        }
    }

    /// Consume and return the next token from source.
    pub fn scan(&mut self) -> Result<Token, ScanError> {
        self.skip_whitespace();
        self.start = self.current;
        if self.is_source_end() {
            return Ok(self.token(token::Type::Eof));
        }
        Ok(match self.advance() {
            '(' => self.token(token::Type::LParen),
            ')' => self.token(token::Type::RParen),
            '{' => self.token(token::Type::LBrace),
            '}' => self.token(token::Type::RBrace),
            ';' => self.token(token::Type::Semicolon),
            ',' => self.token(token::Type::Comma),
            '.' => self.token(token::Type::Dot),
            '-' => self.token(token::Type::Minus),
            '+' => self.token(token::Type::Plus),
            '/' => self.token(token::Type::Slash),
            '*' => self.token(token::Type::Star),
            '!' => {
                if self.consume('=') {
                    self.token(token::Type::BangEqual)
                } else {
                    self.token(token::Type::Bang)
                }
            }
            '=' => {
                if self.consume('=') {
                    self.token(token::Type::EqualEqual)
                } else {
                    self.token(token::Type::Equal)
                }
            }
            '<' => {
                if self.consume('=') {
                    self.token(token::Type::LessEqual)
                } else {
                    self.token(token::Type::Less)
                }
            }
            '>' => {
                if self.consume('=') {
                    self.token(token::Type::GreaterEqual)
                } else {
                    self.token(token::Type::Greater)
                }
            }
            '"' => self.string()?,
            n if is_digit(n) => self.number(),
            c if is_alpha(c) => self.identity(),
            c => {
                return Err(ScanError::UnexpectedCharacter(self.pos, c));
            }
        })
    }

    fn identity(&mut self) -> Token {
        while is_alpha(self.peek()) || is_digit(self.peek()) {
            self.advance();
        }
        self.token(self.identity_token_type())
    }

    /// Determine if an identifer if a keyword by examine its prefix tree.
    fn identity_token_type(&self) -> token::Type {
        match self.src[self.start] {
            'a' => return self.check_keyword(1, 2, &['n', 'd'], token::Type::And),
            'c' => return self.check_keyword(1, 4, &['l', 'a', 's', 's'], token::Type::Class),
            'e' => return self.check_keyword(1, 3, &['l', 's', 'e'], token::Type::Else),
            'i' => return self.check_keyword(1, 1, &['f'], token::Type::If),
            'f' if self.current - self.start > 1 => match self.src[self.start + 1] {
                'a' => return self.check_keyword(2, 3, &['i', 'l'], token::Type::False),
                'o' => return self.check_keyword(2, 1, &['r'], token::Type::For),
                'u' => return self.check_keyword(2, 1, &['r', 'i', 'n', 't'], token::Type::Fun),
                _ => {}
            },
            'n' => return self.check_keyword(1, 2, &['i', 'l'], token::Type::Nil),
            'o' => return self.check_keyword(1, 1, &['r'], token::Type::Or),
            'p' => return self.check_keyword(1, 4, &['r', 'i', 'n', 't'], token::Type::Print),
            'r' => {
                return self.check_keyword(1, 5, &['e', 't', 'u', 'r', 'n'], token::Type::Return)
            }
            's' => return self.check_keyword(1, 4, &['u', 'p', 'e', 'r'], token::Type::Super),
            't' if self.current - self.start > 1 => match self.src[self.start + 1] {
                'h' => return self.check_keyword(2, 2, &['i', 's'], token::Type::This),
                'r' => return self.check_keyword(2, 2, &['u', 'e'], token::Type::True),
                _ => {}
            },
            'v' => return self.check_keyword(1, 2, &['a', 'r'], token::Type::Var),
            'w' => return self.check_keyword(1, 4, &['h', 'i', 'l', 'e'], token::Type::While),
            _ => {}
        }
        token::Type::Ident
    }

    fn check_keyword(
        &self,
        start: usize,
        len: usize,
        rest: &[char],
        typ: token::Type,
    ) -> token::Type {
        if self.current - self.start == start + len
            && &self.src[self.start + start..self.start + start + len] == rest
        {
            return typ;
        }
        token::Type::Ident
    }

    fn number(&mut self) -> Token {
        while is_digit(self.peek()) {
            self.advance();
        }
        if self.peek() == '.' && is_digit(self.peek_next()) {
            self.advance();
            while is_digit(self.peek()) {
                self.advance();
            }
        }
        self.token(token::Type::Number)
    }

    fn string(&mut self) -> Result<Token, ScanError> {
        while self.peek() != '"' && !self.is_source_end() {
            self.advance();
        }

        if self.is_source_end() {
            return Err(ScanError::UnterminatedString(self.pos));
        }
        self.advance();
        Ok(self.token(token::Type::String))
    }

    fn token(&self, typ: token::Type) -> Token {
        Token {
            typ,
            lexeme: self.src[self.start..self.current].iter().cloned().collect(),
            pos: self.pos,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' | '\n' => {
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' && !self.is_source_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn peek(&self) -> char {
        if self.is_source_end() {
            '\0'
        } else {
            self.src[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.is_source_end() || self.current + 1 >= self.src.len() {
            '\0'
        } else {
            self.src[self.current + 1]
        }
    }

    fn advance(&mut self) -> char {
        let c = self.src[self.current];
        if c == '\n' {
            self.pos.next_line();
        } else {
            self.pos.next_column();
        }
        self.current += 1;
        c
    }

    fn consume(&mut self, expected: char) -> bool {
        if self.is_source_end() {
            return false;
        }
        if self.src[self.current] != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn is_source_end(&self) -> bool {
        self.current >= self.src.len()
    }
}

fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

fn is_alpha(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}
