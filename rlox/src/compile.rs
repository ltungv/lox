use crate::{Error, Result};

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

        if token.typ == TokenType::Eof {
            break Ok(());
        }
    }
}

/// Scanner reads characters from the source code and groups them in to
/// a sequence of tokens.
#[derive(Debug)]
struct Scanner<'a> {
    src: &'a str,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    /// Create a new scanner
    fn new(src: &'a str) -> Self {
        Self {
            src,
            start: 0,
            current: 0,
            line: 0,
        }
    }

    /// Consume and return the next token from source.
    fn scan(&mut self) -> Token {
        self.skip_whitespaces();
        self.start = self.current;
        if self.eof() {
            return self.token(TokenType::Eof);
        }
        match self.next() {
            "(" => self.token(TokenType::LParen),
            ")" => self.token(TokenType::RParen),
            "{" => self.token(TokenType::LBrace),
            "}" => self.token(TokenType::RBrace),
            ";" => self.token(TokenType::Semicolon),
            "," => self.token(TokenType::Comma),
            "." => self.token(TokenType::Dot),
            "-" => self.token(TokenType::Minus),
            "+" => self.token(TokenType::Plus),
            "/" => self.token(TokenType::Slash),
            "*" => self.token(TokenType::Star),
            "!" => {
                if self.take("=") {
                    self.token(TokenType::BangEqual)
                } else {
                    self.token(TokenType::Bang)
                }
            }
            "=" => {
                if self.take("=") {
                    self.token(TokenType::EqualEqual)
                } else {
                    self.token(TokenType::Equal)
                }
            }
            "<" => {
                if self.take("=") {
                    self.token(TokenType::LessEqual)
                } else {
                    self.token(TokenType::Less)
                }
            }
            ">" => {
                if self.take("=") {
                    self.token(TokenType::GreaterEqual)
                } else {
                    self.token(TokenType::Greater)
                }
            }
            "\"" => self.string(),
            n if is_digit(n) => self.number(),
            c if is_alpha(c) => self.ident(),
            _ => self.token_err("Unexpected character."),
        }
    }

    fn ident(&mut self) -> Token {
        while is_alpha(self.peek()) || is_digit(self.peek()) {
            self.next();
        }
        self.token(self.ident_type())
    }

    /// Determine if an identifer if a keyword by examine its prefix tree.
    fn ident_type(&self) -> TokenType {
        match self.at(self.start) {
            "a" => return self.check_keyword(1, 2, "nd", TokenType::And),
            "c" => return self.check_keyword(1, 4, "lass", TokenType::Class),
            "e" => return self.check_keyword(1, 3, "lse", TokenType::Else),
            "i" => return self.check_keyword(1, 1, "f", TokenType::If),
            "f" if self.current - self.start > 1 => match self.at(self.start + 1) {
                "a" => return self.check_keyword(2, 3, "il", TokenType::False),
                "o" => return self.check_keyword(2, 1, "r", TokenType::For),
                "u" => return self.check_keyword(2, 1, "rint", TokenType::Fun),
                _ => {}
            },
            "n" => return self.check_keyword(1, 2, "il", TokenType::Nil),
            "o" => return self.check_keyword(1, 1, "r", TokenType::Or),
            "p" => return self.check_keyword(1, 4, "rint", TokenType::Print),
            "r" => return self.check_keyword(1, 5, "eturn", TokenType::Return),
            "s" => return self.check_keyword(1, 4, "uper", TokenType::Super),
            "t" if self.current - self.start > 1 => match self.at(self.start + 1) {
                "h" => return self.check_keyword(2, 2, "is", TokenType::This),
                "r" => return self.check_keyword(2, 2, "ue", TokenType::True),
                _ => {}
            },
            "v" => return self.check_keyword(1, 2, "ar", TokenType::Var),
            "w" => return self.check_keyword(1, 4, "hile", TokenType::While),

            _ => {}
        }
        TokenType::Ident
    }

    fn check_keyword(&self, start: usize, len: usize, rest: &str, typ: TokenType) -> TokenType {
        if self.current - self.start == start + len
            && &self.src[self.start + start..self.start + start + len] == rest
        {
            return typ;
        }
        TokenType::Ident
    }

    fn number(&mut self) -> Token {
        while is_digit(self.peek()) {
            self.next();
        }
        if self.peek() == "." && is_digit(self.peek_next()) {
            self.next();
            while is_digit(self.peek()) {
                self.next();
            }
        }
        self.token(TokenType::Number)
    }

    fn string(&mut self) -> Token {
        while self.peek() != "\"" && !self.eof() {
            if self.peek() == "\n" {
                self.line += 1;
            }
            self.next();
        }

        if self.eof() {
            return self.token_err("Unterminated string.");
        }
        self.next();
        self.token(TokenType::String)
    }

    fn token(&self, typ: TokenType) -> Token {
        Token {
            typ,
            lexeme: &self.src[self.start..self.current],
            line: self.line,
        }
    }

    fn token_err(&self, msg: &'static str) -> Token {
        Token {
            typ: TokenType::Error,
            lexeme: msg,
            line: self.line,
        }
    }

    fn skip_whitespaces(&mut self) {
        loop {
            match self.peek() {
                " " | "\r" | "\t" => {
                    self.next();
                }
                "\n" => {
                    self.line += 1;
                    self.next();
                }
                "/" => {
                    if self.peek_next() == "/" {
                        while self.peek() != "\n" && !self.eof() {
                            self.next();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn peek(&self) -> &str {
        if self.eof() {
            "\0"
        } else {
            self.at(self.current)
        }
    }

    fn peek_next(&self) -> &str {
        if self.eof() || self.current + 1 >= self.src.len() {
            "\0"
        } else {
            self.at(self.current + 1)
        }
    }

    fn next(&mut self) -> &str {
        self.current += 1;
        self.at(self.current - 1)
    }

    fn take(&mut self, expected: &str) -> bool {
        if self.eof() {
            return false;
        }
        if self.at(self.current) != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn at(&self, pos: usize) -> &str {
        &self.src[pos..pos + 1]
    }

    fn eof(&self) -> bool {
        self.current >= self.src.len()
    }
}

/// Lox token. One difference of the implementation of glox is that we don't
/// eagerly evaluate the value of a literal.
#[derive(Debug)]
struct Token<'a> {
    typ: TokenType,
    lexeme: &'a str,
    line: usize,
}

#[derive(Debug, PartialEq, Eq)]
/// Lox token types
enum TokenType {
    /// Single character '('
    LParen,
    /// Single character ')'
    RParen,
    /// Single character '{'
    LBrace,
    /// Single character '}'
    RBrace,
    /// Single character ','
    Comma,
    /// Single character '.'
    Dot,
    /// Single character '-'
    Minus,
    /// Single character '+'
    Plus,
    /// Single character ';'
    Semicolon,
    /// Single character '/'
    Slash,
    /// Single character '*'
    Star,
    /// Single character '!'
    Bang,
    /// Double character '!='
    BangEqual,
    /// Single character '='
    Equal,
    /// Double character '=='
    EqualEqual,
    /// Single character '>'
    Greater,
    /// Double character '>='
    GreaterEqual,
    /// Single character '<'
    Less,
    /// Double character '<='
    LessEqual,
    /// Named entity
    Ident,
    /// String literal
    String,
    /// Number literal
    Number,
    /// Keyword 'and'
    And,
    /// Keyword 'class'
    Class,
    /// Keyword 'else'
    Else,
    /// Boolean literal 'false'
    False,
    /// Keyword 'for'
    For,
    /// Keyword 'fun'
    Fun,
    /// Keyword 'if'
    If,
    /// Nothing literal 'nil'
    Nil,
    /// Keyword 'or'
    Or,
    /// Keyword 'print'
    Print,
    /// Keyword 'return'
    Return,
    /// Keyword 'super'
    Super,
    /// Keyword 'this'
    This,
    /// Boolean literal 'true'
    True,
    /// Keyword 'var'
    Var,
    /// Keyword 'while'
    While,
    /// Special token for end of file
    Eof,
    /// Special token for reporting error
    Error,
}

fn is_digit(s: &str) -> bool {
    s.chars().all(|c| c.is_digit(10))
}

fn is_alpha(s: &str) -> bool {
    s.chars().all(|c| c.is_alphabetic() || c == '_')
}
