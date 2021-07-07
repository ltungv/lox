/// Lox token. One difference of the implementation of glox is that we don't
/// eagerly evaluate the value of a literal.
#[derive(Debug)]
pub struct Token {
    /// Lox token type
    pub typ: Type,
    /// The string segment in source that represents this token.
    ///
    /// # Notes
    ///
    /// We are gonna copy the string segment that contains the lexem of
    /// our token, so we don't have to deal with rust's compile, this can be
    /// implemented more efficiently.
    pub lexeme: String,
    /// The position at which this token was found in source.
    pub pos: Position,
}

/// Lox token types
#[derive(Debug, PartialEq, Eq)]
pub enum Type {
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
}

/// Position of the token in source
#[derive(Debug, Clone, Copy)]
pub struct Position {
    /// Current line in source file
    pub line: usize,
    /// Current column in source file
    pub column: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self { line: 1, column: 1 }
    }
}

impl Position {
    /// Increment the line count by one and reset the column count
    pub fn next_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    /// Increment the column count by one
    pub fn next_column(&mut self) {
        self.column += 1;
    }
}
