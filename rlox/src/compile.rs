use std::iter::Peekable;

use crate::{
    scan, token, Chunk, OpCode, ParseError, Position, Scanner, StringInterner, Token, Value,
};

/// Compile the given source code in to bytecodes that can be read
/// by the virtual machine
pub fn compile(src: &str, strings: &mut StringInterner) -> Option<Chunk> {
    let mut chunk = Chunk::default();
    let mut parser = Parser::new(&mut chunk, strings, src);
    parser.parse();
    if parser.had_error {
        return None;
    }
    Some(chunk)
}

/// Scan for tokens and emit corresponding bytecodes.
///
/// # Grammars
///
/// ```text
/// program    --> decl* EOF ;
/// decl       --> classDecl
///              | funDecl
///              | varDecl
///              | stmt ;
/// classDecl  --> "class" IDENT ( "<" IDENT )? "{" function* "}" ;
/// funDecl    --> "fun" function ;
/// function   --> IDENT "(" params? ")" block ;
/// params     --> IDENT ( "," IDENT )* ;
/// varDecl    --> "var" IDENT ( "=" expr )? ";" ;
/// stmt       --> block
///              | exprStmt
///              | forStmt
///              | ifStmt
///              | printStmt
///              | returnStmt
///              | whileStmt ;
/// block      --> "{" decl* "}" ;
/// exprStmt   --> expr ";" ;
/// forStmt    --> "for" "(" ( varDecl | exprStmt | ";" ) expr? ";" expr? ")" stmt ;
/// ifStmt     --> "if" "(" expr ")" stmt ( "else" stmt )? ;
/// printStmt  --> "print" expr ";" ;
/// returnStmt --> "return" expr? ";" ;
/// whileStmt  --> "while" "(" expr ")" stmt ;
/// expr       --> assign ;
/// assign     --> ( call "." )? IDENT "=" expr ";"
///              | or ;
/// or         --> and ( "or" and )* ;
/// and        --> equality ( "and" equality )* ;
/// equality   --> comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison --> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term       --> factor ( ( "-" | "+" ) factor )* ;
/// factor     --> unary ( ( "/" | "*" ) unary )* ;
/// unary      --> ( "!" | "-" ) unary
///              | call ;
/// call       --> primary ( "(" args? ")" | "." IDENT )* ;
/// args       --> expr ( "," expr )* ;
/// primary    --> IDENT | NUMBER | STRING
///              | "this" | "super" "." IDENT
///              | "true" | "false" | "nil"
///              | "(" expr ")" ;
/// ```
#[derive(Debug)]
struct Parser<'a> {
    chunk: &'a mut Chunk,
    strings: &'a mut StringInterner,
    tokens: Peekable<scan::Iter<'a>>,
    prev_position: Position,
    prev_lexeme: &'a str,
    had_error: bool,
}

impl<'a> Parser<'a> {
    /// Create a new parser
    fn new(chunk: &'a mut Chunk, strings: &'a mut StringInterner, src: &'a str) -> Self {
        Self {
            chunk,
            strings,
            tokens: Scanner::new(src).into_iter().peekable(),
            prev_position: Position::default(),
            prev_lexeme: "",
            had_error: false,
        }
    }

    fn parse(&mut self) {
        while self.peek().is_some() {
            if let Err(err) = self.declaration() {
                eprintln!("{}", err);
                self.had_error = true;
                self.synchronize();
            }
        }
    }

    fn declaration(&mut self) -> Result<(), ParseError> {
        if let Ok(tok) = self.consume(token::Type::Var, "") {
            return self.var_declaration(&tok);
        }
        self.statement()
    }

    fn var_declaration(&mut self, tok: &Token) -> Result<(), ParseError> {
        // variable name
        let ident = self.consume(token::Type::Ident, "Expect variable name")?;
        let ident_id = self
            .chunk
            .write_const(Value::String(self.strings.get_or_intern(ident.lexeme)));
        // initializer
        if self.consume(token::Type::Equal, "").is_ok() {
            self.expression()?;
        } else {
            self.chunk.write_instruction(OpCode::Nil, tok.pos);
        }
        // ; terminated
        self.consume(
            token::Type::Semicolon,
            "Expect ';' after variable declaration",
        )?;

        self.chunk
            .write_instruction(OpCode::DefineGlobal(ident_id), tok.pos);
        Ok(())
    }

    fn statement(&mut self) -> Result<(), ParseError> {
        if let Ok(tok) = self.consume(token::Type::Print, "") {
            return self.print_statement(&tok);
        }
        self.expression_statement()
    }

    fn print_statement(&mut self, tok: &Token) -> Result<(), ParseError> {
        self.expression()?;
        self.consume(token::Type::Semicolon, "Expect ';' after value")?;
        self.chunk.write_instruction(OpCode::Print, tok.pos);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<(), ParseError> {
        self.expression()?;
        self.consume(token::Type::Semicolon, "Expect ';' after value")?;
        self.chunk
            .write_instruction(OpCode::Pop, self.prev_position);
        Ok(())
    }

    fn expression(&mut self) -> Result<(), ParseError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn binary(&mut self, tok: &Token) -> Result<(), ParseError> {
        self.parse_precedence(Precedence::of(&tok.typ).next())?;
        match tok.typ {
            token::Type::BangEqual => {
                self.chunk.write_instruction(OpCode::Equal, tok.pos);
                self.chunk.write_instruction(OpCode::Not, tok.pos);
            }
            token::Type::EqualEqual => self.chunk.write_instruction(OpCode::Equal, tok.pos),
            token::Type::Greater => self.chunk.write_instruction(OpCode::Greater, tok.pos),
            token::Type::GreaterEqual => {
                self.chunk.write_instruction(OpCode::Less, tok.pos);
                self.chunk.write_instruction(OpCode::Not, tok.pos);
            }
            token::Type::Less => self.chunk.write_instruction(OpCode::Less, tok.pos),
            token::Type::LessEqual => {
                self.chunk.write_instruction(OpCode::Greater, tok.pos);
                self.chunk.write_instruction(OpCode::Not, tok.pos);
            }
            token::Type::Plus => self.chunk.write_instruction(OpCode::Add, tok.pos),
            token::Type::Minus => self.chunk.write_instruction(OpCode::Subtract, tok.pos),
            token::Type::Star => self.chunk.write_instruction(OpCode::Multiply, tok.pos),
            token::Type::Slash => self.chunk.write_instruction(OpCode::Divide, tok.pos),
            _ => unreachable!("Rule table is wrong."),
        }
        Ok(())
    }

    fn unary(&mut self, tok: &Token) -> Result<(), ParseError> {
        self.parse_precedence(Precedence::Unary)?;
        match tok.typ {
            token::Type::Bang => self.chunk.write_instruction(OpCode::Not, tok.pos),
            token::Type::Minus => self.chunk.write_instruction(OpCode::Negate, tok.pos),
            _ => unreachable!("Rule table is wrong."),
        }
        Ok(())
    }

    fn variable(&mut self, tok: &Token, can_assign: bool) -> Result<(), ParseError> {
        let ident_id = self
            .chunk
            .write_const(Value::String(self.strings.get_or_intern(tok.lexeme)));

        if can_assign && self.consume(token::Type::Equal, "").is_ok() {
            self.expression()?;
            self.chunk
                .write_instruction(OpCode::SetGlobal(ident_id), tok.pos);
        } else {
            self.chunk
                .write_instruction(OpCode::GetGlobal(ident_id), tok.pos);
        }
        Ok(())
    }

    fn string(&mut self, tok: &Token) {
        debug_assert_eq!(tok.typ, token::Type::String);
        let value = tok.lexeme[1..tok.lexeme.len() - 1].to_string();
        let constant = self
            .chunk
            .write_const(Value::String(self.strings.get_or_intern(value)));
        self.chunk
            .write_instruction(OpCode::Constant(constant), tok.pos);
    }

    fn number(&mut self, tok: &Token) {
        debug_assert_eq!(tok.typ, token::Type::Number);
        let value = tok
            .lexeme
            .parse()
            .expect("Scanner must ensure that the lexeme contains a valid f64 string.");
        let constant = self.chunk.write_const(Value::Number(value));
        self.chunk
            .write_instruction(OpCode::Constant(constant), tok.pos);
    }

    fn literal(&mut self, tok: &Token) {
        match tok.typ {
            token::Type::False => self.chunk.write_instruction(OpCode::False, tok.pos),
            token::Type::Nil => self.chunk.write_instruction(OpCode::Nil, tok.pos),
            token::Type::True => self.chunk.write_instruction(OpCode::True, tok.pos),
            _ => unreachable!("Rule table is wrong."),
        }
    }

    fn grouping(&mut self) -> Result<(), ParseError> {
        self.expression()?;
        self.consume(token::Type::RParen, "Expect ')' after expression")?;
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), ParseError> {
        let tok = self.advance()?;
        let can_assign = precedence <= Precedence::Assignment;
        self.prefix_rule(&tok, can_assign)?;

        loop {
            match self.peek() {
                None => break,
                Some(tok) if precedence > Precedence::of(&tok.typ) => break,
                _ => {}
            }
            let tok = self.advance()?;
            self.infix_rule(&tok)?;
        }

        if can_assign && self.consume(token::Type::Equal, "").is_ok() {
            return Err(ParseError::InvalidAssignTarget(
                self.prev_position,
                self.prev_lexeme.to_string(),
            ));
        }
        Ok(())
    }

    fn prefix_rule(&mut self, tok: &Token, can_assign: bool) -> Result<(), ParseError> {
        match tok.typ {
            token::Type::LParen => self.grouping()?,
            token::Type::Minus | token::Type::Bang => self.unary(tok)?,
            token::Type::Ident => self.variable(tok, can_assign)?,
            token::Type::String => self.string(tok),
            token::Type::Number => self.number(tok),
            token::Type::True | token::Type::False | token::Type::Nil => self.literal(tok),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    tok.pos,
                    tok.lexeme.to_string(),
                    "Expect expression".to_string(),
                ))
            }
        }
        Ok(())
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

    fn synchronize(&mut self) {
        while self.peek().is_some() {
            let tok = self.advance().expect("We have peeked.");
            if tok.typ == token::Type::Semicolon {
                return;
            }

            if let Some(tok) = self.peek() {
                match tok.typ {
                    token::Type::Class
                    | token::Type::Fun
                    | token::Type::Var
                    | token::Type::For
                    | token::Type::If
                    | token::Type::While
                    | token::Type::Print
                    | token::Type::Return => return,
                    _ => {}
                }
            }
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        while let Some(Err(err)) = self.tokens.peek() {
            eprintln!("{}", err);
            self.had_error = true;
            self.tokens.next();
        }
        self.tokens.peek().map(|peeked| match peeked {
            Err(_) => unreachable!("Errors should have been skipped."),
            Ok(tok) => tok,
        })
    }

    fn advance(&mut self) -> Result<Token<'a>, ParseError> {
        while let Some(Err(err)) = self.tokens.peek() {
            eprintln!("{}", err);
            self.had_error = true;
            self.tokens.next();
        }
        self.tokens
            .next()
            .map(|tok| {
                let tok = tok.expect("All errors have been skipped.");
                self.prev_position = tok.pos;
                self.prev_lexeme = tok.lexeme;
                tok
            })
            .ok_or(ParseError::UnexpectedEof)
    }

    fn consume(&mut self, typ: token::Type, msg: &str) -> Result<Token<'a>, ParseError> {
        match self.tokens.peek() {
            Some(Ok(tok)) => {
                if tok.typ == typ {
                    self.advance()
                } else {
                    Err(ParseError::UnexpectedToken(
                        tok.pos,
                        tok.lexeme.to_string(),
                        msg.to_string(),
                    ))
                }
            }
            None => Err(ParseError::UnexpectedToken(
                self.prev_position,
                self.prev_lexeme.to_string(),
                msg.to_string(),
            )),
            Some(Err(_)) => unreachable!("Invalid tokens should already be skipped."),
        }
    }
}

/// Chunk is a sequence of instructions and data that will be written to by the compiler
/// and later run by the virtual-machine.
///
/// # Examples
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
