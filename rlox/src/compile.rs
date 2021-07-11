use std::iter::Peekable;

use crate::{
    intern, scan, token, Chunk, OpCode, ParseError, Position, Scanner, StringId, Token, Value,
    MAX_STACK_SIZE,
};

/// Scan for tokens and emit corresponding bytecodes.
///
/// # The Lox Compiler
///
/// Lox uses lexical scoping so the compiler knows where it is within the stack while parsing the
/// source code. We are simulating the virtual machine's stack so at runtime we can pre-allocate
/// the needed space on to the stack, and access locals through array index for better preformance.
///
/// ## Locals Stack
///
/// ```
/// {
///     var a = 1;             // STACK: [ 1 ]
///     {
///         var b = 2;         // STACK: [ 1 ] [ 2 ]
///         {
///             var c = 3;     // STACK: [ 1 ] [ 2 ] [ 3 ]
///             {
///                 var d = 4; // STACK: [ 1 ] [ 2 ] [ 3 ] [ 4 ]
///             }              // STACK: [ 1 ] [ 2 ] [ 3 ] [ x ]
///
///             var e = 5;     // STACK: [ 1 ] [ 2 ] [ 3 ] [ 5 ]
///         }                  // STACK: [ 1 ] [ 2 ] [ x ] [ x ]
///     }                      // STACK: [ 1 ] [ x ]
///
///     var f = 6;             // STACK: [ 1 ] [ 6 ]
///     {
///         var g = 7;         // STACK: [ 1 ] [ 6 ] [ 7 ]
///     }                      // STACK: [ 1 ] [ 6 ] [ x ]
/// }                          // STACK: [ x ] [ x ]
/// ```
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
///
#[derive(Debug)]
pub struct Compiler<'a> {
    tokens: Peekable<scan::Iter<'a>>,
    prev_token: Token<'a>,
    had_error: bool,

    chunk: Chunk,
    locals: Vec<Local>,
    scope_depth: usize,
}

impl<'a> Compiler<'a> {
    /// Create a new parser
    pub fn new(src: &'a str) -> Self {
        Self {
            tokens: Scanner::new(src).into_iter().peekable(),
            prev_token: Token {
                typ: token::Type::Eof,
                lexeme: "",
                pos: Position::default(),
            },
            had_error: false,
            chunk: Chunk::default(),
            locals: Vec::with_capacity(MAX_STACK_SIZE),
            scope_depth: 0,
        }
    }

    /// Starts building the bytecode chunk
    pub fn compile(&mut self) {
        while let Some(tok) = self.peek() {
            if tok.typ == token::Type::Eof {
                return;
            }

            if let Err(err) = self.declaration() {
                eprintln!("{}", err);
                self.had_error = true;
                self.synchronize();
            }
        }
    }

    /// Return the compiled bytecode chunk if the process finishes without error
    pub fn finish(self) -> Option<Chunk> {
        if self.had_error {
            return None;
        }
        Some(self.chunk)
    }

    fn emit(&mut self, op: OpCode) {
        self.chunk.write_instruction(op, self.prev_token.pos);
    }

    fn declaration(&mut self) -> Result<(), ParseError> {
        if self.advance_if(token::Type::Var)? {
            return self.var_declaration();
        }
        self.statement()
    }

    fn declare_variable(&mut self) -> Result<(), ParseError> {
        if self.scope_depth == 0 {
            return Ok(());
        }
        if self.locals.len() == MAX_STACK_SIZE {
            return Err(ParseError::TooManyLocalVariables(
                self.prev_token.pos,
                self.prev_token.lexeme.to_string(),
            ));
        }

        let name = intern::id(self.prev_token.lexeme);
        for l in self.locals.iter() {
            if l.initialized && l.depth < self.scope_depth {
                break;
            }
            if l.name == name {
                return Err(ParseError::VariableRedeclaration(
                    self.prev_token.pos,
                    self.prev_token.lexeme.to_string(),
                ));
            }
        }

        self.locals
            .push((name, self.scope_depth).into());
        Ok(())
    }

    fn var_declaration(&mut self) -> Result<(), ParseError> {
        // variable name
        self.consume(token::Type::Ident, "Expect variable name")?;
        self.declare_variable()?;
        let ident_id = if self.scope_depth > 0 {
            0 // A dummy value used when we're not in the global scope
        } else {
            self.chunk
                .write_const(Value::String(intern::id(self.prev_token.lexeme)))
        };
        // initializer
        if self.advance_if(token::Type::Equal)? {
            self.expression()?;
        } else {
            self.emit(OpCode::Nil);
        }
        // ; terminated
        self.consume(
            token::Type::Semicolon,
            "Expect ';' after variable declaration",
        )?;

        // Local variables are not looked up by name. There's no need to stuff
        // the variable name into the constant table.
        if self.scope_depth > 0 {
            self.locals
                .last_mut()
                .expect("We just pushed a local.")
                .initialized = true;
            return Ok(());
        }
        self.emit(OpCode::DefineGlobal(ident_id));
        Ok(())
    }

    fn statement(&mut self) -> Result<(), ParseError> {
        if self.advance_if(token::Type::LBrace)? {
            self.begin_scope();
            self.block()?;
            self.end_scope();
            return Ok(());
        }
        if self.advance_if(token::Type::Print)? {
            return self.print_statement();
        }
        self.expression_statement()
    }

    fn block(&mut self) -> Result<(), ParseError> {
        while let Some(tok) = self.peek() {
            if tok.typ == token::Type::RBrace || tok.typ == token::Type::Eof {
                break;
            }
            self.declaration()?;
        }
        self.consume(token::Type::RBrace, "Expect '}' after blok.")
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;
        while let Some(l) = self.locals.last() {
            if l.depth <= self.scope_depth {
                break;
            }
            self.emit(OpCode::Pop);
            self.locals.pop();
        }
    }

    fn print_statement(&mut self) -> Result<(), ParseError> {
        self.expression()?;
        self.consume(token::Type::Semicolon, "Expect ';' after value")?;
        self.emit(OpCode::Print);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<(), ParseError> {
        self.expression()?;
        self.consume(token::Type::Semicolon, "Expect ';' after value")?;
        self.emit(OpCode::Pop);
        Ok(())
    }

    fn expression(&mut self) -> Result<(), ParseError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn binary(&mut self) -> Result<(), ParseError> {
        let token_type = self.prev_token.typ;
        self.parse_precedence(Precedence::of(token_type).next())?;
        match token_type {
            token::Type::BangEqual => {
                self.emit(OpCode::Equal);
                self.emit(OpCode::Not);
            }
            token::Type::EqualEqual => self.emit(OpCode::Equal),
            token::Type::Greater => self.emit(OpCode::Greater),
            token::Type::GreaterEqual => {
                self.emit(OpCode::Less);
                self.emit(OpCode::Not);
            }
            token::Type::Less => self.emit(OpCode::Less),
            token::Type::LessEqual => {
                self.emit(OpCode::Greater);
                self.emit(OpCode::Not);
            }
            token::Type::Plus => self.emit(OpCode::Add),
            token::Type::Minus => self.emit(OpCode::Subtract),
            token::Type::Star => self.emit(OpCode::Multiply),
            token::Type::Slash => self.emit(OpCode::Divide),
            _ => unreachable!("Rule table is wrong."),
        }
        Ok(())
    }

    fn unary(&mut self) -> Result<(), ParseError> {
        match self.prev_token.typ {
            token::Type::Bang => self.emit(OpCode::Not),
            token::Type::Minus => self.emit(OpCode::Negate),
            _ => unreachable!("Rule table is wrong."),
        }
        Ok(())
    }

    fn resolve_local(&self, name: &Token) -> Result<Option<u8>, ParseError> {
        self.locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, l)| l.name == intern::id(name.lexeme))
            .map(|(i, l)| {
                if l.initialized {
                    Ok(i as u8)
                } else {
                    Err(ParseError::SelfReferencingInitializer(
                        name.pos,
                        name.lexeme.to_string(),
                    ))
                }
            })
            .transpose()
    }

    fn variable(&mut self, can_assign: bool) -> Result<(), ParseError> {
        let (op_get, op_set) = if let Some(local) = self.resolve_local(&self.prev_token)? {
            (OpCode::GetLocal(local), OpCode::SetLocal(local))
        } else {
            let ident_id = self
                .chunk
                .write_const(Value::String(intern::id(self.prev_token.lexeme)));
            (OpCode::GetGlobal(ident_id), OpCode::SetGlobal(ident_id))
        };

        if can_assign && self.advance_if(token::Type::Equal)? {
            self.expression()?;
            self.emit(op_set);
        } else {
            self.emit(op_get);
        }
        Ok(())
    }

    fn string(&mut self) {
        let constant = self
            .chunk
            .write_const(Value::String(intern::id(self.prev_token.lexeme)));
        self.emit(OpCode::Constant(constant));
    }

    fn number(&mut self) {
        let value = intern::str(intern::id(self.prev_token.lexeme))
            .parse()
            .expect("Scanner must ensure that the lexeme contains a valid f64 string.");
        let constant = self.chunk.write_const(Value::Number(value));
        self.emit(OpCode::Constant(constant));
    }

    fn literal(&mut self) {
        match self.prev_token.typ {
            token::Type::False => self.emit(OpCode::False),
            token::Type::Nil => self.emit(OpCode::Nil),
            token::Type::True => self.emit(OpCode::True),
            _ => unreachable!("Rule table is wrong."),
        }
    }

    fn grouping(&mut self) -> Result<(), ParseError> {
        self.expression()?;
        self.consume(token::Type::RParen, "Expect ')' after expression")?;
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), ParseError> {
        self.advance()?;
        let can_assign = precedence <= Precedence::Assignment;
        self.prefix_rule(can_assign)?;

        loop {
            match self.peek() {
                None => break,
                Some(tok) if precedence > Precedence::of(tok.typ) => break,
                _ => {}
            }
            self.advance()?;
            self.infix_rule()?;
        }

        if can_assign && self.advance_if(token::Type::Equal)? {
            return Err(ParseError::InvalidAssignTarget(
                self.prev_token.pos,
                self.prev_token.lexeme.to_string(),
            ));
        }
        Ok(())
    }

    fn prefix_rule(&mut self, can_assign: bool) -> Result<(), ParseError> {
        match self.prev_token.typ {
            token::Type::LParen => self.grouping()?,
            token::Type::Minus | token::Type::Bang => self.unary()?,
            token::Type::Ident => self.variable(can_assign)?,
            token::Type::String => self.string(),
            token::Type::Number => self.number(),
            token::Type::True | token::Type::False | token::Type::Nil => self.literal(),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    self.prev_token.pos,
                    self.prev_token.lexeme.to_string(),
                    "Expect expression".to_string(),
                ))
            }
        }
        Ok(())
    }

    fn infix_rule(&mut self) -> Result<(), ParseError> {
        match self.prev_token.typ {
            token::Type::Minus
            | token::Type::Plus
            | token::Type::Slash
            | token::Type::Star
            | token::Type::BangEqual
            | token::Type::EqualEqual
            | token::Type::Greater
            | token::Type::GreaterEqual
            | token::Type::Less
            | token::Type::LessEqual => self.binary(),
            _ => Err(ParseError::UnexpectedToken(
                self.prev_token.pos,
                self.prev_token.lexeme.to_string(),
                "Expect expression".to_string(),
            )),
        }
    }

    fn synchronize(&mut self) {
        while self.peek().is_some() {
            self.advance().expect("We have peeked.");
            if self.prev_token.typ == token::Type::Eof
                || self.prev_token.typ == token::Type::Semicolon
            {
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

    fn advance(&mut self) -> Result<(), ParseError> {
        while let Some(Err(err)) = self.tokens.peek() {
            eprintln!("{}", err);
            self.had_error = true;
            self.tokens.next();
        }
        self.prev_token = self
            .tokens
            .next()
            .map(|tok| tok.expect("All errors have been skipped."))
            .ok_or(ParseError::UnexpectedEof)?;
        Ok(())
    }

    fn advance_if(&mut self, typ: token::Type) -> Result<bool, ParseError> {
        if let Some(Ok(tok)) = self.tokens.peek() {
            if tok.typ == typ {
                self.advance()?;
                return Ok(true);
            }
        }
        Ok(false)
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
                        self.prev_token.lexeme.to_string(),
                        msg.to_string(),
                    ))
                }
            }
            None => Err(ParseError::UnexpectedToken(
                self.prev_token.pos,
                self.prev_token.lexeme.to_string(),
                msg.to_string(),
            )),
            Some(Err(_)) => unreachable!("Invalid tokens should already be skipped."),
        }
    }
}

/// Store name and depth of the resolved identifer.
#[derive(Debug)]
struct Local {
    name: StringId,
    depth: usize,
    initialized: bool,
}

impl From<(StringId, usize)> for Local {
    fn from((name, depth): (StringId, usize)) -> Self {
        Self {
            name,
            depth,
            initialized: false,
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

    fn of(typ: token::Type) -> Self {
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
