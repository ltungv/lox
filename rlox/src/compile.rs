use crate::{
    disassemble_chunk, intern, token, Function, OpCode, ParseError, Position, Scanner, StringId,
    Token, Value, MAX_STACK,
};

/// Function object's type.
///
/// This is used to so that the compiler knows what kind of chunk it's current compilling.
/// We are treating the entire script as a implicit function.
#[derive(Debug)]
pub enum FunctionType {
    /// The compiled chunk is of a function
    Function,
    /// The compiled chunk is of the input script
    Script,
}

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
    scanner: Scanner<'a>,
    current_token: Token<'a>,
    previous_token: Token<'a>,
    had_error: bool,
    // Avoid having a linked list of compiler, solution found from
    // https://github.com/tdp2110/crafting-interpreters-rs/blob/trunk/src/compiler.rs
    nestings: Vec<Nesting>,
}

impl<'a> Compiler<'a> {
    /// Create a new parser
    pub fn new(src: &'a str) -> Self {
        Self {
            scanner: Scanner::new(src),
            current_token: Token {
                typ: token::Type::Eof,
                lexeme: "",
                pos: Position::default(),
            },
            previous_token: Token {
                typ: token::Type::Eof,
                lexeme: "",
                pos: Position::default(),
            },
            had_error: false,
            nestings: vec![Nesting::new(Function::default(), FunctionType::Script)],
        }
    }

    /// Starts building the bytecode chunk
    pub fn compile(&mut self) {
        self.advance();
        while self.current_token.typ != token::Type::Eof {
            if let Err(err) = self.declaration() {
                eprintln!("{}", err);
                self.had_error = true;
                self.synchronize();
            }
        }
    }

    /// Return the compiled bytecode chunk if the process finishes without error
    pub fn finish(&mut self) -> Option<Function> {
        if self.had_error {
            None
        } else {
            self.emit(OpCode::Return);
            #[cfg(debug_assertions)]
            disassemble_chunk(
                &self.function().chunk,
                format!("{}", self.function()).as_str(),
            );
            self.nestings.pop().map(|n| n.function)
        }
    }

    fn function(&self) -> &Function {
        &self.nesting().function
    }

    fn function_mut(&mut self) -> &mut Function {
        &mut self.nesting_mut().function
    }

    fn nesting(&self) -> &Nesting {
        self.nestings
            .last()
            .expect("There's always a nesting item.")
    }

    fn nesting_mut(&mut self) -> &mut Nesting {
        self.nestings
            .last_mut()
            .expect("There's always a nesting item.")
    }

    fn emit(&mut self, op: OpCode) {
        let pos = self.previous_token.pos;
        self.function_mut().chunk.write_instruction(op, pos);
    }

    fn emit_jump<O: Fn(u16) -> OpCode>(&mut self, op: O) -> usize {
        self.emit(op(0xFFFF));
        self.function_mut().chunk.instructions_count()
    }

    fn patch_jump(&mut self, jump: usize) -> Result<(), ParseError> {
        let offset = self.function_mut().chunk.instructions_count() - jump;
        if offset > u16::MAX as usize {
            return Err(ParseError::JumpTooLarge(
                self.current_token.pos,
                self.current_token.lexeme.to_string(),
                "Too much code to jump over",
            ));
        }
        self.function_mut()
            .chunk
            .patch_jump_instruction(jump - 1, offset as u16);
        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize) -> Result<(), ParseError> {
        // +1 because the offset also takes into account the newly emitted loop opcode
        let offset = self.function_mut().chunk.instructions_count() - loop_start + 1;
        if offset > u16::MAX as usize {
            return Err(ParseError::JumpTooLarge(
                self.previous_token.pos,
                self.previous_token.lexeme.to_string(),
                "Loop body too large",
            ));
        }
        self.emit(OpCode::Loop(offset as u16));
        Ok(())
    }

    fn declaration(&mut self) -> Result<(), ParseError> {
        if self.advance_if(token::Type::Var) {
            return self.var_declaration();
        }
        self.statement()
    }

    fn var_declaration(&mut self) -> Result<(), ParseError> {
        let ident_id = self.parse_variable()?;
        // initializer
        if self.advance_if(token::Type::Equal) {
            self.expression()?;
        } else {
            self.emit(OpCode::Nil);
        }
        // ; terminated
        self.consume(
            token::Type::Semicolon,
            "Expect ';' after variable declaration",
        )?;
        self.define_variable(ident_id)
    }

    fn parse_variable(&mut self) -> Result<u8, ParseError> {
        self.consume(token::Type::Ident, "Expect variable name")?;
        self.declare_variable()?;
        Ok(self.identifier_constant())
    }

    fn define_variable(&mut self, ident_id: u8) -> Result<(), ParseError> {
        // Local variables are not looked up by name. There's no need to stuff
        // the variable name into the constant table.
        if self.nesting().scope_depth > 0 {
            self.nesting_mut()
                .locals
                .last_mut()
                .expect("We just pushed a local.")
                .initialized = true;
            return Ok(());
        }
        self.emit(OpCode::DefineGlobal(ident_id));
        Ok(())
    }

    fn identifier_constant(&mut self) -> u8 {
        if self.nesting().scope_depth > 0 {
            0 // A dummy value used when we're not in the global scope
        } else {
            let name = intern::id(self.previous_token.lexeme);
            self.function_mut().chunk.write_const(Value::String(name))
        }
    }

    fn declare_variable(&mut self) -> Result<(), ParseError> {
        if self.nesting().scope_depth == 0 {
            return Ok(());
        }
        if self.nesting().locals.len() == MAX_STACK {
            return Err(ParseError::InvalidDeclaration(
                self.previous_token.pos,
                self.previous_token.lexeme.to_string(),
                "Too many local variables in function",
            ));
        }

        let name = intern::id(self.previous_token.lexeme);
        for l in self.nesting().locals.iter() {
            if l.initialized && l.depth < self.nesting().scope_depth {
                break;
            }
            if l.name == name {
                return Err(ParseError::InvalidDeclaration(
                    self.previous_token.pos,
                    self.previous_token.lexeme.to_string(),
                    "Already a variable with this name in this scope",
                ));
            }
        }
        let scope_depth = self.nesting().scope_depth;
        self.nesting_mut().locals.push((name, scope_depth).into());
        Ok(())
    }

    fn statement(&mut self) -> Result<(), ParseError> {
        if self.advance_if(token::Type::Print) {
            self.print_statement()
        } else if self.advance_if(token::Type::For) {
            self.for_statement()
        } else if self.advance_if(token::Type::If) {
            self.if_statement()
        } else if self.advance_if(token::Type::While) {
            self.while_statement()
        } else if self.advance_if(token::Type::LBrace) {
            self.begin_scope();
            self.block()?;
            self.end_scope();
            Ok(())
        } else {
            self.expression_statement()
        }
    }

    fn block(&mut self) -> Result<(), ParseError> {
        while self.current_token.typ != token::Type::RBrace
            && self.current_token.typ != token::Type::Eof
        {
            self.declaration()?;
        }
        self.consume(token::Type::RBrace, "Expect '}' after blok.")
    }

    fn begin_scope(&mut self) {
        self.nesting_mut().scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.nesting_mut().scope_depth -= 1;
        while let Some(l) = self.nesting().locals.last() {
            if l.depth <= self.nesting().scope_depth {
                break;
            }
            self.emit(OpCode::Pop);
            self.nesting_mut().locals.pop();
        }
    }

    fn if_statement(&mut self) -> Result<(), ParseError> {
        self.consume(token::Type::LParen, "Expect '(' after 'if'")?;
        self.expression()?;
        self.consume(token::Type::RParen, "Expect ')' after condition")?;

        // This jumps to the else clause
        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        // Jump does not pop the conditional out of the stack, so we do it manually.
        // Here we pop the true value.
        self.emit(OpCode::Pop);
        self.statement()?;

        // This jumps through the else clause
        let else_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(then_jump)?;
        // Here we pop the false value.
        self.emit(OpCode::Pop);

        if self.advance_if(token::Type::Else) {
            self.statement()?;
        }
        self.patch_jump(else_jump)
    }

    fn while_statement(&mut self) -> Result<(), ParseError> {
        let loop_start = self.function_mut().chunk.instructions_count();
        self.consume(token::Type::LParen, "Expect '(' after 'while'")?;
        self.expression()?;
        self.consume(token::Type::RParen, "Expect ')' after condition")?;

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit(OpCode::Pop);

        self.statement()?;
        self.emit_loop(loop_start)?;

        self.patch_jump(exit_jump)?;
        self.emit(OpCode::Pop);
        Ok(())
    }

    fn for_statement(&mut self) -> Result<(), ParseError> {
        self.begin_scope();
        self.consume(token::Type::LParen, "Expect '(' after 'for'")?;
        // initializer clause
        if self.advance_if(token::Type::Semicolon) {
            // no initializer
        } else if self.advance_if(token::Type::Var) {
            self.var_declaration()?;
        } else {
            self.expression_statement()?;
        }

        let mut loop_start = self.function_mut().chunk.instructions_count();

        // conditional clause
        let exit_jump = if !self.advance_if(token::Type::Semicolon) {
            // conditional expression
            self.expression()?;
            self.consume(token::Type::Semicolon, "Expect ';' after loop condition.")?;
            // exit if consitional expression is falsey
            let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
            // pop true when not jump
            self.emit(OpCode::Pop);
            Some(exit_jump)
        } else {
            None
        };

        // increment clause
        if !self.advance_if(token::Type::RParen) {
            // immediately jump to the loop's body, skipping the increment expression
            let body_jump = self.emit_jump(OpCode::Jump);
            let increment_start = self.function_mut().chunk.instructions_count();
            // increment expression
            self.expression()?;
            // pop expression result
            self.emit(OpCode::Pop);
            self.consume(token::Type::RParen, "Expect ')' after for clauses")?;

            // this will loop back to the conditional after the increment expression is run
            self.emit_loop(loop_start)?;
            // the loop start to point to the increment expression
            loop_start = increment_start;
            self.patch_jump(body_jump)?;
        }

        self.statement()?;
        // this will loop back to the increment expression if there is one, otherwise it loops back
        // to the conditional expression
        self.emit_loop(loop_start)?;

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump)?;
            // pop false when get jumped into
            self.emit(OpCode::Pop);
        }

        self.end_scope();
        Ok(())
    }

    fn print_statement(&mut self) -> Result<(), ParseError> {
        self.expression()?;
        self.consume(token::Type::Semicolon, "Expect ';' after for clauses value")?;
        self.emit(OpCode::Print);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<(), ParseError> {
        self.expression()?;
        self.consume(token::Type::Semicolon, "Expect ';' after expression")?;
        self.emit(OpCode::Pop);
        Ok(())
    }

    fn expression(&mut self) -> Result<(), ParseError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn or(&mut self) -> Result<(), ParseError> {
        // Short-circuit jump.
        // If the value on top of the stack is falsey, we make a small jump skipping passs the jump
        // right beneath. Otherwise we go to the jump right beneath us to jump pass the rest of the
        // operands. This simulates JumpIfTrue without making a new opcode.
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(else_jump)?;
        // Pop false value if not short-circuited
        self.emit(OpCode::Pop);

        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(end_jump)
    }

    fn and(&mut self) -> Result<(), ParseError> {
        // Short-circuit jump.
        // If the value on top of the stack is falsey, jumps pass the rest of the
        // operands.
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);
        // Pop true value if not short-circuited
        self.emit(OpCode::Pop);

        self.parse_precedence(Precedence::And)?;
        self.patch_jump(end_jump)
    }

    fn binary(&mut self) -> Result<(), ParseError> {
        let token_type = self.previous_token.typ;
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
        let token_type = self.previous_token.typ;
        self.parse_precedence(Precedence::Unary)?;
        match token_type {
            token::Type::Bang => self.emit(OpCode::Not),
            token::Type::Minus => self.emit(OpCode::Negate),
            _ => unreachable!("Rule table is wrong."),
        }
        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> Result<(), ParseError> {
        let (op_get, op_set) = if let Some(local) = self.resolve_local(&self.previous_token)? {
            (OpCode::GetLocal(local), OpCode::SetLocal(local))
        } else {
            let name = intern::id(self.previous_token.lexeme);
            let ident_id = self.function_mut().chunk.write_const(Value::String(name));
            (OpCode::GetGlobal(ident_id), OpCode::SetGlobal(ident_id))
        };

        if can_assign && self.advance_if(token::Type::Equal) {
            self.expression()?;
            self.emit(op_set);
        } else {
            self.emit(op_get);
        }
        Ok(())
    }

    fn resolve_local(&self, name: &Token) -> Result<Option<u8>, ParseError> {
        self.nesting()
            .locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, l)| l.name == intern::id(name.lexeme))
            .map(|(i, l)| {
                if l.initialized {
                    Ok(i as u8)
                } else {
                    Err(ParseError::InvalidDeclaration(
                        name.pos,
                        name.lexeme.to_string(),
                        "Can't read local variable in its own initializer",
                    ))
                }
            })
            .transpose()
    }

    fn string(&mut self) {
        let value =
            intern::id(&self.previous_token.lexeme[1..self.previous_token.lexeme.len() - 1]);
        let constant = self.function_mut().chunk.write_const(Value::String(value));
        self.emit(OpCode::Constant(constant));
    }

    fn number(&mut self) {
        let value = intern::str(intern::id(self.previous_token.lexeme))
            .parse()
            .expect("Scanner must ensure that the lexeme contains a valid f64 string.");
        let constant = self.function_mut().chunk.write_const(Value::Number(value));
        self.emit(OpCode::Constant(constant));
    }

    fn literal(&mut self) {
        match self.previous_token.typ {
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
        self.advance();
        let can_assign = precedence <= Precedence::Assignment;
        self.prefix_rule(can_assign)?;

        while precedence <= Precedence::of(self.current_token.typ) {
            self.advance();
            self.infix_rule()?;
        }

        if can_assign && self.advance_if(token::Type::Equal) {
            return Err(ParseError::UnexpectedToken(
                self.previous_token.pos,
                self.previous_token.lexeme.to_string(),
                "Invalid assignment target",
            ));
        }
        Ok(())
    }

    fn prefix_rule(&mut self, can_assign: bool) -> Result<(), ParseError> {
        match self.previous_token.typ {
            token::Type::LParen => self.grouping()?,
            token::Type::Minus | token::Type::Bang => self.unary()?,
            token::Type::Ident => self.variable(can_assign)?,
            token::Type::String => self.string(),
            token::Type::Number => self.number(),
            token::Type::True | token::Type::False | token::Type::Nil => self.literal(),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    self.previous_token.pos,
                    self.previous_token.lexeme.to_string(),
                    "Expect expression",
                ))
            }
        }
        Ok(())
    }

    fn infix_rule(&mut self) -> Result<(), ParseError> {
        match self.previous_token.typ {
            token::Type::Or => self.or(),
            token::Type::And => self.and(),
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
                self.previous_token.pos,
                self.previous_token.lexeme.to_string(),
                "Expect expression",
            )),
        }
    }

    fn synchronize(&mut self) {
        while self.current_token.typ != token::Type::Eof {
            if self.previous_token.typ == token::Type::Semicolon {
                match self.current_token.typ {
                    token::Type::RParen | token::Type::RBrace => {}
                    _ => return,
                }
            }
            match self.current_token.typ {
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
            self.advance();
        }
    }

    fn advance(&mut self) {
        loop {
            match self.scanner.scan() {
                Err(err) => {
                    eprintln!("{}", err);
                    self.had_error = true;
                }
                Ok(tok) => {
                    self.previous_token = std::mem::replace(&mut self.current_token, tok);
                    break;
                }
            }
        }
    }

    fn advance_if(&mut self, typ: token::Type) -> bool {
        if self.current_token.typ != typ {
            return false;
        }
        self.advance();
        true
    }

    fn consume(&mut self, typ: token::Type, msg: &'static str) -> Result<(), ParseError> {
        if self.current_token.typ != typ {
            return Err(ParseError::UnexpectedToken(
                self.current_token.pos,
                self.current_token.lexeme.to_string(),
                msg,
            ));
        }
        self.advance();
        Ok(())
    }
}

#[derive(Debug)]
struct Nesting {
    function: Function,
    function_type: FunctionType,
    locals: Vec<Local>,
    scope_depth: usize,
}

impl Nesting {
    fn new(function: Function, function_type: FunctionType) -> Self {
        // The first slot on the stack is reserved for the callframe
        let mut locals = Vec::with_capacity(MAX_STACK);
        locals.push(Local {
            name: intern::id(""),
            depth: 0,
            initialized: false,
        });
        Self {
            function,
            function_type,
            locals,
            scope_depth: 0,
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
            token::Type::Or => Precedence::Or,
            token::Type::And => Precedence::And,
            token::Type::BangEqual | token::Type::EqualEqual => Precedence::Equality,
            token::Type::Greater
            | token::Type::GreaterEqual
            | token::Type::Less
            | token::Type::LessEqual => Precedence::Comparison,
            token::Type::Minus | token::Type::Plus => Precedence::Term,
            token::Type::Slash | token::Type::Star => Precedence::Factor,
            _ => Self::None,
        }
    }
}
