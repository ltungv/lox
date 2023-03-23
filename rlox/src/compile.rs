use crate::{
    gc::Gc, intern, token, Chunk, ObjFun, Object, OpCode, Position, Scanner, StrId, Token, Value,
    MAX_CHUNK_CONSTANTS, MAX_LOCAL_VARIABLES, MAX_PARAMS, MAX_UPVALUES,
};

#[cfg(debug_assertions)]
use crate::disassemble_chunk;

/// Function object's type.
///
/// This is used to so that the compiler knows what kind of chunk it's current compilling.
/// We are treating the entire script as a implicit function.
#[derive(Debug, PartialEq, Eq)]
pub enum FunType {
    /// The compiled chunk is of a class initializer
    Initializer,
    /// The compiled chunk is of a class method
    Method,
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
    panic: bool,
    // Avoid having a linked list of compiler, solution found from
    // https://github.com/tdp2110/crafting-interpreters-rs/blob/trunk/src/compiler.rs
    closure_levels: Vec<ClosureLevel>,
    class_levels: Vec<ClassLevel>,
}

impl<'a> Compiler<'a> {
    /// Create a new parser
    pub fn new(src: &'a str) -> Self {
        Self {
            scanner: Scanner::new(src),
            current_token: Token::placeholder(),
            previous_token: Token::placeholder(),
            had_error: false,
            panic: false,
            closure_levels: vec![ClosureLevel::new(
                ObjFun::new(intern::id("")),
                FunType::Script,
            )],
            class_levels: Vec::new(),
        }
    }

    /// Starts building the bytecode chunk
    pub fn compile(&mut self) {
        self.advance();
        while !self.check(token::Type::Eof) {
            self.declaration();
        }
    }

    /// Return the compiled bytecode chunk if the process finishes without error
    pub fn finish(&mut self) -> Option<ObjFun> {
        if self.had_error {
            return None;
        }
        self.emit_return();

        let fun = self.closure_level_pop().fun;

        #[cfg(debug_assertions)]
        disassemble_chunk(&fun.chunk, format!("{fun}").as_str());

        Some(fun)
    }

    fn declaration(&mut self) {
        if self.match_type(token::Type::Class) {
            self.class_declaration()
        } else if self.match_type(token::Type::Fun) {
            self.fun_declaration()
        } else if self.match_type(token::Type::Var) {
            self.var_declaration()
        } else {
            self.statement()
        }

        if self.panic {
            self.synchronize();
        }
    }

    fn fun_declaration(&mut self) {
        let ident_id = self.parse_variable();
        self.mark_initialized();
        self.function(FunType::Function);
        self.define_variable(ident_id);
    }

    fn function(&mut self, fun_t: FunType) {
        let name = intern::id(self.previous_token.lexeme);
        self.closure_levels
            .push(ClosureLevel::new(ObjFun::new(name), fun_t));
        self.begin_scope();

        self.consume(token::Type::LParen, "Expect '(' after function name");
        if !self.check(token::Type::RParen) {
            loop {
                if self.closure_level_mut(0).fun.arity as usize == MAX_PARAMS {
                    self.error_current("Can't have more than 255 parameters");
                }

                self.closure_level_mut(0).fun.arity += 1;
                let ident_id = self.parse_variable();
                self.define_variable(ident_id);

                if !self.match_type(token::Type::Comma) {
                    break;
                }
            }
        }
        self.consume(token::Type::RParen, "Expect ')' after parameters");
        self.consume(token::Type::LBrace, "Expect '{' before function body");
        self.block();

        self.emit_return();
        let level = self.closure_level_pop();
        let fun = level.fun;
        let upvalues = level.upvalues;

        #[cfg(debug_assertions)]
        disassemble_chunk(&fun.chunk, format!("{fun}").as_str());

        let fun = Gc::new(fun);
        let const_id = self.make_const(Value::Object(Object::Fun(fun)));
        self.emit(OpCode::Closure(const_id, upvalues));
    }

    fn class_declaration(&mut self) {
        // class name's
        self.consume(token::Type::Ident, "Expect class name");
        let class_name = intern::id(self.previous_token.lexeme);

        let name_constant = self.identifier_constant();
        self.declare_variable();

        self.emit(OpCode::Class(name_constant));
        self.define_variable(name_constant);
        self.class_levels.push(ClassLevel {
            has_superclass: false,
        });

        // inheritance
        if self.match_type(token::Type::Less) {
            self.consume(token::Type::Ident, "Expect superclass name");
            self.variable(false);
            if class_name == intern::id(self.previous_token.lexeme) {
                self.error("A class can't inherit from itself");
            }
            // We create a local variable that holds the referenced to the superclass.
            // A new scope is created so that namespace of classes defined in the same
            // scope don't collide.
            self.begin_scope();
            self.add_local(intern::id("super"));
            self.define_variable(0);
            // put the variable on stack so the runtime can access it
            self.named_variable(class_name, false);
            self.emit(OpCode::Inherit);
            self.class_level_mut(0).has_superclass = true;
        }

        // methods list
        self.named_variable(class_name, false);
        self.consume(token::Type::LBrace, "Expect '{' before class body");
        while !self.check(token::Type::RBrace) && !self.check(token::Type::Eof) {
            self.method();
        }
        self.consume(token::Type::RBrace, "Expect '}' after class body");
        self.emit(OpCode::Pop);

        if self.class_level(0).has_superclass {
            self.end_scope();
        }
        self.class_level_pop();
    }

    fn method(&mut self) {
        self.consume(token::Type::Ident, "Expect method name");
        let const_id = self.identifier_constant();

        // The method that's named "init" is the class initializer and it always
        // return a class instance when finishes.

        self.function(if self.previous_token.lexeme == "init" {
            FunType::Initializer
        } else {
            FunType::Method
        });
        self.emit(OpCode::Method(const_id));
    }

    fn var_declaration(&mut self) {
        let ident_id = self.parse_variable();
        // initializer
        if self.match_type(token::Type::Equal) {
            self.expression();
        } else {
            self.emit(OpCode::Nil);
        }
        // ; terminated
        self.consume(
            token::Type::Semicolon,
            "Expect ';' after variable declaration",
        );
        self.define_variable(ident_id);
    }

    fn parse_variable(&mut self) -> u8 {
        self.consume(token::Type::Ident, "Expect variable name");
        self.declare_variable();
        if self.closure_level(0).scope_depth > 0 {
            0 // A dummy value used when we're not in the global scope
        } else {
            self.identifier_constant()
        }
    }

    fn identifier_constant(&mut self) -> u8 {
        let name = intern::id(self.previous_token.lexeme);
        self.make_const(Value::Str(name))
    }

    fn add_local(&mut self, name: StrId) {
        if self.closure_level(0).locals.len() == MAX_LOCAL_VARIABLES {
            self.error("Too many local variables in function");
        }

        let scope_depth = self.closure_level(0).scope_depth;
        self.closure_level_mut(0)
            .locals
            .push((name, scope_depth).into());
    }

    fn declare_variable(&mut self) {
        if self.closure_level(0).scope_depth == 0 {
            return;
        }

        let name = intern::id(self.previous_token.lexeme);
        let mut name_duplicated = false;
        for l in self.closure_level(0).locals.iter().rev() {
            if l.initialized && l.depth < self.closure_level(0).scope_depth {
                break;
            }
            if l.name == name {
                name_duplicated = true;
                break;
            }
        }

        if name_duplicated {
            self.error("Already a variable with this name in this scope");
        }
        self.add_local(name)
    }

    fn define_variable(&mut self, ident_id: u8) {
        // Local variables are not looked up by name. There's no need to stuff
        // the variable name into the constant table.
        if self.closure_level(0).scope_depth > 0 {
            self.mark_initialized();
        } else {
            self.emit(OpCode::DefineGlobal(ident_id));
        }
    }

    fn mark_initialized(&mut self) {
        if self.closure_level(0).scope_depth == 0 {
            return;
        }
        self.closure_level_mut(0)
            .locals
            .last_mut()
            .expect("Wrong compiler state")
            .initialized = true;
    }

    fn statement(&mut self) {
        if self.match_type(token::Type::Print) {
            self.print_statement();
        } else if self.match_type(token::Type::For) {
            self.for_statement();
        } else if self.match_type(token::Type::If) {
            self.if_statement();
        } else if self.match_type(token::Type::Return) {
            self.return_statement();
        } else if self.match_type(token::Type::While) {
            self.while_statement();
        } else if self.match_type(token::Type::LBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn block(&mut self) {
        while !self.check(token::Type::RBrace) && !self.check(token::Type::Eof) {
            self.declaration();
        }
        self.consume(token::Type::RBrace, "Expect '}' after block");
    }

    fn begin_scope(&mut self) {
        self.closure_level_mut(0).scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.closure_level_mut(0).scope_depth -= 1;
        while let Some(l) = self.closure_level(0).locals.last() {
            if l.depth <= self.closure_level(0).scope_depth {
                break;
            }
            if l.captured {
                self.emit(OpCode::CloseUpvalue);
            } else {
                self.emit(OpCode::Pop);
            }
            self.closure_level_mut(0).locals.pop();
        }
    }

    fn return_statement(&mut self) {
        if self.closure_level(0).fun_t == FunType::Script {
            self.error("Can't return from top-level code")
        }

        if self.match_type(token::Type::Semicolon) {
            self.emit_return();
        } else {
            if self.closure_level(0).fun_t == FunType::Initializer {
                self.error("Can't return a value from an initializer");
            }
            self.expression();
            self.consume(token::Type::Semicolon, "Expect ';' after return value");
            self.emit(OpCode::Return);
        }
    }

    fn if_statement(&mut self) {
        self.consume(token::Type::LParen, "Expect '(' after 'if'");
        self.expression();
        self.consume(token::Type::RParen, "Expect ')' after condition");

        // This jumps to the else clause
        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        // Jump does not pop the conditional out of the stack, so we do it manually.
        // Here we pop the true value.
        self.emit(OpCode::Pop);
        self.statement();

        // This jumps through the else clause
        let else_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(then_jump);
        // Here we pop the false value.
        self.emit(OpCode::Pop);

        if self.match_type(token::Type::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn while_statement(&mut self) {
        let loop_start = self.chunk().instructions_count();
        self.consume(token::Type::LParen, "Expect '(' after 'while'");
        self.expression();
        self.consume(token::Type::RParen, "Expect ')' after condition");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit(OpCode::Pop);

        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit(OpCode::Pop);
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(token::Type::LParen, "Expect '(' after 'for'");
        // initializer clause
        if self.match_type(token::Type::Semicolon) {
            // no initializer
        } else if self.match_type(token::Type::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.chunk().instructions_count();

        // conditional clause
        let exit_jump = if !self.match_type(token::Type::Semicolon) {
            // conditional expression
            self.expression();
            self.consume(token::Type::Semicolon, "Expect ';' after loop condition");
            // exit if consitional expression is falsey
            let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
            // pop true when not jump
            self.emit(OpCode::Pop);
            Some(exit_jump)
        } else {
            None
        };

        // increment clause
        if !self.match_type(token::Type::RParen) {
            // immediately jump to the loop's body, skipping the increment expression
            let body_jump = self.emit_jump(OpCode::Jump);
            let increment_start = self.chunk().instructions_count();
            // increment expression
            self.expression();
            // pop expression result
            self.emit(OpCode::Pop);
            self.consume(token::Type::RParen, "Expect ')' after for clauses");

            // this will loop back to the conditional after the increment expression is run
            self.emit_loop(loop_start);
            // the loop start to point to the increment expression
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        // this will loop back to the increment expression if there is one, otherwise it loops back
        // to the conditional expression
        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            // pop false when get jumped into
            self.emit(OpCode::Pop);
        }
        self.end_scope();
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(token::Type::Semicolon, "Expect ';' after value");
        self.emit(OpCode::Print);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(token::Type::Semicolon, "Expect ';' after expression");
        self.emit(OpCode::Pop);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment)
    }

    fn or(&mut self) {
        // Short-circuit jump.
        // If the value on top of the stack is falsey, we make a small jump skipping passs the jump
        // right beneath. Otherwise we go to the jump right beneath us to jump pass the rest of the
        // operands. This simulates JumpIfTrue without making a new opcode.
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(else_jump);
        // Pop false value if not short-circuited
        self.emit(OpCode::Pop);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn and(&mut self) {
        // Short-circuit jump.
        // If the value on top of the stack is falsey, jumps pass the rest of the
        // operands.
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);
        // Pop true value if not short-circuited
        self.emit(OpCode::Pop);

        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn binary(&mut self) {
        let token_type = self.previous_token.typ;
        self.parse_precedence(Precedence::of(token_type).next());
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
            _ => unreachable!(),
        }
    }

    fn unary(&mut self) {
        let token_type = self.previous_token.typ;
        self.parse_precedence(Precedence::Unary);
        match token_type {
            token::Type::Bang => self.emit(OpCode::Not),
            token::Type::Minus => self.emit(OpCode::Negate),
            _ => unreachable!(),
        }
    }

    fn call(&mut self) {
        let arg_count = self.argument_list();
        self.emit(OpCode::Call(arg_count));
    }

    fn argument_list(&mut self) -> u8 {
        let mut arg_count = 0;
        if !self.check(token::Type::RParen) {
            loop {
                self.expression();
                if arg_count == MAX_PARAMS {
                    self.error("Can't have more than 255 arguments");
                    break;
                }
                arg_count += 1;
                if !self.match_type(token::Type::Comma) {
                    break;
                }
            }
        }
        self.consume(token::Type::RParen, "Expect ')' after arguments");
        arg_count as u8
    }

    fn dot(&mut self, can_assign: bool) {
        self.consume(token::Type::Ident, "Expect property name after '.'");
        let name = self.identifier_constant();

        if can_assign && self.match_type(token::Type::Equal) {
            self.expression();
            self.emit(OpCode::SetProperty(name))
        } else if self.match_type(token::Type::LParen) {
            // directly invoke a method
            let argc = self.argument_list();
            self.emit(OpCode::Invoke(name, argc))
        } else {
            self.emit(OpCode::GetProperty(name))
        }
    }

    fn super_(&mut self) {
        if self.class_levels.is_empty() {
            self.error("Can't use 'super' outside of a class");
        } else if !self.class_level_mut(0).has_superclass {
            self.error("Can't use 'super' in a class with no superclass");
        }
        // super are always expected to followed by a property
        self.consume(token::Type::Dot, "Expect '.' after 'super'");
        self.consume(token::Type::Ident, "Expect superclass method name");
        let name = self.identifier_constant();

        self.named_variable(intern::id("this"), false);
        if self.match_type(token::Type::LParen) {
            let argc = self.argument_list();
            self.named_variable(intern::id("super"), false);
            self.emit(OpCode::SuperInvoke(name, argc));
        } else {
            self.named_variable(intern::id("super"), false);
            self.emit(OpCode::GetSuper(name));
        }
    }

    fn this(&mut self) {
        if self.class_levels.is_empty() {
            self.error("Can't use 'this' outside of a class");
            return;
        }
        // This is bound to the lexical scope as a normal local variable
        self.variable(false);
    }

    fn variable(&mut self, can_assign: bool) {
        let var_name = intern::id(self.previous_token.lexeme);
        self.named_variable(var_name, can_assign)
    }

    fn named_variable(&mut self, var_name: StrId, can_assign: bool) {
        let (op_get, op_set) = if let Some(local) = self.resolve_local(0, var_name) {
            (OpCode::GetLocal(local), OpCode::SetLocal(local))
        } else if let Some(upval) = self.resolve_upvalue(0, var_name) {
            (OpCode::GetUpvalue(upval), OpCode::SetUpvalue(upval))
        } else {
            let ident_id = self.make_const(Value::Str(var_name));
            (OpCode::GetGlobal(ident_id), OpCode::SetGlobal(ident_id))
        };

        if can_assign && self.match_type(token::Type::Equal) {
            self.expression();
            self.emit(op_set);
        } else {
            self.emit(op_get);
        }
    }

    fn resolve_local(&mut self, level: usize, name: StrId) -> Option<u8> {
        self.closure_level(level)
            .locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| local.name == name)
            .map(|(const_id, local)| (const_id as u8, local.initialized))
            .map(|(const_id, is_init)| {
                if !is_init {
                    self.error("Can't read local variable in its own initializer");
                }
                const_id
            })
    }

    fn add_upvalue(&mut self, level: usize, index: u8, is_local: bool) -> usize {
        let level = self.closure_level_mut(level);
        let upvalue_count = level.upvalues.len();
        // reuse upvalue if a variable is referenced multiple times
        for (i, upvalue) in level.upvalues.iter().enumerate() {
            if upvalue.index == index && upvalue.is_local == is_local {
                return i;
            }
        }
        if upvalue_count == MAX_UPVALUES {
            self.error("Too many closure variables in function");
            return 0;
        }
        level.upvalues.push(Upvalue {
            index, // closed-over local variable's slot index
            is_local,
        });
        upvalue_count
    }

    fn resolve_upvalue(&mut self, level: usize, name: StrId) -> Option<u8> {
        if self.closure_levels.len() - 1 == level {
            return None;
        }
        if let Some(local) = self.resolve_local(level + 1, name) {
            self.closure_level_mut(level + 1).locals[local as usize].captured = true;
            return Some(self.add_upvalue(level, local, true) as u8);
        }
        if let Some(upvalue) = self.resolve_upvalue(level + 1, name) {
            return Some(self.add_upvalue(level, upvalue, false) as u8);
        }
        None
    }

    fn string(&mut self) {
        let value =
            intern::id(&self.previous_token.lexeme[1..self.previous_token.lexeme.len() - 1]);
        let constant = self.make_const(Value::Str(value));
        self.emit(OpCode::Constant(constant));
    }

    fn number(&mut self) {
        let value = intern::str(intern::id(self.previous_token.lexeme))
            .parse()
            .expect("Unreachable");
        let constant = self.make_const(Value::Number(value));
        self.emit(OpCode::Constant(constant));
    }

    fn literal(&mut self) {
        match self.previous_token.typ {
            token::Type::False => self.emit(OpCode::False),
            token::Type::Nil => self.emit(OpCode::Nil),
            token::Type::True => self.emit(OpCode::True),
            _ => unreachable!(),
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(token::Type::RParen, "Expect ')' after expression");
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let can_assign = precedence <= Precedence::Assignment;
        self.prefix_rule(can_assign);

        while precedence <= Precedence::of(self.current_token.typ) {
            self.advance();
            self.infix_rule(can_assign);
        }

        if can_assign && self.match_type(token::Type::Equal) {
            self.error("Invalid assignment target");
        }
    }

    fn prefix_rule(&mut self, can_assign: bool) {
        match self.previous_token.typ {
            token::Type::LParen => self.grouping(),
            token::Type::Minus | token::Type::Bang => self.unary(),
            token::Type::Super => self.super_(),
            token::Type::This => self.this(),
            token::Type::Ident => self.variable(can_assign),
            token::Type::String => self.string(),
            token::Type::Number => self.number(),
            token::Type::True | token::Type::False | token::Type::Nil => self.literal(),
            _ => {
                self.error("Expect expression");
            }
        }
    }

    fn infix_rule(&mut self, can_assign: bool) {
        match self.previous_token.typ {
            token::Type::Dot => self.dot(can_assign),
            token::Type::LParen => self.call(),
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
            _ => self.error("Expect expression"),
        }
    }

    fn synchronize(&mut self) {
        self.panic = false;
        while !self.check(token::Type::Eof) {
            if self.previous_token.typ == token::Type::Semicolon {
                return;
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
                    eprintln!("{err}");
                    self.had_error = true;
                    self.panic = true;
                }
                Ok(tok) => {
                    self.previous_token = std::mem::replace(&mut self.current_token, tok);
                    break;
                }
            }
        }
    }

    fn match_type(&mut self, typ: token::Type) -> bool {
        if !self.check(typ) {
            return false;
        }
        self.advance();
        true
    }

    fn consume(&mut self, typ: token::Type, msg: &'static str) {
        if !self.check(typ) {
            self.error_current(msg);
            return;
        }
        self.advance();
    }

    fn check(&mut self, typ: token::Type) -> bool {
        if self.current_token.typ != typ {
            return false;
        }
        true
    }

    fn chunk(&mut self) -> &mut Chunk {
        &mut self.closure_level_mut(0).fun.chunk
    }

    fn closure_level(&self, lvl: usize) -> &ClosureLevel {
        // last item contains the chunk that is currently written to
        // level 0 --> current
        // level 1 --> enclosing
        // level 2 --> enlosing of enclosing
        // level 3 --> so on...
        let idx = self.closure_levels.len() - lvl - 1;
        &self.closure_levels[idx]
    }

    fn closure_level_mut(&mut self, lvl: usize) -> &mut ClosureLevel {
        let idx = self.closure_levels.len() - lvl - 1;
        &mut self.closure_levels[idx]
    }

    fn closure_level_pop(&mut self) -> ClosureLevel {
        self.closure_levels.pop().expect("Wrong compiler state")
    }

    fn class_level(&self, lvl: usize) -> &ClassLevel {
        let idx = self.class_levels.len() - lvl - 1;
        &self.class_levels[idx]
    }

    fn class_level_mut(&mut self, lvl: usize) -> &mut ClassLevel {
        let idx = self.class_levels.len() - lvl - 1;
        &mut self.class_levels[idx]
    }

    fn class_level_pop(&mut self) {
        self.class_levels.pop().expect("Wrong compiler state");
    }

    fn make_const(&mut self, v: Value) -> u8 {
        if self.chunk().const_count() == MAX_CHUNK_CONSTANTS {
            self.error("Too many constants in one chunk");
            return MAX_CHUNK_CONSTANTS as u8;
        }
        let const_id = self.chunk().write_const(v);
        const_id as u8
    }

    fn emit(&mut self, op: OpCode) {
        let pos = self.previous_token.pos;
        self.chunk().write_instruction(op, pos);
    }

    fn emit_return(&mut self) {
        if self.closure_level(0).fun_t == FunType::Initializer {
            // A class initializer always return the instance, which locates
            // at the start of the call frame
            self.emit(OpCode::GetLocal(0));
        } else {
            self.emit(OpCode::Nil);
        }
        self.emit(OpCode::Return);
    }

    fn emit_jump<O: Fn(u16) -> OpCode>(&mut self, op: O) -> usize {
        self.emit(op(0xFFFF));
        self.chunk().instructions_count()
    }

    fn patch_jump(&mut self, jump: usize) {
        let offset = self.chunk().instructions_count() - jump;
        if offset > u16::MAX as usize {
            self.error_current("Too much code to jump over");
            return;
        }
        self.chunk().patch_jump_instruction(jump - 1, offset as u16);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        // +1 because the offset also takes into account the newly emitted loop opcode
        let offset = self.chunk().instructions_count() - loop_start + 1;
        if offset > u16::MAX as usize {
            self.error("Loop body too large");
            return;
        }
        self.emit(OpCode::Loop(offset as u16));
    }

    fn error(&mut self, message: &'static str) {
        self.error_at(self.previous_token.pos, self.previous_token.lexeme, message)
    }

    fn error_current(&mut self, message: &'static str) {
        self.error_at(self.current_token.pos, self.current_token.lexeme, message)
    }

    fn error_at(&mut self, pos: Position, lexeme: &str, message: &'static str) {
        if self.panic {
            return;
        }
        self.had_error = true;
        self.panic = true;

        if lexeme.is_empty() {
            eprintln!("{pos} Error at end: {message}.")
        } else {
            eprintln!("{pos} Error at '{lexeme}': {message}.")
        }
    }
}

#[derive(Debug)]
struct ClassLevel {
    has_superclass: bool,
}

#[derive(Debug)]
struct ClosureLevel {
    fun: ObjFun,
    fun_t: FunType,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    scope_depth: usize,
}

impl ClosureLevel {
    fn new(fun: ObjFun, fun_t: FunType) -> Self {
        // The first slot on the stack is reserved for the callframe
        // + With function call, the slot stores a function object that is being called
        // + With method call, the slot stores the receiver of the method
        let mut locals = Vec::with_capacity(MAX_LOCAL_VARIABLES);

        if fun_t != FunType::Function {
            locals.push(Local::from((intern::id("this"), 0, true)));
        } else {
            locals.push(Local::from((intern::id(""), 0, true)));
        }

        Self {
            fun,
            fun_t,
            locals,
            upvalues: Vec::with_capacity(MAX_UPVALUES),
            scope_depth: 0,
        }
    }
}

/// An upvalue refers to a local variable in an enclosing function. Every closure
/// maintains an array of upvalues, one for each surrounding local variables that
/// the clossure uses.
#[derive(Debug, Clone)]
pub struct Upvalue {
    /// The local slot that this upvalue is capturing
    pub index: u8,
    /// True if this upvalue refers the a local variable in the enclosing function.
    /// Otherwise, the upvalue is referring to another upvalue in the enclosing function.
    pub is_local: bool,
}

/// Store name and depth of the resolved identifer.
#[derive(Debug)]
struct Local {
    name: StrId,
    depth: usize,
    initialized: bool,
    captured: bool,
}

impl From<(StrId, usize)> for Local {
    fn from((name, depth): (StrId, usize)) -> Self {
        Self {
            name,
            depth,
            initialized: false,
            captured: false,
        }
    }
}
impl From<(StrId, usize, bool)> for Local {
    fn from((name, depth, initialized): (StrId, usize, bool)) -> Self {
        Self {
            name,
            depth,
            initialized,
            captured: false,
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
            token::Type::LParen | token::Type::Dot => Precedence::Call,
            _ => Self::None,
        }
    }
}
