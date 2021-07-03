package lox

import "fmt"

// Parser composes the syntax tree for the Lox language from the sequence of
// valid tokens that follow the following grammar rule.
//
// Grammar
//
//	program    --> decl* EOF ;
//	decl       --> varDecl
//	             | stmt
//
//	stmt       --> block
//	             | exprStmt
//	             | forStmt
//	             | ifStmt
//	             | printStmt
//	             | whileStmt
//	block      --> "{" decl* "}" ;
//	exprStmt   --> expr ";" ;
//	forStmt  --> "for" "(" ( varDecl | exprStmt | ";" ) expr? ";" expr? ")" stmt ;
//	ifStmt     --> "if" "(" expr ")" stmt ( "else" stmt )? ;
//	printStmt  --> "print" expr ";" ;
//	whileStmt  --> "while" "(" expr ")" stmt ;
//
//	expr       --> assign ;
//	assign     --> IDENTIFIER "=" expr ";"
//	             | or ;
//	or         --> and ( "or" and )* ;
//	and        --> equality ( "and" equality )* ;
//	equality   --> comparison ( ( "!=" | "==" ) comparison )* ;
//	comparison --> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//	term       --> factor ( ( "-" | "+" ) factor )* ;
//	factor     --> unary ( ( "/" | "*" ) unary )* ;
//	unary      --> ( "!" | "-" | "+" | "/" | "*" ) unary
//	             | primary ;
//	primary    --> NUMBER | STRING | IDENTIFIER
//	             | "true" | "false" | "nil"
//	             | "(" expr ")" ;
//
// In our unary rule for, we our accepting three unary operators that are not supported
// by the interpreter so we can produce better error
// + Unary '+' expressions are not supported.
// + Unary '/' expressions are not supported.
// + Unary '*' expressions are not supported.
type Parser struct {
	current  int
	tokens   []*Token
	reporter Reporter
}

// NewParse creates a new parse for the Lox language
func NewParser(tokens []*Token, reporter Reporter) *Parser {
	return &Parser{0, tokens, reporter}
}

func (parser *Parser) Parse() []Stmt {
	var stmts []Stmt
	for !parser.isEOF() {
		stmt := parser.decl()
		stmts = append(stmts, stmt)
	}
	return stmts
}

func (parser *Parser) decl() Stmt {
	var stmt Stmt
	var err error

	if parser.match(VAR) {
		stmt, err = parser.varDecl()
	} else {
		stmt, err = parser.stmt()
	}

	if err != nil {
		parser.reporter.Report(err)
		parser.sync()
		return nil
	}
	return stmt
}

func (parser *Parser) varDecl() (Stmt, error) {
	name, err := parser.consume(IDENTIFIER, "Expect variable name.")
	if err != nil {
		return nil, err
	}

	var initializer Expr
	if parser.match(EQUAL) {
		var err error
		initializer, err = parser.expr()
		if err != nil {
			return nil, err
		}
	}

	_, err = parser.consume(SEMICOLON, "Expect ';' after variable declaration.")
	if err != nil {
		return nil, err
	}
	return NewVarStmt(name, initializer), nil
}

func (parser *Parser) stmt() (Stmt, error) {
	if parser.match(FOR) {
		return parser.forStmt()
	}
	if parser.match(WHILE) {
		return parser.whileStmt()
	}
	if parser.match(IF) {
		return parser.ifStmt()
	}
	if parser.match(PRINT) {
		return parser.printStmt()
	}
	if parser.match(LEFT_BRACE) {
		stmts, err := parser.block()
		if err != nil {
			return nil, err
		}
		return NewBlockStmt(stmts), nil
	}
	return parser.exprStmt()
}

func (parser *Parser) block() ([]Stmt, error) {
	var stmts []Stmt
	for !parser.check(RIGHT_BRACE) && !parser.isEOF() {
		stmt := parser.decl()
		stmts = append(stmts, stmt)
	}
	_, err := parser.consume(RIGHT_BRACE, "Expect '}' after block.")
	if err != nil {
		return nil, err
	}
	return stmts, nil
}

func (parser *Parser) exprStmt() (Stmt, error) {
	expr, err := parser.expr()
	if err != nil {
		return nil, err
	}
	_, err = parser.consume(SEMICOLON, "Expect ';' after expression.")
	if err != nil {
		return nil, err
	}
	return NewExprStmt(expr), nil
}

func (parser *Parser) forStmt() (Stmt, error) {
	_, err := parser.consume(LEFT_PAREN, "Expect '(' after 'for'.")
	if err != nil {
		return nil, err
	}
	// initializer clause
	var init Stmt
	if parser.match(SEMICOLON) {
		// do nothing
	} else if parser.match(VAR) {
		init, err = parser.varDecl()
		if err != nil {
			return nil, err
		}
	} else {
		init, err = parser.exprStmt()
		if err != nil {
			return nil, err
		}
	}
	// conditional clause
	var cond Expr
	if !parser.check(SEMICOLON) {
		cond, err = parser.expr()
		if err != nil {
			return nil, err
		}
	}
	_, err = parser.consume(SEMICOLON, "Expect ';' after loop condition.")
	if err != nil {
		return nil, err
	}
	// increment clause
	var inc Expr
	if !parser.check(RIGHT_PAREN) {
		inc, err = parser.expr()
		if err != nil {
			return nil, err
		}
	}
	_, err = parser.consume(RIGHT_PAREN, "Expect ')' after for clauses.")
	if err != nil {
		return nil, err
	}

	// desugaring for statement by building the AST by hand
	body, err := parser.stmt()
	if err != nil {
		return nil, err
	}
	if inc != nil {
		body = NewBlockStmt([]Stmt{body, NewExprStmt(inc)})
	}
	if cond == nil {
		cond = NewLiteralExpr(true)
	}
	body = NewWhileStmt(cond, body)
	if init != nil {
		body = NewBlockStmt([]Stmt{init, body})
	}
	return body, nil
}

func (parser *Parser) ifStmt() (Stmt, error) {
	_, err := parser.consume(LEFT_PAREN, "Expect '(' after 'if'.")
	if err != nil {
		return nil, err
	}
	cond, err := parser.expr()
	if err != nil {
		return nil, err
	}
	_, err = parser.consume(RIGHT_PAREN, "Expect ')' after if condition.")
	if err != nil {
		return nil, err
	}

	thenBranch, err := parser.stmt()
	if err != nil {
		return nil, err
	}

	var elseBranch Stmt
	if parser.match(ELSE) {
		elseBranch, err = parser.stmt()
		if err != nil {
			return nil, err
		}
	}
	return NewIfStmt(cond, thenBranch, elseBranch), nil
}

func (parser *Parser) printStmt() (Stmt, error) {
	expr, err := parser.expr()
	if err != nil {
		return nil, err
	}
	_, err = parser.consume(SEMICOLON, "Expect ';' after value.")
	if err != nil {
		return nil, err
	}
	return NewPrintStmt(expr), nil
}

func (parser *Parser) whileStmt() (Stmt, error) {
	_, err := parser.consume(LEFT_PAREN, "Expect '(' after 'while'.")
	if err != nil {
		return nil, err
	}
	cond, err := parser.expr()
	if err != nil {
		return nil, err
	}
	_, err = parser.consume(RIGHT_PAREN, "Expect ')' after condition.")
	if err != nil {
		return nil, err
	}

	body, err := parser.stmt()
	if err != nil {
		return nil, err
	}
	return NewWhileStmt(cond, body), nil
}

func (parser *Parser) expr() (Expr, error) {
	return parser.assign()
}

func (parser *Parser) assign() (Expr, error) {
	lhs, err := parser.or()
	if err != nil {
		return nil, err
	}
	if parser.match(EQUAL) {
		op := parser.prev()
		rhs, err := parser.assign()
		if err != nil {
			return nil, err
		}
		if lhs, ok := lhs.(*VarExpr); ok {
			return NewAssignExpr(lhs.Name, rhs), nil
		}
		parser.reporter.Report(NewParseError(op, "Invalid assignment target."))
	}
	return lhs, nil
}

func (parser *Parser) or() (Expr, error) {
	lhs, err := parser.and()
	if err != nil {
		return nil, err
	}
	for parser.match(OR) {
		op := parser.prev()
		rhs, err := parser.and()
		if err != nil {
			return nil, err
		}
		lhs = NewLogicalExpr(op, lhs, rhs)
	}
	return lhs, nil
}

func (parser *Parser) and() (Expr, error) {
	lhs, err := parser.equality()
	if err != nil {
		return nil, err
	}
	for parser.match(AND) {
		op := parser.prev()
		rhs, err := parser.equality()
		if err != nil {
			return nil, err
		}
		lhs = NewLogicalExpr(op, lhs, rhs)
	}
	return lhs, nil
}

func (parser *Parser) equality() (Expr, error) {
	lhs, err := parser.comparison()
	if err != nil {
		return nil, err
	}
	for parser.match(BANG_EQUAL, EQUAL_EQUAL) {
		op := parser.prev()
		rhs, err := parser.comparison()
		if err != nil {
			return nil, err
		}
		lhs = NewBinaryExpr(op, lhs, rhs)
	}
	return lhs, nil
}

func (parser *Parser) comparison() (Expr, error) {
	lhs, err := parser.term()
	if err != nil {
		return nil, err
	}
	for parser.match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL) {
		op := parser.prev()
		rhs, err := parser.term()
		if err != nil {
			return nil, err
		}
		lhs = NewBinaryExpr(op, lhs, rhs)
	}
	return lhs, nil
}

func (parser *Parser) term() (Expr, error) {
	lhs, err := parser.factor()
	if err != nil {
		return nil, err
	}
	for parser.match(MINUS, PLUS) {
		op := parser.prev()
		rhs, err := parser.factor()
		if err != nil {
			return nil, err
		}
		lhs = NewBinaryExpr(op, lhs, rhs)
	}
	return lhs, nil
}

func (parser *Parser) factor() (Expr, error) {
	lhs, err := parser.unary()
	if err != nil {
		return nil, err
	}
	for parser.match(SLASH, STAR) {
		op := parser.prev()
		rhs, err := parser.unary()
		if err != nil {
			return nil, err
		}
		lhs = NewBinaryExpr(op, lhs, rhs)
	}
	return lhs, nil
}

func (parser *Parser) unary() (Expr, error) {
	if parser.match(BANG, MINUS, PLUS, SLASH, STAR) {
		op := parser.prev()
		switch expr, err := parser.unary(); op.Typ {
		case PLUS, SLASH, STAR:
			err = NewParseError(
				op,
				fmt.Sprintf("Unary '%s' expressions are not supported.", op.Lexeme),
			)
			fallthrough
		case BANG, MINUS:
			if err != nil {
				return nil, err
			}
			return NewUnaryExpr(op, expr), nil
		}
	}
	return parser.primary()
}

func (parser *Parser) primary() (Expr, error) {
	if parser.match(FALSE) {
		return NewLiteralExpr(false), nil
	}
	if parser.match(TRUE) {
		return NewLiteralExpr(true), nil
	}
	if parser.match(NIL) {
		return NewLiteralExpr(nil), nil
	}
	if parser.match(NUMBER, STRING) {
		return NewLiteralExpr(parser.prev().Literal), nil
	}
	if parser.match(IDENTIFIER) {
		return NewVarExpr(parser.prev()), nil
	}
	if parser.match(LEFT_PAREN) {
		expr, err := parser.expr()
		if err != nil {
			return nil, err
		}
		_, err = parser.consume(RIGHT_PAREN, "Expect ')' after expression.")
		if err != nil {
			return nil, err
		}
		return NewGroupExpr(expr), nil
	}
	return nil, NewParseError(parser.peek(), "Expect expression.")
}

func (parser *Parser) match(types ...TokenType) bool {
	for _, tt := range types {
		if parser.check(tt) {
			parser.advance()
			return true
		}
	}
	return false
}

func (parser *Parser) consume(typ TokenType, message string) (*Token, error) {
	if parser.check(typ) {
		token := parser.advance()
		return token, nil
	}
	return nil, NewParseError(parser.peek(), message)
}

func (parser *Parser) check(tt TokenType) bool {
	if parser.isEOF() {
		return false
	}
	return parser.peek().Typ == tt
}

func (parser *Parser) advance() *Token {
	if !parser.isEOF() {
		parser.current++
	}
	return parser.prev()
}

func (parser *Parser) isEOF() bool {
	return parser.peek().Typ == EOF
}

func (parser *Parser) peek() *Token {
	return parser.tokens[parser.current]
}

func (parser *Parser) prev() *Token {
	return parser.tokens[parser.current-1]
}

func (parser *Parser) sync() {
	parser.advance()
	for !parser.isEOF() {
		if parser.prev().Typ == SEMICOLON {
			return
		}
		switch parser.peek().Typ {
		case CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN:
			return
		}
		parser.advance()
	}
}
