package lox

import "fmt"

const MAX_ARGS_COUNT = 255

// Parser composes the syntax tree for the Lox language from the sequence of
// valid tokens.
type Parser struct {
	current  int
	tokens   []*Token
	reporter Reporter
}

// NewParse creates a new parse for the Lox language
func NewParser(tokens []*Token, reporter Reporter) *Parser {
	parser := new(Parser)
	parser.current = 0
	parser.tokens = tokens
	parser.reporter = reporter
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

	switch {
	case parser.match(CLASS):
		stmt, err = parser.classDecl()
	case parser.match(FUN):
		stmt, err = parser.function("function")
	case parser.match(VAR):
		stmt, err = parser.varDecl()
	default:
		stmt, err = parser.stmt()
	}

	if err != nil {
		parser.reporter.Report(err)
		parser.sync()
		return nil
	}
	return stmt
}

func (parser *Parser) classDecl() (Stmt, error) {
	name, err := parser.consume(IDENT, "Expect class name.")
	if err != nil {
		return nil, err
	}

	var super *VarExpr
	if parser.match(LESS) {
		name, err := parser.consume(IDENT, "Expect superclass name.")
		if err != nil {
			return nil, err
		}
		super = NewVarExpr(name)
	}

	_, err = parser.consume(L_BRACE, "Expect '{' before class body.")
	if err != nil {
		return nil, err
	}
	var methods []*FunctionStmt
	for !parser.check(R_BRACE) && !parser.isEOF() {
		method, err := parser.function("method")
		if err != nil {
			return nil, err
		}
		methods = append(methods, method)
	}
	_, err = parser.consume(R_BRACE, "Expect '}' after class body.")
	if err != nil {
		return nil, err
	}

	return NewClassStmt(name, super, methods), nil
}

// The parameter "kind" is used to control the error message when this method is
// reused when parsing objects' methods.
func (parser *Parser) function(kind string) (*FunctionStmt, error) {
	// function name
	name, err := parser.consume(
		IDENT,
		fmt.Sprintf("Expect %s name.", kind),
	)
	if err != nil {
		return nil, err
	}
	// function parameters, this works similarly to parsing function calls
	_, err = parser.consume(
		L_PAREN,
		fmt.Sprintf("Expect '(' after %s name.", kind),
	)
	if err != nil {
		return nil, err
	}
	params := make([]*Token, 0)
	if !parser.check(R_PAREN) {
		for {
			if len(params) > MAX_ARGS_COUNT {
				parser.reporter.Report(newParseError(
					parser.peek(),
					fmt.Sprintf("Can't have more than %d parameters.", MAX_ARGS_COUNT),
				))
			}

			param, err := parser.consume(IDENT, "Expect parameter name.")
			if err != nil {
				return nil, err
			}
			params = append(params, param)

			if !parser.match(COMMA) {
				break
			}
		}
	}
	_, err = parser.consume(R_PAREN, "Expect ')' after parameters.")
	if err != nil {
		return nil, err
	}
	// function body
	_, err = parser.consume(
		L_BRACE,
		fmt.Sprintf("Expect '{' before %s body.", kind),
	)
	if err != nil {
		return nil, err
	}
	body, err := parser.block()
	if err != nil {
		return nil, err
	}
	return NewFunctionStmt(name, params, body), nil
}

func (parser *Parser) varDecl() (Stmt, error) {
	name, err := parser.consume(IDENT, "Expect variable name.")
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
	if parser.match(IF) {
		return parser.ifStmt()
	}
	if parser.match(PRINT) {
		return parser.printStmt()
	}
	if parser.match(RETURN) {
		return parser.returnStmt()
	}
	if parser.match(WHILE) {
		return parser.whileStmt()
	}
	if parser.match(L_BRACE) {
		stmts, err := parser.block()
		if err != nil {
			return nil, err
		}
		return NewBlockStmt(stmts), nil
	}
	return parser.exprStmt()
}

// block parses all statements with a pair of braces, "{" has to be consumed
// before calling this
func (parser *Parser) block() ([]Stmt, error) {
	var stmts []Stmt
	for !parser.check(R_BRACE) && !parser.isEOF() {
		stmt := parser.decl()
		stmts = append(stmts, stmt)
	}
	_, err := parser.consume(R_BRACE, "Expect '}' after block.")
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
	_, err := parser.consume(L_PAREN, "Expect '(' after 'for'.")
	if err != nil {
		return nil, err
	}
	// initializer clause
	var init Stmt
	switch {
	case parser.match(SEMICOLON):
	case parser.match(VAR):
		init, err = parser.varDecl()
		if err != nil {
			return nil, err
		}
	default:
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
	if !parser.check(R_PAREN) {
		inc, err = parser.expr()
		if err != nil {
			return nil, err
		}
	}
	_, err = parser.consume(R_PAREN, "Expect ')' after for clauses.")
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
	_, err := parser.consume(L_PAREN, "Expect '(' after 'if'.")
	if err != nil {
		return nil, err
	}
	cond, err := parser.expr()
	if err != nil {
		return nil, err
	}
	_, err = parser.consume(R_PAREN, "Expect ')' after if condition.")
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

func (parser *Parser) returnStmt() (Stmt, error) {
	var val Expr
	var err error
	keyword := parser.prev()
	// checking if the expression is absent instead if it is present, since an
	// expression can start with many different token
	if !parser.check(SEMICOLON) {
		val, err = parser.expr()
		if err != nil {
			return nil, err
		}
	}

	_, err = parser.consume(SEMICOLON, "Expect ';' after return value.")
	if err != nil {
		return nil, err
	}
	return NewReturnStmt(keyword, val), nil
}

func (parser *Parser) whileStmt() (Stmt, error) {
	_, err := parser.consume(L_PAREN, "Expect '(' after 'while'.")
	if err != nil {
		return nil, err
	}
	cond, err := parser.expr()
	if err != nil {
		return nil, err
	}
	_, err = parser.consume(R_PAREN, "Expect ')' after condition.")
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
		switch lhs := lhs.(type) {
		case *VarExpr:
			return NewAssignExpr(lhs.Name, rhs), nil
		case *GetExpr:
			return NewSetExpr(lhs.Obj, lhs.Name, rhs), nil
		default:
			parser.reporter.Report(newParseError(op, "Invalid assignment target."))
		}
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
		switch expr, err := parser.unary(); op.Type {
		case PLUS, SLASH, STAR:
			err = newParseError(
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
	return parser.call()
}

// call parses function calls, refers to the grammar to see what's qualified as a
// function call.
func (parser *Parser) call() (Expr, error) {
	expr, err := parser.primary()
	if err != nil {
		return nil, err
	}

	for {
		if parser.match(L_PAREN) {
			expr, err = parser.finishCall(expr)
			if err != nil {
				return nil, err
			}
		} else if parser.match(DOT) {
			name, err := parser.consume(IDENT, "Expect property name after '.'.")
			if err != nil {
				return nil, err
			}
			expr = NewGetExpr(expr, name)
		} else {
			break
		}
	}

	return expr, nil
}

// callFinish parsers a function call's arguments, refers to the grammar to see
// hows arguments are given to a function call.
func (parser *Parser) finishCall(callee Expr) (Expr, error) {
	var args []Expr
	if !parser.check(R_PAREN) {
		for {
			if len(args) >= MAX_ARGS_COUNT {
				parser.reporter.Report(newParseError(
					parser.peek(),
					fmt.Sprintf("Can't have more than %d arguments.", MAX_ARGS_COUNT),
				))
			}

			arg, err := parser.expr()
			if err != nil {
				return nil, err
			}
			args = append(args, arg)

			if !parser.match(COMMA) {
				break
			}
		}
	}

	closeParen, err := parser.consume(R_PAREN, "Expect ')' after arguments.")
	if err != nil {
		return nil, err
	}
	return NewCallExpr(callee, closeParen, args), nil
}

func (parser *Parser) primary() (Expr, error) {
	if parser.match(THIS) {
		return NewThisExpr(parser.prev()), nil
	}
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
	if parser.match(IDENT) {
		return NewVarExpr(parser.prev()), nil
	}
	if parser.match(L_PAREN) {
		expr, err := parser.expr()
		if err != nil {
			return nil, err
		}
		_, err = parser.consume(R_PAREN, "Expect ')' after expression.")
		if err != nil {
			return nil, err
		}
		return NewGroupExpr(expr), nil
	}
	return nil, newParseError(parser.peek(), "Expect expression.")
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
	return nil, newParseError(parser.peek(), message)
}

func (parser *Parser) check(tt TokenType) bool {
	if parser.isEOF() {
		return false
	}
	return parser.peek().Type == tt
}

func (parser *Parser) advance() *Token {
	if !parser.isEOF() {
		parser.current++
	}
	return parser.prev()
}

func (parser *Parser) isEOF() bool {
	return parser.peek().Type == EOF
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
		if parser.prev().Type == SEMICOLON {
			return
		}
		switch parser.peek().Type {
		case CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN:
			return
		}
		parser.advance()
	}
}
