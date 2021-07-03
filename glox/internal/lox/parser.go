package lox

import "fmt"

const MAX_ARGS_COUNT = 255

// Parser composes the syntax tree for the Lox language from the sequence of
// valid tokens.
type Parser struct {
	current  int
	tokens   []*loxToken
	reporter Reporter
}

// NewParse creates a new parse for the Lox language
func NewParser(tokens []*loxToken, reporter Reporter) *Parser {
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

	if parser.match(tokenVar) {
		stmt, err = parser.varDecl()
	} else if parser.match(tokenFun) {
		stmt, err = parser.function("function")
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

// The parameter "kind" is used to control the error message when this method is
// reused when parsing objects' methods.
func (parser *Parser) function(kind string) (Stmt, error) {
	// function name
	name, err := parser.consume(
		tokenIdentifier,
		fmt.Sprintf("Expect %s name.", kind),
	)
	if err != nil {
		return nil, err
	}
	// function parameters, this works similarly to parsing function calls
	_, err = parser.consume(
		tokenLeftParen,
		fmt.Sprintf("Expect '(' after %s name.", kind),
	)
	if err != nil {
		return nil, err
	}
	params := make([]*loxToken, 0)
	if !parser.check(tokenRightParen) {
		for {
			if len(params) > MAX_ARGS_COUNT {
				parser.reporter.Report(newParseError(
					parser.peek(),
					fmt.Sprintf("Can't have more than %d parameters.", MAX_ARGS_COUNT),
				))
			}

			param, err := parser.consume(tokenIdentifier, "Expect parameter name.")
			if err != nil {
				return nil, err
			}
			params = append(params, param)

			if !parser.match(tokenComma) {
				break
			}
		}
	}
	_, err = parser.consume(tokenRightParen, "Expect ')' after parameters.")
	if err != nil {
		return nil, err
	}
	// function body
	_, err = parser.consume(
		tokenLeftBrace,
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
	name, err := parser.consume(tokenIdentifier, "Expect variable name.")
	if err != nil {
		return nil, err
	}

	var initializer Expr
	if parser.match(tokenEqual) {
		var err error
		initializer, err = parser.expr()
		if err != nil {
			return nil, err
		}
	}

	_, err = parser.consume(tokenSemicolon, "Expect ';' after variable declaration.")
	if err != nil {
		return nil, err
	}
	return NewVarStmt(name, initializer), nil
}

func (parser *Parser) stmt() (Stmt, error) {
	if parser.match(tokenFor) {
		return parser.forStmt()
	}
	if parser.match(tokenIf) {
		return parser.ifStmt()
	}
	if parser.match(tokenPrint) {
		return parser.printStmt()
	}
	if parser.match(tokenReturn) {
		return parser.returnStmt()
	}
	if parser.match(tokenWhile) {
		return parser.whileStmt()
	}
	if parser.match(tokenLeftBrace) {
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
	for !parser.check(tokenRightBrace) && !parser.isEOF() {
		stmt := parser.decl()
		stmts = append(stmts, stmt)
	}
	_, err := parser.consume(tokenRightBrace, "Expect '}' after block.")
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
	_, err = parser.consume(tokenSemicolon, "Expect ';' after expression.")
	if err != nil {
		return nil, err
	}
	return NewExprStmt(expr), nil
}

func (parser *Parser) forStmt() (Stmt, error) {
	_, err := parser.consume(tokenLeftParen, "Expect '(' after 'for'.")
	if err != nil {
		return nil, err
	}
	// initializer clause
	var init Stmt
	if parser.match(tokenSemicolon) {
		// do nothing
	} else if parser.match(tokenVar) {
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
	if !parser.check(tokenSemicolon) {
		cond, err = parser.expr()
		if err != nil {
			return nil, err
		}
	}
	_, err = parser.consume(tokenSemicolon, "Expect ';' after loop condition.")
	if err != nil {
		return nil, err
	}
	// increment clause
	var inc Expr
	if !parser.check(tokenRightParen) {
		inc, err = parser.expr()
		if err != nil {
			return nil, err
		}
	}
	_, err = parser.consume(tokenRightParen, "Expect ')' after for clauses.")
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
	_, err := parser.consume(tokenLeftParen, "Expect '(' after 'if'.")
	if err != nil {
		return nil, err
	}
	cond, err := parser.expr()
	if err != nil {
		return nil, err
	}
	_, err = parser.consume(tokenRightParen, "Expect ')' after if condition.")
	if err != nil {
		return nil, err
	}

	thenBranch, err := parser.stmt()
	if err != nil {
		return nil, err
	}

	var elseBranch Stmt
	if parser.match(tokenElse) {
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
	_, err = parser.consume(tokenSemicolon, "Expect ';' after value.")
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
	if !parser.check(tokenSemicolon) {
		val, err = parser.expr()
		if err != nil {
			return nil, err
		}
	}

	_, err = parser.consume(tokenSemicolon, "Expect ';' after return value.")
	if err != nil {
		return nil, err
	}
	return NewReturnStmt(keyword, val), nil
}

func (parser *Parser) whileStmt() (Stmt, error) {
	_, err := parser.consume(tokenLeftParen, "Expect '(' after 'while'.")
	if err != nil {
		return nil, err
	}
	cond, err := parser.expr()
	if err != nil {
		return nil, err
	}
	_, err = parser.consume(tokenRightParen, "Expect ')' after condition.")
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
	if parser.match(tokenEqual) {
		op := parser.prev()
		rhs, err := parser.assign()
		if err != nil {
			return nil, err
		}
		if lhs, ok := lhs.(*VarExpr); ok {
			return NewAssignExpr(lhs.Name, rhs), nil
		}
		parser.reporter.Report(newParseError(op, "Invalid assignment target."))
	}
	return lhs, nil
}

func (parser *Parser) or() (Expr, error) {
	lhs, err := parser.and()
	if err != nil {
		return nil, err
	}
	for parser.match(tokenOr) {
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
	for parser.match(tokenAnd) {
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
	for parser.match(tokenBangEqual, tokenEqualEqual) {
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
	for parser.match(tokenGreater, tokenGreaterEqual, tokenLess, tokenLessEqual) {
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
	for parser.match(tokenMinus, tokenPlus) {
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
	for parser.match(tokenSlash, tokenStar) {
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
	if parser.match(tokenBang, tokenMinus, tokenPlus, tokenSlash, tokenStar) {
		op := parser.prev()
		switch expr, err := parser.unary(); op.typ {
		case tokenPlus, tokenSlash, tokenStar:
			err = newParseError(
				op,
				fmt.Sprintf("Unary '%s' expressions are not supported.", op.lexeme),
			)
			fallthrough
		case tokenBang, tokenMinus:
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
		if parser.match(tokenLeftParen) {
			expr, err = parser.finishCall(expr)
			if err != nil {
				return nil, err
			}
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
	if !parser.check(tokenRightParen) {
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

			if !parser.match(tokenComma) {
				break
			}
		}
	}

	closeParen, err := parser.consume(tokenRightParen, "Expect ')' after arguments.")
	if err != nil {
		return nil, err
	}
	return NewCallExpr(callee, closeParen, args), nil
}

func (parser *Parser) primary() (Expr, error) {
	if parser.match(tokenFalse) {
		return NewLiteralExpr(false), nil
	}
	if parser.match(tokenTrue) {
		return NewLiteralExpr(true), nil
	}
	if parser.match(tokenNil) {
		return NewLiteralExpr(nil), nil
	}
	if parser.match(tokenNumber, tokenString) {
		return NewLiteralExpr(parser.prev().literal), nil
	}
	if parser.match(tokenIdentifier) {
		return NewVarExpr(parser.prev()), nil
	}
	if parser.match(tokenLeftParen) {
		expr, err := parser.expr()
		if err != nil {
			return nil, err
		}
		_, err = parser.consume(tokenRightParen, "Expect ')' after expression.")
		if err != nil {
			return nil, err
		}
		return NewGroupExpr(expr), nil
	}
	return nil, newParseError(parser.peek(), "Expect expression.")
}

func (parser *Parser) match(types ...loxTokenType) bool {
	for _, tt := range types {
		if parser.check(tt) {
			parser.advance()
			return true
		}
	}
	return false
}

func (parser *Parser) consume(typ loxTokenType, message string) (*loxToken, error) {
	if parser.check(typ) {
		token := parser.advance()
		return token, nil
	}
	return nil, newParseError(parser.peek(), message)
}

func (parser *Parser) check(tt loxTokenType) bool {
	if parser.isEOF() {
		return false
	}
	return parser.peek().typ == tt
}

func (parser *Parser) advance() *loxToken {
	if !parser.isEOF() {
		parser.current++
	}
	return parser.prev()
}

func (parser *Parser) isEOF() bool {
	return parser.peek().typ == tokenEOF
}

func (parser *Parser) peek() *loxToken {
	return parser.tokens[parser.current]
}

func (parser *Parser) prev() *loxToken {
	return parser.tokens[parser.current-1]
}

func (parser *Parser) sync() {
	parser.advance()
	for !parser.isEOF() {
		if parser.prev().typ == tokenSemicolon {
			return
		}
		switch parser.peek().typ {
		case tokenClass, tokenFun, tokenVar, tokenFor, tokenIf, tokenWhile, tokenPrint, tokenReturn:
			return
		}
		parser.advance()
	}
}
