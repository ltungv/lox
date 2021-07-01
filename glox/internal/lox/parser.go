package lox

import "fmt"

// Parser composes the syntax tree for the Lox language from the sequence of
// valid tokens that follow the following grammar rule.
//
// Grammar
//
//	program --> declaration* EOF ;
//	declaration --> varDeclaration
//	              | statement
//	varDeclaration --> "var" IDENTIFIER ( "=" expression )? ";" ;
//	statement --> block
//	            | expressionStatement
//	            | printStatement ;
//	block --> "{" declaration* "}" ;
//  expressionStatement --> expression ";" ;
//  printStatement --> "print" expression ";" ;
//	expression --> assignment ;
//	assignment --> IDENTIFIER "=" expression ";"
//	             | equality ;
//	equality --> comparison ( ( "!=" | "==" ) comparison )* ;
//	comparison --> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//	term --> factor ( ( "-" | "+" ) factor )* ;
//	factor --> unary ( ( "/" | "*" ) unary )* ;
//	unary --> ( "!" | "-" | "+" | "/" | "*" ) unary
//	        | primary ;
//	primary --> NUMBER | STRING | IDENTIFIER
//	          | "true" | "false" | "nil"
//	          | "(" expression ")" ;
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
	var statements []Stmt
	for !parser.isEOF() {
		stmt := parser.declaration()
		statements = append(statements, stmt)
	}
	return statements
}

// declaration --> varDeclaration
//               | statement
func (parser *Parser) declaration() Stmt {
	var stmt Stmt
	var err error

	if parser.match(VAR) {
		stmt, err = parser.varDeclaration()
	} else {
		stmt, err = parser.statement()
	}

	if err != nil {
		parser.reporter.Report(err)
		parser.sync()
		return nil
	}
	return stmt
}

// varDeclaration --> "var" IDENTIFIER ( "=" expression )? ";" ;
func (parser *Parser) varDeclaration() (Stmt, error) {
	name, err := parser.consume(IDENTIFIER, "Expect variable name.")
	if err != nil {
		return nil, err
	}

	var initializer Expr
	if parser.match(EQUAL) {
		var err error
		initializer, err = parser.expression()
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

// statement --> block
//             | expressionStatement
//             | printStatement ;
func (parser *Parser) statement() (Stmt, error) {
	if parser.match(PRINT) {
		return parser.printStatement()
	}
	if parser.match(LEFT_BRACE) {
		statements, err := parser.block()
		if err != nil {
			return nil, err
		}
		return NewBlockStmt(statements), nil
	}
	return parser.expressionStatement()
}

// printStatement --> "print" expression ";" ;
func (parser *Parser) printStatement() (Stmt, error) {
	expr, err := parser.expression()
	if err != nil {
		return nil, err
	}
	_, err = parser.consume(SEMICOLON, "Expect ';' after value.")
	if err != nil {
		return nil, err
	}
	return NewPrintStmt(expr), nil
}

// block --> "{" declaration* "}" ;
func (parser *Parser) block() ([]Stmt, error) {
	var statements []Stmt
	for !parser.check(RIGHT_BRACE) && !parser.isEOF() {
		stmt := parser.declaration()
		statements = append(statements, stmt)
	}
	_, err := parser.consume(RIGHT_BRACE, "Expect '}' after block.")
	if err != nil {
		return nil, err
	}
	return statements, nil
}

// expressionStatement --> expression ";" ;
func (parser *Parser) expressionStatement() (Stmt, error) {
	expr, err := parser.expression()
	if err != nil {
		return nil, err
	}
	_, err = parser.consume(SEMICOLON, "Expect ';' after expression.")
	if err != nil {
		return nil, err
	}
	return NewExpressionStmt(expr), nil
}

// expression --> assignment ;
func (parser *Parser) expression() (Expr, error) {
	return parser.assignment()
}

// assignment --> IDENTIFIER "=" expression ";"
//              | equality ;
func (parser *Parser) assignment() (Expr, error) {
	lhs, err := parser.equality()
	if err != nil {
		return nil, err
	}
	if parser.match(EQUAL) {
		op := parser.prev()
		rhs, err := parser.assignment()
		if err != nil {
			return nil, err
		}
		if lhs, ok := lhs.(*VariableExpr); ok {
			return NewAssignExpr(lhs.Name, rhs), nil
		}
		parser.reporter.Report(NewParseError(op, "Invalid assignment target."))
	}
	return lhs, nil
}

// Creates a left-associative nested tree of binary operator nodes. Match a
// higher precedence rule `comparison` if does not hits "!=" or "==".
//
// equality --> comparison ( ( "!=" | "==" ) comparison )* ;
func (parser *Parser) equality() (Expr, error) {
	expr, err := parser.comparison()
	if err != nil {
		return nil, err
	}
	for parser.match(BANG_EQUAL, EQUAL_EQUAL) {
		op := parser.prev()
		right, err := parser.comparison()
		if err != nil {
			return nil, err
		}
		expr = NewBinaryExpr(op, expr, right)
	}
	return expr, nil
}

// comparison --> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
func (parser *Parser) comparison() (Expr, error) {
	expr, err := parser.term()
	if err != nil {
		return nil, err
	}
	for parser.match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL) {
		op := parser.prev()
		right, err := parser.term()
		if err != nil {
			return nil, err
		}
		expr = NewBinaryExpr(op, expr, right)
	}
	return expr, nil
}

// term --> factor ( ( "-" | "+" ) factor )* ;
func (parser *Parser) term() (Expr, error) {
	expr, err := parser.factor()
	if err != nil {
		return nil, err
	}
	for parser.match(MINUS, PLUS) {
		op := parser.prev()
		right, err := parser.factor()
		if err != nil {
			return nil, err
		}
		expr = NewBinaryExpr(op, expr, right)
	}
	return expr, nil
}

// factor --> unary ( ( "/" | "*" ) unary )* ;
func (parser *Parser) factor() (Expr, error) {
	expr, err := parser.unary()
	if err != nil {
		return nil, err
	}
	for parser.match(SLASH, STAR) {
		op := parser.prev()
		right, err := parser.unary()
		if err != nil {
			return nil, err
		}
		expr = NewBinaryExpr(op, expr, right)
	}
	return expr, nil
}

// unary --> ( "!" | "-" | "+" | "/" | "*" ) unary
//         | primary ;
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

// primary --> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
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
		return NewVariableExpr(parser.prev()), nil
	}
	if parser.match(LEFT_PAREN) {
		expr, err := parser.expression()
		if err != nil {
			return nil, err
		}
		_, err = parser.consume(RIGHT_PAREN, "Expect ')' after expression.")
		if err != nil {
			return nil, err
		}
		return NewGroupingExpr(expr), nil
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
