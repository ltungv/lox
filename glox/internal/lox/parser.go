package lox

import "fmt"

// Parser composes the syntax tree for the Lox language from the sequence of
// valid tokens that follow the following grammar rule.
//
// Grammar
//
//	expression --> equality ;
//	equality   --> comparison ( ( "!=" | "==" ) comparison )* ;
//	comparison --> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//	term       --> factor ( ( "-" | "+" ) factor )* ;
//	factor     --> unary ( ( "/" | "*" ) unary )* ;
//	unary      --> ( "!" | "-" | "+" | "/" | "*" ) unary
//	             | primary ;
//	primary    --> NUMBER | STRING
//	             | "true" | "false" | "nil"
//	             | "(" expression ")" ;
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

func (parser *Parser) Parse() Expr {
	expr, err := parser.expression()
	if err != nil {
		parser.reporter.Report(err)
		return nil
	}
	return expr
}

// expression --> equality ;
func (parser *Parser) expression() (Expr, error) {
	return parser.equality()
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
	if parser.match(LEFT_PAREN) {
		expr, err := parser.expression()
		if err != nil {
			return nil, err
		}
		if err := parser.consume(
			RIGHT_PAREN,
			"Expect ')' after expression.",
		); err != nil {
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

func (parser *Parser) consume(typ TokenType, message string) error {
	if parser.check(typ) {
		parser.advance()
		return nil
	}
	return NewParseError(parser.peek(), message)
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
