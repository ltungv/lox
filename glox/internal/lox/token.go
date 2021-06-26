package lox

import "fmt"

// Token represents group a characters with additional information that was
// obtained during the scanning phase.
type Token struct {
	Typ     TokenType
	Lexeme  string
	Literal interface{}
	Line    int
}

// New creates a new token
func NewToken(typ TokenType, lexeme string, literal interface{}, line int) *Token {
	return &Token{typ, lexeme, literal, line}
}

func (t *Token) String() string {
	return fmt.Sprintf("%s %s %v", t.Typ.String(), t.Lexeme, t.Literal)
}

var Keywords = map[string]TokenType{
	"AND":    AND,
	"CLASS":  CLASS,
	"ELSE":   ELSE,
	"FALSE":  FALSE,
	"FUN":    FUN,
	"FOR":    FOR,
	"IF":     IF,
	"NIL":    NIL,
	"OR":     OR,
	"PRINT":  PRINT,
	"RETURN": RETURN,
	"SUPER":  SUPER,
	"THIS":   THIS,
	"TRUE":   TRUE,
	"VAR":    VAR,
	"WHILE":  WHILE,
	"EOF":    EOF,
}

const (
	// Single-character tokens
	LEFT_PAREN TokenType = iota
	RIGHT_PAREN
	LEFT_BRACE
	RIGHT_BRACE
	COMMA
	DOT
	MINUS
	PLUS
	SEMICOLON
	SLASH
	STAR

	// One or two chracter tokens
	BANG
	BANG_EQUAL
	EQUAL
	EQUAL_EQUAL
	GREATER
	GREATER_EQUAL
	LESS
	LESS_EQUAL

	// Literals
	IDENTIFIER
	STRING
	NUMBER

	// Keywords
	AND
	CLASS
	ELSE
	FALSE
	FUN
	FOR
	IF
	NIL
	OR
	PRINT
	RETURN
	SUPER
	THIS
	TRUE
	VAR
	WHILE
	EOF
)

/// TokenType is a just a wrapped string used to represent token's type
type TokenType uint

func (tt *TokenType) String() string {
	switch *tt {
	case LEFT_PAREN:
		return "("
	case RIGHT_PAREN:
		return ")"
	case LEFT_BRACE:
		return "{"
	case RIGHT_BRACE:
		return "}"
	case COMMA:
		return ","
	case DOT:
		return "."
	case MINUS:
		return "-"
	case PLUS:
		return "+"
	case SEMICOLON:
		return ";"
	case SLASH:
		return "/"
	case STAR:
		return "*"
	case BANG:
		return "!"
	case BANG_EQUAL:
		return "!="
	case EQUAL:
		return "="
	case EQUAL_EQUAL:
		return "=="
	case GREATER:
		return ">"
	case GREATER_EQUAL:
		return ">="
	case LESS:
		return "<"
	case LESS_EQUAL:
		return "<="
	case IDENTIFIER:
		return "IDENTIFIER"
	case STRING:
		return "STRING"
	case NUMBER:
		return "NUMBER"
	case AND:
		return "AND"
	case CLASS:
		return "CLASS"
	case ELSE:
		return "ELSE"
	case FALSE:
		return "FALSE"
	case FUN:
		return "FUN"
	case FOR:
		return "FOR"
	case IF:
		return "IF"
	case NIL:
		return "NIL"
	case OR:
		return "OR"
	case PRINT:
		return "PRINT"
	case RETURN:
		return "RETURN"
	case SUPER:
		return "SUPER"
	case THIS:
		return "THIS"
	case TRUE:
		return "TRUE"
	case VAR:
		return "VAR"
	case WHILE:
		return "WHILE"
	case EOF:
		return "EOF"
	}
	return ""
}
