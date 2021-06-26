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
	return fmt.Sprintf("%s %s %v", t.Typ, t.Lexeme, t.Literal)
}

/// TokenType is a just a wrapped string used to represent token's type
type TokenType string

const (
	// Single-character tokens
	LEFT_PAREN  TokenType = "("
	RIGHT_PAREN TokenType = ")"
	LEFT_BRACE  TokenType = "{"
	RIGHT_BRACE TokenType = "}"
	COMMA       TokenType = ","
	DOT         TokenType = "."
	MINUS       TokenType = "-"
	PLUS        TokenType = "+"
	SEMICOLON   TokenType = ";"
	SLASH       TokenType = "/"
	STAR        TokenType = "*"

	// One or two chracter tokens
	BANG          TokenType = "!"
	BANG_EQUAL    TokenType = "!="
	EQUAL         TokenType = "="
	EQUAL_EQUAL   TokenType = "=="
	GREATER       TokenType = ">"
	GREATER_EQUAL TokenType = ">="
	LESS          TokenType = "<"
	LESS_EQUAL    TokenType = "<="

	// Literals
	IDENTIFIER TokenType = "IDENTIFIER"
	STRING     TokenType = "STRING"
	NUMBER     TokenType = "NUMBER"

	// Keywords
	AND    TokenType = "AND"
	CLASS  TokenType = "CLASS"
	ELSE   TokenType = "ELSE"
	FALSE  TokenType = "FALSE"
	FUN    TokenType = "FUN"
	FOR    TokenType = "FOR"
	IF     TokenType = "IF"
	NIL    TokenType = "NIL"
	OR     TokenType = "OR"
	PRINT  TokenType = "PRINT"
	RETURN TokenType = "RETURN"
	SUPER  TokenType = "SUPER"
	THIS   TokenType = "THIS"
	TRUE   TokenType = "TRUE"
	VAR    TokenType = "VAR"
	WHILE  TokenType = "WHILE"
	EOF    TokenType = "EOF"
)

var Keywords = map[string]interface{}{
	"AND":    struct{}{},
	"CLASS":  struct{}{},
	"ELSE":   struct{}{},
	"FALSE":  struct{}{},
	"FUN":    struct{}{},
	"FOR":    struct{}{},
	"IF":     struct{}{},
	"NIL":    struct{}{},
	"OR":     struct{}{},
	"PRINT":  struct{}{},
	"RETURN": struct{}{},
	"SUPER":  struct{}{},
	"THIS":   struct{}{},
	"TRUE":   struct{}{},
	"VAR":    struct{}{},
	"WHILE":  struct{}{},
	"EOF":    struct{}{},
}
