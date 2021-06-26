package token

import "fmt"

// Token represents group a characters with additional information that was
// obtained during the scanning phase.
type Token struct {
	typ     Type
	lexeme  string
	literal interface{}
	line    int
}

// New creates a new token
func New(typ Type, lexeme string, literal interface{}, line int) *Token {
	return &Token{typ, lexeme, literal, line}
}

func (t *Token) String() string {
	return fmt.Sprintf("%s %s %v", t.typ, t.lexeme, t.literal)
}

/// Type is a just a wrapped string used to represent token's type
type Type string

const (
	// Single-character tokens
	LEFT_PAREN  Type = "("
	RIGHT_PAREN Type = ")"
	LEFT_BRACE  Type = "{"
	RIGHT_BRACE Type = "}"
	COMMA       Type = ","
	DOT         Type = "."
	MINUS       Type = "-"
	PLUS        Type = "+"
	SEMICOLON   Type = ";"
	SLASH       Type = "/"
	STAR        Type = "*"

	// One or two chracter tokens
	BANG          Type = "!"
	BANG_EQUAL    Type = "!="
	EQUAL         Type = "="
	EQUAL_EQUAL   Type = "=="
	GREATER       Type = ">"
	GREATER_EQUAL Type = ">="
	LESS          Type = "<"
	LESS_EQUAL    Type = "<="

	// Literals
	IDENTIFIER Type = "IDENTIFIER"
	STRING     Type = "STRING"
	NUMBER     Type = "NUMBER"

	// Keywords
	AND    Type = "AND"
	CLASS  Type = "CLASS"
	ELSE   Type = "ELSE"
	FALSE  Type = "FALSE"
	FUN    Type = "FUN"
	FOR    Type = "FOR"
	IF     Type = "IF"
	NIL    Type = "NIL"
	OR     Type = "OR"
	PRINT  Type = "PRINT"
	RETURN Type = "RETURN"
	SUPER  Type = "SUPER"
	THIS   Type = "THIS"
	TRUE   Type = "TRUE"
	VAR    Type = "VAR"
	WHILE  Type = "WHILE"
	EOF    Type = "EOF"
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
