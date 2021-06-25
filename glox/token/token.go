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
	IDENTIFIER Type = "identifier"
	STRING     Type = "string"
	NUMBER     Type = "number"

	// Keywords
	AND    Type = "and"
	CLASS  Type = "class"
	ELSE   Type = "else"
	FALSE  Type = "false"
	FUN    Type = "fun"
	FOR    Type = "for"
	IF     Type = "if"
	NIL    Type = "nil"
	OR     Type = "or"
	PRINT  Type = "print"
	RETURN Type = "return"
	SUPER  Type = "super"
	THIS   Type = "this"
	TRUE   Type = "true"
	VAR    Type = "var"
	WHILE  Type = "while"
	EOF    Type = "eof"
)

var Keywords = map[string]interface{}{
	"and":    struct{}{},
	"class":  struct{}{},
	"else":   struct{}{},
	"false":  struct{}{},
	"fun":    struct{}{},
	"for":    struct{}{},
	"if":     struct{}{},
	"nil":    struct{}{},
	"or":     struct{}{},
	"print":  struct{}{},
	"return": struct{}{},
	"super":  struct{}{},
	"this":   struct{}{},
	"true":   struct{}{},
	"var":    struct{}{},
	"while":  struct{}{},
	"eof":    struct{}{},
}
