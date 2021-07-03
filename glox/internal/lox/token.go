package lox

import "fmt"

// loxToken represents group a characters with additional information that was
// obtained during the scanning phase.
type loxToken struct {
	typ     loxTokenType
	lexeme  string
	literal interface{}
	line    int
}

// New creates a new token
func newLoxToken(typ loxTokenType, lexeme string, literal interface{}, line int) *loxToken {
	t := new(loxToken)
	t.typ = typ
	t.lexeme = lexeme
	t.literal = literal
	t.line = line
	return t
}

func (t *loxToken) String() string {
	return fmt.Sprintf("%s %s %v", t.typ.String(), t.lexeme, t.literal)
}

var loxKeywords = map[string]loxTokenType{
	"and":    tokenAnd,
	"class":  tokenClass,
	"else":   tokenElse,
	"false":  tokenFalse,
	"fun":    tokenFun,
	"for":    tokenFor,
	"if":     tokenIf,
	"nil":    tokenNil,
	"or":     tokenOr,
	"print":  tokenPrint,
	"return": tokenReturn,
	"super":  tokenSuper,
	"this":   tokenThis,
	"true":   tokenTrue,
	"var":    tokenVar,
	"while":  tokenWhile,
	"eof":    tokenEOF,
}

const (
	// Single-character tokens
	tokenLeftParen loxTokenType = iota
	tokenRightParen
	tokenLeftBrace
	tokenRightBrace
	tokenComma
	tokenDot
	tokenMinus
	tokenPlus
	tokenSemicolon
	tokenSlash
	tokenStar

	// One or two chracter tokens
	tokenBang
	tokenBangEqual
	tokenEqual
	tokenEqualEqual
	tokenGreater
	tokenGreaterEqual
	tokenLess
	tokenLessEqual

	// Literals
	tokenIdentifier
	tokenString
	tokenNumber

	// Keywords
	tokenAnd
	tokenClass
	tokenElse
	tokenFalse
	tokenFun
	tokenFor
	tokenIf
	tokenNil
	tokenOr
	tokenPrint
	tokenReturn
	tokenSuper
	tokenThis
	tokenTrue
	tokenVar
	tokenWhile
	tokenEOF
)

/// loxTokenType is a just a wrapped string used to represent token's type
type loxTokenType uint

func (tt *loxTokenType) String() string {
	switch *tt {
	case tokenLeftParen:
		return "("
	case tokenRightParen:
		return ")"
	case tokenLeftBrace:
		return "{"
	case tokenRightBrace:
		return "}"
	case tokenComma:
		return ","
	case tokenDot:
		return "."
	case tokenMinus:
		return "-"
	case tokenPlus:
		return "+"
	case tokenSemicolon:
		return ";"
	case tokenSlash:
		return "/"
	case tokenStar:
		return "*"
	case tokenBang:
		return "!"
	case tokenBangEqual:
		return "!="
	case tokenEqual:
		return "="
	case tokenEqualEqual:
		return "=="
	case tokenGreater:
		return ">"
	case tokenGreaterEqual:
		return ">="
	case tokenLess:
		return "<"
	case tokenLessEqual:
		return "<="
	case tokenIdentifier:
		return "IDENTIFIER"
	case tokenString:
		return "STRING"
	case tokenNumber:
		return "NUMBER"
	case tokenAnd:
		return "AND"
	case tokenClass:
		return "CLASS"
	case tokenElse:
		return "ELSE"
	case tokenFalse:
		return "FALSE"
	case tokenFun:
		return "FUN"
	case tokenFor:
		return "FOR"
	case tokenIf:
		return "IF"
	case tokenNil:
		return "NIL"
	case tokenOr:
		return "OR"
	case tokenPrint:
		return "PRINT"
	case tokenReturn:
		return "RETURN"
	case tokenSuper:
		return "SUPER"
	case tokenThis:
		return "THIS"
	case tokenTrue:
		return "TRUE"
	case tokenVar:
		return "VAR"
	case tokenWhile:
		return "WHILE"
	case tokenEOF:
		return "EOF"
	}
	return ""
}
