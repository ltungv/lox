package lox

import "fmt"

// InterpreterError wrapped the error message returned by interpreter with
// additional information on where the error occured.
type GloxError struct {
	line    int
	message string
}

// NewGloxError creates a new interpreter error
func NewGloxError(line int, message string) error {
	return &GloxError{line, message}
}

func (err *GloxError) Error() string {
	return fmt.Sprintf(
		"[line %d] Error: %s",
		err.line,
		err.message,
	)
}

// InterpreterError wrapped the error message returned by interpreter with
// additional information on where the error occured.
type ParseError struct {
	token   *Token
	message string
}

// NewGloxError creates a new interpreter error
func NewParseError(token *Token, message string) error {
	return &ParseError{token, message}
}

func (err *ParseError) Error() string {
	if err.token.Typ == EOF {
		return fmt.Sprintf(
			"[line %d] Error at end: %s",
			err.token.Line,
			err.message,
		)
	}
	return fmt.Sprintf(
		"[line %d] Error at '%s': %s",
		err.token.Line,
		err.token.Lexeme,
		err.message,
	)
}
