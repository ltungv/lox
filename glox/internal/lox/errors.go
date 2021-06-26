package lox

import "fmt"

// InterpreterError wrapped the error message returned by interpreter with
// additional information on where the error occured.
type GloxError struct {
	line    int
	where   string
	message string
}

// NewGloxError creates a new interpreter error
func NewGloxError(line int, where string, message string) error {
	return &GloxError{line, where, message}
}

func (err *GloxError) Error() string {
	return fmt.Sprintf(
		"[line %d] Error%s: %s",
		err.line,
		err.where,
		err.message,
	)
}
