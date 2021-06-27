package lox

import "fmt"

type ScanError struct {
	line    int
	message string
}

// NewScanError creates a new interpreter error
func NewScanError(line int, message string) error {
	return &ScanError{line, message}
}

func (err *ScanError) Error() string {
	return fmt.Sprintf(
		"[line %d] Error: %s",
		err.line,
		err.message,
	)
}

// ParserError contains an error message and the token at which the error occurs
type ParseError struct {
	token   *Token
	message string
}

// NewParseError creates a new parsing error
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
