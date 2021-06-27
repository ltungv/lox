package lox

import "fmt"

// ScanError indicates the lexical grammar of the source code is invalid
type ScanError struct {
	line    int
	message string
}

// NewScanError creates a new ScanError
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

// ParserError indicates the syntactic grammar of the source code is invalid
type ParseError struct {
	token   *Token
	message string
}

// NewParseError creates a new ParseError
func NewParseError(token *Token, message string) error {
	return &ParseError{token, message}
}

func (err *ParseError) Error() string {
	var loc string
	if err.token.Typ == EOF {
		loc = "end"
	} else {
		loc = "'" + err.token.Lexeme + "'"
	}

	return fmt.Sprintf(
		"[line %d] Error at %s: %s",
		err.token.Line,
		loc,
		err.message,
	)
}

// RuntimerError represents any error that the interpreter detects when running
type RuntimeError struct {
	token   *Token
	message string
}

// NewRuntimeError creates a new RuntimeError
func NewRuntimeError(token *Token, message string) error {
	return &RuntimeError{token, message}
}

func (err *RuntimeError) Error() string {
	var loc string
	if err.token.Typ == EOF {
		loc = "end"
	} else {
		loc = "'" + err.token.Lexeme + "'"
	}

	return fmt.Sprintf(
		"%s\n[line %d at %s]",
		err.message,
		err.token.Line,
		loc,
	)
}
