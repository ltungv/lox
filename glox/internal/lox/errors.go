package lox

import "fmt"

// scanError indicates the lexical grammar of the source code is invalid
type scanError struct {
	line    int
	message string
}

// newScanError creates a new ScanError
func newScanError(line int, message string) error {
	e := new(scanError)
	e.line = line
	e.message = message
	return e
}

func (err *scanError) Error() string {
	return fmt.Sprintf(
		"[line %d] Error: %s",
		err.line,
		err.message,
	)
}

// ParserError indicates the syntactic grammar of the source code is invalid
type parseError struct {
	token   *loxToken
	message string
}

// newParseError creates a new ParseError
func newParseError(token *loxToken, message string) error {
	e := new(parseError)
	e.token = token
	e.message = message
	return e
}

func (err *parseError) Error() string {
	var loc string
	if err.token.typ == tokenEOF {
		loc = "end"
	} else {
		loc = "'" + err.token.lexeme + "'"
	}

	return fmt.Sprintf(
		"[line %d] Error at %s: %s",
		err.token.line,
		loc,
		err.message,
	)
}

// runtimeError represents any error that the interpreter detects when running
type runtimeError struct {
	token   *loxToken
	message string
}

// newRuntimeError creates a new RuntimeError
func newRuntimeError(token *loxToken, message string) error {
	e := new(runtimeError)
	e.token = token
	e.message = message
	return e
}

func (err *runtimeError) Error() string {
	var loc string
	if err.token.typ == tokenEOF {
		loc = "end"
	} else {
		loc = "'" + err.token.lexeme + "'"
	}

	return fmt.Sprintf(
		"[line %d] RuntimeError at %s: %s",
		err.token.line,
		loc,
		err.message,
	)
}

// ResolveError represents any error that the interpreter detects when resolving
// the syntax tree
type resolveError struct {
	token   *loxToken
	message string
}

// newResolveError creates a new ResolveError
func newResolveError(token *loxToken, message string) error {
	e := new(resolveError)
	e.token = token
	e.message = message
	return e
}

func (err *resolveError) Error() string {
	var loc string
	if err.token.typ == tokenEOF {
		loc = "end"
	} else {
		loc = "'" + err.token.lexeme + "'"
	}

	return fmt.Sprintf(
		"[line %d] ResolveError at %s: %s",
		err.token.line,
		loc,
		err.message,
	)
}
