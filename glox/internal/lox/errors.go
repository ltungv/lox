package lox

import "fmt"

type scanError struct {
	line    int
	message string
}

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

type compileError struct {
	token   *Token
	message string
}

func newCompileError(token *Token, message string) error {
	e := new(compileError)
	e.token = token
	e.message = message
	return e
}

func (err *compileError) Error() string {
	var loc string
	if err.token.Type == EOF {
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

type runtimeError struct {
	token   *Token
	message string
}

func newRuntimeError(token *Token, message string) error {
	e := new(runtimeError)
	e.token = token
	e.message = message
	return e
}

func (err *runtimeError) Error() string {
	var loc string
	if err.token.Type == EOF {
		loc = "end"
	} else {
		loc = "'" + err.token.Lexeme + "'"
	}

	return fmt.Sprintf(
		"[line %d] RuntimeError at %s: %s",
		err.token.Line,
		loc,
		err.message,
	)
}
