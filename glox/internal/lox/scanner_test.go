package lox

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestScanSingleToken(t *testing.T) {
	testCases := []struct {
		src  string
		toks []*Token
	}{
		// single character token
		{"(", []*Token{{LEFT_PAREN, "(", nil, 1}, tokEOF(1)}},
		{")", []*Token{{RIGHT_PAREN, ")", nil, 1}, tokEOF(1)}},
		{"{", []*Token{{LEFT_BRACE, "{", nil, 1}, tokEOF(1)}},
		{"}", []*Token{{RIGHT_BRACE, "}", nil, 1}, tokEOF(1)}},
		{",", []*Token{{COMMA, ",", nil, 1}, tokEOF(1)}},
		{".", []*Token{{DOT, ".", nil, 1}, tokEOF(1)}},
		{"-", []*Token{{MINUS, "-", nil, 1}, tokEOF(1)}},
		{"+", []*Token{{PLUS, "+", nil, 1}, tokEOF(1)}},
		{";", []*Token{{SEMICOLON, ";", nil, 1}, tokEOF(1)}},
		{"/", []*Token{{SLASH, "/", nil, 1}, tokEOF(1)}},
		{"*", []*Token{{STAR, "*", nil, 1}, tokEOF(1)}},
		// single-/double-character token
		{"!", []*Token{{BANG, "!", nil, 1}, tokEOF(1)}},
		{"!=", []*Token{{BANG_EQUAL, "!=", nil, 1}, tokEOF(1)}},
		{"=", []*Token{{EQUAL, "=", nil, 1}, tokEOF(1)}},
		{"==", []*Token{{EQUAL_EQUAL, "==", nil, 1}, tokEOF(1)}},
		{">", []*Token{{GREATER, ">", nil, 1}, tokEOF(1)}},
		{">=", []*Token{{GREATER_EQUAL, ">=", nil, 1}, tokEOF(1)}},
		{"<", []*Token{{LESS, "<", nil, 1}, tokEOF(1)}},
		{"<=", []*Token{{LESS_EQUAL, "<=", nil, 1}, tokEOF(1)}},
		// literals
		{"a", []*Token{{IDENTIFIER, "a", nil, 1}, tokEOF(1)}},
		{"abc", []*Token{{IDENTIFIER, "abc", nil, 1}, tokEOF(1)}},
		{"abc123", []*Token{{IDENTIFIER, "abc123", nil, 1}, tokEOF(1)}},
		{"a1b2c3", []*Token{{IDENTIFIER, "a1b2c3", nil, 1}, tokEOF(1)}},
		{"_abc123", []*Token{{IDENTIFIER, "_abc123", nil, 1}, tokEOF(1)}},
		{"_123abc", []*Token{{IDENTIFIER, "_123abc", nil, 1}, tokEOF(1)}},
		{"\"\"", []*Token{{STRING, "\"\"", "", 1}, tokEOF(1)}},
		{"\"123\"", []*Token{{STRING, "\"123\"", "123", 1}, tokEOF(1)}},
		{"\"abc\n123\"", []*Token{{STRING, "\"abc\n123\"", "abc\n123", 2}, tokEOF(2)}},
		{"\"abc \t\r\n 123\"", []*Token{{STRING, "\"abc \t\r\n 123\"", "abc \t\r\n 123", 2}, tokEOF(2)}},
		{"10", []*Token{{NUMBER, "10", 10.0, 1}, tokEOF(1)}},
		{"01", []*Token{{NUMBER, "01", 1.0, 1}, tokEOF(1)}},
		{"100", []*Token{{NUMBER, "100", 100.0, 1}, tokEOF(1)}},
		{"001", []*Token{{NUMBER, "001", 1.0, 1}, tokEOF(1)}},
		{"0.1", []*Token{{NUMBER, "0.1", 0.1, 1}, tokEOF(1)}},
		{"1.0", []*Token{{NUMBER, "1.0", 1.0, 1}, tokEOF(1)}},
		{"123.456", []*Token{{NUMBER, "123.456", 123.456, 1}, tokEOF(1)}},
		{"789.000", []*Token{{NUMBER, "789.000", 789.0, 1}, tokEOF(1)}},
		{"000.789", []*Token{{NUMBER, "000.789", 0.789, 1}, tokEOF(1)}},
		// keywords
		{"and", []*Token{{AND, "and", nil, 1}, tokEOF(1)}},
		{"class", []*Token{{CLASS, "class", nil, 1}, tokEOF(1)}},
		{"else", []*Token{{ELSE, "else", nil, 1}, tokEOF(1)}},
		{"false", []*Token{{FALSE, "false", nil, 1}, tokEOF(1)}},
		{"fun", []*Token{{FUN, "fun", nil, 1}, tokEOF(1)}},
		{"for", []*Token{{FOR, "for", nil, 1}, tokEOF(1)}},
		{"if", []*Token{{IF, "if", nil, 1}, tokEOF(1)}},
		{"nil", []*Token{{NIL, "nil", nil, 1}, tokEOF(1)}},
		{"or", []*Token{{OR, "or", nil, 1}, tokEOF(1)}},
		{"print", []*Token{{PRINT, "print", nil, 1}, tokEOF(1)}},
		{"return", []*Token{{RETURN, "return", nil, 1}, tokEOF(1)}},
		{"super", []*Token{{SUPER, "super", nil, 1}, tokEOF(1)}},
		{"this", []*Token{{THIS, "this", nil, 1}, tokEOF(1)}},
		{"true", []*Token{{TRUE, "true", nil, 1}, tokEOF(1)}},
		{"var", []*Token{{VAR, "var", nil, 1}, tokEOF(1)}},
		{"while", []*Token{{WHILE, "while", nil, 1}, tokEOF(1)}},
		{"", []*Token{tokEOF(1)}},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := NewMockReporter()
		scan := NewScanner([]rune(tc.src), report)
		toks := scan.Scan()

		assert.False(report.HadError())
		assert.Equal(tc.toks, toks)
	}
}

func TestScanWhiteSpaces(t *testing.T) {
	testCases := []struct {
		src  string
		toks []*Token
	}{
		{"        ", []*Token{tokEOF(1)}},
		{"\r\r\r\r", []*Token{tokEOF(1)}},
		{"\t\t\t\t", []*Token{tokEOF(1)}},
		{"\n\n\n\n", []*Token{tokEOF(5)}},
		{"  \r\t\n", []*Token{tokEOF(2)}},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := NewMockReporter()
		scan := NewScanner([]rune(tc.src), report)
		toks := scan.Scan()

		assert.False(report.HadError())
		assert.Equal(tc.toks, toks)
	}
}

func TestScanComments(t *testing.T) {
	testCases := []struct {
		src  string
		toks []*Token
	}{
		{"// a single-line comment", []*Token{tokEOF(1)}},
		{"/*\na\nsingle-line\ncomment\n*/", []*Token{tokEOF(5)}},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := NewMockReporter()
		scan := NewScanner([]rune(tc.src), report)
		toks := scan.Scan()

		assert.False(report.HadError())
		assert.Equal(tc.toks, toks)
	}
}

type MockReporter struct {
	hadErr        bool
	hadRuntimeErr bool
}

func NewMockReporter() Reporter {
	return &MockReporter{false, false}
}

func (reporter *MockReporter) Report(err error) {
	if _, isRuntimeErr := err.(*RuntimeError); isRuntimeErr {
		reporter.hadRuntimeErr = true
	} else {
		reporter.hadErr = true
	}
}

func (reporter *MockReporter) Reset() {
	reporter.hadErr = false
	reporter.hadRuntimeErr = false
}

func (reporter *MockReporter) HadError() bool {
	return reporter.hadErr
}

func (reporter *MockReporter) HadRuntimeError() bool {
	return reporter.hadRuntimeErr
}

func tokEOF(line int) *Token {
	return NewToken(EOF, "", nil, line)
}
