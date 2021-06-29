package lox

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
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
		report := newMockReporter()
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
		report := newMockReporter()
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
		report := newMockReporter()
		scan := NewScanner([]rune(tc.src), report)
		toks := scan.Scan()

		assert.False(report.HadError())
		assert.Equal(tc.toks, toks)
	}
}

func TestScanValidTokensSequence(t *testing.T) {
	lexemes := []string{
		"(", ")", "{", "}", ",", ".", "-", "+", ";", "/", "*",
		"!", "!=", "=", "==", ">", ">=", "<", "<=",
		"v1", "_2v", "\"string\"", "\"also\nstring\"", "10", "3.14",
		"and", "class", "else", "false", "fun", "for", "if", "nil", "or",
		"print", "return", "super", "this", "true", "var", "while",
	}
	toksWant := []*Token{
		{LEFT_PAREN, "(", nil, 1},
		{RIGHT_PAREN, ")", nil, 1},
		{LEFT_BRACE, "{", nil, 1},
		{RIGHT_BRACE, "}", nil, 1},
		{COMMA, ",", nil, 1},
		{DOT, ".", nil, 1},
		{MINUS, "-", nil, 1},
		{PLUS, "+", nil, 1},
		{SEMICOLON, ";", nil, 1},
		{SLASH, "/", nil, 1},
		{STAR, "*", nil, 1},
		{BANG, "!", nil, 1},
		{BANG_EQUAL, "!=", nil, 1},
		{EQUAL, "=", nil, 1},
		{EQUAL_EQUAL, "==", nil, 1},
		{GREATER, ">", nil, 1},
		{GREATER_EQUAL, ">=", nil, 1},
		{LESS, "<", nil, 1},
		{LESS_EQUAL, "<=", nil, 1},
		{IDENTIFIER, "v1", nil, 1},
		{IDENTIFIER, "_2v", nil, 1},
		{STRING, "\"string\"", "string", 1},
		{STRING, "\"also\nstring\"", "also\nstring", 2},
		{NUMBER, "10", 10.0, 2},
		{NUMBER, "3.14", 3.14, 2},
		{AND, "and", nil, 2},
		{CLASS, "class", nil, 2},
		{ELSE, "else", nil, 2},
		{FALSE, "false", nil, 2},
		{FUN, "fun", nil, 2},
		{FOR, "for", nil, 2},
		{IF, "if", nil, 2},
		{NIL, "nil", nil, 2},
		{OR, "or", nil, 2},
		{PRINT, "print", nil, 2},
		{RETURN, "return", nil, 2},
		{SUPER, "super", nil, 2},
		{THIS, "this", nil, 2},
		{TRUE, "true", nil, 2},
		{VAR, "var", nil, 2},
		{WHILE, "while", nil, 2},
		tokEOF(2),
	}

	report := newMockReporter()
	scan := NewScanner([]rune(strings.Join(lexemes, " ")), report)
	toks := scan.Scan()

	assert := assert.New(t)
	assert.False(report.HadError())
	assert.Equal(toksWant, toks)
}

func TestScanWithErrors(t *testing.T) {
	testCases := []struct {
		src    string
		errors []error
		toks   []*Token
	}{
		{"\"yo where's the closing quote",
			[]error{NewScanError(1, "Unterminated string.")},
			[]*Token{tokEOF(1)}},

		{"\"yo\nwhere's\nthe\nclosing\nquote",
			[]error{NewScanError(5, "Unterminated string.")},
			[]*Token{tokEOF(5)}},

		{"/*yo where's the closing STAR-SLASH",
			[]error{NewScanError(1, "Unterminated multiline comment.")},
			[]*Token{tokEOF(1)}},

		{"/*yo\nwhere's\nthe\nclosing\nSTAR-SLASH",
			[]error{NewScanError(5, "Unterminated multiline comment.")},
			[]*Token{tokEOF(5)}},

		{"@ # $ % \"valid again\"",
			[]error{NewScanError(1, "Unexpected character."),
				NewScanError(1, "Unexpected character."),
				NewScanError(1, "Unexpected character."),
				NewScanError(1, "Unexpected character."),
			},
			[]*Token{{STRING, "\"valid again\"", "valid again", 1}, tokEOF(1)}},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := newMockReporter()
		scan := NewScanner([]rune(tc.src), report)
		toks := scan.Scan()

		assert.True(report.HadError())
		assert.Equal(tc.errors, report.errors)
		assert.Equal(tc.toks, toks)
	}
}
