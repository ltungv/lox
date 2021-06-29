package lox

import (
	"fmt"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParsePrimary(t *testing.T) {
	testCases := []struct {
		toks []*Token
		expr Expr
	}{
		{[]*Token{
			NewToken(NUMBER, "3.14", 3.14, 1),
			tokEOF(1),
		},
			NewLiteralExpr(3.14)},

		{[]*Token{
			NewToken(STRING, "\"a string\"", "a string", 1),
			tokEOF(1),
		},
			NewLiteralExpr("a string")},

		{[]*Token{
			NewToken(TRUE, "true", true, 1),
			tokEOF(1),
		},
			NewLiteralExpr(true)},

		{[]*Token{
			NewToken(FALSE, "false", false, 1),
			tokEOF(1),
		},
			NewLiteralExpr(false)},

		{[]*Token{
			NewToken(NIL, "nil", nil, 1),
			tokEOF(1),
		},
			NewLiteralExpr(nil)},

		{[]*Token{
			NewToken(LEFT_PAREN, "(", nil, 1),
			NewToken(NUMBER, "3.14", 3.14, 1),
			NewToken(RIGHT_PAREN, ")", nil, 1),
			tokEOF(1),
		},
			NewGroupingExpr(NewLiteralExpr(3.14))},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := NewMockReporter()
		parse := NewParser(tc.toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}

func TestParseUnary(t *testing.T) {
	testCases := []struct {
		toks []*Token
		expr Expr
	}{
		{[]*Token{
			NewToken(MINUS, "-", nil, 1),
			NewToken(NUMBER, "3.14", 3.14, 1),
			tokEOF(1),
		},
			NewUnaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewLiteralExpr(3.14)),
		},
		{[]*Token{
			NewToken(BANG, "!", nil, 1),
			NewToken(TRUE, "true", true, 1),
			tokEOF(1),
		},
			NewUnaryExpr(
				NewToken(BANG, "!", nil, 1),
				NewLiteralExpr(true)),
		},
		{[]*Token{
			NewToken(MINUS, "-", nil, 1),
			NewToken(MINUS, "-", nil, 1),
			NewToken(NUMBER, "3.14", 3.14, 1),
			tokEOF(1),
		},
			NewUnaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewUnaryExpr(
					NewToken(MINUS, "-", nil, 1),
					NewLiteralExpr(3.14))),
		},
		{[]*Token{
			NewToken(BANG, "!", nil, 1),
			NewToken(BANG, "!", nil, 1),
			NewToken(TRUE, "true", true, 1),
			tokEOF(1),
		},
			NewUnaryExpr(
				NewToken(BANG, "!", nil, 1),
				NewUnaryExpr(
					NewToken(BANG, "!", nil, 1),
					NewLiteralExpr(true))),
		},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := NewMockReporter()
		parse := NewParser(tc.toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}

func TestParseFactor(t *testing.T) {
	testCases := []struct {
		toks []*Token
		expr Expr
	}{
		{[]*Token{
			NewToken(NUMBER, "2", 2.0, 1),
			NewToken(STAR, "*", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(STAR, "*", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
		},
		{[]*Token{
			NewToken(NUMBER, "6", 6.0, 1),
			NewToken(SLASH, "/", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(SLASH, "/", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
		},
		{[]*Token{
			NewToken(NUMBER, "2", 2.0, 1),
			NewToken(STAR, "*", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			NewToken(SLASH, "/", nil, 1),
			NewToken(NUMBER, "4", 4.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(SLASH, "/", nil, 1),
				NewBinaryExpr(
					NewToken(STAR, "*", nil, 1),
					NewLiteralExpr(2.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(4.0)),
		},
		{[]*Token{
			NewToken(NUMBER, "6", 6.0, 1),
			NewToken(SLASH, "/", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			NewToken(STAR, "*", nil, 1),
			NewToken(NUMBER, "2", 2.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(STAR, "*", nil, 1),
				NewBinaryExpr(
					NewToken(SLASH, "/", nil, 1),
					NewLiteralExpr(6.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(2.0)),
		},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := NewMockReporter()
		parse := NewParser(tc.toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}

func TestParseTerm(t *testing.T) {
	testCases := []struct {
		toks []*Token
		expr Expr
	}{
		{[]*Token{
			NewToken(NUMBER, "2", 2.0, 1),
			NewToken(PLUS, "+", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(PLUS, "+", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
		},
		{[]*Token{
			NewToken(NUMBER, "6", 6.0, 1),
			NewToken(MINUS, "-", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
		},
		{[]*Token{
			NewToken(NUMBER, "2", 2.0, 1),
			NewToken(PLUS, "+", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			NewToken(MINUS, "-", nil, 1),
			NewToken(NUMBER, "4", 4.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewBinaryExpr(
					NewToken(PLUS, "+", nil, 1),
					NewLiteralExpr(2.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(4.0)),
		},
		{[]*Token{
			NewToken(NUMBER, "6", 6.0, 1),
			NewToken(MINUS, "-", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			NewToken(PLUS, "+", nil, 1),
			NewToken(NUMBER, "2", 2.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(PLUS, "+", nil, 1),
				NewBinaryExpr(
					NewToken(MINUS, "-", nil, 1),
					NewLiteralExpr(6.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(2.0)),
		},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := NewMockReporter()
		parse := NewParser(tc.toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}

func TestParseComparison(t *testing.T) {
	testCases := []struct {
		toks []*Token
		expr Expr
	}{
		{[]*Token{
			NewToken(NUMBER, "6", 6.0, 1),
			NewToken(GREATER, ">", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(GREATER, ">", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
		},
		{[]*Token{
			NewToken(NUMBER, "6", 6.0, 1),
			NewToken(GREATER_EQUAL, ">=", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(GREATER_EQUAL, ">=", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
		},
		{[]*Token{
			NewToken(NUMBER, "2", 2.0, 1),
			NewToken(LESS, "<", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(LESS, "<", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
		},
		{[]*Token{
			NewToken(NUMBER, "2", 2.0, 1),
			NewToken(LESS_EQUAL, "<=", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(LESS_EQUAL, "<=", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
		},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := NewMockReporter()
		parse := NewParser(tc.toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}

func TestParseEquality(t *testing.T) {
	testCases := []struct {
		toks []*Token
		expr Expr
	}{
		{[]*Token{
			NewToken(NUMBER, "2", 2.0, 1),
			NewToken(EQUAL_EQUAL, "==", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(EQUAL_EQUAL, "==", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
		},
		{[]*Token{
			NewToken(NUMBER, "6", 6.0, 1),
			NewToken(BANG_EQUAL, "!=", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(BANG_EQUAL, "!=", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
		},
		{[]*Token{
			NewToken(NUMBER, "2", 2.0, 1),
			NewToken(EQUAL_EQUAL, "==", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			NewToken(BANG_EQUAL, "!=", nil, 1),
			NewToken(TRUE, "true", true, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(BANG_EQUAL, "!=", nil, 1),
				NewBinaryExpr(
					NewToken(EQUAL_EQUAL, "==", nil, 1),
					NewLiteralExpr(2.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(true)),
		},
		{[]*Token{
			NewToken(NUMBER, "6", 6.0, 1),
			NewToken(BANG_EQUAL, "!=", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			NewToken(EQUAL_EQUAL, "==", nil, 1),
			NewToken(TRUE, "true", true, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(EQUAL_EQUAL, "==", nil, 1),
				NewBinaryExpr(
					NewToken(BANG_EQUAL, "!=", nil, 1),
					NewLiteralExpr(6.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(true)),
		},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := NewMockReporter()
		parse := NewParser(tc.toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}

func TestParseOpPrecedence(t *testing.T) {
	testCases := []struct {
		toks []*Token
		expr Expr
	}{
		{[]*Token{
			NewToken(NUMBER, "2", 2.0, 1),
			NewToken(STAR, "*", nil, 1),
			NewToken(MINUS, "-", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(STAR, "*", nil, 1),
				NewLiteralExpr(2.0),
				NewUnaryExpr(
					NewToken(MINUS, "-", nil, 1),
					NewLiteralExpr(3.0))),
		},
		{[]*Token{
			NewToken(NUMBER, "6", 6.0, 1),
			NewToken(MINUS, "-", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			NewToken(STAR, "*", nil, 1),
			NewToken(NUMBER, "2", 2.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewLiteralExpr(6.0),
				NewBinaryExpr(
					NewToken(STAR, "*", nil, 1),
					NewLiteralExpr(3.0),
					NewLiteralExpr(2.0))),
		},
		{[]*Token{
			NewToken(NUMBER, "2", 2.0, 1),
			NewToken(LESS, "<", nil, 1),
			NewToken(NUMBER, "6", 6.0, 1),
			NewToken(MINUS, "-", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(LESS, "<", nil, 1),
				NewLiteralExpr(2.0),
				NewBinaryExpr(
					NewToken(MINUS, "-", nil, 1),
					NewLiteralExpr(6.0),
					NewLiteralExpr(3.0))),
		},
		{[]*Token{
			NewToken(FALSE, "false", false, 1),
			NewToken(EQUAL_EQUAL, "==", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			NewToken(LESS, "<", nil, 1),
			NewToken(NUMBER, "2", 2.0, 1),
			tokEOF(1),
		},
			NewBinaryExpr(
				NewToken(EQUAL_EQUAL, "==", nil, 1),
				NewLiteralExpr(false),
				NewBinaryExpr(
					NewToken(LESS, "<", nil, 1),
					NewLiteralExpr(3.0),
					NewLiteralExpr(2.0))),
		},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := NewMockReporter()
		parse := NewParser(tc.toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}

func TestParseWithErrors(t *testing.T) {
	testCases := []struct {
		toks   []*Token
		errors []error
	}{
		{[]*Token{
			tokEOF(1),
		},
			[]error{NewParseError(tokEOF(1), "Expect expression.")}},
		{[]*Token{
			NewToken(LEFT_PAREN, "(", nil, 1),
			NewToken(NUMBER, "1", 1.0, 1),
			NewToken(STAR, "*", nil, 1),
			NewToken(LEFT_PAREN, "(", nil, 1),
			NewToken(NUMBER, "2", 2.0, 1),
			NewToken(PLUS, "+", nil, 1),
			NewToken(NUMBER, "3", 3.0, 1),
			NewToken(RIGHT_PAREN, ")", nil, 1),
			tokEOF(1),
		},
			[]error{NewParseError(tokEOF(1), "Expect ')' after expression.")}},
		{[]*Token{
			NewToken(STAR, "*", nil, 1),
			NewToken(NUMBER, "2", 2.0, 1),
			tokEOF(1),
		},
			[]error{
				NewParseError(
					NewToken(STAR, "*", nil, 1),
					"Unary '*' expressions are not supported.")}},
		{[]*Token{
			NewToken(LEFT_PAREN, "(", nil, 1),
			NewToken(SLASH, "/", nil, 1),
			NewToken(NUMBER, "2", 2.0, 1),
			NewToken(RIGHT_PAREN, ")", nil, 1),
			tokEOF(1),
		},
			[]error{
				NewParseError(
					NewToken(SLASH, "/", nil, 1),
					"Unary '/' expressions are not supported.")}},
		{[]*Token{
			NewToken(NUMBER, "1", 1.0, 1),
			NewToken(STAR, "*", nil, 1),
			NewToken(PLUS, "+", nil, 1),
			NewToken(NUMBER, "2", 2.0, 1),
			tokEOF(1),
		},
			[]error{
				NewParseError(
					NewToken(PLUS, "+", nil, 1),
					"Unary '+' expressions are not supported.")}},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		var out strings.Builder
		report := NewSimpleReporter(&out)
		parse := NewParser(tc.toks, report)
		expr := parse.Parse()

		var messages []string
		for _, e := range tc.errors {
			messages = append(messages, e.Error())
		}

		assert.Nil(expr)
		assert.Equal(fmt.Sprintf("%s\n", strings.Join(messages, "\n")), out.String())
		assert.True(report.HadError())
		assert.False(report.HadRuntimeError())
	}
}
