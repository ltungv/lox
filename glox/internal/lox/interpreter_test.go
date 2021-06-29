package lox

import (
	// "fmt"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestInterpretLiteralExpr(t *testing.T) {
	testCases := []struct {
		expr Expr
		eval string
	}{
		{NewLiteralExpr(1.0), "1"},
		{NewLiteralExpr(3.14), "3.14"},
		{NewLiteralExpr(3.14000), "3.14"},
		{NewLiteralExpr(4294967296.0), "4294967296"},
		{NewLiteralExpr("hello"), "hello"},
		{NewLiteralExpr("hello\nworld"), "hello\nworld"},
		{NewLiteralExpr(true), "true"},
		{NewLiteralExpr(false), "false"},
		{NewLiteralExpr(nil), "nil"},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := newMockReporter()
		interpreter := NewInterpreter(tc.expr, report)
		var out strings.Builder
		interpreter.Interpret(&out)

		assert.False(report.HadError())
		assert.Equal(tc.eval, strings.TrimSpace(out.String()))
	}
}

func TestInterpretUnaryExpr(t *testing.T) {
	testCases := []struct {
		expr Expr
		eval string
	}{
		{
			NewUnaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewLiteralExpr(3.14)),
			"-3.14",
		},
		{
			NewUnaryExpr(
				NewToken(BANG, "!", nil, 1),
				NewLiteralExpr(true)),
			"false",
		},
		{
			NewUnaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewUnaryExpr(
					NewToken(MINUS, "-", nil, 1),
					NewLiteralExpr(3.14))),
			"3.14",
		},
		{
			NewUnaryExpr(
				NewToken(BANG, "!", nil, 1),
				NewUnaryExpr(
					NewToken(BANG, "!", nil, 1),
					NewLiteralExpr(true))),
			"true",
		},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := newMockReporter()
		interpreter := NewInterpreter(tc.expr, report)
		var out strings.Builder
		interpreter.Interpret(&out)

		assert.False(report.HadError())
		assert.Equal(tc.eval, strings.TrimSpace(out.String()))
	}
}

func TestInterpretBinaryExpr(t *testing.T) {
	testCases := []struct {
		expr Expr
		eval string
	}{
		// FACTOR
		{
			NewBinaryExpr(
				NewToken(STAR, "*", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
			"6",
		},
		{
			NewBinaryExpr(
				NewToken(SLASH, "/", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
			"2",
		},
		{
			NewBinaryExpr(
				NewToken(SLASH, "/", nil, 1),
				NewBinaryExpr(
					NewToken(STAR, "*", nil, 1),
					NewLiteralExpr(2.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(4.0)),
			"1.5",
		},
		{
			NewBinaryExpr(
				NewToken(STAR, "*", nil, 1),
				NewBinaryExpr(
					NewToken(SLASH, "/", nil, 1),
					NewLiteralExpr(6.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(2.0)),
			"4",
		},
		// TERM
		{
			NewBinaryExpr(
				NewToken(PLUS, "+", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
			"5",
		},
		{
			NewBinaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
			"3",
		},
		{
			NewBinaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewBinaryExpr(
					NewToken(PLUS, "+", nil, 1),
					NewLiteralExpr(2.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(4.0)),
			"1",
		},
		{
			NewBinaryExpr(
				NewToken(PLUS, "+", nil, 1),
				NewBinaryExpr(
					NewToken(MINUS, "-", nil, 1),
					NewLiteralExpr(6.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(2.0)),
			"5",
		},
		// COMPARISON
		{
			NewBinaryExpr(
				NewToken(GREATER, ">", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
			"true",
		},
		{
			NewBinaryExpr(
				NewToken(GREATER_EQUAL, ">=", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
			"true",
		},
		{
			NewBinaryExpr(
				NewToken(LESS, "<", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
			"true",
		},
		{
			NewBinaryExpr(
				NewToken(LESS_EQUAL, "<=", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
			"true",
		},
		// EQUALITY
		{
			NewBinaryExpr(
				NewToken(EQUAL_EQUAL, "==", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
			"false",
		},
		{
			NewBinaryExpr(
				NewToken(BANG_EQUAL, "!=", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
			"true",
		},
		{
			NewBinaryExpr(
				NewToken(EQUAL_EQUAL, "==", nil, 1),
				NewLiteralExpr("6"),
				NewLiteralExpr(3.0)),
			"false",
		},
		{
			NewBinaryExpr(
				NewToken(EQUAL_EQUAL, "==", nil, 1),
				NewLiteralExpr("6"),
				NewLiteralExpr(true)),
			"false",
		},
		{
			NewBinaryExpr(
				NewToken(BANG_EQUAL, "==", nil, 1),
				NewLiteralExpr(3.0),
				NewLiteralExpr(true)),
			"true",
		},
		{
			NewBinaryExpr(
				NewToken(BANG_EQUAL, "!=", nil, 1),
				NewBinaryExpr(
					NewToken(EQUAL_EQUAL, "==", nil, 1),
					NewLiteralExpr(2.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(true)),
			"true",
		},
		{
			NewBinaryExpr(
				NewToken(EQUAL_EQUAL, "==", nil, 1),
				NewBinaryExpr(
					NewToken(BANG_EQUAL, "!=", nil, 1),
					NewLiteralExpr(6.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(true)),
			"true",
		},
		// COMBINE EXPRs WITH DIFFERENT PRECEDENCE
		{
			NewBinaryExpr(
				NewToken(STAR, "*", nil, 1),
				NewLiteralExpr(2.0),
				NewUnaryExpr(
					NewToken(MINUS, "-", nil, 1),
					NewLiteralExpr(3.0))),
			"-6",
		},
		{
			NewBinaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewLiteralExpr(6.0),
				NewBinaryExpr(
					NewToken(STAR, "*", nil, 1),
					NewLiteralExpr(3.0),
					NewLiteralExpr(2.0))),
			"0",
		},
		{
			NewBinaryExpr(
				NewToken(LESS, "<", nil, 1),
				NewLiteralExpr(2.0),
				NewBinaryExpr(
					NewToken(MINUS, "-", nil, 1),
					NewLiteralExpr(6.0),
					NewLiteralExpr(3.0))),
			"true",
		},
		{
			NewBinaryExpr(
				NewToken(EQUAL_EQUAL, "==", nil, 1),
				NewLiteralExpr(false),
				NewBinaryExpr(
					NewToken(LESS, "<", nil, 1),
					NewLiteralExpr(3.0),
					NewLiteralExpr(2.0))),
			"true",
		},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := newMockReporter()
		interpreter := NewInterpreter(tc.expr, report)
		var out strings.Builder
		interpreter.Interpret(&out)

		assert.False(report.HadError())
		assert.Equal(tc.eval, strings.TrimSpace(out.String()))
	}
}

func TestInterpretGroupingExpr(t *testing.T) {
	testCases := []struct {
		expr Expr
		eval string
	}{
		{NewGroupingExpr(NewLiteralExpr(3.14)), "3.14"},
		{
			NewGroupingExpr(
				NewUnaryExpr(NewToken(MINUS, "-", nil, 1),
					NewLiteralExpr(3.14))),
			"-3.14",
		},
		{
			NewGroupingExpr(
				NewBinaryExpr(
					NewToken(STAR, "*", nil, 1),
					NewLiteralExpr(3.0),
					NewLiteralExpr(2.0))),
			"6",
		},
		{
			NewBinaryExpr(
				NewToken(STAR, "*", nil, 1),
				NewLiteralExpr(3.0),
				NewGroupingExpr(
					NewBinaryExpr(
						NewToken(PLUS, "+", nil, 1),
						NewLiteralExpr(2.0),
						NewLiteralExpr(2.0)))),
			"12",
		},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := newMockReporter()
		interpreter := NewInterpreter(tc.expr, report)
		var out strings.Builder
		interpreter.Interpret(&out)

		assert.False(report.HadError())
		assert.Equal(tc.eval, strings.TrimSpace(out.String()))
	}
}

func TestInterpretWithErrors(t *testing.T) {
	testCases := []struct {
		expr   Expr
		errors []error
	}{
		{
			NewBinaryExpr(
				NewToken(GREATER, ">", nil, 1),
				NewLiteralExpr("6"),
				NewLiteralExpr(3.0)),
			[]error{NewRuntimeError(
				NewToken(GREATER, ">", nil, 1), "Operand must be numbers.",
			)},
		},
		{
			NewBinaryExpr(
				NewToken(LESS, "<", nil, 1),
				NewLiteralExpr(3.0),
				NewLiteralExpr("6")),
			[]error{NewRuntimeError(
				NewToken(LESS, "<", nil, 1), "Operand must be numbers.",
			)},
		},
		{
			NewBinaryExpr(
				NewToken(GREATER_EQUAL, ">=", nil, 1),
				NewLiteralExpr("6"),
				NewLiteralExpr(3.0)),
			[]error{NewRuntimeError(
				NewToken(GREATER_EQUAL, ">=", nil, 1), "Operand must be numbers.",
			)},
		},
		{
			NewBinaryExpr(
				NewToken(LESS_EQUAL, "<=", nil, 1),
				NewLiteralExpr(3.0),
				NewLiteralExpr("6")),
			[]error{NewRuntimeError(
				NewToken(LESS_EQUAL, "<=", nil, 1), "Operand must be numbers.",
			)},
		},
		{
			NewBinaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewLiteralExpr(false),
				NewLiteralExpr(3.0)),
			[]error{NewRuntimeError(
				NewToken(MINUS, "-", nil, 1), "Operand must be numbers.",
			)},
		},
		{
			NewBinaryExpr(
				NewToken(PLUS, "+", nil, 1),
				NewLiteralExpr(true),
				NewLiteralExpr("6")),
			[]error{NewRuntimeError(
				NewToken(PLUS, "+", nil, 1), "Operands must be two numbers or two strings.",
			)},
		},
		{
			NewBinaryExpr(
				NewToken(SLASH, "-", nil, 1),
				NewLiteralExpr(false),
				NewLiteralExpr(true)),
			[]error{NewRuntimeError(
				NewToken(SLASH, "-", nil, 1), "Operand must be numbers.",
			)},
		},
		{
			NewBinaryExpr(
				NewToken(STAR, "*", nil, 1),
				NewLiteralExpr("3"),
				NewLiteralExpr("6")),
			[]error{NewRuntimeError(
				NewToken(STAR, "*", nil, 1), "Operand must be numbers.",
			)},
		},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := newMockReporter()
		interpreter := NewInterpreter(tc.expr, report)
		var interpretOut strings.Builder
		interpreter.Interpret(&interpretOut)

		var expectedReports []string
		for _, e := range tc.errors {
			expectedReports = append(expectedReports, e.Error())
		}

		var actualReports []string
		for _, e := range report.errors {
			actualReports = append(actualReports, e.Error())
		}

		assert.Empty(interpretOut)
		assert.Equal(expectedReports, actualReports)
		assert.False(report.HadError())
		assert.True(report.HadRuntimeError())
	}
}
