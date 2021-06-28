package lox

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParsePrimary(t *testing.T) {
	testCases := []struct {
		src  string
		expr Expr
	}{
		{"3.14", NewLiteralExpr(3.14)},
		{"\"a string\"", NewLiteralExpr("a string")},
		{"true", NewLiteralExpr(true)},
		{"false", NewLiteralExpr(false)},
		{"nil", NewLiteralExpr(nil)},
		{"(3.14)", NewGroupingExpr(NewLiteralExpr(3.14))},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := NewMockReporter()
		scan := NewScanner([]rune(tc.src), report)
		toks := scan.Scan()
		parse := NewParser(toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}

func TestParseUnary(t *testing.T) {
	testCases := []struct {
		src  string
		expr Expr
	}{
		{"-3.14",
			NewUnaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewLiteralExpr(3.14)),
		},
		{"!true",
			NewUnaryExpr(
				NewToken(BANG, "!", nil, 1),
				NewLiteralExpr(true)),
		},
		{"--3.14",
			NewUnaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewUnaryExpr(
					NewToken(MINUS, "-", nil, 1),
					NewLiteralExpr(3.14))),
		},
		{"!!true",
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
		scan := NewScanner([]rune(tc.src), report)
		toks := scan.Scan()
		parse := NewParser(toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}

func TestParseFactor(t *testing.T) {
	testCases := []struct {
		src  string
		expr Expr
	}{
		{"2 * 3",
			NewBinaryExpr(
				NewToken(STAR, "*", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
		},
		{"6 / 3",
			NewBinaryExpr(
				NewToken(SLASH, "/", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
		},
		{"2 * 3 / 4",
			NewBinaryExpr(
				NewToken(SLASH, "/", nil, 1),
				NewBinaryExpr(
					NewToken(STAR, "*", nil, 1),
					NewLiteralExpr(2.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(4.0)),
		},
		{"6 / 3 * 2",
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
		scan := NewScanner([]rune(tc.src), report)
		toks := scan.Scan()
		parse := NewParser(toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}

func TestParseTerm(t *testing.T) {
	testCases := []struct {
		src  string
		expr Expr
	}{
		{"2 + 3",
			NewBinaryExpr(
				NewToken(PLUS, "+", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
		},
		{"6 - 3",
			NewBinaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
		},
		{"2 + 3 - 4",
			NewBinaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewBinaryExpr(
					NewToken(PLUS, "+", nil, 1),
					NewLiteralExpr(2.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(4.0)),
		},
		{"6 - 3 + 2",
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
		scan := NewScanner([]rune(tc.src), report)
		toks := scan.Scan()
		parse := NewParser(toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}

func TestParseComparison(t *testing.T) {
	testCases := []struct {
		src  string
		expr Expr
	}{
		{"6 > 3",
			NewBinaryExpr(
				NewToken(GREATER, ">", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
		},
		{"6 >= 3",
			NewBinaryExpr(
				NewToken(GREATER_EQUAL, ">=", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
		},
		{"2 < 3",
			NewBinaryExpr(
				NewToken(LESS, "<", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
		},
		{"2 <= 3",
			NewBinaryExpr(
				NewToken(LESS_EQUAL, "<=", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
		},
	}

	assert := assert.New(t)
	for _, tc := range testCases {
		report := NewMockReporter()
		scan := NewScanner([]rune(tc.src), report)
		toks := scan.Scan()
		parse := NewParser(toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}

func TestParseEquality(t *testing.T) {
	testCases := []struct {
		src  string
		expr Expr
	}{
		{"2 == 3",
			NewBinaryExpr(
				NewToken(EQUAL_EQUAL, "==", nil, 1),
				NewLiteralExpr(2.0),
				NewLiteralExpr(3.0)),
		},
		{"6 != 3",
			NewBinaryExpr(
				NewToken(BANG_EQUAL, "!=", nil, 1),
				NewLiteralExpr(6.0),
				NewLiteralExpr(3.0)),
		},
		{"2 == 3 != true",
			NewBinaryExpr(
				NewToken(BANG_EQUAL, "!=", nil, 1),
				NewBinaryExpr(
					NewToken(EQUAL_EQUAL, "==", nil, 1),
					NewLiteralExpr(2.0),
					NewLiteralExpr(3.0)),
				NewLiteralExpr(true)),
		},
		{"6 != 3 == true",
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
		scan := NewScanner([]rune(tc.src), report)
		toks := scan.Scan()
		parse := NewParser(toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}

func TestParseOpPrecedence(t *testing.T) {
	testCases := []struct {
		src  string
		expr Expr
	}{
		{"2 * -3",
			NewBinaryExpr(
				NewToken(STAR, "*", nil, 1),
				NewLiteralExpr(2.0),
				NewUnaryExpr(
					NewToken(MINUS, "-", nil, 1),
					NewLiteralExpr(3.0))),
		},
		{"6 - 3 * 2",
			NewBinaryExpr(
				NewToken(MINUS, "-", nil, 1),
				NewLiteralExpr(6.0),
				NewBinaryExpr(
					NewToken(STAR, "*", nil, 1),
					NewLiteralExpr(3.0),
					NewLiteralExpr(2.0))),
		},
		{"2 < 6 - 3",
			NewBinaryExpr(
				NewToken(LESS, "<", nil, 1),
				NewLiteralExpr(2.0),
				NewBinaryExpr(
					NewToken(MINUS, "-", nil, 1),
					NewLiteralExpr(6.0),
					NewLiteralExpr(3.0))),
		},
		{"false == 3 < 2",
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
		scan := NewScanner([]rune(tc.src), report)
		toks := scan.Scan()
		parse := NewParser(toks, report)
		expr := parse.Parse()

		assert.False(report.HadError())
		assert.Equal(tc.expr, expr)
	}
}