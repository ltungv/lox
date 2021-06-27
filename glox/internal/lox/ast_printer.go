package lox

import (
	"fmt"
	"strconv"
)

type AstPrinter struct{}

func (printer *AstPrinter) Print(expr Expr) string {
	s, _ := expr.Accept(printer)
	return fmt.Sprintf("%v", s)
}

func (printer *AstPrinter) VisitBinaryExpr(expr *BinaryExpr) (interface{}, error) {
	left, _ := expr.Left.Accept(printer)
	right, _ := expr.Right.Accept(printer)
	return fmt.Sprintf("(%s %s %s)", expr.Op.Lexeme, left, right), nil
}

func (printer *AstPrinter) VisitGroupingExpr(expr *GroupingExpr) (interface{}, error) {
	exprStr, _ := expr.Expression.Accept(printer)
	return fmt.Sprintf("(group %s)", exprStr), nil
}

func (printer *AstPrinter) VisitLiteralExpr(expr *LiteralExpr) (interface{}, error) {
	var s string
	switch v := expr.Value.(type) {
	case nil:
		s = fmt.Sprintln("nil")
	case float64:
		s = fmt.Sprintln(strconv.FormatFloat(v, 'f', -1, 64))
	default:
		s = fmt.Sprintf("%v\n", v)
	}
	return s, nil
}

func (printer *AstPrinter) VisitUnaryExpr(expr *UnaryExpr) (interface{}, error) {
	exprStr, _ := expr.Expression.Accept(printer)
	return fmt.Sprintf("(%s %s)", expr.Op.Lexeme, exprStr), nil
}
