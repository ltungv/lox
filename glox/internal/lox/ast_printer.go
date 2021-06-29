package lox

import (
	"fmt"
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
	return stringify(expr.Value), nil
}

func (printer *AstPrinter) VisitUnaryExpr(expr *UnaryExpr) (interface{}, error) {
	exprStr, _ := expr.Expression.Accept(printer)
	return fmt.Sprintf("(%s %s)", expr.Op.Lexeme, exprStr), nil
}
