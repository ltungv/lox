package lox

import "fmt"

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
	switch expr.Value.(type) {
	case nil:
		s = "nil"
	case float64:
		s = fmt.Sprintf("%f", expr.Value)
	default:
		s = fmt.Sprintf("%v", expr.Value)
	}
	return s, nil
}

func (printer *AstPrinter) VisitUnaryExpr(expr *UnaryExpr) (interface{}, error) {
	exprStr, _ := expr.Expression.Accept(printer)
	return fmt.Sprintf("(%s %s)", expr.Op.Lexeme, exprStr), nil
}
