package lox

import "fmt"

type AstPrinter struct{}

func (printer *AstPrinter) Print(expr Expr) string {
	return fmt.Sprintf("%s", expr.Accept(printer))
}

func (printer *AstPrinter) VisitBinaryExpr(expr *BinaryExpr) interface{} {
	return fmt.Sprintf(
		"(%s %s %s)",
		expr.Op.Lexeme,
		expr.Left.Accept(printer),
		expr.Right.Accept(printer),
	)
}

func (printer *AstPrinter) VisitGroupingExpr(expr *GroupingExpr) interface{} {
	return fmt.Sprintf("(group %s)", expr.Expression.Accept(printer))
}

func (printer *AstPrinter) VisitLiteralExpr(expr *LiteralExpr) interface{} {
	if expr.Value == nil {
		return "nil"
	}
	return fmt.Sprintf("%v", expr.Value)
}

func (printer *AstPrinter) VisitUnaryExpr(expr *UnaryExpr) interface{} {
	return fmt.Sprintf("(%s %s)", expr.Op.Lexeme, expr.Expression.Accept(printer))
}
