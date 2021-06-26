package main

import (
	"fmt"
	"github.com/letung3105/lox/glox/internal/lox"
)

type AstPrinter struct{}

func (printer *AstPrinter) Print(expr lox.Expr) string {
	return fmt.Sprintf("%s", expr.Accept(printer))
}

func (printer *AstPrinter) VisitBinaryExpr(expr *lox.BinaryExpr) interface{} {
	return fmt.Sprintf(
		"(%s %s %s)",
		expr.Op.Lexeme,
		expr.Left.Accept(printer),
		expr.Right.Accept(printer),
	)
}

func (printer *AstPrinter) VisitGroupingExpr(expr *lox.GroupingExpr) interface{} {
	return fmt.Sprintf("(group %s)", expr.Expression.Accept(printer))
}

func (printer *AstPrinter) VisitLiteralExpr(expr *lox.LiteralExpr) interface{} {
	if expr.Value == nil {
		return "nil"
	}
	return fmt.Sprintf("%v", expr.Value)
}

func (printer *AstPrinter) VisitUnaryExpr(expr *lox.UnaryExpr) interface{} {
	return fmt.Sprintf("(%s %s)", expr.Op.Lexeme, expr.Expression.Accept(printer))
}

func main() {
	expression := lox.NewBinaryExpr(
		lox.NewToken(lox.STAR, "*", nil, 1),
		lox.NewUnaryExpr(
			lox.NewToken(lox.MINUS, "-", nil, 1),
			lox.NewLiteralExpr(123),
		),
		lox.NewGroupingExpr(lox.NewLiteralExpr(45.67)),
	)

	printer := AstPrinter{}
	fmt.Println(printer.Print(expression))
}
