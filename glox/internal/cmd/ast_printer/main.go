package main

import (
	"fmt"
	"github.com/letung3105/lox/glox/internal/lox"
)

func main() {
	expression := lox.NewBinaryExpr(
		lox.NewToken(lox.STAR, "*", nil, 1),
		lox.NewUnaryExpr(
			lox.NewToken(lox.MINUS, "-", nil, 1),
			lox.NewLiteralExpr(123),
		),
		lox.NewGroupingExpr(lox.NewLiteralExpr(45.67)),
	)

	printer := lox.AstPrinter{}
	fmt.Println(printer.Print(expression))
}
