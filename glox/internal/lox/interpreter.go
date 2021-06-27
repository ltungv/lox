package lox

import "fmt"

// Interpreter exposes methods for evaluating then given Lox syntax tree. This
// struct implements ExprVisitor
type Interpreter struct {
	expr     Expr
	reporter Reporter
}

func NewInterpreter(expr Expr, reporter Reporter) *Interpreter {
	return &Interpreter{expr, reporter}
}

func (in *Interpreter) Interpret() {
	printer := AstPrinter{}
	fmt.Println(printer.Print(in.expr))
}
