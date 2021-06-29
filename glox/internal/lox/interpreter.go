package lox

import (
	"fmt"
	"io"
)

// Interpreter exposes methods for evaluating then given Lox syntax tree. This
// struct implements ExprVisitor
type Interpreter struct {
	expr     Expr
	reporter Reporter
}

func NewInterpreter(expr Expr, reporter Reporter) *Interpreter {
	return &Interpreter{expr, reporter}
}

func (in *Interpreter) Interpret(w io.Writer) {
	eval, err := in.eval(in.expr)
	if err != nil {
		in.reporter.Report(err)
	}
	fmt.Fprintln(w, stringify(eval))
}

func (in *Interpreter) VisitBinaryExpr(expr *BinaryExpr) (interface{}, error) {
	leftEval, err := in.eval(expr.Left)
	if err != nil {
		return nil, err
	}
	rightEval, err := in.eval(expr.Right)
	if err != nil {
		return nil, err
	}

	switch expr.Op.Typ {
	case BANG_EQUAL:
		result := leftEval != rightEval
		return result, nil

	case EQUAL_EQUAL:
		result := leftEval == rightEval
		return result, nil

	case GREATER:
		leftNum, okLeftNum := leftEval.(float64)
		rightNum, okRightNum := rightEval.(float64)
		if okLeftNum && okRightNum {
			result := leftNum > rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")

	case GREATER_EQUAL:
		leftNum, okLeftNum := leftEval.(float64)
		rightNum, okRightNum := rightEval.(float64)
		if okLeftNum && okRightNum {
			result := leftNum >= rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")

	case LESS:
		leftNum, okLeftNum := leftEval.(float64)
		rightNum, okRightNum := rightEval.(float64)
		if okLeftNum && okRightNum {
			result := leftNum < rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")

	case LESS_EQUAL:
		leftNum, okLeftNum := leftEval.(float64)
		rightNum, okRightNum := rightEval.(float64)
		if okLeftNum && okRightNum {
			result := leftNum <= rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")

	case MINUS:
		leftNum, okLeftNum := leftEval.(float64)
		rightNum, okRightNum := rightEval.(float64)
		if okLeftNum && okRightNum {
			result := leftNum - rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")

	case PLUS:
		leftStr, okLeftStr := leftEval.(string)
		rightStr, okRightStr := rightEval.(string)
		if okLeftStr && okRightStr {
			result := leftStr + rightStr
			return result, nil
		}
		leftNum, okLeftNum := leftEval.(float64)
		rightNum, okRightNum := rightEval.(float64)
		if okLeftNum && okRightNum {
			result := leftNum + rightNum
			return result, nil
		}

		return nil, NewRuntimeError(expr.Op, "Operands must be two numbers or two strings.")

	case SLASH:
		leftNum, okLeftNum := leftEval.(float64)
		rightNum, okRightNum := rightEval.(float64)
		if okLeftNum && okRightNum {
			result := leftNum / rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")

	case STAR:
		leftNum, okLeftNum := leftEval.(float64)
		rightNum, okRightNum := rightEval.(float64)
		if okLeftNum && okRightNum {
			result := leftNum * rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")
	}
	panic("Unreachable")
}

func (in *Interpreter) VisitGroupingExpr(expr *GroupingExpr) (interface{}, error) {
	return in.eval(expr.Expression)
}

func (in *Interpreter) VisitLiteralExpr(expr *LiteralExpr) (interface{}, error) {
	return expr.Value, nil
}

func (in *Interpreter) VisitUnaryExpr(expr *UnaryExpr) (interface{}, error) {
	exprEval, err := in.eval(expr.Expression)
	if err != nil {
		return nil, err
	}

	switch expr.Op.Typ {
	case BANG:
		return !isTruthy(exprEval), nil
	case MINUS:
		if exprNum, ok := exprEval.(float64); ok {
			return -exprNum, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be a number.")
	}
	panic("Unreachable")
}

func (in *Interpreter) eval(expr Expr) (interface{}, error) {
	return expr.Accept(in)
}
