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
	eval, err := in.eval(in.expr)
	if err != nil {
		in.reporter.Report(err)
		return
	}
	switch eval.(type) {
	case nil:
		fmt.Println("nil")
	case float64:
		fmt.Printf("%f\n", eval)
	default:
		fmt.Printf("%v\n", eval)
	}
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
		result := leftEval.(float64) > rightEval.(float64)
		return result, nil
	case GREATER_EQUAL:
		result := leftEval.(float64) >= rightEval.(float64)
		return result, nil
	case LESS:
		result := leftEval.(float64) < rightEval.(float64)
		return result, nil
	case LESS_EQUAL:
		result := leftEval.(float64) <= rightEval.(float64)
		return result, nil
	case MINUS:
		result := leftEval.(float64) - rightEval.(float64)
		return result, nil
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
	case SLASH:
		result := leftEval.(float64) - rightEval.(float64)
		return result, nil
	case STAR:
		result := leftEval.(float64) - rightEval.(float64)
		return result, nil
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
		return !in.isTruthy(exprEval), nil
	case MINUS:
		return -exprEval.(float64), nil
	}
	panic("Unreachable")
}

func (in *Interpreter) eval(expr Expr) (interface{}, error) {
	return expr.Accept(in)
}

func (in *Interpreter) isTruthy(value interface{}) bool {
	if value == nil {
		return false
	}
	if v, ok := value.(bool); ok {
		return v
	}
	return true
}
