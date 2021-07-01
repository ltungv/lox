package lox

import (
	"fmt"
	"io"
)

// Interpreter exposes methods for evaluating then given Lox syntax tree. This
// struct implements ExprVisitor
type Interpreter struct {
	environment *Environment
	output      io.Writer
	reporter    Reporter
	isREPL      bool
}

func NewInterpreter(output io.Writer, reporter Reporter, isREPL bool) *Interpreter {
	return &Interpreter{NewEnvironment(nil), output, reporter, isREPL}
}

func (in *Interpreter) Interpret(statements []Stmt) {
	for _, stmt := range statements {
		if _, err := in.exec(stmt); err != nil {
			in.reporter.Report(err)
			break
		}
	}
}

func (in *Interpreter) VisitBlockStmt(stmt *BlockStmt) (interface{}, error) {
	return nil, in.execBlock(stmt.Statements, NewEnvironment(in.environment))
}

func (in *Interpreter) VisitExpressionStmt(stmt *ExpressionStmt) (interface{}, error) {
	value, err := in.eval(stmt.Expression)
	if err != nil {
		return nil, err
	}
	if in.isREPL {
		fmt.Fprintln(in.output, stringify(value))
	}
	return nil, nil
}

func (in *Interpreter) VisitPrintStmt(stmt *PrintStmt) (interface{}, error) {
	expr, err := in.eval(stmt.Expression)
	if err != nil {
		return nil, err
	}
	fmt.Fprintln(in.output, stringify(expr))
	return nil, nil
}

func (in *Interpreter) VisitVarStmt(stmt *VarStmt) (interface{}, error) {
	var initVal interface{}
	if stmt.Initializer != nil {
		var err error
		initVal, err = in.eval(stmt.Initializer)
		if err != nil {
			return nil, err
		}
	}
	in.environment.Define(stmt.Name, initVal)
	return nil, nil
}

func (in *Interpreter) VisitAssignExpr(expr *AssignExpr) (interface{}, error) {
	value, err := in.eval(expr.Value)
	if err != nil {
		return nil, err
	}
	err = in.environment.Assign(expr.Name, value)
	return nil, err
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

func (in *Interpreter) VisitVariableExpr(expr *VariableExpr) (interface{}, error) {
	val, err := in.environment.Get(expr.Name)
	if err != nil {
		return nil, err
	}
	return val, nil
}

func (in *Interpreter) execBlock(statements []Stmt, environment *Environment) error {
	in.environment = environment
	defer func() {
		in.environment = environment.enclosing
	}()
	for _, stmt := range statements {
		if _, err := in.exec(stmt); err != nil {
			return err
		}
	}
	return nil
}

func (in *Interpreter) exec(stmt Stmt) (interface{}, error) {
	return stmt.Accept(in)
}

func (in *Interpreter) eval(expr Expr) (interface{}, error) {
	return expr.Accept(in)
}
