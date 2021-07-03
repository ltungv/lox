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
	return nil, in.execBlock(stmt.Stmts, NewEnvironment(in.environment))
}

func (in *Interpreter) VisitExprStmt(stmt *ExprStmt) (interface{}, error) {
	expr, err := in.eval(stmt.Expr)
	if err != nil {
		return nil, err
	}
	if in.isREPL {
		if _, ok := stmt.Expr.(*AssignExpr); !ok {
			fmt.Fprintln(in.output, stringify(expr))
		}
	}
	return nil, nil
}

func (in *Interpreter) VisitIfStmt(stmt *IfStmt) (interface{}, error) {
	cond, err := in.eval(stmt.Cond)
	if err != nil {
		return nil, err
	}
	if isTruthy(cond) {
		return in.exec(stmt.ThenBranch)
	} else {
		return in.exec(stmt.ElseBranch)
	}
	return nil, nil
}

func (in *Interpreter) VisitPrintStmt(stmt *PrintStmt) (interface{}, error) {
	expr, err := in.eval(stmt.Expr)
	if err != nil {
		return nil, err
	}
	fmt.Fprintln(in.output, stringify(expr))
	return nil, nil
}

func (in *Interpreter) VisitVarStmt(stmt *VarStmt) (interface{}, error) {
	var initVal interface{}
	if stmt.Init != nil {
		var err error
		initVal, err = in.eval(stmt.Init)
		if err != nil {
			return nil, err
		}
	}
	in.environment.Define(stmt.Name, initVal)
	return nil, nil
}

func (in *Interpreter) VisitWhileStmt(stmt *WhileStmt) (interface{}, error) {
	for {
		cond, err := in.eval(stmt.Cond)
		if err != nil {
			return nil, err
		}
		if !isTruthy(cond) {
			return nil, nil
		}
		_, err = in.exec(stmt.Body)
		if err != nil {
			return nil, err
		}
	}
}

func (in *Interpreter) VisitAssignExpr(expr *AssignExpr) (interface{}, error) {
	val, err := in.eval(expr.Val)
	if err != nil {
		return nil, err
	}
	err = in.environment.Assign(expr.Name, val)
	return nil, err
}

func (in *Interpreter) VisitBinaryExpr(expr *BinaryExpr) (interface{}, error) {
	lhs, err := in.eval(expr.Lhs)
	if err != nil {
		return nil, err
	}
	rhs, err := in.eval(expr.Rhs)
	if err != nil {
		return nil, err
	}

	switch expr.Op.Typ {
	case BANG_EQUAL:
		result := lhs != rhs
		return result, nil

	case EQUAL_EQUAL:
		result := lhs == rhs
		return result, nil

	case GREATER:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum > rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")

	case GREATER_EQUAL:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum >= rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")

	case LESS:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum < rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")

	case LESS_EQUAL:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum <= rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")

	case MINUS:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum - rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")

	case PLUS:
		leftStr, okLeftStr := lhs.(string)
		rightStr, okRightStr := rhs.(string)
		if okLeftStr && okRightStr {
			result := leftStr + rightStr
			return result, nil
		}
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum + rightNum
			return result, nil
		}

		return nil, NewRuntimeError(expr.Op, "Operands must be two numbers or two strings.")

	case SLASH:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum / rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")

	case STAR:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum * rightNum
			return result, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be numbers.")
	}
	panic("Unreachable")
}

func (in *Interpreter) VisitGroupExpr(expr *GroupExpr) (interface{}, error) {
	return in.eval(expr.Expr)
}

func (in *Interpreter) VisitLiteralExpr(expr *LiteralExpr) (interface{}, error) {
	return expr.Val, nil
}

func (in *Interpreter) VisitLogicalExpr(expr *LogicalExpr) (interface{}, error) {
	lhs, err := in.eval(expr.Lhs)
	if err != nil {
		return nil, err
	}

	switch expr.Op.Typ {
	case OR:
		if isTruthy(lhs) {
			return lhs, nil
		}
	case AND:
		if !isTruthy(lhs) {
			return lhs, nil
		}
	default:
		panic("Unreachable")
	}

	return in.eval(expr.Rhs)
}

func (in *Interpreter) VisitUnaryExpr(expr *UnaryExpr) (interface{}, error) {
	exprVal, err := in.eval(expr.Expr)
	if err != nil {
		return nil, err
	}

	switch expr.Op.Typ {
	case BANG:
		return !isTruthy(exprVal), nil
	case MINUS:
		if exprNum, ok := exprVal.(float64); ok {
			return -exprNum, nil
		}
		return nil, NewRuntimeError(expr.Op, "Operand must be a number.")
	}
	panic("Unreachable")
}

func (in *Interpreter) VisitVarExpr(expr *VarExpr) (interface{}, error) {
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
