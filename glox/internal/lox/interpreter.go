package lox

import (
	"fmt"
	"io"
)

// Interpreter exposes methods for evaluating then given Lox syntax tree. This
// struct implements ExprVisitor
type Interpreter struct {
	globals     *loxEnvironment
	environment *loxEnvironment
	output      io.Writer
	reporter    Reporter
	isREPL      bool
}

func NewInterpreter(output io.Writer, reporter Reporter, isREPL bool) *Interpreter {
	env := newLoxEnvironment(nil)
	env.define("clock", new(loxNativeFnClock))

	interpreter := new(Interpreter)
	interpreter.globals = env
	interpreter.environment = env
	interpreter.output = output
	interpreter.reporter = reporter
	interpreter.isREPL = isREPL
	return interpreter
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
	return nil, in.execBlock(stmt.Stmts, newLoxEnvironment(in.environment))
}

func (in *Interpreter) VisitExprStmt(stmt *ExprStmt) (interface{}, error) {
	expr, err := in.eval(stmt.Expr)
	if err != nil {
		return nil, err
	}
	if in.isREPL {
		switch stmt.Expr.(type) {
		case *AssignExpr, *CallExpr:
			/* expressions of these types are not printed */
		default:
			fmt.Fprintln(in.output, stringify(expr))
		}
	}
	return nil, nil
}

func (in *Interpreter) VisitFunctionStmt(stmt *FunctionStmt) (interface{}, error) {
	fn := newLoxFn(stmt, in.environment)
	in.environment.define(stmt.Name.lexeme, fn)
	return nil, nil
}

func (in *Interpreter) VisitIfStmt(stmt *IfStmt) (interface{}, error) {
	cond, err := in.eval(stmt.Cond)
	if err != nil {
		return nil, err
	}
	if isTruthy(cond) {
		return in.exec(stmt.ThenBranch)
	} else if stmt.ElseBranch != nil {
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
	in.environment.define(stmt.Name.lexeme, initVal)
	return nil, nil
}

func (in *Interpreter) VisitReturnStmt(stmt *ReturnStmt) (interface{}, error) {
	var val interface{}
	var err error
	if stmt.Val != nil {
		val, err = in.eval(stmt.Val)
		if err != nil {
			return nil, err
		}
	}
	return nil, newLoxReturn(val)
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
	err = in.environment.assign(expr.Name, val)
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

	switch expr.Op.typ {
	case tokenBangEqual:
		result := lhs != rhs
		return result, nil

	case tokenEqualEqual:
		result := lhs == rhs
		return result, nil

	case tokenGreater:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum > rightNum
			return result, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be numbers.")

	case tokenGreaterEqual:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum >= rightNum
			return result, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be numbers.")

	case tokenLess:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum < rightNum
			return result, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be numbers.")

	case tokenLessEqual:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum <= rightNum
			return result, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be numbers.")

	case tokenMinus:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum - rightNum
			return result, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be numbers.")

	case tokenPlus:
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

		return nil, newRuntimeError(expr.Op, "Operands must be two numbers or two strings.")

	case tokenSlash:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum / rightNum
			return result, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be numbers.")

	case tokenStar:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum * rightNum
			return result, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be numbers.")
	}
	panic("Unreachable")
}

func (in *Interpreter) VisitCallExpr(expr *CallExpr) (interface{}, error) {
	callee, err := in.eval(expr.Callee)
	if err != nil {
		return nil, err
	}

	/*
		NOTE: Here we evaluate each expressions in order. This is a subtle semantic
		choice. The order in which these arguments are evaluated could be user
		visible, because expressions can have side-effects. Languages like C and Scheme
		don't specify an order, so compilers can freely rearrange them for efficiency,
		but users may be unpleasantly surprised if arguments aren't evaluated in the
		order they expected.
	*/
	var args []interface{}
	for _, arg := range expr.Args {
		argVal, err := in.eval(arg)
		if err != nil {
			return nil, err
		}
		args = append(args, argVal)
	}

	callable, isCallable := callee.(loxCallable)
	if !isCallable {
		return nil, newRuntimeError(expr.Paren, "Can only call functions and classes.")
	}
	/*
		NOTE: The arity check could be done within the Call() method. But we have lots
		of different Lox's objects that can be called, resulting in the check has to
		be reimplemented by each object. We only has to do it once, if it's performed
		here.
	*/
	if len(args) != callable.arity() {
		return nil, newRuntimeError(expr.Paren, fmt.Sprintf(
			"Expected %d arguments but got %d.", callable.arity(), len(args),
		))
	}
	return callable.call(in, args)
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

	switch expr.Op.typ {
	case tokenOr:
		if isTruthy(lhs) {
			return lhs, nil
		}
	case tokenAnd:
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

	switch expr.Op.typ {
	case tokenBang:
		return !isTruthy(exprVal), nil
	case tokenMinus:
		if exprNum, ok := exprVal.(float64); ok {
			return -exprNum, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be a number.")
	}
	panic("Unreachable")
}

func (in *Interpreter) VisitVarExpr(expr *VarExpr) (interface{}, error) {
	val, err := in.environment.get(expr.Name)
	if err != nil {
		return nil, err
	}
	return val, nil
}

func (in *Interpreter) execBlock(statements []Stmt, environment *loxEnvironment) error {
	prevEnv := in.environment
	in.environment = environment
	defer func() {
		in.environment = prevEnv
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
