package lox

import (
	"fmt"
	"io"
	"time"
)

// Interpreter exposes methods for evaluating then given Lox syntax tree. This
// struct implements ExprVisitor
type Interpreter struct {
	globals     *loxEnvironment
	environment *loxEnvironment
	locals      map[Expr]int
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
	interpreter.locals = make(map[Expr]int)
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
			fmt.Fprintln(in.output, Stringify(expr))
		}
	}
	return nil, nil
}

func (in *Interpreter) VisitFunctionStmt(stmt *FunctionStmt) (interface{}, error) {
	fn := newLoxFn(stmt, in.environment)
	in.environment.define(stmt.Name.Lexeme, fn)
	return nil, nil
}

func (in *Interpreter) VisitIfStmt(stmt *IfStmt) (interface{}, error) {
	cond, err := in.eval(stmt.Cond)
	if err != nil {
		return nil, err
	}
	if Truthy(cond) {
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
	fmt.Fprintln(in.output, Stringify(expr))
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
	in.environment.define(stmt.Name.Lexeme, initVal)
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
		if !Truthy(cond) {
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

	if steps, ok := in.locals[expr]; ok {
		in.environment.assignAt(steps, expr.Name, val)
		return nil, nil
	} else {
		return nil, in.globals.assign(expr.Name, val)
	}
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

	switch expr.Op.Type {
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
		return nil, newRuntimeError(expr.Op, "Operand must be numbers.")

	case GREATER_EQUAL:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum >= rightNum
			return result, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be numbers.")

	case LESS:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum < rightNum
			return result, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be numbers.")

	case LESS_EQUAL:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum <= rightNum
			return result, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be numbers.")

	case MINUS:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum - rightNum
			return result, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be numbers.")

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

		return nil, newRuntimeError(expr.Op, "Operands must be two numbers or two strings.")

	case SLASH:
		leftNum, okLeftNum := lhs.(float64)
		rightNum, okRightNum := rhs.(float64)
		if okLeftNum && okRightNum {
			result := leftNum / rightNum
			return result, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be numbers.")

	case STAR:
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

	switch expr.Op.Type {
	case OR:
		if Truthy(lhs) {
			return lhs, nil
		}
	case AND:
		if !Truthy(lhs) {
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

	switch expr.Op.Type {
	case BANG:
		return !Truthy(exprVal), nil
	case MINUS:
		if exprNum, ok := exprVal.(float64); ok {
			return -exprNum, nil
		}
		return nil, newRuntimeError(expr.Op, "Operand must be a number.")
	}
	panic("Unreachable")
}

func (in *Interpreter) VisitVarExpr(expr *VarExpr) (interface{}, error) {
	if steps, ok := in.locals[expr]; ok {
		return in.environment.getAt(steps, expr.Name.Lexeme), nil
	} else {
		return in.globals.get(expr.Name)
	}
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

func (in *Interpreter) resolve(expr Expr, steps int) {
	in.locals[expr] = steps
}

type loxReturn struct {
	val interface{}
}

func newLoxReturn(val interface{}) *loxReturn {
	r := new(loxReturn)
	r.val = val
	return r
}

func (r *loxReturn) Error() string {
	return fmt.Sprintf("return %v", Stringify(r.val))
}

// loxCallable is implemented by Lox's objects that can be called.
type loxCallable interface {
	arity() int
	call(in *Interpreter, args []interface{}) (interface{}, error)
}

type loxNativeFnClock struct{}

func (fn *loxNativeFnClock) arity() int {
	return 0
}

func (fn *loxNativeFnClock) call(
	in *Interpreter,
	args []interface{},
) (interface{}, error) {
	return time.Since(time.Unix(0, 0)).Seconds(), nil
}

func (fn *loxNativeFnClock) String() string {
	return "<native fn clock/0>"
}

// loxFn represents a lox function that can be called
type loxFn struct {
	decl    *FunctionStmt
	closure *loxEnvironment
}

func newLoxFn(decl *FunctionStmt, closure *loxEnvironment) *loxFn {
	fn := new(loxFn)
	fn.decl = decl
	fn.closure = closure
	return fn
}

func (fn *loxFn) arity() int {
	return len(fn.decl.Params)
}

func (fn *loxFn) call(
	in *Interpreter,
	args []interface{},
) (interface{}, error) {
	/*
		A function encapsulates its parameters, which means each function get is
		own environment where it stores the encapsulated variables. Each function
		call dynamically creates a new environment, otherwise, recursion would break.
		If there are multiple calls to the same function in play at the same time,
		each needs its own environment, even though they are all calls to the same
		function.
	*/
	env := newLoxEnvironment(fn.closure)
	for i, param := range fn.decl.Params {
		env.define(param.Lexeme, args[i])
	}

	if err := in.execBlock(fn.decl.Body, env); err != nil {
		/*
			TODO: Here we treats return as an error so we can easily unwound the stack,
			instead of of `error` we can use a custom interface that is returned as the
			second value like `error`
		*/
		if ret, ok := err.(*loxReturn); ok {
			return ret.val, nil
		}
		return nil, err
	}
	return nil, nil
}

func (fn *loxFn) String() string {
	return fmt.Sprintf("<fn %s/%d>", fn.decl.Name.Lexeme, fn.arity())
}
