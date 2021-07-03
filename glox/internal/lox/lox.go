package lox

import (
	"fmt"
	"strconv"
	"time"
	"unicode"
)

type loxReturn struct {
	val interface{}
}

func newLoxReturn(val interface{}) *loxReturn {
	r := new(loxReturn)
	r.val = val
	return r
}

func (r *loxReturn) Error() string {
	return fmt.Sprintf("return %v", stringify(r.val));
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
	decl *FunctionStmt
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
		env.define(param.lexeme, args[i])
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
	return fmt.Sprintf("<fn %s/%d>", fn.decl.Name.lexeme, fn.arity())
}

func stringify(v interface{}) string {
	switch v := v.(type) {
	case nil:
		return fmt.Sprint("nil")
	case float64:
		return fmt.Sprint(strconv.FormatFloat(v, 'f', -1, 64))
	default:
		return fmt.Sprint(v)
	}
}

func isTruthy(value interface{}) bool {
	if value == nil {
		return false
	}
	if v, ok := value.(bool); ok {
		return v
	}
	return true
}

func isAlphanumeric(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsDigit(r)
}

func isBeginIdent(r rune) bool {
	return unicode.IsLetter(r) || r == '_'
}
