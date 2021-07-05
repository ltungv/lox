// This file mostly contains structs that represent lox objects at runtime.
package lox

import (
	"fmt"
	"strconv"
	"time"
	"unicode"
)

func Stringify(v interface{}) string {
	switch v := v.(type) {
	case nil:
		return fmt.Sprint("nil")
	case float64:
		return fmt.Sprint(strconv.FormatFloat(v, 'f', -1, 64))
	default:
		return fmt.Sprint(v)
	}
}

func Truthy(value interface{}) bool {
	if value == nil {
		return false
	}
	if v, ok := value.(bool); ok {
		return v
	}
	return true
}

type loxClass struct {
	name    string
	super   *loxClass
	methods map[string]*loxFn
}

func newClass(name string, super *loxClass, methods map[string]*loxFn) *loxClass {
	class := new(loxClass)
	class.name = name
	class.super = super
	class.methods = methods
	return class
}

func (class *loxClass) String() string {
	return class.name
}

func (class *loxClass) arity() int {
	if init, ok := class.findMethod("init"); ok {
		return init.arity()
	}
	return 0
}

func (class *loxClass) call(
	interpreter *Interpreter,
	args []interface{},
) (interface{}, error) {
	instance := newInstance(class)
	// call the initializer on the instance if it's defined
	if init, ok := class.findMethod("init"); ok {
		init.bind(instance).call(interpreter, args)
	}
	return instance, nil
}

func (class *loxClass) findMethod(name string) (*loxFn, bool) {
	method, ok := class.methods[name]
	if !ok && class.super != nil {
		method, ok = class.super.findMethod(name)
	}
	return method, ok
}

type loxInstance struct {
	class  *loxClass
	fields map[string]interface{}
}

func newInstance(class *loxClass) *loxInstance {
	inst := new(loxInstance)
	inst.class = class
	inst.fields = make(map[string]interface{})
	return inst
}

func (inst *loxInstance) String() string {
	return inst.class.name + " instance"
}

func (inst *loxInstance) get(name *Token) (interface{}, error) {
	if val, ok := inst.fields[name.Lexeme]; ok {
		return val, nil
	}

	// create a bound method on the instance, such that `this` always
	// refers to the instant that gave out the method
	if method, ok := inst.class.findMethod(name.Lexeme); ok {
		return method.bind(inst), nil
	}

	return nil, newRuntimeError(name, fmt.Sprintf(
		"Undefined property '%s'.", name.Lexeme,
	))
}

func (inst *loxInstance) set(name *Token, val interface{}) {
	inst.fields[name.Lexeme] = val
}

type loxReturn struct {
	val interface{}
}

func newReturn(val interface{}) *loxReturn {
	r := new(loxReturn)
	r.val = val
	return r
}

func (r *loxReturn) Error() string {
	return fmt.Sprintf("return %v", Stringify(r.val))
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
	decl          *FunctionStmt
	closure       *loxEnvironment
	isInitializer bool
}

func newFn(decl *FunctionStmt, closure *loxEnvironment, isInitializer bool) *loxFn {
	fn := new(loxFn)
	fn.decl = decl
	fn.closure = closure
	fn.isInitializer = isInitializer
	return fn
}

func (fn *loxFn) String() string {
	return fmt.Sprintf("<fn %s/%d>", fn.decl.Name.Lexeme, fn.arity())
}

func (fn *loxFn) arity() int {
	return len(fn.decl.Params)
}

func (fn *loxFn) call(
	interpreter *Interpreter,
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
	env := newEnvironment(fn.closure)
	for i, param := range fn.decl.Params {
		env.define(param.Lexeme, args[i])
	}

	if err := interpreter.execBlock(fn.decl.Body, env); err != nil {
		/*
			TODO: Here we treats return as an error so we can easily unwound the stack,
			instead of of `error` we can use a custom interface that is returned as the
			second value like `error`
		*/
		if ret, ok := err.(*loxReturn); ok {
			// return this if in an initalizer and no return value is given
			if fn.isInitializer {
				return fn.closure.getAt(0, "this"), nil
			}
			return ret.val, nil
		}
		return nil, err
	}

	if fn.isInitializer {
		// an empty return statement inside the class' `init` method will return
		// `this` instead of nil
		return fn.closure.getAt(0, "this"), nil
	}

	return nil, nil
}

func (fn *loxFn) bind(inst *loxInstance) *loxFn {
	env := newEnvironment(fn.closure)
	env.define("this", inst)
	return newFn(fn.decl, env, fn.isInitializer)
}

func isAlphanumeric(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsDigit(r)
}

func isBeginIdent(r rune) bool {
	return unicode.IsLetter(r) || r == '_'
}
