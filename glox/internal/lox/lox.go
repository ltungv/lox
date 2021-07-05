// This file mostly contains structs that represent lox objects at runtime.
package lox

import (
	"fmt"
	"strconv"
	"time"
)

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

func truthy(value interface{}) bool {
	if value == nil {
		return false
	}
	if v, ok := value.(bool); ok {
		return v
	}
	return true
}

type class struct {
	name    string
	super   *class
	methods map[string]*function
}

func newClass(name string, super *class, methods map[string]*function) *class {
	c := new(class)
	c.name = name
	c.super = super
	c.methods = methods
	return c
}

func (c *class) String() string {
	return c.name
}

func (c *class) arity() int {
	if init, ok := c.findMethod("init"); ok {
		return init.arity()
	}
	return 0
}

func (c *class) call(
	interpreter *Interpreter,
	args []interface{},
) (interface{}, error) {
	instance := newInstance(c)
	// call the initializer on the instance if it's defined
	if init, ok := c.findMethod("init"); ok {
		init.bind(instance).call(interpreter, args)
	}
	return instance, nil
}

func (c *class) findMethod(name string) (*function, bool) {
	method, ok := c.methods[name]
	if !ok && c.super != nil {
		method, ok = c.super.findMethod(name)
	}
	return method, ok
}

type instance struct {
	class  *class
	fields map[string]interface{}
}

func newInstance(klass *class) *instance {
	inst := new(instance)
	inst.class = klass
	inst.fields = make(map[string]interface{})
	return inst
}

func (inst *instance) String() string {
	return inst.class.name + " instance"
}

func (inst *instance) get(name *Token) (interface{}, error) {
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

func (inst *instance) set(name *Token, val interface{}) {
	inst.fields[name.Lexeme] = val
}

type callReturn struct {
	val interface{}
}

func newCallReturn(val interface{}) *callReturn {
	r := new(callReturn)
	r.val = val
	return r
}

func (r *callReturn) Error() string {
	return fmt.Sprintf("return %v", stringify(r.val))
}

type functionClock struct{}

func (fn *functionClock) arity() int {
	return 0
}

func (fn *functionClock) call(
	in *Interpreter,
	args []interface{},
) (interface{}, error) {
	return time.Since(time.Unix(0, 0)).Seconds(), nil
}

func (fn *functionClock) String() string {
	return "<native fn>"
}

// function represents a lox function that can be called
type function struct {
	decl          *FunctionStmt
	closure       *environment
	isInitializer bool
}

func newFunction(decl *FunctionStmt, closure *environment, isInitializer bool) *function {
	fn := new(function)
	fn.decl = decl
	fn.closure = closure
	fn.isInitializer = isInitializer
	return fn
}

func (fn *function) String() string {
	return fmt.Sprintf("<fn %s>", fn.decl.Name.Lexeme)
}

func (fn *function) arity() int {
	return len(fn.decl.Params)
}

func (fn *function) call(
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
		if ret, ok := err.(*callReturn); ok {
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

func (fn *function) bind(inst *instance) *function {
	env := newEnvironment(fn.closure)
	env.define("this", inst)
	return newFunction(fn.decl, env, fn.isInitializer)
}
