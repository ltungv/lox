package lox

import "fmt"

type loxEnvironment struct {
	enclosing *loxEnvironment
	values    map[string]interface{}
}

func newLoxEnvironment(enclosing *loxEnvironment) *loxEnvironment {
	env := new(loxEnvironment)
	env.enclosing = enclosing
	env.values = make(map[string]interface{})
	return env
}

func (env *loxEnvironment) define(name string, value interface{}) {
	env.values[name] = value
}

func (env *loxEnvironment) assign(name *Token, value interface{}) error {
	if _, ok := env.values[name.Lexeme]; ok {
		env.values[name.Lexeme] = value
		return nil
	}
	if env.enclosing != nil {
		return env.enclosing.assign(name, value)
	}
	msg := fmt.Sprintf("Undefined variable '%s'.", name.Lexeme)
	return newRuntimeError(name, msg)
}

func (env *loxEnvironment) get(name *Token) (interface{}, error) {
	if value, ok := env.values[name.Lexeme]; ok {
		return value, nil
	}
	if env.enclosing != nil {
		return env.enclosing.get(name)
	}
	msg := fmt.Sprintf("Undefined variable '%s'.", name.Lexeme)
	return nil, newRuntimeError(name, msg)
}

func (env *loxEnvironment) assignAt(steps int, name *Token, val interface{}) {
	env.ancestor(steps).values[name.Lexeme] = val
}

func (env *loxEnvironment) getAt(steps int, name string) interface{} {
	return env.ancestor(steps).values[name]
}

func (env *loxEnvironment) ancestor(steps int) *loxEnvironment {
	iterEnv := env
	for i := 0; i < steps; i++ {
		iterEnv = iterEnv.enclosing
	}
	return iterEnv
}
