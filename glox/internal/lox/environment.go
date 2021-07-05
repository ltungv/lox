package lox

import "fmt"

type environment struct {
	enclosing *environment
	values    map[string]interface{}
}

func newEnvironment(enclosing *environment) *environment {
	env := new(environment)
	env.enclosing = enclosing
	env.values = make(map[string]interface{})
	return env
}

func (env *environment) define(name string, value interface{}) {
	env.values[name] = value
}

func (env *environment) assign(name *Token, value interface{}) error {
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

func (env *environment) get(name *Token) (interface{}, error) {
	if value, ok := env.values[name.Lexeme]; ok {
		return value, nil
	}
	if env.enclosing != nil {
		return env.enclosing.get(name)
	}
	msg := fmt.Sprintf("Undefined variable '%s'.", name.Lexeme)
	return nil, newRuntimeError(name, msg)
}

func (env *environment) assignAt(steps int, name *Token, val interface{}) {
	env.ancestor(steps).values[name.Lexeme] = val
}

func (env *environment) getAt(steps int, name string) interface{} {
	return env.ancestor(steps).values[name]
}

func (env *environment) ancestor(steps int) *environment {
	iterEnv := env
	for i := 0; i < steps; i++ {
		iterEnv = iterEnv.enclosing
	}
	return iterEnv
}
