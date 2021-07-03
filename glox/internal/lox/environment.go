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

func (env *loxEnvironment) assign(name *loxToken, value interface{}) error {
	if _, ok := env.values[name.lexeme]; ok {
		env.values[name.lexeme] = value
		return nil
	}
	if env.enclosing != nil {
		return env.enclosing.assign(name, value)
	}
	msg := fmt.Sprintf("Undefined variable '%s'.", name.lexeme)
	return newRuntimeError(name, msg)
}

func (env *loxEnvironment) get(name *loxToken) (interface{}, error) {
	if value, ok := env.values[name.lexeme]; ok {
		return value, nil
	}
	if env.enclosing != nil {
		return env.enclosing.get(name)
	}
	msg := fmt.Sprintf("Undefined variable '%s'.", name.lexeme)
	return nil, newRuntimeError(name, msg)
}
