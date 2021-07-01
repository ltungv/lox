package lox

import "fmt"

type Environment struct {
	enclosing *Environment
	values    map[string]interface{}
}

func NewEnvironment(enclosing *Environment) *Environment {
	return &Environment{enclosing, make(map[string]interface{})}
}

func (env *Environment) Define(name *Token, value interface{}) {
	env.values[name.Lexeme] = value
}

func (env *Environment) Assign(name *Token, value interface{}) error {
	if _, ok := env.values[name.Lexeme]; ok {
		env.values[name.Lexeme] = value
		return nil
	}
	if env.enclosing != nil {
		return env.enclosing.Assign(name, value)
	}
	msg := fmt.Sprintf("Undefined variable '%s'.", name.Lexeme)
	return NewRuntimeError(name, msg)
}

func (env *Environment) Get(name *Token) (interface{}, error) {
	if value, ok := env.values[name.Lexeme]; ok {
		return value, nil
	}
	if env.enclosing != nil {
		return env.enclosing.Get(name)
	}
	msg := fmt.Sprintf("Undefined variable '%s'.", name.Lexeme)
	return nil, NewRuntimeError(name, msg)
}
