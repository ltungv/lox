package lox

import (
	"errors"
	"fmt"
	"io/ioutil"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSimpleReporterInit(t *testing.T) {
	assert := assert.New(t)

	r := NewSimpleReporter(ioutil.Discard)

	assert.False(r.HadError())
	assert.False(r.HadRuntimeError())
}

func TestSimpleReporterSendAnyError(t *testing.T) {
	assert := assert.New(t)
	err := errors.New("Test error")

	var out strings.Builder
	r := NewSimpleReporter(&out)
	r.Report(err)

	assert.Equal(fmt.Sprintf("%v\n", err), out.String())
	assert.True(r.HadError())
	assert.False(r.HadRuntimeError())
}

func TestSimpleReporterSendRuntimeError(t *testing.T) {
	assert := assert.New(t)
	err := newRuntimeError(newLoxToken(tokenMinus, "-", nil, 1), "Operand must be numbers.")

	var out strings.Builder
	r := NewSimpleReporter(&out)
	r.Report(err)

	assert.Equal(fmt.Sprintf("%v\n", err), out.String())
	assert.False(r.HadError())
	assert.True(r.HadRuntimeError())
}

func TestSimpleReporterSendErrors(t *testing.T) {
	assert := assert.New(t)
	err1 := errors.New("Test error")
	err2 := newRuntimeError(newLoxToken(tokenMinus, "-", nil, 1), "Operand must be numbers.")

	var out strings.Builder
	r := NewSimpleReporter(&out)
	r.Report(err1)
	r.Report(err2)

	assert.Equal(fmt.Sprintf("%v\n%v\n", err1, err2), out.String())
	assert.True(r.HadError())
	assert.True(r.HadRuntimeError())
}

func TestSimpleReporterReset(t *testing.T) {
	assert := assert.New(t)
	err1 := errors.New("Test error")
	err2 := newRuntimeError(newLoxToken(tokenMinus, "-", nil, 1), "Operand must be numbers.")

	var out strings.Builder
	r := NewSimpleReporter(&out)
	r.Report(err1)
	r.Report(err2)

	r.Reset()
	assert.False(r.HadRuntimeError())
	assert.False(r.HadError())
}
