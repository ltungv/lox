package lox

import (
	"fmt"
	"io"
)

// Reporter defines the interface for structure that can display errors to the
// user. A reporter is defined to separated errors reporting code from errors
// displaying code. Fully-features languages have a complex setup for reporting
// errors to user.
type Reporter interface {
	Report(err error)
	HadError() bool
}

// SimpleReporter writes error as-is to inner writer
type SimpleReporter struct {
	writer io.Writer
	hadErr bool
}

func NewSimpleReporter(writer io.Writer) Reporter {
	return &SimpleReporter{writer, false}
}

func (reporter *SimpleReporter) Report(err error) {
	reporter.hadErr = true
	fmt.Fprintln(reporter.writer, err)
}

func (reporter *SimpleReporter) HadError() bool {
	return reporter.hadErr
}
