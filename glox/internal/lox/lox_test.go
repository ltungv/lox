package lox

type mockReporter struct {
	errors        []error
	hadErr        bool
	hadRuntimeErr bool
}

func newMockReporter() *mockReporter {
	return &mockReporter{make([]error, 0), false, false}
}

func (reporter *mockReporter) Report(err error) {
	reporter.errors = append(reporter.errors, err)
	if _, isRuntimeErr := err.(*RuntimeError); isRuntimeErr {
		reporter.hadRuntimeErr = true
	} else {
		reporter.hadErr = true
	}
}

func (reporter *mockReporter) Reset() {
	reporter.hadErr = false
	reporter.hadRuntimeErr = false
}

func (reporter *mockReporter) HadError() bool {
	return reporter.hadErr
}

func (reporter *mockReporter) HadRuntimeError() bool {
	return reporter.hadRuntimeErr
}

func tokEOF(line int) *Token {
	return NewToken(EOF, "", nil, line)
}
