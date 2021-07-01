package main

// This is an interpreter for the Lox programming language written in Go.

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/letung3105/lox/glox/internal/lox"
)

func main() {
	args := os.Args[1:]
	if len(args) > 1 {
		fmt.Println("Usage: glox [script]")
		os.Exit(64)
	}

	if len(args) != 1 {
		runPrompt()
	} else {
		runFile(args[0])
	}
}

func run(script string, interpreter *lox.Interpreter, reporter lox.Reporter) {
	scanner := lox.NewScanner([]rune(script), reporter)
	tokens := scanner.Scan()
	parser := lox.NewParser(tokens, reporter)
	statements := parser.Parse()
	if reporter.HadError() {
		return
	}
	interpreter.Interpret(statements)
}

// Run the interpreter in REPL mode
func runPrompt() {
	reporter := lox.NewSimpleReporter(os.Stdout)
	interpreter := lox.NewInterpreter(os.Stdout, reporter, true)

	s := bufio.NewScanner(os.Stdin)
	s.Split(bufio.ScanLines)
	for {
		fmt.Print("> ")
		if !s.Scan() {
			break
		}
		run(s.Text(), interpreter, reporter)
		reporter.Reset()
	}
	exitOnError(s.Err(), 1)
}

// Run the given file as script
func runFile(fpath string) {
	bytes, err := ioutil.ReadFile(fpath)
	exitOnError(err, 1)

	reporter := lox.NewSimpleReporter(os.Stdout)
	interpreter := lox.NewInterpreter(os.Stdout, reporter, false)
	run(string(bytes), interpreter, reporter)

	exitIf(reporter.HadError(), 65)
	exitIf(reporter.HadRuntimeError(), 70)
}

func exitOnError(err error, status int) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "%v", err)
		os.Exit(status)
	}
}

func exitIf(cond bool, status int) {
	if cond {
		os.Exit(status)
	}
}
