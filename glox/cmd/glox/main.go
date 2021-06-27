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

func run(script string, reporter lox.Reporter) {
	sc := lox.NewScanner([]rune(script), reporter)
	tokens := sc.Scan()
	parser := lox.NewParser(tokens, reporter)
	expr := parser.Parse()
	if reporter.HadError() {
		return
	}
	printer := lox.AstPrinter{}
	fmt.Println(printer.Print(expr))
}

// Run the interpreter in REPL mode
func runPrompt() {
	reporter := lox.NewSimpleReporter(os.Stdout)
	s := bufio.NewScanner(os.Stdin)
	s.Split(bufio.ScanLines)
	for {
		fmt.Print("> ")
		if !s.Scan() {
			break
		}
		run(s.Text(), reporter)
		reporter.Reset()
	}
	exitOnError(s.Err(), 1)
}

// Run the given file as script
func runFile(fpath string) {
	bytes, err := ioutil.ReadFile(fpath)
	exitOnError(err, 1)

	reporter := lox.NewSimpleReporter(os.Stdout)
	run(string(bytes), reporter)
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
