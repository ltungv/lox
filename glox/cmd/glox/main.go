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

	reporter := lox.NewSimpleReporter(os.Stdout)
	if len(args) != 1 {
		runPrompt(reporter)
	} else {
		runFile(args[0], reporter)
	}

	if reporter.HadError() {
		os.Exit(65)
	}
}

func run(script string, reporter lox.Reporter) {
	sc := lox.NewScanner([]rune(script), reporter)
  tokens := sc.Scan()
  parser := lox.NewParser(tokens)
  expr, err := parser.Parse()
  if err != nil {
    reporter.Report(err)
    return
  }

	printer := lox.AstPrinter{}
	fmt.Println(printer.Print(expr))
}

// Run the interpreter in REPL mode
func runPrompt(reporter lox.Reporter) {
	s := bufio.NewScanner(os.Stdin)
	s.Split(bufio.ScanLines)
	for {
		fmt.Print("> ")
		if !s.Scan() {
			break
		}
		run(s.Text(), reporter)
	}
	if err := s.Err(); err != nil {
		reporter.Report(err)
	}
}

// Run the given file as script
func runFile(fpath string, reporter lox.Reporter) {
	bytes, err := ioutil.ReadFile(fpath)
	if err != nil {
		reporter.Report(err)
		return
	}
	run(string(bytes), reporter)
}
