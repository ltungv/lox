package main

// This is an interpreter for the Lox programming language written in Go.

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	if len(os.Args) > 1 {
		fmt.Println("Usage: glox [script]")
		os.Exit(64)
	}

	var err error
	if len(os.Args) != 1 {
		err = runPrompt()
	} else {
		err = runFile(os.Args[0])
	}

	if err != nil {
		panic(err)
	}
}

// Run the interpreter in REPL mode
func runPrompt() error {
	s := bufio.NewScanner(os.Stdin)
	s.Split(bufio.ScanLines)
	for {
		fmt.Print("> ")
		if !s.Scan() {
			break
		}
		if err := run(s.Text()); err != nil {
			return err
		}
	}

	return nil
}

// Run the given file as script
func runFile(fpath string) error {
	bytes, err := ioutil.ReadFile(fpath)
	if err != nil {
		return err
	}

	return run(string(bytes))
}

func run(script string) error {
	return nil
}
