package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage: generate_ast <output directory>")
		os.Exit(64)
	}

	outputDir := os.Args[1]
	// we do it the scripting way, instead of having types support from Go stdlib
	expressionTypes := []string{
		"Assign: Name *Token, Val Expr",
		"Binary: Op *Token, Lhs Expr, Rhs Expr",
		// Call stores the token for the closing parenthesis so the token's location
		// can be used when we report RuntimeError caused by a function call.
		"Call: Callee Expr, Paren *Token, Args []Expr",
		"Get: Obj Expr, Name *Token",
		"Group: Expr Expr",
		"Literal: Val interface{}",
		"Logical: Op *Token, Lhs Expr, Rhs Expr",
		"Set: Obj Expr, Name *Token, Val Expr",
		"This: Keyword *Token",
		"Unary: Op *Token, Expr Expr",
		"Var: Name *Token",
	}
	statementTypes := []string{
		"Block: Stmts []Stmt",
		"Class: Name *Token, Superclass *VarExpr, Methods []*FunctionStmt",
		"Expr: Expr Expr",
		"Function: Name *Token, Params []*Token, Body []Stmt",
		"If: Cond Expr, ThenBranch Stmt, ElseBranch Stmt",
		"Print: Expr Expr",
		"Return: Keyword *Token, Val Expr",
		"Var: Name *Token, Init Expr",
		"While: Cond Expr, Body Stmt",
	}

	defineAst(outputDir, "Expr", expressionTypes)
	defineAst(outputDir, "Stmt", statementTypes)
}

func defineAst(outputDir string, baseName string, types []string) {
	fpath := filepath.Join(
		outputDir,
		fmt.Sprintf("%s.go", strings.ToLower(baseName)),
	)
	f, err := os.OpenFile(fpath, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0644)
	if err != nil {
		panic(err)
	}
	defer f.Close()

	writer := bufio.NewWriter(f)
	if err != nil {
		panic(err)
	}
	defer writer.Flush()

	packageName := filepath.Base(outputDir)
	fmt.Fprintf(writer, "package %s\n", packageName)

	// Interface for Expr in AST
	fmt.Fprintf(writer, "type %s interface {\n", baseName)
	fmt.Fprintf(writer, "\tAccept(visitor %sVisitor) (interface{}, error)\n", baseName)
	fmt.Fprintf(writer, "}\n")

	defineVisitor(writer, baseName, types)

	// Generate struct for each AST type
	for _, t := range types {
		typeName := strings.TrimSpace(strings.Split(t, ":")[0])
		fields := strings.TrimSpace(strings.Split(t, ":")[1])
		defineType(writer, baseName, typeName, fields)
	}
}

func defineVisitor(writer io.Writer, baseName string, types []string) {
	// We have one method for each AST type
	fmt.Fprintf(writer, "type %sVisitor interface {\n", baseName)
	for _, t := range types {
		typeName := strings.TrimSpace(strings.Split(t, ":")[0])
		fmt.Fprintf(
			writer,
			"\tVisit%s%s(%s *%s%s) (interface{}, error)\n",
			typeName, baseName,
			strings.ToLower(baseName),
			typeName, baseName,
		)
	}
	fmt.Fprintf(writer, "}\n")
}

func defineType(
	writer io.Writer,
	baseName string,
	typeName string,
	fieldList string,
) {
	var fields []string
	for _, f := range strings.Split(fieldList, ",") {
		field := strings.TrimSpace(f)
		fields = append(fields, field)
	}

	// Struct definition
	fmt.Fprintf(writer, "type %s%s struct {\n", typeName, baseName)
	for _, f := range fields {
		fmt.Fprintf(writer, "\t%s\n", f)
	}
	fmt.Fprintf(writer, "}\n")

	// Constructor
	fmt.Fprintf(
		writer,
		"func New%s%s(%s) *%s%s {\n",
		typeName, baseName,
		fieldList,
		typeName, baseName,
	)
	var fieldNames []string
	for _, f := range strings.Split(fieldList, ",") {
		field := strings.TrimSpace(f)
		fieldName := strings.TrimSpace(strings.Split(field, " ")[0])
		fieldNames = append(fieldNames, fieldName)
	}
	fmt.Fprintf(
		writer,
		"\treturn &%s%s{%s}\n",
		typeName, baseName,
		strings.Join(fieldNames, ","),
	)
	fmt.Fprintf(writer, "}\n")

	// Accept method
	fmt.Fprintf(
		writer,
		"func (%s *%s%s) Accept(visitor %sVisitor) (interface{}, error) {\n",
		strings.ToLower(baseName),
		typeName, baseName,
		baseName,
	)
	fmt.Fprintf(
		writer,
		"\treturn visitor.Visit%s%s(%s)\n",
		typeName, baseName,
		strings.ToLower(baseName),
	)
	fmt.Fprintf(writer, "}\n")
}
