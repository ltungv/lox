default: fmt run

run:
	go run ./cmd/glox

build:
	rm -rf ./target/
	mkdir target
	go build -o ./target/ ./cmd/glox

fmt:
	go fmt -x ./cmd/glox
	go fmt -x ./cmd/ast_printer
	go fmt -x ./internal/lox
	go fmt -x ./internal/lox/ast_codegen

ast_codegen:
	go run ./internal/cmd/ast_codegen ./internal/lox/
