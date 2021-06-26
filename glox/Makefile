default: fmt build

run:
	go run ./cmd/glox

build:
	rm -rf ./target/
	mkdir target
	go build -o ./target/ ./cmd/glox

fmt:
	go fmt -x ./cmd/glox
	go fmt -x ./internal/cmd/ast_codegen
	go fmt -x ./internal/cmd/ast_printer
	go fmt -x ./internal/lox

ast_codegen:
	go run ./internal/cmd/ast_codegen ./internal/lox/
