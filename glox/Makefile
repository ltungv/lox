default: fmt run

run:
	go run ./cmd/glox

build:
	rm -rf ./target/
	mkdir target
	go build -o ./target/ ./cmd/glox

fmt:
	go fmt -x ./cmd/glox
	go fmt -x ./internal/ast_codegen
	go fmt -x ./internal/errors
	go fmt -x ./internal/scanner
	go fmt -x ./internal/token

ast_codegen:
	go run ./internal/ast_codegen ./internal/ast/
