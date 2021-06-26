default: fmt run

run: codegen
	go run ./cmd/glox

build: codegen
	rm -rf ./target/
	mkdir target
	go build -o ./target/ ./cmd/glox

fmt: codegen
	go fmt -x ./cmd/glox
	go fmt -x ./internal/codegen
	go fmt -x ./internal/errors
	go fmt -x ./internal/scanner
	go fmt -x ./internal/token

codegen:
	go run ./internal/codegen
