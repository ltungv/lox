PKG_CMD=./cmd/glox

TARGET_DIR=./target/

AST_CODEGEN_PKG=./internal/cmd/ast_codegen
AST_CODEGEN_OUT=./internal/lox/

default: fmt build

build:
	go build -o ${TARGET_DIR} ${PKG_CMD}

run:
	go run ${PKG_CMD}

codegen:
	go run ${AST_CODEGEN_PKG} ${AST_CODEGEN_OUT}

fmt:
	go fmt ./...

clean:
	rm -rf ${TARGET_DIR}

