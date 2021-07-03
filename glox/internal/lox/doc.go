/*
Grammars

	program    --> decl* EOF ;
	decl       --> funDecl
	             | varDecl
	             | stmt ;
	funDecl    --> "fun" function ;
	function   --> IDENTIFIER "(" params? ")" block ;
	params     --> IDENTIFIER ( "," IDENTIFIER )* ;
	varDecl    --> "var" IDENTIFIER ( "=" expr )? ";" ;
	stmt       --> block
	             | exprStmt
	             | forStmt
	             | ifStmt
	             | printStmt
	             | returnStmt
	             | whileStmt ;
	block      --> "{" decl* "}" ;
	exprStmt   --> expr ";" ;
	forStmt    --> "for" "(" ( varDecl | exprStmt | ";" ) expr? ";" expr? ")" stmt ;
	ifStmt     --> "if" "(" expr ")" stmt ( "else" stmt )? ;
	printStmt  --> "print" expr ";" ;
	returnStmt --> "return" expr? ";" ;
	whileStmt  --> "while" "(" expr ")" stmt ;
	expr       --> assign ;
	assign     --> IDENTIFIER "=" expr ";"
	             | or ;
	or         --> and ( "or" and )* ;
	and        --> equality ( "and" equality )* ;
	equality   --> comparison ( ( "!=" | "==" ) comparison )* ;
	comparison --> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
	term       --> factor ( ( "-" | "+" ) factor )* ;
	factor     --> unary ( ( "/" | "*" ) unary )* ;
	unary      --> ( "!" | "-" | "+" | "/" | "*" ) unary
	             | call ;
	call       --> primary ( "(" args? ")" )* ;
	args       --> expr ( "," expr )* ;
	primary    --> NUMBER | STRING | IDENTIFIER
	             | "true" | "false" | "nil"
	             | "(" expr ")" ;
*/
package lox

/*
Notes

Unary rule has some matches for error generations:
+ Unary '+' expressions are not supported.
+ Unary '/' expressions are not supported.
+ Unary '*' expressions are not supported.

Handling function calls:
+ "(" acts like a postfix operator that has higer precedence than any other
operator.
+ The grammar rule for function call is quite complex so it can handle cases when
there's no argument, one argument, or multiple comma separated argument.
+ The number of arguments are limited to 255 to simplified the bytecode
representation of the bytecode interpreter
+ Checking
*/
