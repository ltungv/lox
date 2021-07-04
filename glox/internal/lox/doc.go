/*
Grammars

	program    --> decl* EOF ;
	decl       --> classDecl
	             | funDecl
	             | varDecl
	             | stmt ;
	classDecl  --> "class" IDENTIFIER "{" function* "}" ;
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
	assign     --> ( call "." )? IDENTIFIER "=" expr ";"
	             | or ;
	or         --> and ( "or" and )* ;
	and        --> equality ( "and" equality )* ;
	equality   --> comparison ( ( "!=" | "==" ) comparison )* ;
	comparison --> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
	term       --> factor ( ( "-" | "+" ) factor )* ;
	factor     --> unary ( ( "/" | "*" ) unary )* ;
	unary      --> ( "!" | "-" | "+" | "/" | "*" ) unary
	             | call ;
	call       --> primary ( "(" args? ")" | "." IDENTIFIER )* ;
	args       --> expr ( "," expr )* ;
	primary    --> NUMBER | STRING | IDENTIFIER
	             | "true" | "false" | "nil" | "this"
	             | "(" expr ")" ;

"unary" rule has some matches for error generations:
+ Unary '+' expressions are not supported.
+ Unary '/' expressions are not supported.
+ Unary '*' expressions are not supported.
*/
package lox
