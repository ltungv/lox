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

"unary" rule has some matches for error generations:
+ Unary '+' expressions are not supported.
+ Unary '/' expressions are not supported.
+ Unary '*' expressions are not supported.
*/
package lox
