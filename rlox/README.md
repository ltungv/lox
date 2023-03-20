# Rlox

A bytecode virtual machine written in Rust for the Lox programming language. This implementation uses Rust's reference counted object to manage memory, instead of a mark-and-sweep garbage collector.
As a result, memory is leaked when there's a reference cycle (see [reference_cycle.lox](../playground/reference_cycle.lox))

## Implemented challenges

+ [x] Do inplace update on the stack for operators that pop a value and push back a value immediately after.
+ [x] Check arity when calling native function
+ [x] Efficient storage for string constants and literals.
  + String literals are interned and stored in a global interner
  + Concaternating strings allocated a string object on the heap
+ [ ] Memory efficient encoding for line information.
+ [ ] Dynamic VM' stack size. We already got this from Rust's Vec, but limiting ourself to 256.
+ [ ] Support `OP_CONSTANT_LONG` that takes a 24-bit number to extend the number of constants that can be contained.
+ [ ] String interpolation.
  ```ruby
  var drink = "Tea";
  var steep = 4;
  var cool = 2;
  print "${drink} will be ready in ${steep + cool} minutes.";
  ```
+ [ ] Reuse variable name constant each time a variable is referenced.
+ [ ] Find better data structure for storing global variables.
+ [ ] Allow more than 256 local variables.
+ [ ] Const declaration.
+ [ ] Better data structure/algorithm for resoving variable at compile time.
+ [ ] Multi-way `switch` statement. Each case automatically jumps to the end of the switch statement after its statements are done, no `break` or `fallthrough`. Grammar
  ```
  switchStmt     → "switch" "(" expression ")"
                   "{" switchCase* defaultCase? "}" ;
  switchCase     → "case" expression ":" statement* ;
  defaultCase    → "default" ":" statement* ;
  ```
  1. Evaluate the switch value
  2. Walk the cases
    + Evaluate the expression of each case
    + If the evaluated value equal to the switch value, execute the statements under the case, then exit the `switch` statement.
    + If no case matches and there is a default case, execute its statements.
+ [ ] `continue`/`break` statement in loop.
