# Rlox

A bytecode virtual machine written in Rust for the Lox programming language.

## Implemented challenges

### From part 3

+ [ ] Dynamic VM' stack size. We already got this from Rust's Vec, but limiting ourself to 256.
+ [ ] Support `OP_CONSTANT_LONG` that takes a 24-bit number to extend the number of constants that can be contained
+ [ ] Do inplace update on the stack for operatos that pop a value and push back a value immediately after.
+ [ ] String interpolation
  ```ruby
  var drink = "Tea";
  var steep = 4;
  var cool = 2;
  print "${drink} will be ready in ${steep + cool} minutes.";
  ```
+ [ ] ~Memory efficient encoding for line information~

### From part 2

+ [ ] Multi-line comments
+ [ ] Expressive error report for binary operator appearing w/o LHS
  + By using *error productions* technique
  + Also parse and discard the RHS of that erroneous expression
+ [ ] Evaluate and print entered expression in REPL
+ [ ] Comma operator
+ [ ] C-style conditional `?:`
+ [ ] RuntimeError: Division by zero 
+ [ ] Accessing an uninitialized variable returns a runtime error
+ [ ] `break` statement in loops.
+ [ ] Make `print` a native function
+ [ ] Support anonymous functions
+ [ ] Report error if local variable is never used
+ [ ] Use array to stores variable for environment representation 
+ [ ] Static methods. See metaclasses used by Smalltalk and Ruby


## Addtional features

+ [x] Report error at line number and column number
