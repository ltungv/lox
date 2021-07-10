# Rlox

A bytecode virtual machine written in Rust for the Lox programming language.

## Implemented challenges

+ [x] Do inplace update on the stack for operators that pop a value and push back a value immediately after.
+ [ ] Dynamic VM' stack size. We already got this from Rust's Vec, but limiting ourself to 256.
+ [ ] Support `OP_CONSTANT_LONG` that takes a 24-bit number to extend the number of constants that can be contained.
+ [ ] Efficient storage for string constants and literals.
+ [ ] String interpolation.
  ```ruby
  var drink = "Tea";
  var steep = 4;
  var cool = 2;
  print "${drink} will be ready in ${steep + cool} minutes.";
  ```
+ [ ] Reuse variable name constant each time a variable is referenced.
+ [ ] Find better data structure for storing global variables.
+ [ ] ~Memory efficient encoding for line information~.

## Addtional features

+ [x] Report error at line number and column number.
