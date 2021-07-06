# Rlox

A bytecode virtual machine written in Rust for the Lox programming language.

## Implemented challenges

+ [x] Memory efficient encoding for line information
+ [x] ~Dynamic VM' stack size~ We already got this from Rust's Vec
+ [ ] Support `OP_CONSTANT_LONG` that takes a 24-bit number to extend the number of constants that can be contained
+ [ ] Do inplace update on the stack for operatos that pop a value and push back a value immediately after.
+ [ ] String interpolation
  ```ruby
  var drink = "Tea";
  var steep = 4;
  var cool = 2;
  print "${drink} will be ready in ${steep + cool} minutes.";
  ```

