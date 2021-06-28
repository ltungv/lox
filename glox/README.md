# Glox

A tree-walk interpreter for the Lox programming language. Lox is a very basic programming language created for the [Crafting Interpreters] book, written by [Bob Nystrom]. Visit [chapter 3] for an overview of the language. Also visit the [author's Github repository] to see the original implementation and many other implementations in different languages.

## Notes

Instead of using `panic` and `recover` in Go for error reporting and recovery, similar to Java's `throw` and `try-catch`, all of our failable methods return a tuple with the error object as the last element. Callers of the failable methods can check the last element of the returned tuple for possible error. If there's an error callers can decide to handle it or pass it up in the call stack.

I'll try to write some unit tests along the way, they probably just do some simple sanity checks. The [author's Github repository] contains a full test suite that can be used to test our final interpreter, so we will rely on that for better tests.


## Additional features / Implemented challenges

In additional to the basic features implemented by the author of the book, our implementation also supports:
+ Multi-line comments

[author's Github repository]: https://github.com/munificent/craftinginterpreters
[Bob Nystrom]: https://github.com/munificent
[chapter 3]: http://craftinginterpreters.com/the-lox-language.html
[Crafting Interpreters]: http://craftinginterpreters.com/
