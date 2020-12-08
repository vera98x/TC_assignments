
# Arrow
A framework for the Arrow assignment of the course Talen & Compilers at the University of Utrecht.

## Building
For building, use this command:

```
cabal v2-build
```

For testing in a GHCi shell, use this command:

```
cabal v2-repl
```

And then load in the file you want to test.

## File structure
The exercises are spread out over various (source) files. Please adhere to this distribution for the sake of your grader.
 - [open-questions.md](open-questions.md): Exercises 4 and 10
 - [src/Model.hs](src/Model.hs): Exercises 1 and 2
 - [src/Lexer.x](src/Lexer.x): Exercise 1, generating [src/Lexer.hs](src/Lexer.hs) with Alex
 - [src/Parser.y](src/Parser.y): Exercise 3, generating [src/Parser.hs](src/Parser.hs) with Happy
 - [src/Algebra.hs](src/Algebra.hs): Exercises 5 and 6
 - [src/Interpreter.hs](src/Interpreter.hs): Exercises 7, 8 and 9
 - [src/Driver.hs](src/Driver.hs): Exercise 11