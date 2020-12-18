
# Open questions

## Exercise 4
Happy is more efficion at parsing left-recursice rules. With the parse combinators, right recursive is more efficient.
## Exercise 10
Recursive call in the middle of a command sequence is creating more stack size. The last bit of commands are not handled yet, but new commands are
added, so the stack will grow.

Recursive call at the end of a command sequence is using less stack size, since some commands are handled already and are removed. Only the new commands are in the stack at that moment.