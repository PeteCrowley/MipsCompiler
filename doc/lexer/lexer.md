# Lexer

This lexer tokenizes a tiger programming language input file using ML-Lex.
It's mostly straightforward.

## General Operation
[src/lexer/tiger.lex] is compiled into a state machine by the ML-Lex tool.
regex matches correspond to lines of sml code which mostly function to instantiate Tokens into the lexer.

## Comments
Tiger recognizes nested inline comments of the form `/* */`
Upon detecting a `/*`, the state machine enters the `<comment>` state with a comment level of 1.
This state machine increments comment level on every comment start, and decrements it on every comment end.
Upon reaching a comment level of 0, the lexer reverts to the `<initial>` state.

## Strings

### Escape Sequences
yuck


