# Tiger Compiler Project Description

This repository contains a semester-long compiler construction project for ECE/COMPSCI 553. The project implements a compiler for the Tiger programming language (from Appel's textbook) in Standard ML, with a backend that emits MIPS assembly.

## Goals

- Build a full compiler pipeline for a non-trivial language.
- Practice core compiler techniques: lexical analysis, parsing, semantic analysis, intermediate representation (IR), instruction selection, liveness analysis, and register allocation.
- Validate each stage with automated differential tests.

## Compiler Pipeline

The compiler is organized into stages that correspond to classic compiler passes:

1. Lexing: tokenizes Tiger source programs.
2. Parsing: builds abstract syntax trees (ASTs).
3. Escape analysis: determines variable escape behavior.
4. Semantic analysis and translation: performs type checking and translates AST to IR.
5. Linearization and instruction selection: lowers IR to MIPS-like assembly instructions.
6. Liveness analysis and register allocation: constructs interference graphs and assigns temporaries to machine registers.
7. Code emission: writes final assembly, including runtime support.

The main driver in `src/main.sml` resets compiler state for each input file, runs parse + semantic translation, partitions generated fragments (string and procedure fragments), emits `.data` and `.text` sections, and appends runtime assembly from `src/assemruntime/runtime.s`.

## Project Structure

- `src/lexer`: lexer definitions, token types, and lexer drivers.
- `src/parser`: grammar, AST definitions, symbol tables, parser output helpers.
- `src/semant`: semantic analysis, type system, IR trees, frame and temporary
	abstractions, translation logic.
- `src/selection`: linearization and MIPS code generation.
- `src/liveness`: control-flow graphs, liveness computation, graph coloring, and
	register allocation.
- `src/assemruntime`: assembly runtime support linked into generated output.
- `tests`: test driver, manifests, expected outputs, and stage-specific diff baselines.

## Testing Strategy

Tests are organized by compiler stage (for example: `lex`, `parse`,
`typecheck`, `ir`, `selection`, `liveness`, `regalloc`, and `main`). Input programs are grouped in `tests/input/*`, expected outputs live in `tests/expected/*`, and generated outputs are written to `tests/output/*`.

The test harness (documented in `tests/TESTS.md`) supports running named test suites from a manifest. Many input programs are drawn from the Appel Tiger test set, and diff-based comparisons are used to detect regressions.
