# EIGHTBOL — Agent Guidelines

## Overview

EightBol is an EIGHTBOL compiler for 8-bit and 16-bit systems. It produces assembly for 6502, 65c02, 65c816, HuC6280, RP2A03, cp1610, Z80, SM83, m68k, i286, ARM7.

## Key Paths

| Item          | Path                             |
|---------------|----------------------------------|
| Lexer         | `src/lexer.lisp`                 |
| Parser        | `src/parser.lisp`                |
| AST           | `src/ast.lisp`                   |
| 6502 backend  | `src/backend-6502.lisp`          |
| Tests         | `tests/eightbol-tests.lisp`      |
| Documentation | `doc/EIGHTBOL.texi`, `README.md` |

## Supported / Unsupported

- **Supported:** MOVE, ADD, SUBTRACT, COMPUTE, IF/THEN/ELSE, PERFORM, INVOKE, STRING DELIMITED BY SIZE (BLT), SET identifier TO expression, CALL, GOBACK, EXIT METHOD, LOG FAULT, BREAK
- **Unsupported (compile-time error):** DIVIDE, MULTIPLY, UNSTRING, INSPECT, GOTO, EVALUATE, STRING with character delimiter, SET variants (UP BY, DOWN BY, TO NULL, etc.)

## Testing

- Run: `(asdf:test-system :eightbol)`
- Regression tests must prevent recurrence of known errors
- New tests must address added functionality

## Format / Style

- Prefer printing newline or fresh-line at the *start* of a line rather than the end (e.g. `"~%content"` so the newline belongs to the line it introduces).
- Prefer `~10t` (and `~nT`) for indents instead of literal spaces.
- **Comments:** Use `;;` with normal indent for the surrounding block (line-length and end-of-line comments). Use `;;;` or `;;;;` for section headers. End-of-line (shared-line) comments start at column 32 and run up to column 72; if a comment would exceed that space, promote it to a `;;` comment on the line(s) before.
- **Assembly comments:** Same column 32–72 rule for end-of-line comments; promote long comments to block form above.
- **COBOL comments:** All comments begin with `*` in column 7.

## Documentation

- Update README, doc/EIGHTBOL.texi, and AGENTS.md when adding backends or changing behavior
- Code comments and docstrings for non-obvious logic

## Build Plan

See `.cursor/rules/eightbol-build-plan.mdc` in the Phantasia project for testing and no-regression policy.
