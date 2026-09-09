# EIGHTBOL — Agent Guidelines

## Overview

EightBol is an EIGHTBOL compiler for 8-bit and 16-bit systems. It produces assembly for 6502, 65c02, 65c816, HuC6280, RP2A03, cp1610, Z80, SM83, m68k, i286, ARM7 (Thumb GAS aimed at Game Boy Advance / `armv4t`), F8 (Fairchild Channel F), and a stack-based virtual machine backend.

## Key Paths

| Item          | Path                             |
|---------------|----------------------------------|
| AST           | `src/ast.lisp`                   |
| Tests         | `tests/eightbol-tests.lisp`      |
| Documentation | `doc/EIGHTBOL.texi`, `README.md` |

Frontends are in `src/frontend-*/` and backends in `src/backend-*/`.

## Supported / Unsupported

- **Supported:** MOVE, ADD, SUBTRACT, COMPUTE, IF/THEN/ELSE, PERFORM, INVOKE, STRING DELIMITED BY SIZE (BLT), SET identifier TO expression, SET identifier UP BY expression, SET identifier DOWN BY expression, SET identifier TO NULL, CALL, GOBACK, EXIT METHOD, EXIT PROGRAM, EXIT, STOP RUN, LOG FAULT, DEBUG BREAK, INSPECT (TALLYING, CONVERTING, REPLACING CHARACTERS), EVALUATE (WHEN clauses), subscripted access, GOTO/GO TO, UNSTRING (partial), STRING with length delimiter
- **Unsupported (compile-time error):** DIVIDE, MULTIPLY, STRING with character delimiter, SET condition-name TO TRUE, SET ... TO NULLS, UNSTRING (without length), SET variants with TO expression not implemented

## Call Types (AST)

Local nullary: `:call :target name` — return value in accumulator (if any)  
Local unary: `:call-acc :target name :using expr` — ret.val. in acc. (if any)  
Method: `:invoke :object instance :method "Name"` — no explicit ret. val.  
Library nullary: `:call :target name :library t` — ret.val. in acc. (if any)  
Library unary: `:call-acc :target name :library t :using expr` — ret.val. in acc. (if any)  
Remote service: `:call :bank service-bank :target service-id` — no explicit ret. val.

## Numeric Types

Arithmetic operates on:
- Binary fixed point (any number of bytes; point at any fixed bit position)
- BCD fixed point (any number of bytes; point at any fixed nybble position)  
- Display characters (screen codes, e.g. ASCII, EBCDIC, PETSCII, minicode)

Binary and BCD must support adding/subtracting arbitrary bit width numbers with the output size being the determining factor of the scale: e.g., add a 1v15 number to a 15v1 number giving a 16v0 number with precision to 16v0. Bit-shift operations apply only to BINARY numbers.

## Testing

- Run: `(asdf:test-system :eightbol)` (includes `:phantasia-classes-compile` when the Phantasia tree and `Source/Generated/7800/Classes/Phantasia-Globals.cpy` exist).
- **FiveAM suites:** `:eightbol`, `:backend-matrix`, `:ast-optimize`, `:copybook-generation`, `:compile-regression`, `:phantasia-classes-compile`, `:phantasia-method-port`, `:backend-output`, `:parser-structure`, `:service-bank-lut`, `:numeric-precision-all-backends`. Run one suite: `(fiveam:run! :backend-matrix)`.

## Format / Style

- Prefer printing newline or fresh-line at the *start* of a line (e.g. `"~%content"`).
- Prefer `~10t` (and `~nT`) for indents instead of literal spaces.
- **Comments:** Use `;;` with normal indent. Use `;;;` or `;;;;` for section headers. End-of-line comments start at column 32 and run to column 72.
- **Assembly comments:** Same column 32–72 rule for end-of-line comments; promote long comments to block form above.
- **COBOL comments:** All comments begin with `*` in column 7.

## Documentation

- Update README, doc/EIGHTBOL.texi when adding backends or changing behavior
- Code comments and docstrings for non-obvious logic

## Build Plan

See `.cursor/rules/eightbol-build-plan.mdc` in the Phantasia project root for build targets, testing, and no-regression policy.

## 6.2. Decimal Usage in PIC

This feature allows:
- Use of 'S' in PIC for decimal: Counts as one nybble
- Usages: :BINARY, :DECIMAL (automatically picks up 'S' for one extra nybble)
- Examples: PIC: 99 (BINARY: 8 digits) vs PIC: S99 (BINARY: 9 digits)