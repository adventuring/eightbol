# EightBol - EIGHTBOL Compiler for 8-bit Systems

EightBol is an EIGHTBOL compiler that targets 8-bit microprocessors.

Eight-Bit-Oriented-Language.

There will also be support for some 16-bit CPUs.

## Usage

```bash

eightbol --version

eightbol --help

eightbol src/my-cool-file.cob -o obj/my-cool-file.s -m 6502 -I lib/

```

## Output stages supported

* 6502 (implemented)
* 65c02 (implemented, derived from 6502)
* 65c816 (implemented, derived from 6502)
* HuC6280 (implemented, derived from 6502)
* RP2A03 (implemented, NES CPU, derived from 6502)
* cp1610 (implemented, Intellivision)
* Z80 (implemented)
* SM83 (implemented, Game Boy)
* m68k (implemented, Motorola 68000)
* i286 (implemented, Intel 80286)
* ARM7 (implemented, ARM7TDMI)

## Pipeline

```
Source/Classes/{ClassName}.cob
  │
  ├─ Lexer (lexer.lisp) — COPY expansion inline at lex time
  │     ↓
  ├─ YACC Parser (parser.lisp) → AST plist
  │     ↓  written to Object/Classes/{ClassName}.eightbol
  │
  └─ Backends — one per CPU
        ↓  written to Source/Generated/Classes/{cpu}/{ClassName}.s
```

## Supported Statements

* **MOVE** — MOVE source TO destination
* **ADD** — ADD x TO y; ADD x TO y GIVING z
* **SUBTRACT** — SUBTRACT x FROM y; SUBTRACT x FROM y GIVING z
* **COMPUTE** — COMPUTE x = expression
* **IF/THEN/ELSE** — conditional execution
* **PERFORM** — PERFORM procedure-name; PERFORM UNTIL/THRU
* **INVOKE** — INVOKE object method-name
* **STRING** — STRING source DELIMITED BY SIZE INTO dest (block transfer only); supports reference modification `name(start:length)`
* **SET** — SET identifier TO expression (only)
* **CALL**, **CANCEL**, **GOBACK**, **EXIT METHOD**, **LOG FAULT**, **BREAK**

## Supported (parser + backends)

* **INSPECT** — TALLYING, CONVERTING, REPLACING CHARACTERS (all backends)
* **EVALUATE** — WHEN clauses (all backends)
* **Subscripted access** — `MOVE arr(idx) TO x`, `MOVE x TO arr(idx)`, etc.

## Unsupported (compile-time error)

* **DIVIDE**, **MULTIPLY**
* **STRING** with character delimiter (use DELIMITED BY SIZE for BLT)
* **UNSTRING**
* **GOTO** / **GO TO**
* **SET** — UP BY, DOWN BY, TO NULL/NULLS/SELF, TO ADDRESS OF, condition-name TO TRUE

## Testing

Run the test suite:

```bash
(asdf:test-system :eightbol)
```

Tests cover:
* **Lexer** — tokenization, prefixed literals, reference modification, picture-sequence, COPY expansion
* **Parser** — class structure, methods, statements (MOVE, ADD, SUBTRACT, COMPUTE, SET, IF, INVOKE), subscript
* **AST** — node structure (move, add, subtract, compute, set, refmod, string-blt), write/read round-trip
* **Unsupported statements** — DIVIDE, MULTIPLY, UNSTRING, GOTO, SET variants signal compile-time error
* **STRING BLT** — DELIMITED BY SIZE with reference modification `name(start:length)`
* **Backends** — 6502, 65c02, 65c816, cp1610, Z80, m68k, SM83, i286, ARM7; shift/bit ops; subscript

## EIGHTBOL Copybooks

Class slot definitions are generated automatically by `make-classes-for-oops`
from `Source/Classes/Classes.Defs` into `Source/Generated/{ClassName}.cpy`.
These are included into `.cob` files via `COPY ClassName.`

**COPY is required.** If a copybook cannot be found, the compiler signals a fatal error.
COPY statements cannot be ignored.

**Dependency tracking.** The compiler writes `Source/Generated/Classes/{ClassName}.d`
(Makefile dependency format) listing all assembly outputs and their dependencies
(the `.cob` source plus every copybook resolved during lex). Include these in your
Makefile for correct rebuilds when copybooks change.

Slot annotations in Classes.Defs:
* `.SlotName size` — default PIC (1 byte → `PIC 99`, 2 bytes → `PIC 9999`, n bytes → `OCCURS n`)
* `.SlotName size @ClassName` — `OBJECT REFERENCE ClassName` pointer
* `.SlotName size = PIC-string` — verbatim PIC clause
* `.SlotName size = VARCHAR(n) DEPENDING ON Field` — variable-length string (correct COBOL: `PIC X OCCURS 0 TO n TIMES DEPENDING ON Field`)

## License

MIT License - see LICENSE file for details.
