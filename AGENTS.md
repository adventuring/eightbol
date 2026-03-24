# EIGHTBOL — Agent Guidelines

## Overview

EightBol is an EIGHTBOL compiler for 8-bit and 16-bit systems. It produces assembly for 6502, 65c02, 65c816, HuC6280, RP2A03, cp1610, Z80, SM83, m68k, i286, ARM7 (Thumb GAS aimed at Game Boy Advance / `armv4t`), and F8 (Fairchild Channel F).

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

- **Supported:** MOVE, ADD, SUBTRACT, COMPUTE, IF/THEN/ELSE, PERFORM, INVOKE, STRING DELIMITED BY SIZE (BLT), SET identifier TO expression, CALL, GOBACK, EXIT METHOD, LOG FAULT, BREAK, INSPECT (TALLYING, CONVERTING, REPLACING CHARACTERS), EVALUATE (WHEN clauses), subscripted access
- **6502 family INVOKE:** `INVOKE Self` emits `.CallMethod Call{Class}{Method}, {Class}Class` (Phantasia `CallMethod` / `DoCallMethod`), not a bare `jsr Invoke…` label. Other CPUs keep their own dispatch text (`call Invoke…`, etc.).
- **6502 CALL:** Local near calls emit `jsr` unless a tail `CALL … GOBACK` is annotated for `jmp`; far/service/library paths use `.FarCall` / `.FarJSR` / `jsr Lib.…`.
- **Unsupported (compile-time error):** DIVIDE, MULTIPLY, UNSTRING, GOTO, STRING with character delimiter, SET variants (UP BY, DOWN BY, TO NULL, etc.)

## Testing

- Run: `(asdf:test-system :eightbol)` (includes `:phantasia-classes-compile` when the Phantasia tree and `Source/Generated/7800/Classes/Phantasia-Globals.cpy` exist).
- **FiveAM suites** (in order used by `test-op`): `:eightbol`, `:backend-matrix`, `:ast-optimize`, `:copybook-generation`, `:compile-regression`, `:phantasia-classes-compile`, `:phantasia-method-port`, `:backend-output`, `:parser-structure`, `:service-bank-lut`. Run one suite: `(fiveam:run! :backend-matrix)`.
- **Method port completeness:** when Phantasia is present, `:phantasia-method-port` checks `Classes.Defs` `#` introductions vs `.cob` `METHOD-ID`s, legacy `Source/Code/7800/Classes/*Class.s` `Method…` labels vs `.cob`, and that hyphenated `METHOD-ID` strings emit OOPS-compatible `MethodClassName` labels (6502). See `tests/phantasia-method-port-tests.lisp` and `Source/Documentation/EIGHTBOL-ClassDesignNotes.texi` (porting checklist).
- **`compile-method-ast-with-tables`** (`tests/eightbol-tests.lisp`): compiles a `(:method …)` AST with explicit `*slot-table*`, `*const-table*`, `*pic-width-table*`, etc. Supports all backends: 6502 family (`:6502` … `:huc6280`), `:z80`, `:cp1610`, `:sm83`, `:m68k`, `:i286`, `:arm7`, `:f8`.
- **Regression / matrix:** `tests/eightbol-tests.lisp` holds 6502-focused checks (globals vs `Character…` prefixes, `Song--Heal--ID` → `Song_Heal_ID`, `lax` vs `lda`/`tax` on plain `:6502` only, 16-bit `ADD` with PIC width 2). `tests/backend-matrix-tests.lisp` (`:backend-matrix`) mirrors MOVE/global and subscript label expectations for **Z80** and **cp1610**, and smoke-compiles `MOVE` for **SM83** / **m68k** / **i286** / **ARM7** / **F8**.
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

See `.cursor/rules/eightbol-build-plan.mdc` in the Phantasia project root for build targets, testing, and no-regression policy.

## Phantasia: COBOL replaces hand-written `*Class.s`

- Port behavior from `Source/Code/7800/Classes/<Name>Class.s` into `Source/Classes/<Name>.cob`, then delete the hand-written assembly once `Source/Generated/Classes/${CPUDIR}/<Name>Class.s` is produced by `bin/eightbol` and behavior is verified.
- Regenerated `Source/Generated/${PORT}/Makefile` lists `$(EIGHTBOL_CLASS_OUTPUTS)` as an order-only prerequisite on ROM (and test) bank assembly so missing generated class files cannot slip past the build.
- See `Source/Documentation/EIGHTBOL-ClassDesignNotes.texi` (sections “Porting `*Class.s` assembly to EIGHTBOL” and “Legacy module removal checklist”) for the full checklist and removal gates.
