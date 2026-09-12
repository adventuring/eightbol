# GOAL Language Research & Audit

**Language Name:** GOAL (Lisp-like language for game scripting)

**Status:** Partially Implemented Frontend (lexer + parser functional, tests mostly stubbed)  
**Last Updated:** 2026-09-09  
**Implementation Files:** `src/frontend-goal/goal-lexer.lisp`, `src/frontend-goal/goal-parser.lisp`  
**Test Files:** `tests/frontends/frontend-goal-tests/`  

---

## 1. WHAT IS GOAL? LANGUAGE & CONTEXT

### 1.1 Language Overview

GOAL is a **Lisp-like functional programming language** designed for retro game scripting and system-level programming on 8-bit and 16-bit systems. It combines:
- S-expression syntax (parenthesized prefix notation)
- Functional programming primitives (first-class functions, lambdas, closures)
- Imperative control flow (if/when/unless, loops, defun, defmethod)
- Multi-numeric type support (binary fixed-point, BCD, display characters)

### 1.2 Game Engine Context

GOAL is most famously associated with **Naughty Dog's proprietary game engine** used in:
- **Crash Bandicoot** series (PS1)
- **Jak and Daxter** series (PS2)
- Other PlayStation-era titles

However, EIGHTBOL's implementation is a **generalized retro adaptation** for 8-bit and 16-bit systems, providing a canonical Lisp dialect for game scripting across diverse CPU architectures.

### 1.3 Design Goals

- **Low-level integration:** Direct memory access via `#d"..."` dword literals
- **Cross-platform:** Compiles to assembly for 6502, 65c02, 65c816, Z80, m68k, ARM7, etc.
- **Game-friendly:** Built-in support for dialogue, narratives, and structured control flow
- **Type-safe arithmetic:** Explicit numeric type tracking (binary vs. BCD)

---

## 2. LANGUAGE SYNTAX & FEATURES

### 2.1 Number Formats

GOAL supports multiple number literal formats:

| Format | Syntax | Example | Notes |
|--------|--------|---------|-------|
| **Decimal** | Plain digits | `42`, `0`, `255` | Base-10 integers |
| **Hexadecimal** | `#x` prefix | `#xFF`, `#xDEADBEEF` | Case-insensitive, base-16 |
| **Octal** | `#o` prefix | `#o777`, `#o1000` | Base-8 |
| **Binary** | `#b` prefix | `#b1111`, `#b10101010` | Base-2 |
| **Dword** | `#d"..."` | `#d"WORD"`, `#d"12345678"` | String/memory literal for low-level ops |

### 2.2 Identifier Normalization

The lexer normalizes identifiers to **kebab-case** (lowercase with hyphens):

```
Source         →  Normalized
PlayerName     →  player-name
player_score   →  player-score
PLAYER_HEALTH  →  player-health
playerId       →  player-id
```

**Rules:**
- Underscores convert to hyphens
- Camel-case transitions become hyphens
- All letters downcase
- Result is consistent across AST operations

### 2.3 Operators

#### Arithmetic
| Operator | Description |
|----------|-------------|
| `+` | Addition → `(:add x y)` |
| `-` | Subtraction → `(:subtract x y)` |
| `*` | Multiplication → `(:* x y)` ⚠️ **BUG: Should be :multiply** |
| `/` | Division → `(:/ x y)` ⚠️ **BUG: Should be :divide** |
| `%` | Modulo → `(:mod x y)` |

#### Comparison
| Operator | Symbol | AST Node |
|----------|--------|----------|
| `=` | Equal | `(= x y)` |
| `!=` | Not equal | `(/= x y)` |
| `<` | Less than | `(< x y)` |
| `>` | Greater than | `(> x y)` |
| `<=` | Less than or equal | `(<= x y)` |
| `>=` | Greater than or equal | `(>= x y)` |
| `==` | Identity equal | Not implemented in parser |

#### Logical
| Operator | Description |
|----------|-------------|
| `and` | Logical AND → `(:and x y)` |
| `or` | Logical OR → `(:or x y)` |
| `not` | Logical NOT → `(:not x)` |

#### Bitwise
| Operator | Description | Implementation |
|----------|-------------|-----------------|
| `&` | Bitwise AND | ✓ Lexed as `:BITAND` |
| `` | `` | Bitwise OR | ✓ Lexed as `:BITOR` |
| `^` | Bitwise XOR | ✓ Lexed as `:BITXOR` |
| `~` | Bitwise NOT | ✓ Lexed as `:BITNOT` |
| `<<` | Left shift | ✓ Lexed as `:LSHIFT` |
| `>>` | Right shift | ✓ Lexed as `:RSHIFT` |

#### Access & Dereference
| Operator | Symbol |
|----------|--------|
| Member access | `.` (`:DOT`) |
| Pointer deref | `->` (`:ARROW`) |

### 2.4 Assignment & Variable Definition

**SET/SETQ (Assignment):**
```lisp
(set! variable value)
(setq variable value)
```

Both emit `:set` node:
```lisp
(make-set-node (goal-normalize-identifier var) value-expr)
```

**LET Binding (Local Variables):**
```lisp
(let ((x 10) (y 20))
  (+ x y))

(let* ((x 10)
       (y (+ x 5)))
  (* x y))
```

Emits sequence of `:set` nodes followed by body statements.

### 2.5 Control Flow

#### IF/WHEN/UNLESS
```lisp
(if test-expr then-branch else-branch)
(when test-expr body...)
(unless test-expr body...)
```

**AST Output:**
```lisp
(make-if-node test-expr
              (mapcan #'goal-form-to-statements then)
              (mapcan #'goal-form-to-statements else))
```

#### COND / CASE / LOOP
Defined in keyword list but **NOT IMPLEMENTED in parser** (lines 71-76 of lexer):
```lisp
(:cond . "cond")
(:case . "case")
(:loop . "loop")
(:do . "do")
(:while . "while")
(:until . "until")
```

### 2.6 Function Definition & Invocation

#### DEFUN (Nullary & Unary Functions)
```lisp
(defun add-ten (x)
  (+ x 10))

(defun get-player-name ()
  "Hero")
```

**AST Output:** `:method` node with normalized name
```lisp
(make-method-node (goal-normalize-identifier name)
                  :statements (mapcan #'goal-form-to-statements body))
```

#### DEFMETHOD (Class Methods)
```lisp
(defmethod player update (delta)
  (set! health (- health 1)))
```

**AST Output:** `:method` node with class-method naming:
```lisp
(make-method-node (format nil "~a-~a"
                          (goal-normalize-identifier class)
                          (goal-normalize-identifier method-name))
                  :statements ...)
```

#### Function Calls (Invocation)
```lisp
(function-name arg1 arg2 ...)
```

**AST Output:**
```lisp
(make-call-node (goal-normalize-identifier op)
                :args (mapcar #'goal-form-to-expr (cdr form)))
```

Or for special cases:
```lisp
(list :call (goal-normalize-identifier op)
      :args (mapcar #'goal-form-to-expr (cdr form)))
```

#### RETURN
```lisp
(return value)
(return)
```

**AST Output:**
```lisp
(list :exit-method :value (goal-form-to-expr value))
(list :exit-method)
```

### 2.7 Special Forms

Supported by keyword definitions:

| Form | Symbol | Parser Status |
|------|--------|---------------|
| `defun` | `:DEFUN` | ✓ Implemented |
| `defmethod` | `:DEFMETHOD` | ✓ Implemented |
| `deftype` | `:DEFTYPE` | Lexed only (⚠️ not parsed) |
| `let` / `let*` | `:LET`, `:LETSTAR` | ✓ Implemented |
| `if` / `when` / `unless` | `:IF`, `:WHEN`, `:UNLESS` | ✓ Implemented |
| `cond` / `case` | `:COND`, `:CASE` | Lexed only (⚠️ not parsed) |
| `loop` / `do` / `while` / `until` | Loop keywords | Lexed only (⚠️ not parsed) |
| `quote` / `quasiquote` / `unquote` | Quote variants | Lexed; quote handled minimally |
| `lambda` | `:LAMBDA` | Lexed only (⚠️ not parsed) |

### 2.8 I/O Operations

**Not Implemented** in current Goal parser. Lexer recognizes no specific I/O keywords beyond generic function calls.

Retro systems typically use platform-specific I/O:
- Screen writes (device-specific assembly)
- Keyboard reads (interrupt-driven or polling)
- Memory-mapped I/O (via `#d"..."` literals)

---

## 3. IMPLEMENTATION STATUS

### 3.1 Frontend Files

```
src/frontend-goal/
├── goal-lexer.lisp      (282 lines)  ✓ Functional
└── goal-parser.lisp     (224 lines)  ✓ Functional (limited)
```

### 3.2 Lexer (goal-lexer.lisp)

**Complete Features:**
- ✓ Number parsing (decimal, hex, octal, binary, dword)
- ✓ Keyword tokenization (94 keywords defined)
- ✓ Operator recognition (17 operators)
- ✓ Identifier normalization (kebab-case)
- ✓ String literal scanning (with escape sequences)
- ✓ Comment skipping (semicolon to end-of-line)
- ✓ Delimiter pairing (parens, brackets, braces)

**Line-by-Line Breakdown:**
- Lines 6–45: Number parsing with radix support
- Lines 47–63: Identifier normalization to kebab-case
- Lines 65–95: Keyword and operator association lists
- Lines 128–251: `goal-lex-line` — tokenize single source line
- Lines 253–260: `goal-lex-source` — tokenize full source string
- Lines 262–276: `goal-lex-token` — interactive token reading
- Lines 277–282: Token list thunk generator

### 3.3 Parser (goal-parser.lisp)

**Implemented:**
- ✓ Atom/number parsing
- ✓ S-expression (list) parsing
- ✓ `defun` → `:method` node
- ✓ `defmethod` → `:method` node (with class prefix)
- ✓ `let` / `let*` → sequence of `:set` nodes
- ✓ `if` / `when` / `unless` → `:if` node
- ✓ `set!` / `setq` → `:set` node
- ✓ `return` → `:exit-method` node
- ✓ Arithmetic (`+`, `-`, `*`, `/`, `%`)
- ✓ Comparison (`=`, `!=`, `<`, `>`, `<=`, `>=`)
- ✓ Logical (`and`, `or`, `not`)
- ✓ Quote/quasiquote minimal handling
- ✓ Top-level form parsing

**Not Implemented:**
- ⚠️ `cond` — keyword recognized, no parser rule
- ⚠️ `case` — keyword recognized, no parser rule
- ⚠️ Loop constructs (`loop`, `do`, `while`, `until`) — keywords recognized, no parser rules
- ⚠️ `lambda` — keyword recognized, no parser rule
- ⚠️ Array/subscript access (`array[index]`)
- ⚠️ Nested field access (`object.field`)
- ⚠️ Pointer dereference (`ptr->field`)
- ⚠️ Bitwise operators — lexed but not parsed
- ⚠️ Progn (sequential evaluation) — minimal handling

**Line-by-Line Breakdown:**
- Lines 7–17: AST node constructors
- Lines 19–29: Function/method definition builders
- Lines 31–41: `let` binding to SET statement conversion
- Lines 37–41: Conditional builders (if/when/unless)
- Lines 43–45: Quote node builder
- Lines 47–49: Number parsing dispatcher
- Lines 51–94: `goal-form-to-statements` — convert forms to statement lists
- Lines 96–144: `goal-form-to-expr` — convert forms to expressions
- Lines 146–155: Top-level parsing entry point
- Lines 157–182: Single form parsing
- Lines 184–201: S-expression (list) parsing

### 3.4 Tests

```
tests/frontends/frontend-goal-tests/
├── package.lisp                  ✓ Package definition
├── lexer-tests.lisp              ⚠️ All tests marked "Implementation pending"
├── parser-tests.lisp             ⚠️ All tests marked "Implementation pending"
├── functions-tests.lisp          ⚠️ All tests marked "Implementation pending"
├── variable-names-tests.lisp     ⚠️ All tests marked "Implementation pending"
├── numeric-types-tests.lisp      ⚠️ All tests marked "Implementation pending"
└── integration-tests.lisp        ⚠️ All tests marked "Implementation pending"
```

**Status:** Test suites are **scaffolded but not executed** — all tests are marked `(skip "Implementation pending")`.

---

## 4. CRITICAL BUG: (:* and (:/ INSTEAD OF :multiply/:divide

### 4.1 Bug Location

**File:** `src/frontend-goal/goal-parser.lisp`  
**Lines:** 117–120  
**Function:** `goal-form-to-expr`

### 4.2 Current (Incorrect) Code

```lisp
((:multiply * MULTIPLY)
 (list :* (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
((:divide / DIVIDE)
 (list :/ (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
```

### 4.3 Issue & Impact

The parser emits **non-canonical AST nodes**:

| Operation | Current Output | Expected Output |
|-----------|-----------------|-----------------|
| Multiply | `(:* left right)` | `(:multiply left right)` or structured form |
| Divide | `(:/ left right)` | `(:divide left right)` or structured form |

**Consequences:**
1. **Backend Incompatibility:** Backends expect `:multiply` and `:divide` keyword symbols, not `(:* ...)` and `(:/ ...)` raw lists
2. **Pattern-Matching Failures:** Code using case/match on `:multiply` will miss Goal's `(:* ...)` forms
3. **Code Generation Failures:** Backend emit routines will not recognize the non-canonical nodes
4. **Same Bug as BurgerMistress:** This is identical to the bug documented in `doc/chapters/burgermistress_research_audit.md` (§3, lines 290–366)

### 4.4 Reference Implementations

**BASIC Frontend (`src/frontend-basic/basic-parser.lisp`, lines 139–142):**
```lisp
(defun basic-parse-expression-mul (e1 e2)
  (list :multiply e1 e2))

(defun basic-parse-expression-div (e1 e2)
  (list :divide e1 e2))
```

**Addition/Subtraction (Correct in Goal):**
```lisp
((:add + ADD)
 (list :add (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
((:subtract - SUBTRACT)
 (list :subtract (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
```

### 4.5 Recommended Fix

Replace lines 117–120 in `goal-parser.lisp`:

**Before:**
```lisp
((:multiply * MULTIPLY)
 (list :* (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
((:divide / DIVIDE)
 (list :/ (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
```

**After:**
```lisp
((:multiply * MULTIPLY)
 (list :multiply (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
((:divide / DIVIDE)
 (list :divide (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
```

---

## 5. ADDITIONAL IMPLEMENTATION GAPS

### 5.1 Unimplemented Loop Constructs

**Keywords Recognized:**
- `loop`, `do`, `while`, `until`

**Parser Coverage:** **MISSING** — No grammar rules for loops

**Current State:**
- Lexer tokenizes loop keywords correctly
- Parser has no case statements to handle them
- Attempting `(loop ...)`, `(while ...)` etc. will error

**Recommended Fix:**
Add parser rules similar to `if`:
```lisp
((:loop LOOP)
 (list (goal-loop-node ...)))
((:while WHILE)
 (list (goal-while-node ...)))
```

### 5.2 Unimplemented Pattern Matching (COND/CASE)

**Keywords Recognized:** `cond`, `case`

**Parser Coverage:** **MISSING**

**Current State:**
- Lexed but not parsed
- Would require more complex matching logic

**Example Syntax:**
```lisp
(cond
  (condition1 action1)
  (condition2 action2)
  (t default))

(case value
  ((a b) action-ab)
  ((c d) action-cd)
  (t default))
```

### 5.3 Unimplemented Lambda & Higher-Order Functions

**Keyword:** `lambda`  
**Status:** Lexed only; no parser rule  
**Example:**
```lisp
(lambda (x y) (+ x y))
(map (lambda (x) (* x 2)) list)
```

### 5.4 Unimplemented Bitwise Operators

**Lexer Tokens:**
- `:BITAND`, `:BITOR`, `:BITXOR`, `:BITNOT`, `:LSHIFT`, `:RSHIFT`

**Parser Coverage:** **MISSING** — No case statements

**Recommended Fix:**
```lisp
((:and BITAND)
 (list :and-bitwise ...))
((:lshift LSHIFT)
 (list :lshift ...))
```

### 5.5 Unimplemented Array/Subscript Access

**Tokens:** `:lbracket` `[`, `:rbracket` `]`

**Parser Coverage:** **MISSING**

**Syntax:**
```lisp
(aref array index)
(setf (aref array index) value)
```

Or via subscript node:
```lisp
(list :subscript :array array :index index)
```

### 5.6 Missing Progn (Sequential Execution)

**Keyword:** Recognized but minimal handling (line 87)  
**Current Handler:**
```lisp
((:progn PROGN)
 (mapcan #'goal-form-to-statements (cdr form)))
```

**Status:** Partially working but not explicitly tested

---

## 6. BACKEND COMPATIBILITY

### 6.1 Supported Backends

GOAL can compile to **all EIGHTBOL backends** (when implementation is complete):

- ✓ 6502 / 65c02 / 65c816 (Commodore, Atari, SNES, etc.)
- ✓ HuC6280 (PC Engine / TurboGrafx-16)
- ✓ RP2A03 (NES)
- ✓ cp1610 (Intellivision)
- ✓ Z80 (Sega, Game Boy, CP/M, etc.)
- ✓ SM83 (Game Boy, Game Boy Color)
- ✓ m68k (Motorola 68000 — Amiga, Atari ST, Sega Genesis)
- ✓ i286 (Intel 80286 — PC, Compaq)
- ✓ ARM7 (ARM7TDMI — Game Boy Advance)
- ✓ F8 (Fairchild Channel F)
- ✓ Stack-based VM

### 6.2 Backend Status for Goal

**Integration:** Goal is registered in `eightbol-test.asd` and `eightbol.asd` test suites  
**Current Barrier:** `:* :/ bug` blocks arithmetic-heavy programs from compiling

---

## 7. CANONICAL AST STRUCTURE

### 7.1 Core AST Nodes for Goal

| Expression Type | Canonical Form | Status |
|-----------------|----------------|--------|
| Move/Assign | `:move :from expr :to var` | ✓ |
| Add | `(:add x y)` | ✓ |
| Subtract | `(:subtract x y)` | ✓ |
| Multiply | `(:multiply x y)` | ⚠️ Currently `(:* x y)` |
| Divide | `(:divide x y)` | ⚠️ Currently `(:/ x y)` |
| Modulo | `(:mod x y)` | ✓ |
| Comparison | `(op x y)` where op in `{=, /=, <, >, <=, >=}` | ✓ |
| Logical AND | `(:and x y)` | ✓ |
| Logical OR | `(:or x y)` | ✓ |
| Logical NOT | `(:not x)` | ✓ |
| Conditional | `(:if test then-branch else-branch)` | ✓ |
| Function Call | `(:call name :args (arg1 arg2 ...))` | ✓ |
| Method Def | `(:method name :statements (...))` | ✓ |
| Set Variable | `(:set var value)` | ✓ |
| Return | `(:exit-method :value expr)` | ✓ |

### 7.2 Specialized Nodes Not Yet Used

These are available in canonical AST but not yet employed in Goal parser:

- `:subscribe` — array subscripting
- `:perform` — iterative loops
- `:dialogue` — narrative/conversation
- `:print` / `:input` — I/O operations
- `:invoke` — method invocation on typed objects
- Bitwise operations (`:lshift`, `:rshift`, `:and-bitwise`, etc.)

---

## 8. TESTING STRATEGY & CONFORMANCE

### 8.1 Current Test Gaps

All test suites are **stubbed with `(skip "Implementation pending")`:**

- **Lexer tests** (6 tests) — All skipped
- **Parser tests** (7 tests) — All skipped
- **Functions tests** (5 tests) — All skipped
- **Variable names tests** (4 tests) — All skipped
- **Numeric types tests** (5 tests) — All skipped
- **Integration tests** (4 tests) — All skipped

**Total:** 31 test cases awaiting implementation

### 8.2 Recommended Test Implementation Order

1. **Lexer tests** — Verify tokenization of all literal formats, keywords, operators
2. **Parser tests** — Verify arithmetic, conditionals, function definitions
3. **Arithmetic bug fix** — Validate `:multiply` and `:divide` emit correctly
4. **Numeric type tests** — Verify binary/BCD/character numeric handling
5. **Integration tests** — End-to-end compile of realistic Goal programs
6. **Unimplemented features tests** — Document current gaps (loop, cond, lambda)

### 8.3 Suggested Test Cases

**Example 1: Basic Arithmetic**
```lisp
(defun compute-damage ()
  (let ((base 10)
        (multiplier 2))
    (* base multiplier)))
```

Expected AST should contain `(:multiply ...)` not `(:* ...)`.

**Example 2: Conditional Logic**
```lisp
(defun check-health (health)
  (if (> health 0)
      (print "Alive")
      (print "Dead")))
```

Expected: `:if` node with correct branches.

**Example 3: Multi-Statement Method**
```lisp
(defmethod player update (dt)
  (set! position (+ position (* velocity dt)))
  (if (< position 0)
      (set! position 0)))
```

Expected: `:method` node with two statements in body.

---

## 9. RECOMMENDATIONS FOR CONFORMANCE

### 9.1 Priority 1: Critical Bug Fix

**Fix the `:* :/ Bug**
- Change lines 117–120 in `src/frontend-goal/goal-parser.lisp`
- Replace `(:* ...)` → `(:multiply ...)`
- Replace `(:/ ...)` → `(:divide ...)`
- Add test to verify fix

**Estimated effort:** 5 minutes

### 9.2 Priority 2: Implement Loop Constructs

Add parser rules for `loop`, `do`, `while`, `until`:
- Emit `:perform` nodes with appropriate modifiers
- Cross-reference BASIC/Fountain parsers for patterns
- Add tests for each loop type

**Estimated effort:** 1–2 hours

### 9.3 Priority 3: Implement Pattern Matching

Add `cond` and `case` parsing:
- `cond` → multiple `:if`-then-else chains or specialized node
- `case` → dispatch node with multiple branches
- Add tests

**Estimated effort:** 2–3 hours

### 9.4 Priority 4: Implement Lambda & Higher-Order Functions

Support first-class functions:
- `lambda` → `:lambda` node with parameters and body
- Higher-order operations (map, filter, reduce) via library calls
- Add tests

**Estimated effort:** 2–3 hours

### 9.5 Priority 5: Bitwise Operators

Implement bitwise operations:
- Add parser rules for `&`, `|`, `^`, `~`, `<<`, `>>`
- Emit `:lshift`, `:rshift`, `:and-bitwise`, `:or-bitwise`, etc.
- Add tests

**Estimated effort:** 1–2 hours

### 9.6 Priority 6: Array/Subscript Access

Implement array subscripting:
- Parse `array[index]` syntax (currently lexed but not parsed)
- Emit `:subscript :array x :index i` nodes
- Add tests

**Estimated effort:** 1–2 hours

### 9.7 Priority 7: Finalize Tests

Execute full test suite:
- Implement all 31 currently-skipped tests
- Ensure lexer, parser, and numeric handling pass
- Add regression tests for `:* :/ bug` fix

**Estimated effort:** 3–4 hours

---

## 10. SUMMARY TABLE

| Aspect | Status | Notes |
|--------|--------|-------|
| **Language Name** | GOAL | Lisp-like retro game scripting language |
| **Lexer Implementation** | ✓ Complete | 282 lines; all number formats, keywords, operators |
| **Parser Implementation** | ⚠️ Partial | 224 lines; core features work, loops/lambda/bitwise missing |
| **Number Formats** | ✓ Full | Decimal, hex, octal, binary, dword literals |
| **Operators** | ⚠️ Partial | Arithmetic/comparison/logical ✓, bitwise ⚠️ |
| **Control Flow** | ⚠️ Partial | if/when/unless ✓, loop/cond/case ⚠️ |
| **Functions** | ✓ Full | defun, defmethod, return ✓ |
| **I/O Operations** | ✗ None | Not implemented; requires platform-specific bindings |
| **Multiply/Divide Bug** | ⚠️ Critical | Emits `(:* ...)` `(:/ ...)` instead of `:multiply` `:divide` |
| **Test Coverage** | ✗ None | 31 tests stubbed as "Implementation pending" |
| **Backend Support** | ✓ All | Can target 11 CPU architectures (once bugs fixed) |
| **Identifier Normalization** | ✓ Complete | Kebab-case; underscores/camelCase handled correctly |

---

## 11. RESEARCH REFERENCES

### 11.1 Related EIGHTBOL Frontends

- **BurgerMistress** (`doc/chapters/burgermistress_research_audit.md`) — Has identical `:* :/ bug`
- **Fountain** — Lisp-like language (similar design)
- **SCUMM** — Game scripting (narrative features)
- **BASIC** — Procedural language (comparison baseline)

### 11.2 External Context

- **Naughty Dog GOAL** — PS1/PS2 game development language
- **Lisp Language Family** — S-expressions, prefix notation, functional paradigms
- **Retro Game Development** — Targeting 8-bit/16-bit systems with limited memory

### 11.3 AST Specification

- `src/ast.lisp` — Canonical node definitions
- `doc/EIGHTBOL.texi` — EIGHTBOL language specification
- `README.md` — Project overview

---

**Document prepared by:** Research Agent  
**Date:** 2026-09-09  
**Review Status:** Awaiting conformance fixes
