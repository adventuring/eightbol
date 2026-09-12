# BurgerMistress Language Quick Reference & Findings

## What is BurgerMistress?

BurgerMistress (aka "The Thief of Fate" by Burger Becky) is a hybrid BASIC-like + Prolog scripting language for 8-bit/16-bit game development. It supports:

### Core Language Features ✓
- **Variables:** DIM, INTEGER, STRING, BOOLEAN, DOUBLE
- **Control Flow:** IF/THEN/ELSE, WHILE/WEND, FOR/NEXT, DO/LOOP/UNTIL, EXIT
- **Functions:** SUB, FUNCTION, CALL, GOSUB, RETURN
- **I/O:** PRINT, INPUT with optional prompts
- **Operators:** +, -, *, /, =, <>, <, >, <=, >=, AND, OR, NOT, <<, >>
- **Number Formats:** Decimal, hex (&h/0x), octal (&o/0o), binary (&b/0b), dword (&d"...")
- **Dialogue:** Special DIALOGUE statement for NPC interactions with Prolog-style goals

---

## THE CRITICAL BUG: (:* and (:/ Instead of :multiply/:divide

### The Problem
**File:** `src/frontend-burgermistress/burger-parser.lisp`, lines 239-242

The parser emits **non-canonical** AST nodes for arithmetic:

```lisp
; WRONG:
(expression times expression
 (lambda (l r) (list :* l r)))          ; Emits (:* a b) ✗
(expression divide expression
 (lambda (l r) (list :/ l r)))          ; Emits (:/ a b) ✗
```

### What It Should Be

```lisp
; CORRECT:
(expression times expression
 (lambda (l r) (list :multiply l r)))   ; Emits (:multiply a b) ✓
(expression divide expression
 (lambda (l r) (list :divide l r)))     ; Emits (:divide a b) ✓
```

### Why This Matters
1. **Backends expect `:multiply` and `:divide`** nodes, not `(:* ...)` expressions
2. **AST pattern-matching breaks** for multiplication/division operations
3. **Code generation fails** for BurgerMistress math operations
4. **All other arithmetic works:** `:add` and `:subtract` are correct; only multiply/divide are broken

### Reference (BASIC frontend does it right):
```lisp
; From src/frontend-basic/basic-parser.lisp:
(defun basic-parse-expression-mul (e1 e2)
  (list :multiply e1 e2))  ; ✓ Correct
(defun basic-parse-expression-div (e1 e2)
  (list :divide e1 e2))    ; ✓ Correct
```

---

## OTHER AST ISSUES

### 2. INPUT Prompt Not Supported
**File:** `src/frontend-burgermistress/burger-parser.lisp`, line 95
- Parser tries to pass `:prompt` keyword to `make-input-node`
- But `make-input-node` in `src/ast.lisp` doesn't accept prompt parameter
- **Status:** Runtime error or silently ignored prompt

### 3. Shift Operators Not Parsed
**Status:** Documented but not implemented
- Lexer recognizes `<<` and `>>`
- No grammar rules to emit `:lshift` and `:rshift` nodes
- **Impact:** Shift operations in code produce parse errors

### 4. Array Subscripting Not Parsed
**Status:** Partially supported
- Array syntax `array[index]` is not parsed
- No `:subscript` node generation
- **Impact:** Array access statements will fail

### 5. FOR Loop STEP Parameter Not Extracted
**File:** `burger-parser.lisp`, lines 150-151
- Grammar accepts STEP but parser doesn't use it
- Only default step of 1 used regardless of STEP clause
- **Impact:** Incorrect FOR loop counting

### 6. Qualified Field Access Not Parsed
- `object.field` syntax tokenized but not parsed into `:of` nodes
- **Impact:** Nested field access will fail

---

## LEXER CAPABILITIES

### ✓ What Works
- **Number parsing:** Decimal, hex, octal, binary, dword formats
- **Keywords:** 37 keywords recognized (IF, WHILE, DIM, etc.)
- **Comments:** Line-starting apostrophe (') supported
- **String literals:** Double-quoted strings
- **Operators:** All standard arithmetic, relational, logical, bitwise

### ✓ Identifier Normalization
```
myVariable    → MyVariable
MY_FUNCTION   → My-function
player.score  → Player.score
```

---

## CANONICAL AST MAPPINGS

| Feature | Current | Expected | Status |
|---------|---------|----------|--------|
| Addition | `:add` | `:add` | ✓ OK |
| Subtraction | `:subtract` | `:subtract` | ✓ OK |
| Multiplication | `(:* a b)` | `:multiply` | ✗ **BUG** |
| Division | `(:/ a b)` | `:divide` | ✗ **BUG** |
| Left Shift | Not parsed | `:lshift` | ✗ MISSING |
| Right Shift | Not parsed | `:rshift` | ✗ MISSING |
| Array Access | Not parsed | `:subscript` | ✗ MISSING |
| Field Access | Not parsed | `:of` | ✗ MISSING |
| Assignment | `:move` | `:move` | ✓ OK |
| Control Flow | `:if`, `:perform` | `:if`, `:perform` | ✓ OK |
| Subroutines | `:call`, `:method` | `:call`, `:method` | ✓ OK |

---

## RECOMMENDED FIXES (Priority Order)

### 🔴 CRITICAL (Breaks Code Generation)
1. **Fix multiply/divide AST nodes**
   - Change line 240: `(list :* l r)` → `(list :multiply l r)`
   - Change line 242: `(list :/ l r)` → `(list :divide l r)`

2. **Fix INPUT prompt parameter**
   - Either extend `make-input-node` to support `:prompt` keyword
   - OR remove prompt support from parser if not canonical

### 🟠 HIGH PRIORITY (Missing Core Features)
3. **Add shift operator parsing**
   - Add grammar rules for `<<` and `>>`
   - Emit `:lshift` and `:rshift` nodes

4. **Add array subscripting**
   - Add grammar rule for `identifier[expression]`
   - Emit `:subscript` nodes

5. **Fix FOR STEP extraction**
   - Modify parser lambda to capture and use STEP value

### 🟡 MEDIUM PRIORITY (Compatibility)
6. **Add qualified field access**
   - Parse `object.field` syntax
   - Emit `:of` nodes (already tested by other frontends)

7. **Implement comprehensive test suite**
   - Current tests all skipped
   - Use BASIC/Fountain tests as template

8. **Backend validation**
   - Verify generated AST works with all 9+ backends
   - Test arithmetic, loops, I/O on actual targets

---

## UNSUPPORTED (BY DESIGN)

- Classes/OOP
- Exception handling
- Lambda expressions
- File I/O
- Dynamic arrays
- Recursion (stack-limited)
- Floating-point literals
- Inline comments

---

## FILES & LOCATIONS

| File | Purpose |
|------|---------|
| `src/frontend-burgermistress/burger-lexer.lisp` | Tokenization |
| `src/frontend-burgermistress/burger-parser.lisp` | Parse rules (YACC) |
| `doc/chapters/burgermistress_frontend.texi` | Language spec |
| `src/ast.lisp` | Canonical AST forms |
| `src/frontend-basic/basic-parser.lisp` | Reference (correct forms) |
| `tests/frontends/frontend-burgermistress-tests/` | Test suite |

---

**Research Status:** ✅ Complete  
**Document:** `/home/brpocock/Projects/eightbol/doc/chapters/burgermistress_research_audit.md`
