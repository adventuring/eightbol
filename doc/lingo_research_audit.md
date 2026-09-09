# Lingo Language Research: Comparative Analysis Against EIGHTBOL Canonical AST

## Executive Summary

Lingo is a verbose, event-driven scripting language for Macromedia (now Adobe) Director that was dominant in multimedia production during the 1990s-2000s. This research identifies key constructs, architectural differences from canonical EIGHTBOL AST, and **critical issues** that need fixing in the Lingo frontend implementation.

### Critical Issues Found

1. **AND/OR/NOT Shadowing Canonical Constructors**: Lingo parser defines `make-conditional-and/or/not` using **Lisp symbols** (`'and`, `'or`, `'not`) instead of canonical **keyword symbols** (`:and`, `:or`, `:not`).

2. **:compute Misuse for Arithmetic**: Multiplication and division expressions incorrectly use `:compute` with embedded operators instead of canonical `:multiply` and `:divide` AST nodes.

---

## 1. Language Overview

**Lingo** (Macromedia Director Lingo Script)
- Verbose, natural-language-like syntax
- Event-driven scripting for multimedia timeline and sprites
- Object-oriented with inheritance via "ancestor" property
- Behavioral (scripts attached to sprites/frames)
- Dynamically typed
- Case-insensitive identifiers (normalized to Header-Case)

**Timeline**: Invented 1989 by John H. Thompson; shipped with Director 2.2; peak usage 1990s-early 2000s

---

## 2. Lingo Constructs vs. EIGHTBOL Canonical AST

### 2.1 Assignment/Variable Definition

**Lingo Syntax:**
```lingo
set myVariable = 100
myVariable = 100
move 50 to myVar
set the property of object = value
object.property = value
property myPropName
global gGlobalVar
```

**Canonical EIGHTBOL AST:**
- `:move :from expr :to identifier` — variable assignment
- `:set :target identifier :value expr` — alternative assignment (less common in parser output)

**Lingo Status:** ✓ Mostly mapped correctly via `:move` nodes

---

### 2.2 Control Flow: if/then/else

**Lingo Syntax:**
```lingo
if condition then
    statements
end if

if condition then
    statements
else
    statements
end if

if x > 10 then
    -- statements
else if y < 5 then
    -- statements  
else
    -- statements
end if
```

**Canonical EIGHTBOL AST:**
- `:if :condition expr :then stmts :else stmts`

**Lingo Status:** ✓ Correctly generates `:if` nodes via `make-if-node`

---

### 2.3 Loop Constructs

#### Numeric Loop (FOR)
**Lingo Syntax:**
```lingo
repeat with i = 1 to 10
    -- statements
end repeat

repeat with i = 1 to 100 step 5
    -- statements
end repeat

repeat with i = 10 down to 1
    -- statements
end repeat
```

**Canonical EIGHTBOL AST:**
- `:perform :procedure name :from start :to end :by step :body stmts`

**Lingo Status:** ✓ Maps to `:perform` nodes

#### While Loop
**Lingo Syntax:**
```lingo
repeat while condition
    -- statements
end repeat
```

**Canonical EIGHTBOL AST:**
- Would map to `:perform :until (make-conditional-not condition) :body stmts`

**Lingo Status:** ⚠ Generates comment-only node (NOT IMPLEMENTED)

#### Until Loop
**Lingo Syntax:**
```lingo
repeat
    -- statements
until condition
end repeat
```

**Canonical EIGHTBOL AST:**
- `:perform :until condition :body stmts`

**Lingo Status:** ⚠ Generates comment-only node (NOT IMPLEMENTED)

#### Loop Control
**Lingo Syntax:**
```lingo
exit repeat
next repeat
```

**Canonical EIGHTBOL AST:**
- `:exit-method` or `:exit-program` (EIGHTBOL doesn't support loop-local exit)

**Lingo Status:** ✗ NOT SUPPORTED

---

### 2.4 Subroutine/Handler Calls

**Lingo Syntax:**
```lingo
handlerName()
handlerName(arg1, arg2)
call "libRoutine" of "LibraryName"
object:#method(arg1)
```

**Canonical EIGHTBOL AST:**
- `:call :target name` — nullary local call (ret.val in accumulator)
- `:call-acc :target name :using expr` — unary call with argument
- `:invoke :object obj :method "Name"` — method on object
- `:call :target name :library t` — library call

**Lingo Status:** ⚠ Partial (only basic `:call` and `:invoke` mapped)

---

### 2.5 I/O Operations

**Lingo Syntax:**
```lingo
put "Hello"
put gScore
put "Score:" && gScore
get userName
input playerChoice
print "Message"
print "Score:", score
```

**Canonical EIGHTBOL AST:**
- `:print :expressions (list expr1 expr2 ...)`
- `:input :variables (list var1 var2 ...)`

**Lingo Status:** ✓ Maps `put` → `:print`, `get`/`input` → `:input`

---

### 2.6 Operators

#### Arithmetic

| Lingo | Op | Canonical AST | Status |
|-------|----|----|--------|
| `a + b` | Addition | `:add :from a :to b :giving nil` | ✓ via `make-expression-add` |
| `a - b` | Subtraction | `:subtract :subtrahend b :from a :giving nil` | ✓ via `make-expression-subtract` |
| `a * b` | **Multiplication** | **`:multiply :by a :multiplier b :giving nil`** | **✗ BROKEN** |
| `a / b` | **Division** | **`:divide :numerator a :denominator b :giving nil`** | **✗ BROKEN** |
| `a ^ b` | Exponentiation | ??? | ✗ NOT SUPPORTED |

**CRITICAL ISSUE #2:**
```lisp
;; Current BROKEN implementation in lingo-parser.lisp:
(defun make-expression-multiply (left right)
  (list :compute :target 'result :expression (list '* left right)))

(defun make-expression-divide (left right)
  (list :compute :target 'result :expression (list '/ left right)))
```

**Should be:**
```lisp
(defun make-expression-multiply (left right)
  (list :multiply :by left :multiplier right :giving nil))

(defun make-expression-divide (left right)
  (list :divide :numerator left :denominator right :giving nil))
```

#### Bitwise Operations

**Lingo Syntax:**
```lingo
bitShift(value, 2)      -- left shift 2 bits
bitShift(value, -1)     -- right shift 1 bit
```

**Canonical EIGHTBOL AST:**
- `:shift-left expr n`
- `:shift-right expr n`

**Lingo Status:** ✗ NOT SUPPORTED (only functions, not operators)

#### Logical Operators

| Lingo | Canonical AST | Current Impl | Status |
|-------|---|---|--------|
| `a and b` | **`:and c1 c2`** | **`(list 'and left right)`** | **✗ CRITICAL BUG** |
| `a or b` | **`:or c1 c2`** | **`(list 'or left right)`** | **✗ CRITICAL BUG** |
| `not a` | **`:not cond`** | **`(list 'not expr)`** | **✗ CRITICAL BUG** |

**CRITICAL ISSUE #1:**

Current implementation in `lingo-parser.lisp` (lines 304-314):
```lisp
(defun make-conditional-or (left right)
  "Create a conditional OR node."
  (list 'or left right))  ;; <-- WRONG: using Lisp symbol 'or

(defun make-conditional-and (left right)
  "Create a conditional AND node."
  (list 'and left right))  ;; <-- WRONG: using Lisp symbol 'and

(defun make-conditional-not (expr)
  "Create a conditional NOT node."
  (list 'not expr))  ;; <-- WRONG: using Lisp symbol 'not
```

**Should be** (matching grammar-build.lisp canonical forms):
```lisp
(defun make-conditional-or (left right)
  (list :or left right))  ;; Keyword :or

(defun make-conditional-and (left right)
  (list :and left right))  ;; Keyword :and

(defun make-conditional-not (expr)
  (list :not expr))  ;; Keyword :not
```

#### Comparison Operators

| Lingo | Meaning | Canonical | Status |
|-------|---------|-----------|--------|
| `a = b` | Equal | `(= a b)` | ✓ |
| `a <> b` | Not equal | `(/= a b)` | ✓ |
| `a != b` | Not equal (alt) | `(/= a b)` | ✓ |
| `a < b` | Less than | `(< a b)` | ✓ |
| `a > b` | Greater than | `(> a b)` | ✓ |
| `a <= b` | Less or equal | `(<= a b)` | ✓ |
| `a >= b` | Greater or equal | `(>= a b)` | ✓ |

**Lingo Status:** ✓ Comparison operators correct (use Lisp-style symbols)

---

### 2.7 String Operations

**Lingo Syntax:**
```lingo
"Hello"
'Hello'
str1 && str2     -- concatenation
str.length()     -- string length (method call)
str.char(n)      -- character access
str.offset(substring)
str.field(delim, n)
```

**Canonical EIGHTBOL AST:**
- String literals as self-representing values
- String concatenation not natively supported (use explicit operations)

**Lingo Status:** ⚠ String literals work; concatenation `&&` not implemented

---

### 2.8 Data Structures

#### Lists
**Lingo Syntax:**
```lingo
myList = [1, 2, 3]
item i of myList
set item i of myList = value
```

**Canonical EIGHTBOL AST:**
- `:subscript name index` — subscripted identifier access

**Lingo Status:** ⚠ Partial (array indexing not fully implemented)

#### Properties/Fields
**Lingo Syntax:**
```lingo
property pX
property pHealth
me.pX = 100
me.pHealth = 50
the pX of me
```

**Canonical EIGHTBOL AST:**
- `:of slot obj` — qualified identifier (slot OF obj)

**Lingo Status:** ✓ Maps to `:of` nodes

---

### 2.9 Special Lingo Features (Domain-Specific)

#### Sprites and Channels
**Not Canonical EIGHTBOL** — specific to Director's timeline/sprite system
- Sprite properties: visible, loc, rect, member, etc.
- Channel operations: sprite(n), frame properties

**Status:** ✗ NOT SUPPORTED (would require Director runtime binding)

#### Behaviors and Events
**Lingo handlers are event-driven:**
```lingo
on startMovie        -- movie starts
on exitFrame         -- frame exits
on enterFrame        -- frame enters
on mouseDown         -- mouse clicked
on keyDown           -- key pressed
```

**Canonical EIGHTBOL:** Maps to regular method definitions (events not preserved in AST)

**Status:** ⚠ Event context lost; treated as regular procedures

#### Object Model (Parent Scripts)
```lingo
on new me, x, y
    me.pX = x
    me.pY = y
    return me
end

-- Create instance:
myObject = new(script "Player", 10, 20)
```

**Canonical EIGHTBOL:** No native OO constructor syntax

**Status:** ✗ NOT SUPPORTED (would require special method handling)

---

## 3. Numeric Type Handling

**Lingo Supports:**
- Integers (32-bit signed)
- Floats (IEEE 754 double)
- Automatic type coercion

**EIGHTBOL Supports:**
- Binary fixed-point (arbitrary width, arbitrary scale)
- BCD fixed-point (arbitrary width, arbitrary nybble scale)
- Display characters (PETSCII, ASCII, EBCDIC, minicode)

**Status:** Incompatibility — Lingo has no fixed-point; EIGHTBOL computations assume fixed-point precision. Conversion layer would be needed.

---

## 4. Error Handling and Debugging

**Lingo:**
- No native try/catch
- Runtime errors halt script
- `put` used for debugging output
- Limited debugging support

**EIGHTBOL:**
- `:log-fault :code expr` — structured fault logging
- `:debug-break :code expr` — debug breakpoints

**Status:** ✓ Can map debug statements; fault logging requires translation

---

## 5. Documentation Summary: Key Findings

From `/home/brpocock/Projects/eightbol/doc/chapters/lingo_macromedia_director_lingo_script.texi`:

**Naming:** Case-insensitive, normalized to Header-Case-With-Hyphens
```
myVariable → My-Variable
playerHealth → Player-Health
SPRITE_COUNT → Sprite-Count
```

**Keywords:** ~60 core keywords (on, end, if, then, else, repeat, while, for, etc.)

**Number Formats:**
- Decimal: `123`
- Hex: `0xFF`, `$FF`, `$hFF`
- Octal: `0o77`, `$o77`
- Binary: `0b1010`, `%1010`, `$b1010`

**Comments:**
- Line: `--`
- Block: `(* ... *)`

**Operators Precedence (High to Low):**
```
not, *, /, +, -, =, <>, <, >, <=, >=, and, or
```

---

## 6. Issues Requiring Fixes

### Issue #1: AND/OR/NOT Canonical Constructors (CRITICAL)

**Files:**
- `/home/brpocock/Projects/eightbol/src/frontend-lingo/lingo-parser.lisp` (lines 304-314)

**Problem:**
```lisp
(defun make-conditional-or (left right)
  (list 'or left right))  ;; Lisp symbol, not keyword

(defun make-conditional-and (left right)
  (list 'and left right))  ;; Lisp symbol, not keyword

(defun make-conditional-not (expr)
  (list 'not expr))  ;; Lisp symbol, not keyword
```

**Fix:**
Use keyword symbols to match canonical AST (see `src/grammar-build.lisp`):
```lisp
(defun make-conditional-or (left right)
  (list :or left right))

(defun make-conditional-and (left right)
  (list :and left right))

(defun make-conditional-not (expr)
  (list :not expr))
```

**Impact:** All boolean expressions with AND/OR/NOT will have wrong AST representation; backends expecting `:and`/`:or`/`:not` will fail.

---

### Issue #2: :compute Misuse for Arithmetic (CRITICAL)

**File:**
- `/home/brpocock/Projects/eightbol/src/frontend-lingo/lingo-parser.lisp` (lines 348-354)

**Problem:**
```lisp
(defun make-expression-multiply (left right)
  "Create a multiplication expression node."
  (list :compute :target 'result :expression (list '* left right)))

(defun make-expression-divide (left right)
  "Create a division expression node."
  (list :compute :target 'result :expression (list '/ left right)))
```

**Issues:**
1. Uses `:compute` instead of `:multiply`/`:divide`
2. Embeds raw Lisp operators instead of canonical AST
3. Uses `'result` (symbol) as target instead of proper identifier
4. `:compute` with literal `* / ` operators is non-canonical

**Fix (from grammar-build.lisp):**
```lisp
(defun make-expression-multiply (e1 e2)
  (list :multiply :by e1 :multiplier e2 :giving nil))

(defun make-expression-divide (e1 e2)
  (list :divide :numerator e1 :denominator e2 :giving nil))
```

**Impact:**
- Backend codegen expects `:multiply` and `:divide` AST nodes
- Backends have handlers for `:multiply` and `:divide` but may not process `:compute` with embedded operators
- All arithmetic expressions will fail to compile

---

### Issue #3: Incomplete Loop Implementation

**File:**
- `/home/brpocock/Projects/eightbol/src/frontend-lingo/lingo-parser.lisp` (lines 166-173)

**Problem:**
While loops and until loops generate comment nodes instead of proper AST:
```lisp
(while expression do statement-list end
  (lambda (cond body)
    (declare (ignore cond))
    (list :comment "While loop body")))  ;; Not executable!

(repeat statement-list until expression end
  (lambda (body cond)
    (declare (ignore cond))
    (list :comment "Repeat loop body")))  ;; Not executable!
```

**Fix:**
```lisp
(while expression do statement-list end
  (lambda (cond body)
    (list :perform :until (make-conditional-not cond) :body body)))

(repeat statement-list until expression end
  (lambda (body cond)
    (list :perform :until cond :body body)))
```

**Impact:** While and until loops won't execute; they'll be ignored or cause compilation errors.

---

### Issue #4: Missing Function Definitions

**File:**
- `/home/brpocock/Projects/eightbol/src/frontend-lingo/lingo-parser.lisp`

**Missing functions:**
- `lingo-normalize-identifier` — called but defined only in lexer
- `lingo-parse-number` — needed for number handling (in lexer, not parser)
- Comparison helpers: `make-conditional-eq`, etc. are defined but redundant

**Fix:** Import from lexer or remove redundancy

---

### Issue #5: Assignment Statement Inconsistency

**File:**
- `/home/brpocock/Projects/eightbol/src/frontend-lingo/lingo-parser.lisp` (lines 126-135)

**Problem:**
```lisp
(set-stmt
 (set ident to expression
      (lambda (target value)
        (list :move :from value :to (lingo-make-ident target)))))

(move-stmt
 (on expression to ident
     (lambda (value target)
       (list :move :from value :to (lingo-make-ident target)))))
```

The `move-stmt` rule uses `on` keyword (from Lingo's `on` handler definition), which is **incorrect syntax**. Should be `move`:

```lingo
move 100 to myVariable  ;; Correct Lingo
on 100 to myVariable    ;; Wrong!
```

**Fix:**
```lisp
(move-stmt
 (move expression to ident
       (lambda (value target)
         (list :move :from value :to (lingo-make-ident target)))))
```

---

## 7. Audit Issues Summary

| Issue # | Category | Severity | File | Line | Fix Complexity |
|---------|----------|----------|------|------|---|
| 1 | AND/OR/NOT symbols | CRITICAL | lingo-parser.lisp | 304-314 | Low |
| 2 | :compute vs :multiply/:divide | CRITICAL | lingo-parser.lisp | 348-354 | Low |
| 3 | While/until loops | HIGH | lingo-parser.lisp | 166-173 | Low |
| 4 | Move stmt keyword | HIGH | lingo-parser.lisp | 133 | Trivial |
| 5 | Missing imports | MEDIUM | lingo-parser.lisp | Various | Low |
| 6 | Exponentiation | LOW | N/A | N/A | Medium |
| 7 | String concatenation (&&) | LOW | N/A | N/A | Low |
| 8 | Loop-local exit | LOW | N/A | N/A | N/A |

---

## 8. Comparison Matrix: Lingo vs. Canonical AST

| Construct | Lingo Syntax | Canonical AST | Parser Support | Status |
|-----------|---|---|---|---|
| Assignment | `x = 10` | `:move :from 10 :to X` | ✓ | OK |
| Addition | `a + b` | `:add :from a :to b :giving nil` | ✓ | OK |
| Subtraction | `a - b` | `:subtract :subtrahend b :from a :giving nil` | ✓ | OK |
| **Multiply** | **`a * b`** | **`:multiply :by a :multiplier b :giving nil`** | **✗** | **BROKEN** |
| **Divide** | **`a / b`** | **`:divide :numerator a :denominator b :giving nil`** | **✗** | **BROKEN** |
| **AND** | **`a and b`** | **`:and a b`** | **✗** | **BROKEN** |
| **OR** | **`a or b`** | **`:or a b`** | **✗** | **BROKEN** |
| **NOT** | **`not a`** | **`:not a`** | **✗** | **BROKEN** |
| If/Then/Else | `if ... then ... else ... end if` | `:if :condition c :then t :else e` | ✓ | OK |
| For Loop | `repeat with i = 1 to 10 ... end repeat` | `:perform :procedure i :from 1 :to 10 :body ...` | ✓ | OK |
| **While Loop** | **`repeat while c ... end repeat`** | **`:perform :until (NOT c) :body ...`** | **✗** | **STUB** |
| **Until Loop** | **`repeat ... until c end repeat`** | **`:perform :until c :body ...`** | **✗** | **STUB** |
| Function Call | `func()` | `:call :target func` | ✓ | OK |
| Method Call | `obj:#method()` | `:invoke :object obj :method "Method"` | ✓ | OK |
| Print | `put "text"` | `:print :expressions (...)` | ✓ | OK |
| Input | `get x` | `:input :variables (x)` | ✓ | OK |
| Property Access | `obj.prop` | `:of prop obj` | ✓ | OK |

---

## 9. Recommendations

### Immediate Fixes (Before Compilation)
1. **Fix canonical operators** (Issue #1): Replace Lisp symbols with keywords (:and, :or, :not)
2. **Fix arithmetic AST** (Issue #2): Replace :compute with :multiply/:divide
3. **Implement loops** (Issue #3): Generate proper :perform nodes for while/until
4. **Fix move keyword** (Issue #4): Change `on` to `move` in move-stmt rule

### Short-Term Improvements
5. Export/import shared functions (normalize-identifier, parse-number)
6. Add exponentiation support (`^` → function call or shift operation)
7. Implement string concatenation (`&&`)
8. Add support for bitShift function

### Long-Term Enhancements
9. Implement array/list operations (subscripting, iteration)
10. Add support for parent script constructors (new)
11. Document event-handler mapping to methods
12. Consider Director runtime binding layer for sprites/channels

### Testing Strategy
- Unit tests for each operator and construct
- Regression tests for existing working features
- Backend output validation (6502, Z80, etc.) to ensure AST generates correct assembly
- Parity tests with other frontends (BASIC, COBOL, Pascal, etc.)

---

## 10. Reference Materials

**Lingo Documentation Files:**
- `/home/brpocock/Projects/eightbol/doc/chapters/lingo_macromedia_director_lingo_script.texi`
- `/home/brpocock/Projects/eightbol/frontend_plans/lingo_task.md`

**EIGHTBOL Canonical References:**
- `/home/brpocock/Projects/eightbol/src/ast.lisp` — AST shape definitions
- `/home/brpocock/Projects/eightbol/src/grammar-build.lisp` — Canonical node constructors

**Frontend Implementation:**
- `/home/brpocock/Projects/eightbol/src/frontend-lingo/lingo-lexer.lisp`
- `/home/brpocock/Projects/eightbol/src/frontend-lingo/lingo-parser.lisp`

**Comparables (Working Frontends):**
- BASIC: `src/frontend-basic/`
- COBOL: `src/frontend-cobol/`
- Pascal: `src/frontend-pascal/`

---

## Conclusion

Lingo is a sophisticated multimedia scripting language with significant semantic distance from EIGHTBOL's COBOL-like canonical AST. The parser implementation has **2 critical bugs** (AND/OR/NOT symbols, :compute misuse) and **3-4 high-priority issues** (loop stubs, keyword errors). All are straightforward to fix and will enable proper code generation across backends.

The main architectural challenge is Lingo's Domain-Specific nature: sprite manipulation, event handlers, and Director timeline operations don't map directly to EIGHTBOL's procedure-oriented model. However, core control flow, operators, and data movement constructs are fully expressible in the canonical AST.

**Estimated fix time:** 2-4 hours for critical+high issues; 1-2 weeks for comprehensive implementation including array operations and advanced features.
