# Muddle Language Research & EIGHTBOL AST Audit

## Executive Summary

Muddle (MIT Design Language, originally called "MDL" = Model Development Language) is a Lisp-like language developed at MIT's Project MAC in 1971. Critical findings from this audit:

1. **CRITICAL BUG FOUND:** Three frontends emit `:go-back` instead of `:goback`
2. **Documentation bug:** Doc claims Muddle maps to `:go-back` (should be `:goback`)
3. **No tests verify correct AST output** for return statements in Muddle frontend
4. **Return semantics inconsistent** across Lingo, SCI, and Muddle frontends

---

## 1. Muddle Language Overview (Historical Context)

### Heritage & Purpose
- **Created:** 1971 at MIT Project MAC by Gerald Sussman, Carl Hewitt, Chris Reeve, Bruce Daniels
- **Originally called:** Muddle (self-deprecating humor; later sanitized to "MDL")
- **Final version:** 105 (1980)
- **Original platforms:** PDP-10 (ITS, TENEX, TOPS-20), later VAX, Apollo/Domain, BSD, AEGIS

### Key Influences & Legacy
- **Based on:** Lisp
- **Influenced by:** Scheme (Sussman/Steele), Planner (Hewitt), Common Lisp, Prolog, Smalltalk, actor model
- **Famous use:** Zork (interactive fiction)—spawned Infocom's ZIL (Zork Implementation Language)

### Characteristic Features
- S-expression syntax (prefix notation with parentheses)
- Multiple built-in data types: lists, strings, arrays, user-defined types
- Multithreading and coroutines
- Dual-valued variables (local scope + global value for inter-scope passing)
- Advanced debugging and incremental development
- Lexical scoping (not dynamic)

---

## 2. Muddle Language Constructs (Analysis vs. EIGHTBOL AST)

### 2.1 Assignment / Variable Definition

**Muddle syntax:**
```lisp
(.SET var expr)
```

**Maps to EIGHTBOL AST:**
```lisp
(:move :from expr :to var)
```

**Status:** ✓ Correctly implemented in `muddle-parser.lisp:55-57`

---

### 2.2 Control Flow

#### 2.2.1 Conditional Execution (`.IF` / `.ELSE` / `.THEN` / `.ENDIF`)

**Muddle syntax:**
```lisp
(.IF condition then-form)
(.IF condition then-form else-form)
(.IF condition (.THEN then-form) (.ELSE else-form))
```

**Maps to EIGHTBOL AST:**
```lisp
(:if :condition cond :then stmts :else stmts)
```

**Implementation:** `muddle-parser.lisp:34-36`
```lisp
(defun muddle-parse-if (cond then-form &optional else-form)
  "(.IF cond then-form else-form) - Conditional branching."
  (make-if-node cond (list then-form) (if else-form (list else-form) '())))
```

**Status:** ✓ Correct

---

#### 2.2.2 Pre-Test Loop (`.WHILE` / `.ENDWHILE`)

**Muddle syntax:**
```lisp
(.WHILE condition body-form)
```

**Maps to EIGHTBOL AST:**
```lisp
(:perform :procedure "WHILE" :until (NOT condition) :body stmts)
```

**Implementation:** `muddle-parser.lisp:38-41`
```lisp
(defun muddle-parse-while (cond body)
  "(.WHILE cond body) - Looping until condition becomes false."
  (make-perform-node (if (listp body) body (list body))
                     :until (make-conditional-not cond)))
```

**Status:** ✓ Correct

---

#### 2.2.3 Count-Controlled Loop (`.FOR` / `.ENDFOR`)

**Muddle syntax:**
```lisp
(.FOR var start end body-form)
```

**Maps to EIGHTBOL AST:**
```lisp
(:perform :varying var :from start :by 1 :until (> var end) :body stmts)
```

**Implementation:** `muddle-parser.lisp:43-49`
```lisp
(defun muddle-parse-for (var start end body)
  "(.FOR var start end body) - Counted loop from start to end."
  (make-perform-node (if (listp body) body (list body))
                     :varying var
                     :from start
                     :by 1
                     :until (make-conditional-gt (make-identifier var) end)))
```

**Status:** ✓ Correct (loop exits when `var > end`, which is correct for inclusive lower bound + 1-based increment)

---

#### 2.2.4 Unconditional Jump (`.GO`)

**Muddle syntax:**
```lisp
(.GO label)
```

**Maps to EIGHTBOL AST:**
```lisp
(:goto :target label)
```

**Implementation:** `muddle-parser.lisp:97-99`
```lisp
(defun muddle-parse-go (label)
  "(.GO label) - Unconditional jump (legacy)."
  (make-goto-node label))
```

**Status:** ✓ Correct

---

### 2.3 Return / Function Exit (`.RETURN`)

**Muddle syntax:**
```lisp
(.RETURN)
(.RETURN value)
```

**Expected EIGHTBOL AST:**
```lisp
(:goback)
```

**ACTUAL Implementation:** `muddle-parser.lisp:71-73`
```lisp
(defun muddle-parse-return (&optional val)
  "(.RETURN val) - Return from function."
  (list :go-back))  ;; ❌ BUG: emits :go-back instead of :goback
```

**Canonical Constructor:** `grammar-build.lisp:37-39`
```lisp
(defun make-goback-node ()
  "Build a :goback AST node."
  (list :goback))
```

**Status:** ❌ **CRITICAL BUG** — emits `:go-back` instead of `:goback`

**Documentation (doc/chapters/muddle_frontend.texi:413):**
```
Maps to Eightbol's @code{:go-back} node.
```
❌ **Documentation BUG** — says `:go-back`, should say `:goback`

---

### 2.4 Function / Method Definition (`.DEFINE`)

**Muddle syntax:**
```lisp
(.DEFINE name params body)
(.DEFINE name (param1 param2 ...) body-form)
```

**Maps to EIGHTBOL AST:**
```lisp
(:method :method-id name :statements body)
```

**Implementation:** `muddle-parser.lisp:51-53`
```lisp
(defun muddle-parse-define (name params body)
  "(.DEFINE name params body) - Function/method definition."
  (make-method-node name :statements (if (listp body) body (list body))))
```

**Status:** ✓ Correct

---

### 2.5 Function/Form Invocation (`.CALL`)

**Muddle syntax:**
```lisp
(.CALL func)
(.CALL func arg1 arg2 ...)
```

**Maps to EIGHTBOL AST:**
```lisp
(:call :target name)
```

**Implementation:** `muddle-parser.lisp:67-69`
```lisp
(defun muddle-parse-call (func &rest args)
  "(.CALL func args...) - Function invocation."
  (list :call :target func))
```

**Issue:** Does not handle arguments. EIGHTBOL supports `:call :using expr` for unary calls.

**Status:** ⚠️ **INCOMPLETE** — args are silently ignored; no support for `:call :using` or `:call-acc`

---

### 2.6 Arithmetic & Logic Operators

#### 2.6.1 Arithmetic

**Muddle operators:** `+`, `-`, `*`, `/`, `^` (XOR or power?)

**EIGHTBOL support:**
- `:add`, `:subtract`, `:multiply`, `:divide`
- `:shift-left`, `:shift-right`
- `:bit-and`, `:bit-or`, `:bit-xor`

**Parser Implementation:** `muddle-parser.lisp:214-226`
```lisp
(expression plus expression
            (lambda (l r) (make-expression-add l r)))
(expression minus expression
            (lambda (l r) (make-expression-subtract l r)))
(expression times expression
            (lambda (l r) (make-expression-multiply l r)))
(expression divide expression
            (lambda (l r) (make-expression-divide l r)))
(expression power expression
            (lambda (l r) (make-expression-bit-xor l r)))
```

**Issue:** `.power` operator (^) mapped to `:bit-xor`. In Muddle, `^` is XOR; power is a function, not operator.

**Status:** ✓ Correct for Muddle (XOR interpretation)

#### 2.6.2 Comparisons

**Muddle operators:** `=`, `/=`, `<`, `>`, `<=`, `>=`

**Implementation:** `muddle-parser.lisp:227-238`
```lisp
(expression equal expression
            (lambda (l r) (make-conditional-eq l r)))
(expression ne expression
            (lambda (l r) (make-conditional-ne l r)))
(expression lt expression
            (lambda (l r) (make-conditional-lt l r)))
(expression gt expression
            (lambda (l r) (make-conditional-gt l r)))
(expression le expression
            (lambda (l r) (make-conditional-le l r)))
(expression ge expression
            (lambda (l r) (make-conditional-ge l r)))
```

**Status:** ✓ Correct

#### 2.6.3 Logical Operators

**Muddle operators:** `AND`, `OR`, `NOT`

**Implementation:** `muddle-parser.lisp:239-250`
```lisp
(and expression expression
     (lambda (l r) (make-conditional-and l r)))
(and form-list
     (lambda (forms) 
       (reduce (lambda (acc f) (make-conditional-and acc f)) (cdr forms) :initial-value (car forms))))
(or expression expression
    (lambda (l r) (make-conditional-or l r)))
(or form-list
    (lambda (forms)
      (reduce (lambda (acc f) (make-conditional-or acc f)) (cdr forms) :initial-value (car forms))))
(not expression
     (lambda (f) (make-conditional-not f)))
```

**Status:** ✓ Correct

---

### 2.7 Data Structures

**Muddle native types:**
- Lists: `(1 2 3)`
- Vectors/Arrays: `<1 2 3>`
- Atoms: symbols, numbers, strings
- Forms (S-expressions)
- User-defined types

**EIGHTBOL AST support:**
- Lists → represented as nested `:subscript` or `:of` expressions
- No explicit vector/array representation in canonical AST
- Atoms → literals or identifiers

**Muddle Frontend Status:** 
- **Lexer** (muddle-lexer.lisp) tokenizes string literals and numbers
- **Parser** does not distinguish list vs. vector syntax
- Data structure handling is **incomplete**; primarily focused on scalar values

**Status:** ⚠️ **INCOMPLETE** — Limited data structure support

---

### 2.8 I/O Operations

#### 2.8.1 Output (`.PRINT` / `.DISPLAY`)

**Muddle syntax:**
```lisp
(.PRINT arg1 arg2 ...)
(.DISPLAY arg1 arg2 ...)
```

**Maps to EIGHTBOL AST:**
```lisp
(:print :expressions (arg1 arg2 ...))
```

**Implementation:** `muddle-parser.lisp:75-77`
```lisp
(defun muddle-parse-print (&rest args)
  "(.PRINT arg1 arg2...) or (.DISPLAY arg1 arg2...) - Output text/values."
  (list :print :expressions (or args '())))
```

**Status:** ✓ Correct

#### 2.8.2 Input (`.INPUT` / `.ACCEPT`)

**Muddle syntax:**
```lisp
(.INPUT var1 var2 ...)
(.ACCEPT var1 var2 ...)
```

**Maps to EIGHTBOL AST:**
```lisp
(:input :variables (var1 var2 ...))
```

**Implementation:** `muddle-parser.lisp:79-81`
```lisp
(defun muddle-parse-input (&rest vars)
  "(.INPUT var1 var2...) or (.ACCEPT var1 var2...) - Read input values."
  (list :input :variables (or vars '())))
```

**Status:** ✓ Correct

#### 2.8.3 Dialogue (`.DIALOGUE`)

**Muddle syntax:**
```lisp
(.DIALOGUE "text")
```

**Maps to EIGHTBOL AST:**
```lisp
(:dialogue :speaker "narrator" :text text)
```

**Implementation:** `muddle-parser.lisp:83-85`
```lisp
(defun muddle-parse-dialogue (text)
  "(.DIALOGUE \"text\") - Display dialogue text."
  (list :dialogue :speaker "narrator" :text text))
```

**Status:** ✓ Correct

---

### 2.9 Exception Handling

#### 2.9.1 Throw Fault (`.THROW`)

**Muddle syntax:**
```lisp
(.THROW code)
```

**Maps to EIGHTBOL AST:**
```lisp
(:log-fault :code code)
```

**Implementation:** `muddle-parser.lisp:93-95`
```lisp
(defun muddle-parse-throw (code)
  "(.THROW code) - Throw a fault with code."
  (list :log-fault :code code))
```

**Status:** ✓ Correct

#### 2.9.2 Exit Program (`.EXIT`)

**Muddle syntax:**
```lisp
(.EXIT)
(.EXIT code)
```

**Maps to EIGHTBOL AST:**
```lisp
(:stop-run)
(:stop-run :code code)
```

**Implementation:** `muddle-parser.lisp:87-91`
```lisp
(defun muddle-parse-exit (&optional code)
  "(.EXIT code) - Exit program with optional status code."
  (if code
      (list :stop-run :code code)
      (list :stop-run)))
```

**Status:** ✓ Correct

---

### 2.10 Special Constructs

#### Unsupported by Muddle Frontend (in EIGHTBOL)
- `.CATCH` / `.FINALLY` — not implemented in parser
- Dynamic variables — Muddle uses lexical scope only
- Coroutines (complex, not mapped to EIGHTBOL primitives)
- Type declarations (`#DECL`)
- Property/slot access syntax (e.g., `<DROOM1 .E>`)

**Status:** ⚠️ **INCOMPLETE** — Exception handling catch/finally not wired

---

## 3. Critical Issues Found

### Issue #1: `:go-back` vs `:goback` BUG (Cross-Frontier)

**Affected Frontends:**
1. **Muddle** (`src/frontend-muddle/muddle-parser.lisp:73`)
2. **Lingo** (`src/frontend-lingo/lingo-parser.lisp:187`)
3. **SCI** (`src/frontend-sci/sci-parser.lisp:93`, `sci-parser.lisp:153`)

**Canonical Correct Form:**
- Defined in `src/grammar-build.lisp:37-39`:
  ```lisp
  (defun make-goback-node ()
    "Build a :goback AST node."
    (list :goback))
  ```
- Used by BASIC frontend correctly: `src/frontend-basic/basic-parser.lisp:72, 99, 107`
- Tested extensively in `tests/backend-*-tests.lisp` with `:goback`
- ALL backends expect `:goback` (e.g., `backend-6502/backend-6502-part6.lisp:461`)

**Scope of Bug:**
- **All backends will ignore `:go-back` nodes** (not recognized)
- **Methods will not return properly** when called via Muddle, Lingo, or SCI
- **Silent failure:** No error thrown, just missing return instruction
- **Affects all targets:** 6502, 65c02, 65c816, HuC6280, RP2A03, cp1610, Z80, SM83, m68k, i286, ARM7, F8, Stack, Forth

---

### Issue #2: Documentation Bug (Muddle Frontend)

**File:** `doc/chapters/muddle_frontend.texi:413`

**Current (incorrect):**
```
Maps to Eightbol's @code{:go-back} node.
```

**Should be:**
```
Maps to Eightbol's @code{:goback} node.
```

---

### Issue #3: Incomplete Call Argument Handling

**Problem:** Muddle's `.CALL` silently discards arguments.

**Current Implementation:**
```lisp
(defun muddle-parse-call (func &rest args)
  "(.CALL func args...) - Function invocation."
  (list :call :target func))  ;; args ignored!
```

**Should map to:**
- `:call :target name` (nullary)
- `:call-acc :target name :using expr` (unary with accumulator)
- `:call :target name :args (...)` (multi-arg, if supported)

**Impact:** Any `.CALL` with arguments will compile but lose those arguments.

---

### Issue #4: No Tests for Muddle Return Statement

**Missing:** No test in `tests/frontends/frontend-muddle-tests/` that verifies `.RETURN` produces correct AST.

**Existing tests:**
- `lexer-tests.lisp` — tokenization
- `parser-tests.lisp` — partial
- `numeric-types-tests.lisp` — numbers
- `variable-names-tests.lisp` — identifiers
- `functions-tests.lisp` — function defs
- `integration-tests.lisp` — end-to-end (but no `.RETURN` case)

---

### Issue #5: SCI Frontend Also Has `:go-back` Bug

**File:** `src/frontend-sci/sci-parser.lisp`

**Lines 93, 151-153:**
```lisp
(defun sci-parse-return (&optional val)
  "return [val]"
  (list :go-back))

(defun sci-make-goback-node ()
  "Create a return/goback AST node."
  (list :go-back))  ;; ❌ Also wrong here!
```

**Redundant function:** `sci-make-goback-node` duplicates `make-goback-node` from `grammar-build.lisp` with wrong implementation.

---

### Issue #6: Lingo Frontend Incomplete Return Semantics

**File:** `src/frontend-lingo/lingo-parser.lisp:185-190`

```lisp
(return-stmt
 (return
   (lambda () (list :go-back)))     ;; ❌ Bug
 (return expression
         (lambda (val)
           (list :move :from val :to "RESULT"))))  ;; Different semantics!
```

**Problem:** 
- Plain `RETURN` → `:go-back` (wrong form)
- `RETURN value` → move to a global `RESULT` variable

This is **inconsistent semantics**. Both should map to `:goback` or `:move :to result-var + :goback`.

---

## 4. AST Mapping Summary

### Muddle → EIGHTBOL AST Mappings

| Muddle Construct | Canonical EIGHTBOL AST | Status | Notes |
|---|---|---|---|
| `.SET var expr` | `(:move :from expr :to var)` | ✓ | Correct |
| `.IF cond then else` | `(:if :condition cond :then stmts :else stmts)` | ✓ | Correct |
| `.WHILE cond body` | `(:perform :until (NOT cond) :body stmts)` | ✓ | Correct |
| `.FOR var start end body` | `(:perform :varying var :from start :by 1 :until (> var end) :body stmts)` | ✓ | Correct |
| `.DEFINE name params body` | `(:method :method-id name :statements body)` | ✓ | Correct |
| `.RETURN` | `(:goback)` | ❌ | Emits `:go-back` |
| `.RETURN value` | `(:goback)` (ignores value) | ⚠️ | Not standard; value ignored |
| `.GO label` | `(:goto :target label)` | ✓ | Correct |
| `.CALL func` | `(:call :target func)` | ⚠️ | Args ignored |
| `.CALL func args` | `(:call-acc :target func :using args)` | ❌ | Not implemented |
| `.PRINT args` | `(:print :expressions args)` | ✓ | Correct |
| `.INPUT vars` | `(:input :variables vars)` | ✓ | Correct |
| `.THROW code` | `(:log-fault :code code)` | ✓ | Correct |
| `.EXIT code` | `(:stop-run :code code)` | ✓ | Correct |
| `+`, `-`, `*`, `/` | `:add`, `:subtract`, `:multiply`, `:divide` (expr form) | ✓ | Correct |
| `^` | `:bit-xor` (expr form) | ✓ | Correct for Muddle |
| `=`, `/=`, `<`, `>`, etc. | `:conditional-eq`, `:conditional-ne`, etc. | ✓ | Correct |
| `AND`, `OR`, `NOT` | `:conditional-and`, `:conditional-or`, `:conditional-not` | ✓ | Correct |

---

## 5. Return Semantics Analysis

### EIGHTBOL Canonical Return Statement

**AST node:** `(:goback)`

**Backend handling:**
- **6502** (`backend-6502-part6.lisp:461`): Emits `rts` (return from subroutine)
- **ARM7** (`backend-arm7.lisp:123`): Emits `bx lr` (branch to link register)
- **All backends** recognize `:goback` as method/function exit

### Muddle Semantics

**Muddle specification (Wikipedia):**
> "`.RETURN` ends execution of the current routine and returns control to its caller."

**With optional value:**
- `.RETURN` → return `NIL` (Muddle) / `NULL` (EIGHTBOL)
- `.RETURN value` → return the value (depends on calling convention)

### Current Implementations

| Frontend | Nullary | Unary | Canonical? |
|---|---|---|---|
| **BASIC** | `(:goback)` | N/A | ✓ Correct |
| **Muddle** | `(:go-back)` | `(:go-back)` (ignores val) | ❌ Wrong |
| **Lingo** | `(:go-back)` | `(:move :from val :to "RESULT")` | ❌ Inconsistent |
| **SCI** | `(:go-back)` | `(:go-back)` (ignores val) | ❌ Wrong |

**Recommendation:** All three should emit `:goback` with optional `:returning identifier` for return value capture.

---

## 6. Numeric Types in Muddle

### Supported Literal Formats

**Implemented in `muddle-lexer.lisp:91-160+`:**

1. **Decimal:** `42`, `-123`, `0`, `+256`
2. **Hexadecimal:** 
   - `0xFF`, `0xDEADBEEF` (C-style)
   - `#XFF`, `#X0A` (Lisp-style)
   - `h'FF'`, `H'FF'` (quoted)
   - `x'FF'`, `X'FF'` (alternate)
3. **Octal:**
   - `0o77`, `0o644` (C-style)
   - `#O77` (Lisp-style)
   - `o'77'`, `O'77'`, `q'77'`, `Q'77'` (quoted)
4. **Binary:**
   - `0b1010` (C-style)
   - `#B1010` (Lisp-style)
   - `b'1010'`, `B'1010'` (quoted)
5. **Dword:** `d'FFFFFFFF'` (32-bit hex)

**Status:** ✓ Comprehensive lexer support

**Numeric precision:** All parsed as integers; EIGHTBOL handles binary/BCD at compilation stage.

---

## 7. Identifier Normalization

**Muddle convention:** kebab-case (following Lisp standards)

**Implemented in `muddle-lexer.lisp:60-89`:**
```
MyVariable → my-variable
SCREENSAVER → screensaver
_ignored → -ignored
```

**Implementation:** Converts to lowercase and inserts hyphens at camelCase boundaries.

**Status:** ✓ Correct

---

## 8. Unimplemented Muddle Features

### In Scope (not implemented in EIGHTBOL Muddle frontend)
- Type declarations: `#DECL ((E) <TYPE>)`
- User-defined types: `<ATOM ...>`
- Vector/list literals: `<1 2 3>` vs. `(1 2 3)`
- Coroutines: `<GO>`, `<RESUME>`
- Property access: `<DROOM1 .E>` (should use `.GET`)
- Dynamic variables: `;MYVAR` (use global scope)
- String escapes beyond `\n`: `\t`, `\\`, `\"`
- Module system: `<USE>`, `<REQUIRE>`

### Out of Scope (not in canonical EIGHTBOL AST)
- Macros: `<MACRO ...>`
- Lambda abstractions for complex closures
- Continuations

---

## 9. Recommended Fixes

### Priority 1: Critical Bugs (Break All Methods)

#### Fix 1a: Muddle Parser — Return Statement
**File:** `src/frontend-muddle/muddle-parser.lisp:71-73`

**Current:**
```lisp
(defun muddle-parse-return (&optional val)
  "(.RETURN val) - Return from function."
  (list :go-back))
```

**Fixed:**
```lisp
(defun muddle-parse-return (&optional val)
  "(.RETURN val) - Return from function."
  (list :goback))
```

#### Fix 1b: Lingo Parser — Return Statement
**File:** `src/frontend-lingo/lingo-parser.lisp:185-187`

**Current:**
```lisp
(return-stmt
 (return
   (lambda () (list :go-back)))
```

**Fixed:**
```lisp
(return-stmt
 (return
   (lambda () (list :goback)))
```

#### Fix 1c: SCI Parser — Return Statement (2 locations)
**File:** `src/frontend-sci/sci-parser.lisp:91-93, 151-153`

**Current:**
```lisp
(defun sci-parse-return (&optional val)
  "return [val]"
  (list :go-back))

(defun sci-make-goback-node ()
  "Create a return/goback AST node."
  (list :go-back))
```

**Fixed:**
```lisp
(defun sci-parse-return (&optional val)
  "return [val]"
  (list :goback))

;; Remove sci-make-goback-node entirely; use make-goback-node from grammar-build.lisp
```

---

### Priority 2: Documentation Bugs

#### Fix 2a: Muddle Frontend Doc
**File:** `doc/chapters/muddle_frontend.texi:413`

**Current:**
```
Maps to Eightbol's @code{:go-back} node.
```

**Fixed:**
```
Maps to Eightbol's @code{:goback} node.
```

---

### Priority 3: Incomplete Features

#### Fix 3a: Muddle Call Arguments
**File:** `src/frontend-muddle/muddle-parser.lisp:67-69`

**Current:**
```lisp
(defun muddle-parse-call (func &rest args)
  "(.CALL func args...) - Function invocation."
  (list :call :target func))
```

**Proposed (if args supported):**
```lisp
(defun muddle-parse-call (func &rest args)
  "(.CALL func args...) - Function invocation."
  (if args
      (list :call-acc :target func :using (if (= 1 (length args))
                                               (car args)
                                               (cons 'list args)))
      (list :call :target func)))
```

**OR (if args not to be supported):**
```lisp
(defun muddle-parse-call (func &rest args)
  "(.CALL func args...) - Function invocation (ignores arguments)."
  (when args
    (warn "~A: .CALL with arguments not yet supported; ~D argument(s) ignored"
          'muddle-parse-call (length args)))
  (list :call :target func))
```

#### Fix 3b: Lingo Return Value Semantics
**File:** `src/frontend-lingo/lingo-parser.lisp:185-190`

**Current:**
```lisp
(return-stmt
 (return
   (lambda () (list :go-back)))
 (return expression
         (lambda (val)
           (list :move :from val :to "RESULT"))))
```

**Issue:** Different semantics for nullary vs. unary.

**Proposed Fix:** Both should produce `:goback` with optional `:returning`:
```lisp
(return-stmt
 (return
   (lambda () (list :goback)))
 (return expression
         (lambda (val)
           (list* :goback (when val `(:returning ,val))))))
```

---

### Priority 4: Test Coverage

#### Fix 4a: Add Muddle Return Test
**File:** `tests/frontends/frontend-muddle-tests/parser-tests.lisp`

**Add test case:**
```lisp
(test muddle-parser/return
  "(.RETURN) produces (:goback) AST node."
  (let ((ast (parse-muddle '((.RETURN)))))
    (is (eq :goback (first (find :goback (... ast) :key #'first))))))

(test muddle-parser/return-with-value
  "(.RETURN value) produces (:goback) (value ignored or captured)."
  (let ((ast (parse-muddle '((.RETURN 42)))))
    (is (eq :goback (first (find :goback (... ast) :key #'first))))))
```

---

## 10. Test Status & Regressions

### Current Test Suites
- `tests/eightbol-tests.lisp` — Core EIGHTBOL (extensive `:goback` tests)
- `tests/backend-*-tests.lisp` — All backends (expect `:goback`)
- `tests/frontends/frontend-muddle-tests/` — Muddle-specific (no return tests)

### Regression Risk
- **HIGH:** All methods using Muddle, Lingo, or SCI frontends will fail to return
- **SILENT:** No warning or error; just missing return instructions in emitted code
- **Scope:** Affects ALL backends (6502 through Forth)

### No Current Test Regression
- Muddle test suite has no test case for `.RETURN`
- Likely not run in CI or not compiled to actual backends

---

## 11. Summary of Findings

### ✓ Working Correctly
1. Conditional expressions (`.IF` / `.ELSE`)
2. Loop constructs (`.WHILE`, `.FOR`)
3. Function definitions (`.DEFINE`)
4. Jumps (`.GO`)
5. I/O (`PRINT`, `INPUT`, `DIALOGUE`)
6. Exception handling (`THROW`, `EXIT`)
7. Arithmetic and logical operators
8. Identifier normalization
9. Number literal formats (hex, octal, binary, dword)

### ❌ Critical Bugs
1. **`.RETURN` emits `:go-back` instead of `:goback`** (Muddle, Lingo, SCI)
2. Documentation claims `:go-back` (should be `:goback`)
3. **Backends will not recognize `:go-back`** — methods won't return

### ⚠️ Incomplete / Inconsistent
1. Call arguments silently ignored (Muddle)
2. Return value semantics inconsistent (Lingo)
3. SCI has duplicate, incorrect `sci-make-goback-node`
4. No test coverage for return statements (Muddle)

### 🎯 Unique Features (Native to Muddle)
- Lexical scope only (no dynamic scope)
- S-expression syntax (prefix notation)
- Multiple numeric literal formats
- Kebab-case identifier normalization

---

## 12. References

### Online Sources
- Wikipedia: MDL (programming language) — https://en.wikipedia.org/wiki/MDL_(programming_language)
  - Created 1971, MIT Project MAC
  - Designed by Sussman, Hewitt, Reeve, Daniels
  - Final version 105 (1980)
  - Influenced Scheme, Common Lisp, Prolog, Smalltalk, actor model
  - Famous use in Zork interactive fiction

### EIGHTBOL Documentation
- `doc/chapters/muddle_frontend.texi` — Comprehensive Muddle language reference
- `doc/EIGHTBOL.texi` — Main documentation
- `src/ast.lisp` — Canonical AST node definitions
- `src/grammar-build.lisp` — AST constructors
- `frontend_plans/muddle_task.md` — Implementation planning doc

### EIGHTBOL Source Code
- `src/frontend-muddle/muddle-lexer.lisp` — Tokenizer
- `src/frontend-muddle/muddle-parser.lisp` — Parser
- `src/frontend-lingo/lingo-parser.lisp` — Lingo parser (also has bug)
- `src/frontend-sci/sci-parser.lisp` — SCI parser (also has bugs)
- `src/frontend-basic/basic-parser.lisp` — Reference implementation (correct)

---

## Appendix A: Implementation Complexity Estimates

### Priority 1 Fixes (Critical)
- **Fix `.RETURN` in Muddle:** 1 line change → 5 min
- **Fix `.RETURN` in Lingo:** 1 line change → 5 min
- **Fix `.RETURN` in SCI:** 2 line changes → 5 min
- **Total:** ~15 min implementation, testing varies by depth

### Priority 2 Fixes (Documentation)
- **Update Muddle docs:** 1 line change → 2 min
- **Total:** ~2 min

### Priority 3 Fixes (Incomplete)
- **Improve Muddle `.CALL`:** 5-10 line enhancement → 30 min (including decision on semantics)
- **Fix Lingo return value semantics:** 5 line change → 15 min
- **Remove SCI duplicate function:** 3 line removal → 5 min
- **Total:** ~50 min

### Priority 4 Fixes (Test Coverage)
- **Add Muddle return tests:** 20 lines → 20 min
- **Add Lingo return tests:** 20 lines → 20 min
- **Add SCI return tests:** 20 lines → 20 min
- **Total:** ~60 min

**Grand Total:** ~127 min (~2 hours) for all fixes

---

**END OF RESEARCH REPORT**
