# BurgerMistress Language Research & Audit

**Language Name:** BurgerMistress (also known as "The Thief of Fate" scripting language by Burger Becky)

**Status:** Incomplete frontend implementation  
**Last Updated:** 2026-09-09  
**Implementation Files:** `src/frontend-burgermistress/burger-lexer.lisp`, `src/frontend-burgermistress/burger-parser.lisp`

---

## 1. LANGUAGE OVERVIEW

BurgerMistress is a hybrid scripting language combining BASIC-like syntax with Prolog-style logic programming constructs. It is designed for scripting game events, dialogues, and AI behaviors on 8-bit and 16-bit systems.

### File Extensions
- `.bmrs` — Standard extension
- `.rah` — Undocumented alternative (treated identically but not officially supported)

---

## 2. LANGUAGE CONSTRUCTS

### 2.1 Variable Declaration & Assignment

**Declaration (DIM statement):**
```burgermistress
DIM counter
DIM score AS INTEGER
DIM playerName AS STRING
DIM x, y, z AS INTEGER
```

**Supported Types:**
- `INTEGER` — Signed integer (typically 16-bit on 8-bit targets, 32-bit on 16-bit)
- `STRING` — Text string (variable length, up to 255 bytes)
- `BOOLEAN` — Boolean value (true/false)
- `DOUBLE` — 32-bit fixed-point or floating-point (limited support)

**Assignment:**
```burgermistress
counter = 0
score = score + 10
playerName = "Hero"
isAlive = TRUE
```

**AST Output:** Maps to `:move` node
```lisp
(:move :from expr :to identifier)
```

### 2.2 Control Flow Structures

#### IF/THEN/ELSE/ENDIF
```burgermistress
IF condition THEN statement
IF condition THEN
    statements
ENDIF
IF condition THEN
    statements
ELSE
    statements
ENDIF
```

**AST Output:** `:if` node

#### WHILE/WEND
```burgermistress
WHILE condition :
    statements
WEND
```

**Note:** A colon (`:`) separates the condition from body statements.  
**AST Output:** Maps to `:perform :until` construct

#### FOR/NEXT
```burgermistress
FOR variable = start TO end :
    statements
NEXT

FOR variable = start TO end STEP step :
    statements
NEXT
```

**Features:**
- Loop variable automatically incremented by 1 (or specified STEP)
- STEP can be negative for countdown loops
- Variables must be previously declared or simple identifiers

**AST Output:** Maps to `:perform :varying` construct

#### DO/LOOP/UNTIL
```burgermistress
DO :
    statements
LOOP UNTIL condition
```

**Features:** Post-test loop; body executes at least once.  
**AST Output:** Maps to `:perform :until` with post-test semantics

#### EXIT
```burgermistress
EXIT
```

**Purpose:** Terminates current subroutine/function. Control returns to caller.  
**AST Output:** Maps to `:exit-method` or `:go-back` node

### 2.3 Functions & Subroutines

#### Subroutine Definition (SUB/END SUB)
```burgermistress
SUB name :
    statements
END SUB

SUB UPDATE_SCORE(points) :
    score = score + points
    IF score > highScore THEN
        highScore = score
    ENDIF
END SUB
```

**AST Output:** `:method` node

#### Function Definition (FUNCTION/END FUNCTION)
```burgermistress
FUNCTION name :
    statements
END FUNCTION

FUNCTION MAX(a, b) :
    IF a > b THEN
        RETURN a
    ELSE
        RETURN b
    ENDIF
END FUNCTION
```

**AST Output:** `:method` node with return value semantics

#### Function Calls
```burgermistress
CALL subroutineName
GOSUB labelName
RETURN
RETURN expression
```

**AST Output:**
- CALL → `:call` node
- GOSUB → `:call` node (legacy BASIC construct)
- RETURN → `:go-back` node

### 2.4 Dialogue Statements

```burgermistress
DIALOGUE speakerName : goals
DIALOGUE speakerName : predicate(arg1, arg2, ...)
DIALOGUE speakerName : goal1, goal2, goal3
```

**Examples:**
```burgermistress
DIALOGUE Merchant : GREET()
DIALOGUE Hero : SAY("I need a sword")
DIALOGUE Narrator : DESCRIBE(SCENE, LOCATION), EMOTION(MOOD)
```

**Built-in Goal Patterns:**
- `HAS_ITEM(character, item)` — Check if character has item
- `CAN_TALK(speaker)` — Check if speaker can talk
- `EMOTION(mood)` — Set emotional state
- `DESCRIBE(scene, location)` — Describe scene/location
- `QUEST_STATUS(quest, status)` — Check quest status

**AST Output:** Maps to `:dialogue` node

### 2.5 Input/Output Operations

#### PRINT Statement
```burgermistress
PRINT expression, expression, ...
PRINT "Hello, World!"
PRINT "Score: ", score
PRINT "X=", x, " Y=", y
```

**AST Output:** `:print` node

#### INPUT Statement
```burgermistress
INPUT variableName
INPUT variable1, variable2, ...
INPUT variableName WITH "prompt"
INPUT firstName, lastName WITH "Enter your name: "
```

**AST Output:** `:input` node with optional prompt

### 2.6 Operators

#### Arithmetic Operators
| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `x + y` |
| `-` | Subtraction | `a - b` |
| `*` | Multiplication | `width * height` |
| `/` | Division | `total / count` |

#### Relational Operators
| Operator | Description | Example |
|----------|-------------|---------|
| `=` | Equality | `x = y` |
| `<>` | Inequality | `x <> y` |
| `<` | Less than | `health < 0` |
| `>` | Greater than | `score > 100` |
| `<=` | Less than or equal | `x <= 10` |
| `>=` | Greater than or equal | `level >= 5` |

#### Logical Operators
| Operator | Description | Example |
|----------|-------------|---------|
| `AND` | Logical conjunction | `x > 0 AND y > 0` |
| `OR` | Logical disjunction | `health = 0 OR lives = 0` |
| `NOT` | Logical negation | `NOT isDead` |

#### Bitwise Shift Operators
| Operator | Description | Example |
|----------|-------------|---------|
| `<<` | Left shift | `x << 2` |
| `>>` | Right shift | `y >> 1` |

#### Operator Precedence (lowest to highest)
1. Logical OR
2. Logical AND
3. NOT
4. Relational (=, <>, <, >, <=, >=)
5. Bitwise shift (<<, >>)
6. Addition and subtraction (+, -)
7. Multiplication and division (*, /)

### 2.7 Number Literal Formats

Burgermistress supports multiple number literal formats:

| Format | Syntax | Examples |
|--------|--------|----------|
| Decimal | Plain digits | `42`, `0`, `255` |
| Hexadecimal | `&h` or `0x` prefix | `&hFF`, `0xFF`, `0xDEADBEEF` |
| Octal | `&o` or `0o` prefix | `&o77`, `0o755` |
| Binary | `&b` or `0b` prefix | `&b1010`, `0b11110000` |
| Dword | `&d"WORD"` or `0d"WORD"` | `&d"12345678"` |

### 2.8 String Operations

**String Literals:**
```burgermistress
"Hello, World!"
"Line one"
```

**Note:** Single quotes are reserved for comments, not strings.

**Identifier Normalization:**
- First character converted to uppercase
- Subsequent alphabetic characters converted to lowercase unless preceded by a dot
- Dots preserved to delimit nested fields
- Underscores converted to hyphens

**Examples:**
```
Source          Normalized          Notes
myVariable      MyVariable          camelCase to Header-Case
MY_FUNCTION     My-function         Underscores become hyphens
player.score    Player.score        Nested field access
HTTPServer      Httpsserver         All lowercase after first
```

---

## 3. CRITICAL BUG: (:* and (:/ INSTEAD OF :multiply/:divide

### Bug Description

**Location:** `src/frontend-burgermistress/burger-parser.lisp`, lines 239-242

**Current (Incorrect) Code:**
```lisp
(expression times expression
 (lambda (l r) (list :* l r)))
(expression divide expression
 (lambda (l r) (list :/ l r)))
```

**Issue:** The parser emits non-canonical AST nodes:
- `(:* left right)` instead of `(:multiply :multiplicand left :multiplier right :giving nil)`
- `(:/ left right)` instead of `(:divide :numerator left :denominator right :giving nil)`

### Canonical AST Forms

According to the AST specification and other frontends (BASIC, Fountain, Fortran):

**Multiplication:**
```lisp
(:multiply :multiplicand left :multiplier right :giving nil)
```

**Division:**
```lisp
(:divide :numerator left :denominator right :giving nil)
```

### Impact

This bug causes:
1. **Backend Compatibility Issues:** Backends expect `:multiply` and `:divide` keyword nodes, not `(:* ...)` and `(:/ ...)` expressions
2. **AST Traversal Failures:** Code that patterns-matches on arithmetic nodes will miss these operations
3. **Code Generation Failures:** Backends will not recognize multiplication/division operations from BurgerMistress

### Reference Implementations

**BASIC Frontend (`src/frontend-basic/basic-parser.lisp`):**
```lisp
(defun basic-parse-expression-mul (e1 e2)
  (list :multiply e1 e2))

(defun basic-parse-expression-div (e1 e2)
  (list :divide e1 e2))
```

**Addition/Subtraction (Working Correctly in BurgerMistress):**
```lisp
(expression plus expression
 (lambda (l r) (list :add l r)))
(expression minus expression
 (lambda (l r) (list :subtract l r)))
```

### Recommended Fix

Replace lines 239-242 in `burger-parser.lisp`:

**Before:**
```lisp
(expression times expression
 (lambda (l r) (list :* l r)))
(expression divide expression
 (lambda (l r) (list :/ l r)))
```

**After:**
```lisp
(expression times expression
 (lambda (l r) (list :multiply l r)))
(expression divide expression
 (lambda (l r) (list :divide l r)))
```

---

## 4. ADDITIONAL AST ISSUES

### 4.1 Incomplete Arithmetic Expression Handling

The current parser only emits simple two-operand lists for arithmetic operations. However, some backends may expect more structured forms with explicit `:multiplicand`, `:multiplier` keywords.

**Current:** `(:multiply a b)`  
**Expected:** `(:multiply :multiplicand a :multiplier b :giving nil)`

The simpler form works with BASIC (`src/frontend-basic/basic-parser.lisp`), but this needs verification against backend expectations.

### 4.2 Missing Input Prompt Support

The `:input` node constructor in `burger-parser.lisp` calls `make-input-node` with an optional prompt:
```lisp
(defun burgermistress-parse-input (variables &optional prompt)
  "INPUT var [, var, …] [WITH prompt]"
  (make-input-node (ensure-list variables) :prompt prompt))
```

However, `make-input-node` in `src/ast.lisp` is defined as:
```lisp
(defun make-input-node (variables)
  "Build an :input AST node for INPUT statements.
VARIABLES is a list of identifiers to read into."
  (list :input :variables (or variables '())))
```

**Issue:** The `make-input-node` function does not accept a `:prompt` keyword argument, but the parser tries to pass it.

**Impact:** Runtime error or ignored prompt parameter.

**Recommended Fix:** Either:
1. Update `make-input-node` to accept and preserve the prompt keyword, OR
2. Use `list* :input :variables variables :prompt prompt` in the parser, OR
3. Remove prompt support from the parser if not supported by the canonical AST

### 4.3 Missing Bitwise Operator AST Nodes

The lexer tokenizes bitwise shift operators (`<<`, `>>`), but the parser only mentions them in documentation without providing grammar rules:

**In `burger-parser.lisp`:**
- Line 750-759 documents bitwise shifts
- But no grammar rules produce `:lshift` or `:rshift` nodes

**Current Lexer Output:** Tokens marked as `:op` for shift operators  
**Parser Coverage:** **MISSING** — No rules to handle shift operators

**Recommended Fix:** Add rules to grammar:
```lisp
(expression lshift expression
 (lambda (l r) (list :lshift l r)))
(expression rshift expression
 (lambda (l r) (list :rshift l r)))
```

### 4.4 Incomplete Logical Operator Support

Logical operators are partially handled:
- `:and` — ✓ Implemented (line 255-256)
- `:or` — ✓ Implemented (line 257-258)
- `:not` — ✓ Implemented (line 259-260)

These appear correct and match addition/subtraction patterns.

---

## 5. MISSING AST FORMS & UNSUPPORTED FEATURES

### 5.1 Intentionally Unsupported (Per Design)

The Burgermistress frontend intentionally omits:
- Object-oriented programming features (classes, inheritance)
- Exception handling (TRY/CATCH/FINALLY)
- Structured exception handling
- Lambda expressions and closures
- First-class functions
- Regular expressions
- File I/O beyond basic INPUT/PRINT
- Dynamic memory allocation (arrays must be fixed-size)
- Recursive function calls (limited stack on 8-bit targets)
- Floating-point literals (use INTEGER or scaled arithmetic)
- Complex number support
- Bit field declarations
- Pointer arithmetic
- Multiple statements on one line
- Line numbers (use labels for GOSUB)
- DATA/READ statements

### 5.2 Not Yet Implemented in Parser

Despite being defined in the lexer and documented:
- **Array subscripting** — `array[index]` — No grammar rule
- **Nested field access** — `object.field` — Partially tokenized but not parsed
- **FOR loop with STEP** — Documented but parser doesn't extract STEP parameter (line 150-151)
- **Qualified identifiers** — Dots are tokenized but not handled in expression parsing

---

## 6. IDENTIFIER NORMALIZATION

The lexer includes a comprehensive identifier normalization function:

```lisp
(defun normalize-identifier-to-header-case (s)
  "Normalize identifier S to Header-Case format (MyIdentifier or My.Nested.Field).
Handles case-insensitive input with dots for structure."
  (when (and (stringp s) (plusp (length s)))
    (let ((parts (split-sequence:split-sequence #\. s)))
      (format nil "~{~:(~A~)~^.~}" parts))))
```

**Behavior:**
- Splits on dots to preserve nested field structure
- Applies `~:(~A~)` formatting to capitalize first letter, lowercase rest
- Preserves dot delimiters

**Examples:**
```
myVariable    → MyVariable
MY_FUNCTION   → My-function (underscores converted to hyphens earlier in lexer)
player.score  → Player.score
```

---

## 7. LEXER ANALYSIS

### 7.1 Number Literal Parsing

The lexer implements `parse-number-literal` to handle:
- Decimal: `42`, `0`, `255`
- Hexadecimal: `&hFF`, `0xFF`
- Octal: `&o77`, `0o755`
- Binary: `&b1010`, `0b11110000`
- Dword: `&d"12345678"`, `0d"WORD"`

**Output Format:** `(type . value)` pairs stored as strings: `"decimal:42"`, `"hex:255"`

### 7.2 Comment Handling

Comments begin with apostrophe (`'`) at the start of a line. Inline comments are not supported (apostrophe must start the line).

```burgermistress
' This is a comment
PRINT "Hello"  ' This is treated as content, not comment
```

### 7.3 Keyword Recognition

The lexer maintains `*burgermistress-keyword-alist*` with 37 keywords including:
- Control flow: IF, ELSE, THEN, ENDIF, WHILE, WEND, FOR, TO, STEP, NEXT, DO, LOOP, UNTIL, EXIT
- Subroutines: SUB, END, FUNCTION, CALL, RETURN, GOSUB
- Types: AS, INTEGER, STRING, BOOLEAN, DOUBLE
- Operators: AND, OR, NOT
- I/O: DIALOGUE, PRINT, INPUT
- Declaration: DIM

---

## 8. COMPARISON WITH CANONICAL AST

### Correct Mappings
- `:add` — ✓ Correct
- `:subtract` — ✓ Correct
- `:and` — ✓ Correct
- `:or` — ✓ Correct
- `:not` — ✓ Correct
- `:if` — ✓ Correct
- `:move` — ✓ Correct (assignment)
- `:call` — ✓ Correct
- `:exit-method` — ✓ Correct
- `:perform` — ✓ Correct (loops)

### Incorrect/Non-Canonical Mappings
- `(:* left right)` — ✗ Should be `:multiply`
- `(:/ left right)` — ✗ Should be `:divide`

### Not Yet Implemented
- Array subscripting — No `:subscript` node generation
- Shift operations — No `:lshift`/`:rshift` node generation
- Nested field access — No `:of` node generation

---

## 9. PARSER GENERATOR FRAMEWORK

The parser uses Lisp's YACC framework (yacc:define-parser) to generate a bottom-up parser from production rules.

**Current Status:** Grammar is skeletal; many productions are skipped with `(skip "Implementation pending")` in the test suite.

**Test File:** `tests/frontends/frontend-burgermistress-tests/parser-tests.lisp`

---

## 10. SUMMARY & RECOMMENDATIONS

### Critical Issues (Must Fix)
1. **(:* and (:/ Bug** — Replace with `:multiply` and `:divide` canonical forms
2. **Input Prompt Parameter** — Align parser with `make-input-node` signature or extend AST node

### High Priority
3. **Array Subscripting** — Add grammar rule to emit `:subscript` nodes
4. **Shift Operators** — Add grammar rules for `<<` and `>>`
5. **FOR STEP Parameter** — Extract and pass STEP value to `:perform` node

### Medium Priority
6. **Qualified Identifiers** — Support object.field syntax via `:of` nodes
7. **More Comprehensive Tests** — Implement test suite (currently all skipped)
8. **Backend Validation** — Verify generated AST works with all target backends

### Gaps vs. Canonical AST
- **Multiply/Divide Forms:** Currently non-canonical; need keyword-based structure
- **Array Operations:** Not yet parsed
- **Shift Operations:** Not yet parsed
- **Nested Field Access:** Partially supported

### Recommended Action Items
1. Fix (:* and (:/ to use `:multiply` and `:divide`
2. Add array and shift operator support
3. Validate input prompt handling
4. Implement comprehensive test suite (use BASIC frontend tests as template)
5. Run integration tests against all backends to verify AST compatibility

---

## 11. REFERENCES

- **Implementation:** `src/frontend-burgermistress/burger-lexer.lisp`, `src/frontend-burgermistress/burger-parser.lisp`
- **Documentation:** `doc/chapters/burgermistress_frontend.texi`
- **Task Plan:** `frontend_plans/burgermistress_task.md`
- **AST Specification:** `src/ast.lisp`
- **Reference Frontend (BASIC):** `src/frontend-basic/basic-parser.lisp`
- **Reference Frontend (Fountain):** `src/frontend-fountain/fountain-parser.lisp`

---

**Document Generated:** 2026-09-09  
**Status:** Research Complete — Ready for Implementation
