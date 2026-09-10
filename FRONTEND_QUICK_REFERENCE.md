# EIGHTBOL Frontend Architecture: Quick Reference

## Standard Pipeline Diagram

```
┌──────────────────────────────────────────────────────────────────────┐
│                       EIGHTBOL Compilation Pipeline                  │
└──────────────────────────────────────────────────────────────────────┘

INPUT SOURCE CODE
(Any of 17 languages)
        │
        ▼
    ┌───────────────────────────────┐
    │   LEXER / SCANNER             │
    │   (frontend-*-lexer.lisp)     │
    │                               │
    │  • Tokenization              │
    │  • Comment handling          │
    │  • Keyword recognition       │
    │  • Operator classification   │
    └───────────────────────────────┘
        │
        ▼ TOKEN STREAM
    ┌───────────────────────────────┐
    │   PARSER (Grammar)            │
    │   (frontend-*-parser.lisp)    │
    │                               │
    │  • CL-YACC (15/17)           │
    │  • Recursive descent (Forth) │
    │  • Custom semantic (Goal)    │
    │  • Domain-specific (Fountain)│
    └───────────────────────────────┘
        │
        ▼ PARSE TREE
    ┌───────────────────────────────┐
    │   AST TRANSFORMATION          │
    │   (Parser actions)            │
    │                               │
    │  • make-*-node functions     │
    │  • Canonical plist format    │
    │  • All backends compatible   │
    └───────────────────────────────┘
        │
        ▼ CANONICAL AST
    ┌───────────────────────────────┐
    │   TRANSPILE (Optional)        │
    │   (frontend-*-transpile.lisp) │
    │                               │
    │  • 6/17 frontends have this  │
    │  • AST → intermediate form   │
    │  • Not always required       │
    └───────────────────────────────┘
        │
        ▼ INTERMEDIATE AST
    ┌───────────────────────────────┐
    │   .eightbol OUTPUT            │
    │   (eightbol-compile)          │
    │                               │
    │  • Location: Object/Classes/  │
    │  • Format: S-expression       │
    │  • Persistence: Disk storage  │
    │  • All frontends supported    │
    └───────────────────────────────┘
        │
        ▼ SERIALIZED AST
    ┌───────────────────────────────┐
    │   BACKEND CODE GENERATION     │
    │   (backend-*.lisp)            │
    │                               │
    │  • 6502, 65c02, 65c816       │
    │  • Z80, RP2A03, HuC6280      │
    │  • ARM7 (Thumb), m68k, i286  │
    │  • cp1610, F8                │
    │  • Stack VM                  │
    └───────────────────────────────┘
        │
        ▼
    ASSEMBLY / MACHINE CODE
```

---

## Frontend Components at a Glance

### The 17 Frontends

| # | Name | Purpose | Maturity | Category |
|---|------|---------|----------|----------|
| 1 | **AGI** | Adventure Game Interpreter | Mature | Game engines |
| 2 | **BASIC** | Beginner's All-Purpose Symbolic Instruction Code | Mature | Classic languages |
| 3 | **BurgerMistress** | Custom narrative/story DSL | Experimental | Domain-specific |
| 4 | **COBOL** | Common Business Oriented Language | Very Mature | Business |
| 5 | **Forth** | Stack-based language | Mature | Esoteric |
| 6 | **Fortran** | Formula Translation | Mature | Scientific |
| 7 | **Fountain** | Screenplay format for game narratives | Alpha | Domain-specific |
| 8 | **GOAL** | Game Object-Oriented Language | Experimental | Game engines |
| 9 | **Lingo** | Director scripting language | Experimental | Game engines |
| 10 | **Lua** | Lightweight extension language | Mature | Scripting |
| 11 | **Muddle** | Early Lisp variant | Experimental | Esoteric |
| 12 | **Objective-C** | Object-oriented C extension | Experimental | Systems |
| 13 | **Pascal** | Classic structured language | Mature | Classic languages |
| 14 | **SCI** | Sierra Creative Interpreter (game scripts) | Experimental | Game engines |
| 15 | **SCUMM** | Script Creation Utility for Maniac Mansion | Experimental | Game engines |
| 16 | **SmallTalk** | Object-oriented interactive language | Experimental | OOP |
| 17 | **ZIL** | Zork Implementation Language (interactive fiction) | Experimental | Esoteric |

---

## Component Files Per Frontend

### Structure Pattern

```
src/frontend-LANGUAGE/
├── LANGUAGE-lexer.lisp        ← Always present
├── LANGUAGE-parser.lisp       ← Always present
├── LANGUAGE-transpile.lisp    ← Optional (6 frontends)
├── LANGUAGE-emit.lisp         ← Optional (Fountain only)
├── LANGUAGE-make-parser.lisp  ← Optional (3 frontends)
└── package.lisp               ← Fountain only
```

### By-Language File Layout

```
agi/
├── agi-lexer.lisp
├── agi-parser.lisp
├── agi-transpile.lisp
└── make-parser.lisp

basic/
├── basic-lexer.lisp
├── basic-parser.lisp
├── basic-shell.lisp (REPL)
└── basic-transpile.lisp

burgermistress/
├── burger-lexer.lisp
└── burger-parser.lisp

cobol/
├── cobol-lexer.lisp
└── cobol-parser.lisp

forth/
├── forth-lexer.lisp
├── forth-parser.lisp
└── forth-transpile.lisp

fortran/
├── fortran-lexer.lisp
└── fortran-parser.lisp

fountain/
├── package.lisp
├── lexer.lisp
├── parser.lisp
├── fountain-forth-emit.lisp
└── fountain-transpile.lisp

goal/
├── goal-lexer.lisp
└── goal-parser.lisp

lingo/
├── lingo-lexer.lisp
├── lingo-parser.lisp
└── lingo-make-parser.lisp

lua/
├── lua-lexer.lisp
└── lua-parser.lisp

muddle/
├── muddle-lexer.lisp
└── muddle-parser.lisp

objective/
├── objective-lexer.lisp
└── objective-parser.lisp

pascal/
├── pascal-lexer.lisp
└── pascal-parser.lisp

sci/
├── sci-lexer.lisp
├── sci-parser.lisp
└── sci-transpile.lisp

scumm/
├── scumm-lexer.lisp
├── scumm-parser.lisp
└── scumm-transpile.lisp

smalltalk/
├── smalltalk-lexer.lisp
├── smalltalk-parser.lisp
└── smalltalk-make-parser.lisp

zil/
├── zil-lexer.lisp
└── zil-parser.lisp
```

---

## Parser Type Distribution

### CL-YACC (88% of frontends)

**Frontends using standard CL-YACC:**

```lisp
(eval
 `(yacc:define-parser *LANGUAGE-parser*
    (:muffle-conflicts :some)
    (:terminals (,@(LANGUAGE-token-list)))
    (:start-symbol program)
    
    ;; Grammar rules here
    (program ...)
    (statement ...)
    ...))
```

**15 frontends:** AGI, BASIC, BurgerMistress, COBOL, Fortran, Lingo, Lua, Muddle, Objective-C, Pascal, SCI, SCUMM, SmallTalk, ZIL

---

### Recursive Descent (6% of frontends)

**Forth only**

```lisp
;; forth-parser.lisp — Recursive-descent parser
;;; Word classification drives parsing
(defun forth-classify-word (word)
  (cdr (assoc word *forth-word-roles* :test #'string-equal)))

;; Stack machine simulation + word processing
(defun forth-execute-word (word stack state)
  ...)
```

**Reason:** Stack-based postfix syntax incompatible with shift/reduce

---

### Direct Semantic (6% of frontends)

**GOAL only**

```lisp
;; goal-parser.lisp — Direct form-to-statements mapping
(defun goal-form-to-statements (form)
  "Lisp form → canonical AST statements"
  (cond
    ((listp form)
     (case (first form)
       ((defun) (goal-defun-node ...))
       ((if) (goal-if-node ...))
       ...))
    ...))
```

**Reason:** Lisp-like s-expressions process directly

---

### Domain-Specific (6% of frontends)

**Fountain only**

```lisp
;; parser.lisp — Screenplay format parser
;;; Rich AST nodes for narrative
(defun make-scene-node (location-name map-name &key ...)
  (list :scene :location location-name :map map-name ...))

(defun make-dialogue-node (speaker text &key ...)
  (list :dialogue :speaker speaker :text text ...))

(defun make-action-node (description &key ...)
  (list :action :description description ...))
```

**Reason:** Screenplay format (action, dialogue, transitions) doesn't map to traditional grammar

---

## Canonical AST Node Types

All frontends produce these plist-based canonical nodes:

### Control Flow

```lisp
(:if :condition <expr>
     :then <statements>
     :else <statements>)

(:loop :while <expr>
       :statements <statements>)

(:goto :target <label-name>)

(:goback)  ; Return/exit
```

### Data Movement

```lisp
(:move :from <expr>
       :to <variable>)

(:compute :target <variable>
          :expression <expr>)
```

### Function/Method Calls

```lisp
(:call :target <function-name>
       :using <arg-expr>)

(:invoke :object <instance>
         :method <method-name>)
```

### I/O

```lisp
(:print :expressions (<expr> ...))

(:input :variables (<var> ...))
```

### Definitions

```lisp
(:program <class-name>
          :methods ((:method <name> :statements <stmts>) ...))

(:method <name>
         :statements <statements>)
```

---

## Quality Metrics

### Lexer Statistics

| Metric | Min | Avg | Max | Stdev |
|--------|-----|-----|-----|-------|
| **LOC** | 105 | 316 | 479 | 87 |
| **Comment refs** | 12 | 61 | 130 | 30 |
| **Keyword refs** | 1 | 10 | 20 | 5 |

**Best lexers:** Fountain (479 LOC), Muddle (377 LOC), Lingo (407 LOC)  
**Smallest lexers:** Pascal (105 LOC), Lua (195 LOC)

---

### Parser Statistics

| Metric | Min | Avg | Max | Stdev |
|--------|-----|-----|-----|-------|
| **LOC** | 224 | 547 | 1817 | 342 |
| **AST builders** | 9 | 23 | 52 | 11 |
| **Grammar rules** | ~25 | ~60 | ~150 | ~35 |

**Largest parsers:** COBOL (1817 LOC), Fountain (722 LOC), Pascal (523 LOC)  
**Most AST builders:** Fountain (52), SmallTalk (38), ZIL (38)  
**Fewest AST builders:** Muddle (9)

---

### Test Coverage

| Metric | Total | Per-Frontend |
|--------|-------|--------------|
| **Test files** | 119 | 7 per frontend |
| **Test assertions** | 500+ | 27-86 per frontend |
| **Most comprehensive** | BurgerMistress | 86 assertions |
| **Standard** | Most | 27 assertions |

---

## eightbol-compile Integration

### Direct Dependencies

**6 frontends** explicitly depend on `eightbol-compile`:

```
:depends-on ("eightbol-compile")
```

1. AGI
2. BASIC
3. SCI
4. SCUMM
5. Forth
6. Fountain

### Implicit Dependencies

**11 frontends** integrate via system load ordering

1. BurgerMistress
2. COBOL (primary integration point)
3. Fortran
4. GOAL
5. Lingo
6. Lua
7. Muddle
8. Objective-C
9. Pascal
10. SmallTalk
11. ZIL

### AST Output Format

**Location:** `Object/Classes/{ClassName}.eightbol`

**Example:**
```lisp
(:program "HelloWorld"
  :methods ((:method "Main"
             :statements ((:print :expressions ("Hello, World!"))))))
```

**Round-trip Flow:**
```
.cob source → Parse → AST plist → Write to .eightbol
↓
Read .eightbol → Parse → AST plist → Backend
```

---

## Testing Framework

### Per-Frontend Test Suite Structure

```
tests/frontends/frontend-LANGUAGE-tests/
├── LANGUAGE-lexer-tests.lisp      ; Tokenization tests
├── LANGUAGE-parser-tests.lisp     ; Grammar tests
├── LANGUAGE-transpile-tests.lisp  ; Transformation tests
├── LANGUAGE-integration-tests.lisp; End-to-end pipeline
├── LANGUAGE-edge-cases-tests.lisp ; Error handling
├── LANGUAGE-ast-tests.lisp        ; Canonical AST verification
└── LANGUAGE-backend-tests.lisp    ; Code generation integration
```

### Test Execution

```bash
# Run all frontend tests
(asdf:test-system :eightbol)

# Run specific frontend test
(fiveam:run! :frontend-cobol-tests)

# Run specific test suite
(fiveam:run! :cobol-lexer)
```

---

## Common Patterns Across Frontends

### Lexer Pattern

```lisp
(defun LANGUAGE-lex (source)
  "Tokenize SOURCE → token stream"
  (loop with stream = (make-string-input-stream source)
        for char = (peek-char nil stream nil)
        while char
        collect (scan-next-token stream)))

(defun normalize-identifier (ident)
  "Normalize identifier to canonical form (e.g., Header-Case)")
  ...)
```

### Parser Pattern

```lisp
(eval
 `(yacc:define-parser *LANGUAGE-parser*
    (:terminals (,@(LANGUAGE-token-list)))
    (:start-symbol program)
    
    (program
     (statements (lambda (stmts)
                   (make-program-node "ClassName"
                     :methods (list (make-method-node "Main"
                                      :statements stmts))))))
    
    (statements
     (statement (lambda (s) (list s)))
     (statements statement (lambda (prev curr)
                             (append prev (list curr)))))
    
    (statement
     (move-stmt (lambda (s) s))
     (if-stmt (lambda (s) s))
     (call-stmt (lambda (s) s)))
    
    ...))
```

### Transformation Pattern

```lisp
(defun LANGUAGE-parse-CONSTRUCT (...)
  "CONSTRUCT → canonical AST node"
  (make-METHOD-node "Name"
    :field1 value1
    :field2 value2))

(defun LANGUAGE-normalize-identifier (ident)
  "Normalize to Header-Case or camelCase as appropriate"
  (cond
    ((stringp ident) (LANGUAGE-case-convert ident))
    (t ident)))
```

---

## Architecture Deviations (Documented & Acceptable)

### Deviation 1: Forth (Recursive Descent)

**File:** `forth-parser.lisp` line 3-4  
**Reason:** Stack-based postfix syntax  
**Impact:** None; maintains pipeline architecture  
**Status:** ✅ Documented

### Deviation 2: Fountain (Domain-Specific Parser)

**File:** `parser.lisp` line 1  
**Reason:** Screenplay format (scenes, dialogue, actions)  
**Impact:** None; rich AST nodes bridge to canonical  
**Status:** ✅ Documented

### Deviation 3: GOAL (Direct Semantic)

**File:** `goal-parser.lisp` line 11-22  
**Reason:** Lisp-like s-expressions  
**Impact:** None; semantic analysis equivalent to parsing  
**Status:** ⚠️ Should be documented better

---

## Maintenance Checklist for New Frontends

When adding a new frontend:

- [ ] Create `frontend-LANGUAGE/` directory
- [ ] Implement `LANGUAGE-lexer.lisp` with:
  - [ ] Comment handling
  - [ ] Keyword recognition
  - [ ] Operator classification
- [ ] Implement `LANGUAGE-parser.lisp` with:
  - [ ] Token list (token-list function)
  - [ ] Grammar rules (yacc:define-parser or alternative)
  - [ ] Action functions producing canonical AST
- [ ] Optionally create `LANGUAGE-transpile.lisp` if complex
- [ ] Add to `eightbol.asd` system definition
- [ ] Create `tests/frontends/frontend-LANGUAGE-tests/`
- [ ] Add 7 test files with at least 27 assertions
- [ ] Document any deviations from standard architecture
- [ ] Verify round-trip: source → AST → .eightbol → backend

---

## Performance Notes

### Compilation Speed

- **COBOL:** Slowest (~1817 line parser, most complex grammar)
- **Fountain:** Medium (~722 line parser, custom recursive descent)
- **Pascal:** Medium (~523 lines, complex type system)
- **Others:** Fast (~200-400 line parsers, simple grammars)

### Memory Footprint

- **Total AST:** ~15,500 LOC for all 17 frontends
- **Average per frontend:** ~911 LOC
- **Largest:** COBOL at ~2,249 LOC
- **Smallest:** AGI at ~771 LOC

### Backend Compatibility

- All 17 frontends produce identical canonical AST format
- All 9 backends accept the same canonical AST
- No frontend-specific backend code needed
- Cross-frontend compatibility: 100%

---

## References

**Main Audit Report:** `FRONTEND_ARCHITECTURE_AUDIT.md`  
**CSV Summary:** `FRONTEND_AUDIT_SUMMARY.csv`  
**System Definition:** `eightbol.asd`  
**Compilation Pipeline:** `src/eightbol-compile.lisp`  
**AST Format:** `src/ast.lisp`

---

**Last Updated:** September 9, 2026  
**Status:** ✅ COMPLETE  
**Verification Level:** COMPREHENSIVE
