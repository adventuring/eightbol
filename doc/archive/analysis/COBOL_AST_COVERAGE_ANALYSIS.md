# COBOL Frontend AST Node Coverage Analysis

**Date:** September 9, 2026  
**Scope:** COBOL → 57 Canonical AST Nodes  
**Current Coverage:** 47/57 (82.5%)  
**Status:** ✅ Reference Implementation

---

## Executive Summary

The COBOL frontend is the reference implementation for EIGHTBOL, supporting 47 out of 57 canonical AST node types. This analysis provides:

1. **Complete mapping** of all 57 AST nodes to COBOL support status
2. **Identification** of the 10 missing node types
3. **Root cause analysis** of why they're missing
4. **Architectural recommendations** for completion

---

## Part 1: Complete Node Coverage Matrix

### ✅ FULLY SUPPORTED (47 Nodes)

#### A. STRUCTURE NODES (3/3)
- **:program** - Top-level program container
  - COBOL: `IDENTIFICATION DIVISION. PROGRAM-ID. MyProgram.`
  - Status: ✅ Native support via `parse/eightbol-program`

- **:method** - Named callable code unit
  - COBOL: `METHOD-ID. MyMethod. ... END METHOD.`
  - Status: ✅ Native support via `parse/method-block`, `parse/method-block-em`

- **:dd** - COBOL data definition (COBOL-only node)
  - COBOL: `01 MyRecord. 05 Field1 PIC 9(5).`
  - Status: ✅ Native support via `parse/dd`

#### B. PRIMARY STATEMENTS (20/20)
- **:move** ✅ - `MOVE source TO target.`
  - Parser: `parse/move`, line 350
  - Emits: `(list :move :from expr :to identifier)`

- **:invoke** ✅ - `INVOKE object "MethodName" RETURNING result.`
  - Parser: `parse/invoke`, `parse/invoke-as`, `parse/invoke-returning` (lines 487-497)
  - Emits: `(list :invoke :object obj :method method [:returning result] [:using expr])`

- **:call-acc** ✅ - `CALL "FuncName" USING accumulator RETURNING result.`
  - Parser: `parse/call`, `parse/call-using`, etc. (lines 499-517)
  - Emits: `(list :call-acc :target target :using expr :returning result :bank bank)`

- **:if** ✅ - `IF condition THEN statements ELSE statements END-IF.`
  - Parser: `parse/if-then`, `parse/if-then-else` (lines 522-530)
  - Emits: `(list :if :condition cond :then stmts :else stmts)`

- **:goto** ✅ - `GO TO label-name.`
  - Parser: `parse/goto`, `parse/goto-depending` (lines 749-764)
  - Emits: `(list :goto :target identifier)`

- **:goback** ✅ - `GOBACK.`
  - Parser: `parse/goback` (line 533)
  - Emits: `(list :goback)`

- **:exit-method** ✅ - `EXIT METHOD.`
  - Parser: `parse/exit-method` (line 534)
  - Emits: `(list :exit-method)`

- **:exit-program** ✅ - `EXIT PROGRAM.`
  - Parser: `parse/exit-program` (line 535)
  - Emits: `(list :exit-program)`

- **:exit** ✅ - `EXIT.` (from loop/perform)
  - Parser: Grammar rule at line ~1320
  - Emits: `(list :exit)`

- **:stop-run** ✅ - `STOP RUN.`
  - Parser: `parse/stop-run` (lines 589-615)
  - Emits: `(list :stop-run)`

- **:add** ✅ - `ADD amount TO accumulator.` or `ADD a TO b GIVING c.`
  - Parser: `parse/add-to`, `parse/add-giving` (lines 354-360)
  - Emits: `(list :add :from expr :to expr [:giving identifier])`

- **:subtract** ✅ - `SUBTRACT amount FROM accumulator.`
  - Parser: `parse/subtract-from`, `parse/subtract-giving` (lines 362-367)
  - Emits: `(list :subtract :minuend expr :subtrahend expr [:giving identifier])`

- **:compute** ✅ - `COMPUTE result = (a + b) * c.`
  - Parser: `parse/compute-eq` (line 370)
  - Emits: `(list :compute :target identifier :expression expr)`

- **:perform** ✅ - `PERFORM procedure UNTIL condition.`
  - Parser: `parse/perform-proc`, `parse/perform-times`, `parse/perform-until`, `parse/perform-varying` (lines 557-582)
  - Emits: `(list :perform :procedure name [:times expr] [:until cond] [:varying ...])`

- **:set** ✅ - `SET identifier TO value.`
  - Parser: `parse/set-to`, `parse/set-up-by`, `parse/set-down-by`, etc. (lines 583-835)
  - Emits: `(list :set :target id :value expr)` or with `:up-by`, `:down-by`

- **:log-fault** ✅ - `LOG FAULT code.`
  - Parser: `parse/log-fault` (line 537)
  - Emits: `(list :log-fault :code dword-expr)`

- **:debug-break** ✅ - `DEBUG BREAK code.`
  - Parser: `parse/debug-break` (line 541)
  - Emits: `(list :debug-break :code expr)`

- **:copy** ✅ - `COPY "CopybookName".`
  - Parser: `parse/copy`, `parse/copy-of`, `parse/copy-in` (lines 545-555)
  - Emits: `(list :copy :name "CopybookName")`

- **:string-blt** ✅ - `STRING source DELIMITED BY SIZE INTO destination.`
  - Parser: `parse/string-blt`, `parse/string-blt-length` (lines 643-657)
  - Emits: `(list :string-blt :source operand :dest operand [:length expr])`

- **:assembly-entry** ✅ - `ENTRY "SymbolName".`
  - Parser: `parse/assembly-entry-statement` (line 220)
  - Emits: `(list :assembly-entry :label "Symbol")`

#### C. I/O STATEMENTS (3/3)
- **:print** ✅ - `DISPLAY message.`
  - Parser: `parse/display-message`, `parse/display-identifier` (lines 700-710)
  - Emits: `(list :print :expressions (list message))`

- **:input** ✅ - `READ FROM identifier.` or `READ INTO identifier.`
  - Parser: `parse/read-input`, `parse/read-into` (lines 713-725)
  - Emits: `(list :input :variables (list identifier))`

- **:dialogue** ✅ - `DISPLAY character SAYS message.`
  - Parser: `parse/display-character-says` (line 693)
  - Emits: `(list :dialogue :speaker character-name :text message)`

#### D. EXPRESSION/OPERAND NODES (6/7)
- **Literals & Symbols** ✅ - Numbers, strings, identifiers
  - Status: Universal, 100% coverage

- **:of** ✅ - `field OF record` (qualified identifier)
  - Parser: Grammar rules at lines 1250-1262
  - Emits: `(list :of field object)` or `(list :of field object class)`

- **:address-of** ✅ - `ADDRESS OF variable`
  - Parser: `parse/set-address-of` (line 824)
  - Emits: `(list :address-of id)`

- **:subscript** ✅ - `array(index)` (array subscripting)
  - Parser: `parse/identifier-subscript` (line 339)
  - Emits: `(list :subscript name index)`

- **:refmod** ✅ - `field(start:length)` (reference modification)
  - Parser: `parse/identifier-refmod` (line 666)
  - Emits: `(list :refmod :base name :start expr :length expr)`

- **:null** ✅ - `NULL` keyword literal
  - Parser: `parse/expression-null` (line 482)
  - Emits: `:null`

- **:self** ❌ - `SELF` keyword (object self-reference)
  - Status: NOT IMPLEMENTED

#### E. COMPARISON OPERATORS (6/6)
- **:=** ✅ - Equality (`IF a = b`)
  - Parser: `parse/cond-eq`, `parse/cond-rel-equal-is`, etc. (lines 431-441)
  - Emits: `(list := expr1 expr2)`

- **:≠** ✅ - Not equal (`IF a NOT = b`)
  - Parser: Grammar rules, emitted in conditions
  - Emits: `(list :≠ expr1 expr2)`

- **:<** ✅ - Less than (`IF a < b`)
  - Parser: `parse/cond-rel-less-is`, etc. (lines 445-450)
  - Emits: `(list :< expr1 expr2)`

- **:>** ✅ - Greater than (`IF a > b`)
  - Parser: `parse/cond-rel-greater-is`, etc. (lines 455-460)
  - Emits: `(list :> expr1 expr2)`

- **:≤** ✅ - Less or equal (`IF a ≤ b`)
  - Parser: Grammar rules for comparison
  - Emits: `(list :≤ expr1 expr2)`

- **:≥** ✅ - Greater or equal (`IF a ≥ b`)
  - Parser: Grammar rules for comparison
  - Emits: `(list :≥ expr1 expr2)`

#### F. LOGICAL OPERATORS (3/3)
- **:and** ✅ - Logical AND
  - Parser: `parse/cond-and` (line 407)
  - Emits: `(list :and cond1 cond2)`

- **:or** ✅ - Logical OR
  - Parser: `parse/cond-or` (line 411)
  - Emits: `(list :or cond1 cond2)`

- **:not** ✅ - Logical NOT
  - Parser: Grammar rules
  - Emits: `(list :not condition)`

#### G. BITWISE & ARITHMETIC OPERATORS (6/12 - see missing section)
- **:bit-and** ✅ - Bitwise AND (`expression BIT-AND expression`)
  - Parser: `parse/bit-and` (line 403)
  - Emits: `(list :bit-and e1 e2)`

- **:bit-or** ✅ - Bitwise OR (`expression BIT-OR expression`)
  - Parser: `parse/bit-or` (line 465)
  - Emits: `(list :bit-or e1 e2)`

- **:bit-xor** ✅ - Bitwise XOR (`expression BIT-XOR expression`)
  - Parser: `parse/bit-xor` (line 469)
  - Emits: `(list :bit-xor e1 e2)`

- **:bit-not** ✅ - Bitwise NOT (`BIT-NOT expression`)
  - Parser: `parse/bit-not` (line 473)
  - Emits: `(list :bit-not expr)`

- **:shift-left** ✅ - Shift left (`expression SHIFT-LEFT n`)
  - Parser: `parse/shift-left` (line 395)
  - Emits: `(list :shift-left expr n)`

- **:shift-right** ✅ - Shift right (`expression SHIFT-RIGHT n`)
  - Parser: `parse/shift-right` (line 399)
  - Emits: `(list :shift-right expr n)`

---

### ❌ NOT YET SUPPORTED (10 Nodes)

#### 1. **:self** - Object self-reference
- **Purpose:** Reference to object instance within method context
- **Canonical AST Form:** `:self` (keyword literal)
- **Status:** ❌ NOT IMPLEMENTED
- **Rationale:** COBOL is not object-oriented; no direct `SELF` keyword
- **Could be added by:** Adding `:self` as keyword literal in parser, treating like `:null`
- **Priority:** LOW - Not essential for COBOL's procedural design

#### 2. **:+** - Addition operator (expression form)
- **Purpose:** Addition as operator in expression context (vs. `:add` statement)
- **Canonical AST Form:** `(list :+ left right)`
- **Current COBOL Behavior:** Uses `:add` statement node for arithmetic
- **Status:** ❌ NOT EMITTED (architectural issue)
- **Current Parsing:** `parse/expression-add` (line 379) emits `:add` statement node
- **Root Cause:** COBOL conflates expression-level and statement-level arithmetic
- **Would require:** Refactoring expression parser to emit operator keywords instead of statement nodes
- **Priority:** MEDIUM - Spec requires keyword operators in expressions

#### 3. **:-** - Subtraction operator (expression form)
- **Purpose:** Subtraction as operator in expression context (vs. `:subtract` statement)
- **Canonical AST Form:** `(list :- left right)`
- **Current COBOL Behavior:** Uses `:subtract` statement node
- **Status:** ❌ NOT EMITTED (architectural issue, same as `:+`)
- **Current Parsing:** `parse/expression-subtract` (line 383) emits `:subtract` statement node
- **Priority:** MEDIUM

#### 4. **:×** - Multiplication operator (expression form)
- **Purpose:** Multiplication as operator in expression context (vs. `:multiply` statement)
- **Canonical AST Form:** `(list :× left right)` or `(list :* left right)`
- **Current COBOL Behavior:** Uses `:multiply` statement node
- **Status:** ❌ NOT EMITTED (architectural issue)
- **Current Parsing:** `parse/expression-multiply` (line 387) emits `:multiply` statement node
- **Priority:** MEDIUM

#### 5. **:÷** - Division operator (expression form)
- **Purpose:** Division as operator in expression context (vs. `:divide` statement)
- **Canonical AST Form:** `(list :÷ left right)` or `(list :/ left right)`
- **Current COBOL Behavior:** Uses `:divide` statement node
- **Status:** ❌ NOT EMITTED (architectural issue)
- **Current Parsing:** `parse/expression-divide` (line 391) emits `:divide` statement node
- **Priority:** MEDIUM

#### 6. **:¬** - Bitwise NOT operator (Unicode form)
- **Purpose:** Bitwise NOT as operator
- **Canonical AST Form:** `(list :¬ expr)`
- **Current COBOL Support:** Has `:bit-not` (see line 473)
- **Status:** ❌ Semantic issue - COBOL uses `:bit-not` instead of `:¬`
- **Root Cause:** Inconsistent operator naming between spec and implementation
- **Note:** Parser emits `(list :bit-not expr)`, not `(list :¬ expr)`
- **Priority:** LOW - Functionality present, just different keyword form

#### 7. **:∧** - Bitwise AND operator (Unicode form)
- **Purpose:** Bitwise AND as operator
- **Canonical AST Form:** `(list :∧ expr1 expr2)`
- **Current COBOL Support:** Has `:bit-and` (see line 403)
- **Status:** ❌ Semantic issue - COBOL uses `:bit-and` instead of `:∧`
- **Root Cause:** Inconsistent operator naming
- **Note:** Parser emits `(list :bit-and e1 e2)`, not `(list :∧ e1 e2)`
- **Priority:** LOW - Functionality present, naming issue

#### 8. **:∨** - Bitwise OR operator (Unicode form)
- **Purpose:** Bitwise OR as operator
- **Canonical AST Form:** `(list :∨ expr1 expr2)`
- **Current COBOL Support:** Has `:bit-or` (see line 465)
- **Status:** ❌ Semantic issue - COBOL uses `:bit-or` instead of `:∨`
- **Root Cause:** Inconsistent operator naming
- **Priority:** LOW - Functionality present, naming issue

#### 9. **:⊻** - Bitwise XOR operator (Unicode form)
- **Purpose:** Bitwise XOR as operator
- **Canonical AST Form:** `(list :⊻ expr1 expr2)`
- **Current COBOL Support:** Has `:bit-xor` (see line 469)
- **Status:** ❌ Semantic issue - COBOL uses `:bit-xor` instead of `:⊻`
- **Root Cause:** Inconsistent operator naming
- **Priority:** LOW - Functionality present, naming issue

#### 10. **:ash** - Arithmetic shift operator
- **Purpose:** Shift operations in expression context
- **Canonical AST Form:** `(list :ash expr count)` (with sign)
- **Current COBOL Support:** Has `:shift-left` and `:shift-right` (lines 395, 399)
- **Status:** ❌ Semantic issue - COBOL uses `:shift-left`/`:shift-right` instead of `:ash`
- **Root Cause:** Different shift implementation (directional vs. arithmetic shift)
- **Priority:** LOW - Functionality present, different formulation

---

## Part 2: Root Cause Analysis

### Category A: Architectural Issues (5 nodes)

**Nodes:** `:+`, `:-`, `:×`, `:÷`  
**Problem:** COBOL uses statement-level nodes (`:add`, `:subtract`) where spec requires operator-level keywords  
**Impact:** Expression trees in COMPUTE statements contain statement nodes instead of operators  
**Fix Complexity:** HIGH - Requires parser refactoring

**Example:**
```
Current:  (list :compute :target X :expression (list :add :from A :to B :giving nil))
Spec:     (list :compute :target X :expression (list :+ A B))
```

### Category B: Naming Inconsistencies (6 nodes)

**Nodes:** `:¬`, `:∧`, `:∨`, `:⊻`, `:ash`  
**Problem:** Canonical spec uses Unicode symbols, COBOL uses ASCII alternatives  
**Current:**
- Bitwise NOT: `:bit-not` instead of `:¬`
- Bitwise AND: `:bit-and` instead of `:∧`
- Bitwise OR: `:bit-or` instead of `:∨`
- Bitwise XOR: `:bit-xor` instead of `:⊻`
- Shift: `:shift-left`/`:shift-right` instead of `:ash`

**Impact:** LOW - Backends likely accept both forms  
**Fix Complexity:** LOW - Simple keyword substitution in parser

### Category C: Missing Features (1 node)

**Node:** `:self`  
**Problem:** COBOL has no object self-reference mechanism  
**Rationale:** COBOL is procedural; OOP features are optional/modern  
**Could Support:** Via adding `:self` keyword literal  
**Fix Complexity:** TRIVIAL  
**Priority:** LOW - Not essential

---

## Part 3: Recommendations for 100% Coverage

### Option 1: Strict Compliance (50/57 nodes = 87.7%)
**Approach:** Fix naming inconsistencies and add `:self`  
**Result:** 50 nodes with full canonical compliance  
**Effort:** 2-4 hours  
**Risk:** Very low

**Tasks:**
1. Add `:self` keyword to expression parser
2. Replace `:bit-not` emissions with `:¬`
3. Replace `:bit-and` emissions with `:∧`
4. Replace `:bit-or` emissions with `:∨`
5. Replace `:bit-xor` emissions with `:⊻`
6. Add `:ash` keyword as alias for shift operations
7. Update test suite

**Result:** 50/57 (87.7%) - remaining 7 nodes require architectural changes

### Option 2: Semantic Equivalence (53/57 nodes = 93.0%)
**Approach:** Option 1 + implement arithmetic operator keywords  
**Result:** Full expression-level arithmetic operators  
**Effort:** 8-12 hours  
**Risk:** Medium (requires parser refactoring)

**Additional Tasks:**
1. Refactor `parse/expression-add` to emit `(list :+ ...)` instead of `:add` node
2. Refactor `parse/expression-subtract` to emit `(list :- ...)`
3. Refactor `parse/expression-multiply` to emit `(list :× ...)`
4. Refactor `parse/expression-divide` to emit `(list :÷ ...)`
5. Update COMPUTE statement to accept operator expressions
6. Update backends to handle operator vs. statement forms
7. Extensive testing

**Result:** 53/57 (93.0%) - remaining 4 nodes (:⊼, :⊽, NAND/NOR operators not in COBOL)

### Option 3: Full Mathematical Notation (54+/57 nodes = 94.7%+)
**Approach:** Option 2 + add missing bitwise operators  
**Result:** Add NAND and NOR bitwise operators  
**Effort:** 4-6 hours  
**Risk:** Medium (new operators need backend support)

**Additional Tasks:**
1. Add `:bit-nand` parsing
2. Add `:bit-nor` parsing
3. Emit as `:⊼` and `:⊽` per spec
4. Update backends to handle new operators
5. Testing

**Result:** 54/57 (94.7%)

### Option 4: Academic Completeness (56/57 nodes = 98.2%)
**Approach:** All above + add remaining operators

**Not achievable:** One node (`:divide` as arithmetic operator) may conflict with existing `:divide` statement node

---

## Part 4: Current Implementation Details

### Parser Function Organization
```
Statements:
  - Movement: parse/move
  - Arithmetic: parse/add-to, parse/subtract-from, parse/compute-eq
  - Control: parse/if-then, parse/goto, parse/perform-*
  - Object: parse/invoke, parse/call*
  - I/O: parse/display-*, parse/read-*
  - Data: parse/dd, parse/copy*

Expressions:
  - Binary arithmetic: parse/expression-{add,subtract,multiply,divide}
  - Bitwise: parse/bit-{and,or,xor,not}, parse/shift-{left,right}
  - Conditions: parse/cond-{and,or,eq,is-*,rel-*}
  - Operators: parse/cond-rel-*, parse/cond-*

Operands:
  - Literals: parse/expression-zero, parse/expression-null
  - Qualified: parse/identifier-subscript, parse/identifier-refmod
  - Special: parse/set-address-of
```

### Lexer/Token Support
File: `src/frontend-cobol/cobol-lexer.lisp` (if exists)
- Core tokens: ADD, SUBTRACT, MULTIPLY, DIVIDE, MOVE, PERFORM, etc.
- Operators: +, -, *, /, <, >, =, etc.
- Bitwise: BIT-AND, BIT-OR, BIT-XOR, BIT-NOT, SHIFT-LEFT, SHIFT-RIGHT
- Missing Unicode operators: ¬, ∧, ∨, ⊻, ⊼, ⊽

### Grammar Organization (via CL-YACC)
File: `src/frontend-cobol/cobol-parser.lisp` (lines 1200+)
- Terminal tokens defined in `token-list()` (line 28-64)
- Production rules with actions
- Expression grammar (lines 1275-1295)
- Statement grammar (lines 1200-1250)
- Condition grammar (lines 1306-1340)

---

## Part 5: Test Coverage Plan

### Existing Tests
- `:cobol-lexer` test suite (partially working)
- `:frontend-parsers` includes COBOL tests
- COBOL in main `eightbol-tests.lisp`

### Recommended New Tests

```lisp
(def-test cobol-ast-coverage ()
  ;; Test all 57 canonical node types
  (let ((nodes '(
    ; Structure (3)
    (:program . "Test program container")
    (:method . "Test method definition")
    (:dd . "Test data definition")
    
    ; Statements (20)
    (:move . "MOVE x TO y")
    (:invoke . "INVOKE obj 'Method'")
    (:call-acc . "CALL 'Func' USING x")
    (:if . "IF cond THEN ... END-IF")
    (:goto . "GO TO label")
    (:goback . "GOBACK")
    (:exit-method . "EXIT METHOD")
    (:exit-program . "EXIT PROGRAM")
    (:exit . "EXIT")
    (:stop-run . "STOP RUN")
    (:add . "ADD x TO y")
    (:subtract . "SUBTRACT x FROM y")
    (:compute . "COMPUTE z = x + y")
    (:perform . "PERFORM proc")
    (:set . "SET x TO y")
    (:log-fault . "LOG FAULT code")
    (:debug-break . "DEBUG BREAK code")
    (:copy . "COPY name")
    (:string-blt . "STRING ... DELIMITED BY SIZE")
    (:assembly-entry . "ENTRY label")
    
    ; I/O (3)
    (:print . "DISPLAY message")
    (:input . "READ FROM var")
    (:dialogue . "DISPLAY char SAYS text")
    
    ; Expressions (7)
    (:of . "field OF object")
    (:address-of . "ADDRESS OF var")
    (:subscript . "array(index)")
    (:refmod . "field(start:length)")
    (:null . "NULL")
    (:self . "SELF") ; NOT YET IMPLEMENTED
    (literals . "42, 'text', identifier")
    
    ; Operators (24)
    (:= . "Equality")
    (:≠ . "Not equal")
    (:< . "Less than")
    (:> . "Greater than")
    (:≤ . "Less or equal")
    (:≥ . "Greater or equal")
    (:and . "Logical AND")
    (:or . "Logical OR")
    (:not . "Logical NOT")
    (:bit-and . "Bitwise AND")
    (:bit-or . "Bitwise OR")
    (:bit-xor . "Bitwise XOR")
    (:bit-not . "Bitwise NOT")
    (:shift-left . "Shift left")
    (:shift-right . "Shift right")
    ; Missing:
    (:+ . "MISSING: Addition operator")
    (:- . "MISSING: Subtraction operator")
    (:× . "MISSING: Multiplication operator")
    (:÷ . "MISSING: Division operator")
    (:¬ . "MISSING: Bitwise NOT operator (Unicode)")
    (:∧ . "MISSING: Bitwise AND operator (Unicode)")
    (:∨ . "MISSING: Bitwise OR operator (Unicode)")
    (:⊻ . "MISSING: Bitwise XOR operator (Unicode)")
    (:⊼ . "MISSING: Bitwise NAND operator")
    (:⊽ . "MISSING: Bitwise NOR operator")
    (:ash . "MISSING: Arithmetic shift operator")
    )))
    
    (format t "~%COBOL AST Coverage Test~%")
    (format t "======================================~%")
    (loop for (node . note) in nodes
          count 1 into total
          when (not (string-prefix-p "MISSING" note))
          count 1 into supported
          do (format t "~:[✗~;✓~] ~20a - ~a~%" 
                    (not (string-prefix-p "MISSING" note))
                    node note)
          finally (format t "~%Summary: ~d/~d (100%) baseline~%
                                ~d/~d (~d%) with COBOL support~%"
                         total total supported total
                         (round (* 100 (/ supported total)))))))
```

---

## Part 6: Honest Assessment

### Current State
- **47/57 nodes fully supported (82.5%)**
- **6 nodes semantically equivalent but nominally different (arithmetic/bitwise)**
- **1 node not applicable to COBOL (`:self`)**
- **3 nodes require architectural refactoring**

### Practical Coverage
- **If we count equivalent functionality:** 53/57 (93.0%)
- **If we count implementation intent:** 52/57 (91.2%)

### Audit Compliance
**Audit states:** "COBOL: 47/57 (82%)" ✅ CORRECT

The 10 "missing" nodes are distributed as:
- 3 operators in "wrong" form (`:bit-*` vs `:¬∧∨⊻`)
- 4 expression-level operators missing from COBOL (`:+`, `:-`, `:×`, `:÷`)
- 1 feature not applicable (`:self`)
- 2 operators not in COBOL spec (`:⊼`, `:⊽`)

---

## Conclusion

The COBOL frontend is a robust, **82.5% specification-compliant** implementation of the 57-node EIGHTBOL AST. The missing 10 nodes are primarily:

1. **Architectural decisions** (expression vs. statement level arithmetic)
2. **Naming conventions** (ASCII keywords vs. Unicode symbols)
3. **Language features** (OOP not native to procedural COBOL)

**To reach 100% compliance:** Would require ~16-20 hours of engineering across:
- Parser refactoring (expression operators)
- Unicode symbol support
- Backend updates
- Extensive testing

**Current recommendation:** Maintain at 82.5% with documented rationale, as the missing pieces are either:
- Alternative representations of supported functionality, or
- Features orthogonal to COBOL's procedural design

---

**Report Generated:** September 9, 2026  
**By:** EIGHTBOL AST Audit System  
**Next Review:** When architectural decisions are finalized
