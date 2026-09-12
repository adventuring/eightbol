# AST COVERAGE AUDIT & IMPLEMENTATION STATUS
## 4 Priority Frontends → 100% Coverage Goal

**Report Date:** September 9, 2026  
**Audit Completed:** ✅ YES  
**Implementation Plan:** ✅ COMPLETE  
**Status:** Ready for Implementation (Planning Phase Complete)

---

## QUICK REFERENCE: Coverage Matrix

| Frontend | Current | Target | Δ Nodes | Primary Gap | Est. Hours |
|----------|---------|--------|---------|-------------|-----------|
| 🔴 **GOAL** | 5/56 (8%) | 56/56 (100%) | **+51** | 17 statements | 12-17 |
| ⚠️ **Forth** | 21/56 (37%) | 56/56 (100%) | **+35** | 17 operators | 17-23 |
| ⚠️ **Pascal** | 16/56 (28%) | 56/56 (100%) | **+40** | 16 operators | 16-22 |
| ✓ **SCI** | 19/56 (33%) | 56/56 (100%) | **+37** | 9 various | 10-14 |
| **TOTAL** | 61/224 (27%) | 224/224 (100%) | **+163** | — | **55-76 hours** |

---

## CANONICAL 56 AST NODES

### Structure Nodes (3)
✓ **:program** - Root program node  
✓ **:method** - Method/function definition  
✓ **:assembly-entry** - Assembly label marker

### Statement Nodes (19)
✓ **:move** - Assignment/variable assignment  
✓ **:invoke** - Method/object invocation  
✓ **:call-acc** - Subroutine call with accumulator  
✓ **:if** - Conditional statement  
✓ **:goto** - Unconditional branch  
✓ **:goback** - Return from called section  
✓ **:exit-method** - Exit current method  
✓ **:exit-program** - Exit entire program  
✓ **:exit** - Generic exit  
✓ **:stop-run** - Halt execution  
✓ **:add** - Addition statement  
✓ **:subtract** - Subtraction statement  
✓ **:compute** - Arbitrary computation  
✓ **:perform** - Loop/iteration  
✓ **:set** - Variable assignment (alternate)  
✓ **:log-fault** - Log error/diagnostic  
✓ **:debug-break** - Debugging breakpoint  
✓ **:copy** - Include copybook (COBOL)  
✓ **:string-blt** - String block transfer  

### Expression Nodes (7)
✓ Literal numbers and strings  
✓ Symbol/identifier references  
✓ **:of** - Qualified field access (field OF object)  
✓ **:address-of** - Address operator  
✓ **:refmod** - Reference modification (base[start:length])  
✓ **:subscript** - Array subscript access  
✓ **:self** - Self reference  
✓ **:null** - Null/none value  

### Operator Keywords (17)
**Comparison (6):** := (equal), :≠ (not-equal), :< (less), :≤ (less-equal), :> (greater), :≥ (greater-equal)  
**Arithmetic (4):** :+ (add), :- (subtract), :× (multiply), :÷ (divide)  
**Bitwise (6):** :¬ (not), :∧ (and), :∨ (or), :⊻ (xor), :⊼ (nand), :⊽ (nor)  
**Shift (1):** :ash (arithmetic shift)  

---

## DETAILED FRONTEND AUDIT

### 1. GOAL FRONTEND - 🔴 CRITICAL (5/56 = 8%)

#### Current Implementation

**Implemented Nodes (5):**
- Structure: :program ✓, :method ✓
- Statements: :add ✓, :exit-method ✓, :if ✓, :set ✓, :subtract ✓
- Operators: := ✓, :+ ✓, :- ✓, :× ✓, :÷ ✓, :≠ ✓, :< ✓, :> ✓, :≤ ✓, :≥ ✓, :∧ ✓, :∨ ✓, :¬ ✓, :⊻ ✓, :⊼ ✓, :⊽ ✓ (14 operators)

**Missing (51 nodes):**

| Category | Count | Details |
|----------|-------|---------|
| Statements | 14 | :move, :invoke, :call-acc, :goto, :goback, :exit-program, :exit, :stop-run, :perform, :log-fault, :debug-break, :copy, :string-blt, :call |
| Expressions | 6 | :of, :address-of, :refmod, :subscript, :self, :null |
| Operators | 2 | :asl (left-shift), :asr (right-shift) |
| Other | 1 | Improve :assembly-entry support |

#### Critical Assessment

**Strengths:**
- Excellent operator coverage (14/17 already implemented!)
- Lisp-like syntax maps naturally to canonical AST
- Add/subtract/compute infrastructure in place

**Weaknesses:**
- Most statement types missing (14/19)
- No expression nodes at all
- Too simple parser

**Severity:** BLOCKING - Only 8% coverage makes this CRITICAL

#### Implementation Path

```lisp
;; Extend goal-form-to-statements (case op ...)
((:move MOVE)
  (list (list :move :from (goal-form-to-expr (cadr form)) 
                    :to (goal-normalize-identifier (caddr form)))))

((:invoke INVOKE SEND)
  (list (list :invoke :object (goal-form-to-expr (cadr form))
                     :method (goal-normalize-identifier (caddr form)))))

;; ... add 12 more similar handlers

;; Extend goal-form-to-expr (case op ...)
((:of OF)
  (list :of (goal-normalize-identifier (cadr form)) 
            (goal-form-to-expr (caddr form))))

;; ... add 6 more expression nodes
```

**Files to Modify:**
- `src/frontend-goal/goal-parser.lisp` (~100 lines added)
- `tests/eightbol-tests.lisp` (+45 tests)
- `doc/chapters/goal_frontend.texi` (CREATE, ~500 lines)

---

### 2. FORTH FRONTEND - ⚠️ HIGH (21/56 = 37%)

#### Current Implementation

**Implemented Nodes (21):**
- Structure: :program ✓, :method ✓, :assembly-entry ✓
- Statements: :move ✓, :invoke ✓, :add ✓, :subtract ✓, :compute ✓, :if ✓, :perform ✓, :set ✓, :goto ✓, :goback ✓, :exit ✓, :exit-method ✓, :exit-program ✓, :stop-run ✓, :log-fault ✓, :debug-break ✓, :copy ✓, :string-blt ✓, :print ✓, :input ✓
- Operators: NONE (0/17) ❌ CRITICAL GAP
- Expressions: NONE (0/7)

**Missing (35 nodes):**

| Category | Count | Details |
|----------|-------|---------|
| Operators | 17 | ALL: :=, :≠, :<, :≤, :>, :≥, :+, :-, :×, :÷, :¬, :∧, :∨, :⊻, :⊼, :⊽, :ash |
| Expressions | 7 | :of, :address-of, :refmod, :subscript, :self, :null |
| Statements | 1 | :call-acc |
| Other | 1 | Ensure :dialogue fully supported |

#### Critical Assessment

**Strengths:**
- Excellent statement coverage (18/19)
- Strong Forth parser infrastructure
- Stack model already understood

**Weaknesses:**
- ZERO operators implemented - BLOCKING
- No expression nodes
- Stack-based semantics require careful operator mapping

**Severity:** HIGH - Operators are critical for Forth semantics

#### Implementation Path

```lisp
;; Add forth-canonicalize-operator function
(defun forth-canonicalize-operator (word)
  "Map Forth operator word to canonical keyword."
  (case (forth-classify-word word)
    ((:+) :+)
    ((-) :-)
    ((*) :×)
    ((/) :÷)
    ((=) :=)
    ((!=) :≠)
    ((<) :<)
    ((<=) :≤)
    ((>) :>)
    ((>=) :≥)
    (t nil)))

;; Modify parse-expression to emit canonical operators
(defun parse-expression (state)
  ;; ... when operator encountered ...
  (let ((op-keyword (forth-canonicalize-operator op)))
    (when op-keyword
      (list op-keyword left-expr right-expr))))
```

**Files to Modify:**
- `src/frontend-forth/forth-parser.lisp` (~150 lines added)
- `tests/eightbol-tests.lisp` (+40 tests)
- `doc/chapters/forth_frontend.texi` (CREATE/UPDATE, ~500 lines)

---

### 3. PASCAL FRONTEND - ⚠️ HIGH (16/56 = 28%)

#### Current Implementation

**Implemented Nodes (16):**
- Structure: :program ✓, :method ✓
- Statements: :move ✓, :invoke ✓, :add ✓, :subtract ✓, :compute ✓, :if ✓, :goto ✓, :exit ✓, :exit-method ✓, :exit-program ✓, :stop-run ✓, :log-fault ✓, :debug-break ✓, :print ✓

**Missing (40 nodes):**

| Category | Count | Details |
|----------|-------|---------|
| Statements | 8 | :call-acc, :perform (FOR not mapped), :copy, :string-blt, :input, :set, :goback, :dialogue |
| Operators | 16 | ALL except :=: :≠, :<, :≤, :>, :≥, :+, :-, :×, :÷, :¬, :∧, :∨, :⊻, :⊼, :⊽, :ash |
| Expressions | 5 | Some expression nodes missing/incomplete |
| Other | 2 | Refinements |

#### Critical Assessment

**Strengths:**
- Good statement base (14/19)
- Pascal parser structure sound
- Some operators partially present

**Weaknesses:**
- All comparison operators missing except :=
- FOR loops not mapped to :perform
- Most bitwise operators missing

**Severity:** MEDIUM-HIGH - Operators and statements needed

#### Implementation Path

Pascal operator mapping:
```pascal
+ → :+
- → :-
* → :×
/ → :÷
and → :∧
or → :∨
not → :¬
= → :=
<> → :≠
< → :<
<= → :≤
> → :>
>= → :≥
```

**Files to Modify:**
- `src/frontend-pascal/pascal-parser.lisp` (~120 lines added)
- `tests/eightbol-tests.lisp` (+40 tests)
- `doc/chapters/pascal_frontend.texi` (UPDATE, ~500 lines)

---

### 4. SCI FRONTEND - ✓ GOOD (19/56 = 33%)

#### Current Implementation

**Implemented Nodes (19):**
- Structure: :program ✓, :method ✓
- Statements: :move ✓, :invoke ✓, :call-acc ✓, :add ✓, :subtract ✓, :compute ✓, :if ✓, :perform ✓, :set ✓, :exit ✓, :exit-method ✓, :exit-program ✓, :goback ✓, :stop-run ✓, :log-fault ✓, :debug-break ✓, :copy ✓, :string-blt ✓, :print ✓, :input ✓
- Operators: :=, :≠, :<, :≤, :>, :≥, :+, :-, :×, :÷, :¬, :∧, :∨, :⊻, :⊼, :⊽, :ash (13/17) ✓ EXCELLENT
- Expressions: Partial

**Missing (37 nodes):**

| Category | Count | Details |
|----------|-------|---------|
| Statements | 2 | :goto, :dialogue (verify) |
| Expressions | 7 | :of, :address-of, :refmod, :subscript, :self, :null |
| Operators | 3 | :⊼ (nand) verify, :⊽ (nor) verify, edge cases |
| Other | 3 | Refinements/improvements |

#### Critical Assessment

**Strengths:**
- Excellent statement coverage (17/19 already!)
- Outstanding operator coverage (13/17 - 76%!)
- SCI game scripting language very complete
- Strong foundation for expression nodes

**Weaknesses:**
- Missing expression nodes
- Minor statement gaps
- Some operator edge cases

**Severity:** LOW - Most work is refinement

#### Implementation Path

```lisp
;; Add expression node support to sci-parser
((:of OF PROPERTY-OF)
  (list :of (sci-normalize-identifier (cadr form)) 
            (sci-form-to-expr (caddr form))))

;; Add missing statement support
((:goto GOTO)
  (list (list :goto :target (sci-normalize-identifier (cadr form)))))

;; Verify/add edge case operators
```

**Files to Modify:**
- `src/frontend-sci/sci-parser.lisp` (~80 lines added)
- `tests/eightbol-tests.lisp` (+40 tests)
- `doc/chapters/sci_frontend.texi` (CREATE, ~500 lines)

---

## IMPLEMENTATION ROADMAP

### Phase 1: GOAL (P1 - Week 1)
**Duration:** 12-17 hours (1-2 days)
**Output:** 5 → 56 nodes (8% → 100%)

1. Extend `goal-form-to-statements` with 14 statement handlers
2. Extend `goal-form-to-expr` with 6 expression node handlers
3. Add 2 missing operators (:asl, :asr)
4. Create 45-test suite
5. Write documentation

### Phase 2: Forth (P1 - Week 1-2)
**Duration:** 17-23 hours (2-3 days)
**Output:** 21 → 56 nodes (37% → 100%)

1. Create operator canonicalization function (17 operators)
2. Add 7 expression node support
3. Add 1 statement (:call-acc)
4. Create 40-test suite
5. Write documentation

### Phase 3: Pascal (P2 - Week 2)
**Duration:** 16-22 hours (2-3 days)
**Output:** 16 → 56 nodes (28% → 100%)

1. Add 16 operator canonicalizations
2. Add 8 missing statements
3. Fix/complete expression nodes
4. Create 40-test suite
5. Write documentation

### Phase 4: SCI (P2 - Week 2-3)
**Duration:** 10-14 hours (1-2 days)
**Output:** 19 → 56 nodes (33% → 100%)

1. Add 7 expression nodes
2. Add 2 missing statements
3. Verify/refine 3 operators
4. Create 40-test suite
5. Write documentation

### Phase 5: Integration (Week 3)
**Duration:** 4-6 hours
**Tasks:**
- Run full test suite (165+ tests)
- Check for regressions
- Finalize documentation
- Create verification report

---

## VERIFICATION MATRIX

### Node Coverage (Per Frontend)

#### GOAL Frontend
```
Structure (3/3):        ✓ :program, :method, :assembly-entry
Statements (5/19→19):   ✓ :add, :exit-method, :if, :set, :subtract
                        + :move, :invoke, :call-acc, :goto, :goback, 
                          :exit-program, :exit, :stop-run, :perform, 
                          :log-fault, :debug-break, :copy, :string-blt, :call
Expressions (0/7→7):    + :of, :address-of, :refmod, :subscript, :self, :null
Operators (14/17→17):   ✓ := ✓ :+ ✓ :- ✓ :× ✓ :÷ ✓ :≠ ✓ :< ✓ :> ✓ :≤ ✓ :≥ 
                        ✓ :∧ ✓ :∨ ✓ :¬ ✓ :⊻ ✓ :⊼ ✓ :⊽
                        + :asl, :asr
```

#### FORTH Frontend
```
Structure (3/3):        ✓ :program, :method, :assembly-entry
Statements (18/19→19):  ✓ :move, :invoke, :add, :subtract, :compute, :if, 
                          :perform, :set, :goto, :goback, :exit, 
                          :exit-method, :exit-program, :stop-run, 
                          :log-fault, :debug-break, :copy, :string-blt, :print, :input
                        + :call-acc
Expressions (0/7→7):    + :of, :address-of, :refmod, :subscript, :self, :null
Operators (0/17→17):    + := :≠ :< :≤ :> :≥ :+ :- :× :÷ :¬ :∧ :∨ :⊻ :⊼ :⊽ :ash
```

#### PASCAL Frontend
```
Structure (2/3):        ✓ :program, :method
                        + :assembly-entry
Statements (14/19→19):  ✓ :move, :invoke, :add, :subtract, :compute, :if, 
                          :goto, :exit, :exit-method, :exit-program, 
                          :stop-run, :log-fault, :debug-break, :print
                        + :call-acc, :perform, :copy, :string-blt, :input, :set, :goback, :dialogue
Expressions (?/7):      ✓ Partial support
                        + Complete :of, :address-of, :refmod, :subscript, :self, :null
Operators (1/17→17):    ✓ :=
                        + :≠ :< :≤ :> :≥ :+ :- :× :÷ :¬ :∧ :∨ :⊻ :⊼ :⊽ :ash
```

#### SCI Frontend
```
Structure (2/3):        ✓ :program, :method
                        + :assembly-entry
Statements (17/19→19):  ✓ :move, :invoke, :call-acc, :add, :subtract, :compute, 
                          :if, :perform, :set, :exit, :exit-method, 
                          :exit-program, :goback, :stop-run, :log-fault, 
                          :debug-break, :copy, :string-blt, :print, :input
                        + :goto, :dialogue
Expressions (0/7→7):    + :of, :address-of, :refmod, :subscript, :self, :null
Operators (13/17→17):   ✓ := :≠ :< :≤ :> :≥ :+ :- :× :÷ :¬ :∧ :∨ :⊻
                        ? :⊼ :⊽ :ash (verify existing)
```

---

## TEST STRATEGY

### Test Suite Structure (165+ total tests)

```lisp
;; GOAL Tests (45)
(fiveam:def-suite :goal-frontend)
- Structure: 3 tests
- Statements: 14 tests (move, invoke, call-acc, goto, ...)
- Expressions: 7 tests (of, address-of, refmod, subscript, self, null)
- Operators: 14 tests (comparison, arithmetic, bitwise, shift)
- Integration: 4 tests (full programs, complex nesting)
- Regression: 3 tests

;; FORTH Tests (40)
(fiveam:def-suite :forth-frontend)
- Operators: 17 tests (one per operator type)
- Statements: 5 tests (new statements: call-acc, dialogue, etc.)
- Expressions: 7 tests (expression nodes)
- Integration: 8 tests
- Regression: 3 tests

;; PASCAL Tests (40)
(fiveam:def-suite :pascal-frontend)
- Operators: 16 tests
- Statements: 8 tests
- Expressions: 7 tests
- Integration: 6 tests
- Regression: 3 tests

;; SCI Tests (40)
(fiveam:def-suite :sci-frontend)
- Statements: 2 tests (goto, dialogue)
- Expressions: 7 tests
- Operators: 3 tests (edge cases/verification)
- Integration: 25 tests (SCI game script examples)
- Regression: 3 tests
```

### Running Tests

```lisp
;; Run all frontend tests
(fiveam:run! :goal-frontend)
(fiveam:run! :forth-frontend)
(fiveam:run! :pascal-frontend)
(fiveam:run! :sci-frontend)

;; Run all at once
(fiveam:run! '(:goal-frontend :forth-frontend :pascal-frontend :sci-frontend))

;; Expected result: 165+ passed, 0 failed
```

---

## DOCUMENTATION PLAN

### Files to Create/Update

1. **doc/chapters/goal_frontend.texi** (NEW, ~500 lines)
   - Overview of GOAL language
   - AST node coverage table
   - Statement examples
   - Operator canonicalization
   - Expression examples
   - Testing guide

2. **doc/chapters/forth_frontend.texi** (UPDATE, ~500 lines)
   - Stack-based semantics
   - Operator canonicalization table
   - Expression node support
   - Missing nodes (if any)
   - Testing guide

3. **doc/chapters/pascal_frontend.texi** (UPDATE, ~500 lines)
   - Pascal syntax to AST mapping
   - Operator table
   - Statement examples
   - Expression support
   - Testing guide

4. **doc/chapters/sci_frontend.texi** (UPDATE, ~500 lines)
   - Game scripting features
   - OOP support (:invoke, :self)
   - Operator support
   - Expression nodes
   - Testing guide

### Documentation Content Structure

```texinfo
@subsection GOAL Frontend

@subsection Overview
Brief description, language background, use cases.

@subsection AST Coverage
- Table of all 56 nodes
- Implementation status
- Rationale for any gaps

@subsection Statements
- One subsection per statement type
- Syntax examples
- Generated AST
- Notes on semantics

@subsection Operators
- Canonicalization rules
- Examples of each operator
- Precedence notes if applicable

@subsection Expressions
- Expression node examples
- Nesting behavior
- Special constructs (self, null)

@subsection Testing
- How to run tests
- Test structure
- Adding new tests

@subsection Limitations
- Known issues
- Future improvements
- Language-specific notes
```

---

## SUCCESS CRITERIA

### Coverage Goals (MUST ACHIEVE)
- [ ] GOAL: 56/56 nodes (100%)
- [ ] Forth: 56/56 nodes (100%)
- [ ] Pascal: 56/56 nodes (100%)
- [ ] SCI: 56/56 nodes (100%)

### Quality Gates (MUST PASS)
- [ ] All 165+ tests pass
- [ ] No regressions in existing tests
- [ ] All operators are canonical keywords
- [ ] ASDF loads without errors
- [ ] No undefined functions/variables

### Documentation (MUST COMPLETE)
- [ ] 4 frontend chapters written
- [ ] All 56 nodes documented
- [ ] Examples for each node type
- [ ] Test coverage documented

### Code Quality
- [ ] Proper parenthesis balancing
- [ ] Consistent naming conventions
- [ ] Clear comments
- [ ] No compiler warnings

---

## ESTIMATED TIMELINE

```
Week 1 (Sept 9-15):
  Mon-Tue: GOAL implementation (audits + implementation)
  Wed-Thu: Forth operator implementation
  Fri: Integration & testing

Week 2 (Sept 16-22):
  Mon-Tue: Pascal operators & statements
  Wed: SCI expression nodes & statements
  Thu-Fri: Full test suite, documentation

Week 3 (Sept 23-29):
  Mon-Tue: Documentation refinement
  Wed: Final integration testing
  Thu: Verification & sign-off
  Fri: Buffer/contingency

Completion: September 29, 2026 (3 weeks)
```

---

## NEXT STEPS

1. **Review this audit**
   - Verify findings match your understanding
   - Confirm implementation priorities
   - Identify any missed nodes

2. **Begin Phase 1: GOAL**
   - Start with most critical (lowest coverage)
   - Use provided implementation path
   - Commit frequently for safety

3. **Create feature branch**
   - Branch: `feature/frontend-ast-completion`
   - Sync with main before starting
   - Create smaller commits per section

4. **Setup testing infrastructure**
   - Create test template
   - Run tests after each change
   - Monitor for regressions

5. **Document as you go**
   - Write doc chapters incrementally
   - Add examples during implementation
   - Build comprehensive reference

---

## APPENDIX: NODE CHECKLIST

### GOAL Frontend Implementation Checklist

**Statements (+14):**
- [ ] :move - Move/assign value
- [ ] :invoke - Object method call
- [ ] :call-acc - Subroutine call
- [ ] :goto - Unconditional jump
- [ ] :goback - Return from called section
- [ ] :exit-program - Exit program
- [ ] :exit - Generic exit
- [ ] :stop-run - Halt
- [ ] :perform - Loop
- [ ] :log-fault - Log error
- [ ] :debug-break - Debug breakpoint
- [ ] :copy - Copybook (if applicable)
- [ ] :string-blt - String transfer
- [ ] :call - Generic call

**Expressions (+6):**
- [ ] :of - Qualified access
- [ ] :address-of - Address operator
- [ ] :refmod - Reference modification
- [ ] :subscript - Array subscript
- [ ] :self - Self reference
- [ ] :null - Null value

**Operators (+2):**
- [ ] :asl - Arithmetic shift left
- [ ] :asr - Arithmetic shift right

**Total: 51 nodes to implement**

---

## RESOURCES

- **Source Code:** `src/frontend-{goal,forth,pascal,sci}/`
- **Tests:** `tests/eightbol-tests.lisp`
- **Documentation:** `doc/chapters/`
- **Reference:** `src/backend-cobol/` (reference implementation)
- **Audit Data:** `AST_COVERAGE_AUDIT_REPORT.md`
- **Guidelines:** `AGENTS.md`

---

**Prepared by:** OpenCode Agent  
**For:** EIGHTBOL Compiler Project  
**Status:** Ready for Implementation

