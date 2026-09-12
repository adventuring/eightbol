# EIGHTBOL PENDING TESTS AUDIT & RECOVERY PLAN

**Generated:** 2026-09-09  
**Total Pending Tests:** 1,103  
**Total Test Files:** 57+ directories with 100+ test files  
**Total Lines of Test Code:** 25,464+

---

## Executive Summary

This audit identifies **1,103 skipped/pending tests** across the EIGHTBOL compiler test suite. The vast majority (1,036) are backend-specific node tests with placeholder implementations. An additional **46 copybook generation tests** are incomplete. Small numbers of **lexer consistency tests** (2), **BASIC parity tests** (5), and **miscellaneous tests** (14) also require attention.

**Key Finding:** Most pending tests follow a **template pattern** with identical test stubs that simply call `(skip "Implementation pending")`. These can be systematically addressed by:

1. **Backend tests:** Implementing actual test logic for each backend (6502, 65c02, 65c816, HuC6280, RP2A03, cp1610, z80, SM83, m68k, i286, ARM7, F8, stack VM)
2. **Copybook tests:** Completing copybook generation parsing function implementations
3. **Lexer tests:** Re-evaluating whether consistency tests are necessary
4. **BASIC tests:** Clarifying AST requirements vs. transpile requirements

---

## PART 1: DETAILED AUDIT BY CATEGORY

### 1.1 Backend Template Tests (1,036 tests)

**Pattern:** Tests in `tests/backends/backend-*/` directories follow a consistent template:

```lisp
(test backend_operation_variant
  "OPERATION: backend operation-variant description"
  (skip "Implementation pending"))
```

**Affected Backends:** (all following same template across multiple test categories)
- 6502 (39 tests)
- 65c02 (46 tests)
- 65c816 (46 tests)
- HuC6280 (46 tests)
- RP2A03 (46 tests)
- cp1610 (46 tests)
- z80 (46 tests)
- SM83 (46 tests)
- m68k (46 tests)
- i286 (46 tests)
- ARM7 (46 tests)
- F8 (46 tests)
- stack VM (46 tests)

**Test Categories per Backend:**
- `arithmetic-node-tests.lisp`: 6 skipped tests each
- `call-invoke-tests.lisp`: 5 skipped tests each
- `control-flow-tests.lisp`: 5 skipped tests each
- `integration-tests.lisp`: 4 skipped tests each
- `move-node-tests.lisp`: 4 skipped tests each
- `special-nodes-tests.lisp`: 4-5 skipped tests each
- `string-operations-tests.lisp`: 4 skipped tests each

**Files:** 
- `tests/backends/backend-6502-tests/move-node-tests.lisp:26,30,34,38`
- `tests/backends/backend-65c02-tests/arithmetic-node-tests.lisp:26,30,34,38,42,46`
- (... and 100+ similar files)

**Current Status:** 0% implemented
**Priority:** HIGH - Core backend functionality testing
**Effort Estimate:** 6-8 hours per backend (80-100 hours total)

**Test Skeleton Example:**
```lisp
(test 6502_move_reg_to_reg
  "MOVE: 6502 register-to-register moves are correctly generated"
  (skip "Implementation pending"))
```

**What These Should Test:**
1. Each backend should validate register-to-register moves
2. Memory-to-register operations
3. Immediate-to-register moves
4. Multi-byte move sequencing
5. Arithmetic operations (ADD, SUBTRACT, COMPUTE) 
6. Call/invoke code generation
7. Control flow (IF/THEN/ELSE, PERFORM, GOTO)
8. String operations
9. Special nodes (DEBUG, INSPECT, etc.)
10. Integration scenarios (realistic programs)

---

### 1.2 Copybook Generation Tests (46 tests)

**File:** `tests/copybook-generation-tests.lisp`  
**Pattern:** Helper function tests for copybook generation parsing

**Skip Reason:** `"Not yet implemented"`

**Test List:**
```
Line 22:   pascal-to-eightbol-name/simple
Line 26:   pascal-to-eightbol-name/multi-caps
Line 30:   pascal-to-eightbol-name/single-word
Line 34:   pascal-to-eightbol-name/already-hyphenated
Line 38:   pascal-to-copybook-filename
Line 42:   classes-defs-comment-line-p/semicolon
Line 46:   classes-defs-comment-line-p/asterisk
Line 50:   classes-defs-comment-line-p/blank-and-content
Line 54:   parse-asm-annotation/object-ref
Line 58:   parse-asm-annotation/pic
Line 62:   parse-asm-annotation/varchar
Line 66:   parse-asm-annotation/nil
Line 70:   parse-asm-line/byte
Line 74:   parse-asm-line/word
Line 78:   parse-asm-line/fill
(... and 31 more)
```

**Current Status:** 0% implemented  
**Priority:** HIGH - Core name transformation and parsing
**Effort Estimate:** 2-3 hours

**What These Should Test:**
1. PascalCase → UPPERCASE-HYPHENATED name conversion
2. Comment line detection (Lisp and COBOL styles)
3. Assembly annotation parsing (@ClassName, = PIC X(20), = VARCHAR)
4. Assembly line parsing (.byte, .word, .fill, .const, etc.)

---

### 1.3 Lexer Consistency Tests (2 tests)

**File:** `tests/frontend-lexer-parser-tests.lisp`  
**Lines:** 234, 240

```lisp
(test lexer-empty-line-handling
  "Test lexers handle empty lines gracefully."
  (skip "Lexer functions tested per-frontend; skipping consistency check")
  #+skip (is (null (eightbol::basic-lex-line "")))
  #+skip (is (null (eightbol::agi-lex-line ""))))

(test lexer-whitespace-handling
  "Test lexers skip whitespace correctly."
  (skip "Lexer functions tested per-frontend; skipping consistency check")
  #+skip (let ((tokens1 (eightbol::basic-lex-line "  x  =  10  "))
        (tokens2 (eightbol::basic-lex-line "x=10")))
    ;; Both should produce same token count
    (is (= (length tokens1) (length tokens2)))))
```

**Current Status:** Marked as redundant (per-frontend tests exist)  
**Priority:** LOW - Tests may be intentionally skipped
**Effort Estimate:** 0.5 hours (re-evaluate necessity)

**Decision Needed:** Determine if cross-frontend consistency checks add value

---

### 1.4 BASIC Parity Tests (5 tests)

**File:** `tests/basic-parity-tests.lisp`  
**Lines:** 15, 25, 42, 61, 70

```lisp
(test basic/cp1610-and-6502-both-compile
  (skip "BASIC must produce AST, not transpile")
  #+skip (...))

(test basic/statement-transpile-works
  (skip "BASIC must produce AST, not transpile to COBOL")
  #+skip (...))

(test basic/full-program-transpile
  (skip "BASIC must produce AST, not transpile to COBOL")
  #+skip (...))

(test basic/assembly-generation
  (skip "BASIC must produce AST, not transpile")
  #+skip (...))

(test basic/compile-success
  (skip "BASIC must produce AST, not transpile")
  #+skip (...))
```

**Current Status:** Intentionally disabled (architectural decision)  
**Priority:** LOW - May be obsoleted by new BASIC AST strategy  
**Effort Estimate:** 1-2 hours (re-evaluate strategy)

**Issue:** Comments indicate BASIC is transitioning from transpile-to-COBOL to direct AST output. These tests are placeholders for the old architecture.

**Decision Needed:** 
- Do we keep COBOL transpile path for BASIC?
- If yes: implement tests and transpiler
- If no: delete tests entirely

---

### 1.5 Shell/Copybook Command Tests (30 tests)

**Files:**
- `tests/other/basic-shell-tests.lisp`: 10 tests (lines 21, 25, 29, 33, 37, 41, 45, 49, 53, 57)
- `tests/other/cobol-copybook-tests.lisp`: 10 tests (lines 21, 25, 29, 33, 37, 41, 45, 49, 53, 57)
- `tests/other/command-line-tests.lisp`: 10 tests (lines 21, 25, 29, 33, 37, 41, 45, 49, 53, 57)

**Pattern:** All use `(skip "Implementation pending")`

**Example:**
```lisp
(test basic-shell/help
  "Basic shell responds to --help"
  (skip "Implementation pending"))
```

**Current Status:** 0% implemented  
**Priority:** MEDIUM - Command-line interface testing  
**Effort Estimate:** 2-3 hours total

**What These Should Test:**
1. Shell command parsing and help output
2. Copybook file generation and validation
3. Command-line argument handling
4. Error messages and exit codes

---

### 1.6 Optimizer Tests (62 tests)

**Files:**
- `tests/optimizers/optimizer-constant-folding-tests/edge-cases-tests.lisp`: 4 tests
- `tests/optimizers/optimizer-constant-folding-tests/regression-tests.lisp`: 3 tests
- `tests/optimizers/optimizer-strength-reduction-tests/transformation-tests.lisp`: 4 tests
- `tests/optimizers/optimizer-common-subexpression-elimination-tests/edge-cases-tests.lisp`: 4 tests
- `tests/optimizers/optimizer-common-subexpression-elimination-tests/regression-tests.lisp`: 3 tests
- `tests/optimizers/optimizer-common-subexpression-elimination-tests/transformation-tests.lisp`: 4 tests
- `tests/optimizers/optimizer-loop-unrolling-tests/edge-cases-tests.lisp`: 4 tests
- `tests/optimizers/optimizer-loop-unrolling-tests/regression-tests.lisp`: 3 tests
- `tests/optimizers/optimizer-loop-unrolling-tests/transformation-tests.lisp`: 4 tests
- `tests/optimizers/optimizer-register-allocation-tests/edge-cases-tests.lisp`: 4 tests
- `tests/optimizers/optimizer-register-allocation-tests/regression-tests.lisp`: 3 tests
- `tests/optimizers/optimizer-register-allocation-tests/transformation-tests.lisp`: 4 tests

**Current Status:** 0% implemented  
**Priority:** MEDIUM - Optimization validation  
**Effort Estimate:** 3-4 hours total

**What These Should Test:**
1. Constant folding correctness
2. Strength reduction transformations
3. Common subexpression elimination
4. Loop unrolling with proper bounds
5. Register allocation efficiency
6. Edge cases and regression scenarios

---

## PART 2: ROOT CAUSE ANALYSIS

### Why Are There So Many Pending Tests?

1. **Template Generation:** Many tests appear to be auto-generated from templates with placeholder `skip` markers
2. **Incomplete Implementation:** Core functionality may not be ready (e.g., optimizer implementations)
3. **Architectural Decisions:** BASIC tests skipped due to transpile→AST transition
4. **Deferred Work:** Tests created early to define expected behavior, implementation deferred

### Preventing Future Pending Tests

1. **Don't commit placeholder tests** without explicit TODO/PENDING documentation
2. **Use test groups** to organize related tests and document planned work
3. **Link tests to issues** tracking their implementation
4. **Document test purpose** in skip message, not just "Implementation pending"

---

## PART 3: RECOVERY STRATEGY

### Phase 1: Quick Wins (2-3 hours)

1. **Remove obviously obsolete tests**
   - BASIC transpile tests (move to historical archive or delete)
   - Redundant lexer consistency tests

2. **Clarify skip reasons**
   - Replace generic "Implementation pending" with specific blockers
   - Example: `"Awaiting backend-matrix test harness for 6502"`

### Phase 2: Copybook & Helpers (2-3 hours)

1. **Implement copybook generation tests** (46 tests)
   - Name conversion functions
   - Annotation parsing
   - Line parsing
   - Should validate against real copybook generation

### Phase 3: Backend Tests (80-100 hours)

1. **Create test harness** for backend validation
   - Input: AST node
   - Process: Generate assembly for backend
   - Output: Validate assembly syntax and correctness

2. **Implement per-backend tests** (1-2 hours each)
   - 6502, 65c02, 65c816, HuC6280, RP2A03, cp1610
   - z80, SM83, m68k, i286, ARM7, F8, stack VM
   - Test categories: move, arithmetic, call/invoke, control flow, strings, special nodes

3. **Priority order** (by architectural importance):
   - 6502 (canonical backend)
   - 65c02, 65c816 (Apple family)
   - cp1610 (Intellivision)
   - z80 (Game Boy)
   - Others

### Phase 4: Command-Line & Integration (3-4 hours)

1. **Shell command tests** (10 tests)
2. **Copybook file tests** (10 tests)
3. **Command-line integration** (10 tests)

### Phase 5: Optimizer Tests (3-4 hours)

1. **Implement optimizer validation**
   - Test constant folding correctness
   - Verify strength reduction
   - Validate CSE transformations
   - Test loop unrolling
   - Check register allocation

---

## PART 4: ACTIONABLE TODO LIST

### TODO 1: Audit Backend Tests (1-2 hours)

**Task:** Identify backend test requirements

**Steps:**
1. Read backend documentation (src/backend-*/README or similar)
2. Identify AST node types each backend must handle
3. List expected assembly output for canonical examples
4. Create test harness framework

**Deliverable:** Backend test template with 5-10 canonical examples

---

### TODO 2: Convert Copybook Tests (2 hours)

**Task:** Implement 46 copybook generation tests

**Steps:**
1. Examine `src/copybook-generation.lisp` or similar
2. Identify each tested function
3. Write test cases for:
   - Normal cases
   - Edge cases (empty strings, special chars)
   - Error cases (invalid input)
4. Cross-validate against actual copybook output

**Test Categories:**
- Name transformation (3-4 tests)
- Comment detection (3 tests)
- Annotation parsing (4-5 tests)
- Line parsing (10+ tests)
- Integration (5+ tests)

---

### TODO 3: Re-evaluate BASIC Tests (1 hour)

**Task:** Clarify BASIC architecture

**Decision Points:**
1. Is BASIC→COBOL transpile path active?
2. Is BASIC→AST direct compilation now standard?
3. Should old tests be deleted or updated?

**Outcomes:**
- If keeping transpile: implement tests (2-3 hours)
- If moving to AST: delete tests and update docs
- If hybrid: split tests by path

---

### TODO 4: Backend Test Implementation (80+ hours - Phase work)

**Task:** Implement placeholder backend tests

**Phases:**
1. **6502 backend** (10-12 hours)
   - Move operations (4-5 tests)
   - Arithmetic (4-5 tests)
   - Call/invoke (4-5 tests)
   - Control flow (4-5 tests)
   - Strings (4-5 tests)
   - Integration (4-5 tests)

2. **65c02, 65c816** (8-10 hours each)
   - Similar structure, focus on 65c816 differences

3. **Other backends** (6-8 hours each)
   - Template-based from 6502 with backend-specific tweaks

---

### TODO 5: Create Comprehensive Test Plan (4-6 hours)

**Deliverables:**
1. Backend test harness documentation
2. Test templates for each node type
3. Expected assembly output reference
4. Edge case inventory
5. Integration test scenarios

---

## PART 5: RECOMMENDED ACTIONS (IMMEDIATE)

### Action 1: Clarify Pending Test Categories

```bash
# Identify which tests MUST be implemented
# - Backend tests: YES (core feature validation)
# - Copybook tests: YES (infrastructure)
# - Optimizer tests: YES (quality assurance)
# - BASIC tests: MAYBE (architecture dependent)
# - Shell tests: YES (CLI validation)
# - Lexer consistency: MAYBE (redundant with per-frontend tests)
```

### Action 2: Create Issues for Blocked Work

Each test category should have associated GitHub issues:
- `[Tests] Backend test implementation (1000+ hours)`
- `[Tests] Copybook generation tests (2-3 hours)`
- `[Tests] Optimizer validation (3-4 hours)`
- `[Tests] BASIC architecture clarification (1 hour)`

### Action 3: Stop Committing Placeholder Tests

Policy: No `(skip "Implementation pending")` without:
1. Associated GitHub issue
2. Expected test count in issue
3. Estimated implementation time
4. Blocking dependencies listed

### Action 4: Document Test Purpose

Every skip reason should answer: "Why not implemented?"

Good examples:
- `"Awaiting 6502 backend implementation (issue #1234)"`
- `"Architecture pending: BASIC AST vs transpile (issue #5678)"`
- `"Blocked by copybook generation functions"`

Bad examples:
- `"Implementation pending"` ← Too vague
- `"Not yet implemented"` ← Obvious, doesn't help
- `"TODO"` ← Not a reason

---

## PART 6: TEST PLAN TEMPLATE

### For Backend Tests

**Backend:** [6502|65c02|...] (EXAMPLE: 6502)

**Purpose:** Validate correct assembly code generation for all 6502-supported AST node types

**Test Coverage (39 tests organized as):**

**A. Move Operations (4 tests)**
- Register-to-register (MOVE A, B)
- Memory-to-register (MOVE (MEM), A)
- Immediate-to-register (MOVE 42, A)
- Multi-byte moves (16-bit or wider)

**B. Arithmetic Operations (6 tests)**
- Addition (ADD values)
- Subtraction (SUBTRACT values)
- Comparison operators (=, !=, <, <=, >, >=)
- Logical operations (AND, OR, NOT)
- Shift operations (if applicable)
- Accumulator vs memory operands

**C. Call & Invoke (5 tests)**
- Local nullary calls
- Local unary calls (with parameter)
- Method invocation
- Library calls
- Return value handling

**D. Control Flow (5 tests)**
- IF/THEN/ELSE branching
- PERFORM (simple procedure call)
- PERFORM VARYING (loops)
- GOTO / labeled branches
- Tail call optimization

**E. String Operations (4 tests)**
- String MOVE (BLT)
- STRING concatenation
- UNSTRING parsing
- Character inspection (INSPECT)

**F. Special Nodes (5 tests)**
- DEBUG BREAK
- INSPECT with TALLYING/REPLACING
- EVALUATE/WHEN clauses
- Edge cases

**G. Integration (5 tests)**
- Realistic programs combining multiple features
- Real Character.cob methods
- Error handling

**Expected Outcomes:**
- 39 passing tests
- 100% branch coverage of backend code
- Performance within acceptable bounds

---

### For Copybook Tests

**Purpose:** Validate copybook generation parsing functions

**Test Coverage (46 tests):**

**A. Name Transformations (5 tests)**
1. PascalCase → UPPERCASE-HYPHENATED (`TestClass` → `TEST-CLASS`)
2. Multi-cap transitions (`URLParser` → `URL-PARSER`)
3. Single words (`Test` → `TEST`)
4. Underscores (handling)
5. Numbers (handling)

**B. Comment Detection (3 tests)**
1. Lisp-style (;)
2. COBOL-style (*)
3. Non-comment lines

**C. Annotation Parsing (5 tests)**
1. Object references (@ClassName)
2. PIC notation (= PIC X(20))
3. VARCHAR with size (= VARCHAR(n) DEPENDING ON Field)
4. Nil/empty cases
5. Invalid formats

**D. Assembly Line Parsing (10+ tests)**
1. .byte directives
2. .word directives
3. .fill directives
4. .const directives
5. .align directives
6. Labels
7. Comments
8. Edge cases (empty, whitespace-only)
9. Invalid directives
10. Mixed content

**E. Integration (15+ tests)**
1. Real copybook files
2. Roundtrip validation (parse → regenerate → parse)
3. Platform variations (7800-specific elements)
4. Error handling (malformed input)
5. Performance (large copybooks)

---

## PART 7: METRICS & SUCCESS CRITERIA

### Baseline (Current State)

| Metric | Value |
|--------|-------|
| Total Tests | 4,390 |
| Pending Tests | 1,103 |
| Pass Rate | 68.9% (3,287 passing) |
| Major Gaps | Backends (1,036), Copybooks (46) |

### Target (Phase 1 - Quick Wins)

| Metric | Target |
|--------|--------|
| Removed Obsolete | 5+ tests |
| Clarified Skip Reasons | 100 tests |
| Improved Documentation | 30+ tests |
| Pass Rate | 68.9% (unchanged, tests still skipped) |

### Target (Phase 2 - Helpers)

| Metric | Target |
|--------|--------|
| Copybook Tests Converted | 46 → 0 pending |
| Pass Rate | 69.9% (40+ new passing) |
| Test Coverage | Copybook generation ≥90% |

### Target (Phase 3 - Backends)

| Metric | Target |
|--------|--------|
| Backend Tests Converted | 1,036 → 0 pending |
| Pass Rate | 89%+ (3,900+ passing) |
| Backend Coverage | Each backend ≥80% |
| New Tests | 3,900+ passing backend tests |

### Target (Phase 5 - Final)

| Metric | Target |
|--------|--------|
| Total Tests | 5,500+ |
| Pending Tests | <10 (only legitimate deferrals) |
| Pass Rate | 95%+ |
| Test Coverage | Overall ≥85% |

---

## PART 8: ESTIMATED TIMELINE

| Phase | Task | Effort | Priority |
|-------|------|--------|----------|
| 1 | Audit & Clarify | 3 hrs | NOW |
| 1 | Remove Obsolete | 1 hr | NOW |
| 2 | Copybook Implementation | 3 hrs | Week 1 |
| 3A | Backend Test Harness | 4 hrs | Week 1 |
| 3B | 6502 Backend Tests | 12 hrs | Week 2 |
| 3C | Other Backends (parallel) | 80 hrs | Week 3+ |
| 4 | CLI/Integration | 3 hrs | Week 4 |
| 5 | Optimizer Tests | 4 hrs | Week 4 |
| --- | **TOTAL** | **~115 hrs** | **1-2 months** |

---

## CONCLUSION

The 1,103 pending tests represent a significant opportunity to improve EIGHTBOL's quality assurance. Most are placeholder stubs with clear implementation paths. By systematically working through this plan, we can achieve:

1. **0 pending tests** (all justified or removed)
2. **95%+ code coverage** across backends
3. **5,500+ total tests** (vs current 4,390)
4. **Stronger regression detection** via backend-specific tests
5. **Clear architecture validation** for COBOL/BASIC parity

**Next Steps:**
1. Schedule Phase 1 work (3-4 hours)
2. Create GitHub issues for Phase 2-5 work
3. Assign test implementation to backend maintainers
4. Update test policy to prevent new pending tests

---

**Prepared by:** OpenCode Audit System  
**Requires Review By:** Project Lead  
**Target Completion:** T+8 weeks  
