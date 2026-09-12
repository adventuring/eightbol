# 4 Frontends to 100% AST Coverage - Implementation Plan

**Goal:** Complete Forth, Pascal, SCI, and GOAL frontends to achieve 56/56 canonical AST node coverage

**Date:** September 9, 2026  
**Status:** Planning Phase  
**Effort:** ~200-300 hours (5-7 weeks, parallel work recommended)

---

## Executive Summary

### Current Coverage Status

| Frontend | Coverage | Nodes | Priority | Effort | Blockers |
|----------|----------|-------|----------|--------|----------|
| **GOAL** | 5/56 (8%) | 🔴 CRITICAL | P1 | Very High | 17 stmt nodes missing |
| **Forth** | 21/56 (37%) | ⚠️ High | P1 | High | All operators (17) missing |
| **Pascal** | 16/56 (28%) | ⚠️ High | P2 | Medium-High | 8 statements, 16 operators |
| **SCI** | 19/56 (33%) | ✓ Good | P2 | Medium | 9 nodes missing (expr nodes) |

### Work Breakdown

- **GOAL:** +51 nodes (17 statements + 7 expr + 17 operators + 3 structure)
- **Forth:** +35 nodes (25 operators/expr + 1 call-acc + 1 dialogue + 8 improvements)
- **Pascal:** +40 nodes (16 operators + 8 statements + improvements)
- **SCI:** +37 nodes (9 nodes missing: goto, dialogue, expr nodes)

**Total Estimated Work:** 163 new AST node implementations

---

## GOAL Frontend (5/56 → 56/56)

### Current Capabilities

| Category | Status | Details |
|----------|--------|---------|
| Structure | ✓ | :program, :method |
| Statements | ⚠️ Limited | :add, :exit-method, :if, :set, :subtract (5/19) |
| Expressions | ✗ | No expression nodes |
| Operators | ✓ Excellent | All 14/17 (missing :- and :asr only) |

### Missing Nodes (51)

**Statements (14):**
- :move, :invoke, :call-acc, :goto, :goback, :exit-program, :exit, :stop-run
- :perform, :log-fault, :debug-break, :copy, :string-blt, :call

**Expression Nodes (7):**
- :of, :address-of, :refmod, :subscript, :self, :null

**Operators (3):**
- :-, :asr (already has most operators!)

### Implementation Strategy

1. **Phase 1: Statements** (Est. 4-6 hours)
   - Extend `goal-form-to-statements` case statement
   - Add handlers for all 14 missing statement types
   - Map GOAL's Lisp-like syntax to canonical AST nodes
   - Example: `(move 10 hp)` → `:move :from 10 :to hp`

2. **Phase 2: Expression Nodes** (Est. 2-3 hours)
   - Extend `goal-form-to-expr` case statement
   - Add :of, :address-of, :refmod, :subscript, :self, :null
   - Example: `(of health player)` → `(:of health player)`

3. **Phase 3: Remaining Operators** (Est. 1 hour)
   - Add :- arithmetic operator
   - Add :asr (arithmetic shift right) operator

4. **Phase 4: Testing** (Est. 3-4 hours)
   - Create 40+ unit tests covering all 56 nodes
   - Run comprehensive test suite
   - Fix any regressions

5. **Phase 5: Documentation** (Est. 2-3 hours)
   - Create/update doc/chapters/goal_frontend.texi
   - Document all 56 nodes with examples
   - Explain canonicalization from GOAL syntax

**Total: 12-17 hours (1-2 days)**

### Code Changes Required

**File:** `src/frontend-goal/goal-parser.lisp`

- Modify `goal-form-to-statements` (add 14 case handlers)
- Modify `goal-form-to-expr` (add 7 case handlers + 2 operators)
- Ensure all operators use canonical keyword form (`:`, not symbols)

**Files to Create/Update:**
- `tests/eightbol-tests.lisp` - Add :goal-frontend test suite (40+ tests)
- `doc/chapters/goal_frontend.texi` - New documentation

---

## FORTH Frontend (21/56 → 56/56)

### Current Capabilities

| Category | Status | Details |
|----------|--------|---------|
| Structure | ✓ | :program, :method, :assembly-entry |
| Statements | ✓ Excellent | Most implemented (20/19) |
| Expressions | ✗ | No expression nodes |
| Operators | ✗ | Zero (0/17) - CRITICAL GAP |

### Missing Nodes (35)

**Operators (17):** ALL - :=, :≠, :<, :≤, :>, :≥, :+, :-, :×, :÷, :¬, :∧, :∨, :⊻, :⊼, :⊽, :ash

**Expression Nodes (7):**
- :of, :address-of, :refmod, :subscript, :self, :null

**Statements (1):**
- :call-acc

**Other (1):**
- Ensure :dialogue is fully supported

### Implementation Strategy

1. **Phase 1: Operators** (Est. 6-8 hours)
   - Create `forth-canonicalize-operator` function
   - Map Forth operators (=, <, >, <=, >=, +, -, etc.) to canonical keywords
   - Handle both word-based ("=", ">") and symbol-based operators
   - Example: "=" → :=, "+" → :+, etc.
   - Ensure stack-based evaluation semantics preserved

2. **Phase 2: Expression Nodes** (Est. 3-4 hours)
   - Add support for :of, :address-of, :refmod, :subscript
   - Handle Forth's stack-based access patterns
   - Map Forth syntax to canonical expression nodes

3. **Phase 3: Missing Statements** (Est. 1-2 hours)
   - Add :call-acc support
   - Complete :dialogue support if incomplete

4. **Phase 4: Testing** (Est. 4-5 hours)
   - Create 40+ comprehensive tests
   - Test each operator with stack model
   - Test expression nodes

5. **Phase 5: Documentation** (Est. 3-4 hours)
   - Create/update doc/chapters/forth_frontend.texi
   - Explain operator canonicalization
   - Show stack-based examples

**Total: 17-23 hours (2-3 days)**

### Code Changes Required

**File:** `src/frontend-forth/forth-parser.lisp`

- Add operator canonicalization function
- Extend `parse-expression` to emit canonical operators
- Add expression node support

**Files to Create/Update:**
- `tests/eightbol-tests.lisp` - Add :forth-frontend suite (40+ tests)
- `doc/chapters/forth_frontend.texi` - New documentation

---

## PASCAL Frontend (16/56 → 56/56)

### Current Capabilities

| Category | Status | Details |
|----------|--------|---------|
| Structure | ✓ | :program, :method |
| Statements | ⚠️ Partial | 16/19 implemented |
| Expressions | ⚠️ Partial | Missing some nodes |
| Operators | ✗ Limited | Only := in specific contexts |

### Missing Nodes (40)

**Statements (3):**
- :perform (FOR loops exist but not mapped), :copy, :string-blt, :input, :call-acc, :goback, :dialogue

**Operators (16):** - ALL except :=
- Comparisons: :≠, :<, :≤, :>, :≥
- Arithmetic: :+, :-, :×, :÷
- Bitwise: :¬, :∧, :∨, :⊻, :⊼, :⊽
- Shift: :ash

**Expression Nodes:** Missing some

**Other:** 2-3 improvements

### Implementation Strategy

1. **Phase 1: Operators** (Est. 5-7 hours)
   - Add operator canonicalization to parser
   - Map Pascal operators to canonical keywords
   - Example: "+" → :+, "and" → :∧, etc.

2. **Phase 2: Statements** (Est. 3-4 hours)
   - Map FOR loops to :perform nodes
   - Add :copy, :string-blt, :input, :call-acc
   - Add :goback and :dialogue

3. **Phase 3: Expressions** (Est. 2-3 hours)
   - Ensure all expression nodes present
   - Add missing qualifiers if needed

4. **Phase 4: Testing** (Est. 4-5 hours)
   - Create 35+ tests
   - Coverage for all operators and statements

5. **Phase 5: Documentation** (Est. 2-3 hours)
   - Update doc/chapters/pascal_frontend.texi
   - Document all supported nodes

**Total: 16-22 hours (2-3 days)**

### Code Changes Required

**File:** `src/frontend-pascal/pascal-parser.lisp`

- Add operator canonicalization
- Extend statement handling
- Add missing operators and statement types

**Files to Create/Update:**
- `tests/eightbol-tests.lisp` - Add :pascal-frontend suite
- `doc/chapters/pascal_frontend.texi` - Update documentation

---

## SCI Frontend (19/56 → 56/56)

### Current Capabilities

| Category | Status | Details |
|----------|--------|---------|
| Structure | ✓ | :program, :method |
| Statements | ✓ Excellent | 19/19 (nearly complete) |
| Expressions | ⚠️ Partial | Missing some nodes |
| Operators | ✓ Excellent | 13/17 implemented |

### Missing Nodes (37)

**Statements (2):**
- :goto, :dialogue (verify current implementation)

**Expression Nodes (7):**
- :of, :address-of, :refmod, :subscript, :self, :null (verify which are missing)

**Operators (3):**
- :-, :asr (missing from existing coverage)

**Other:** Refinements

### Implementation Strategy

1. **Phase 1: Expression Nodes** (Est. 3-4 hours)
   - Add/verify :of, :address-of, :refmod, :subscript, :self, :null
   - SCI already has strong base for these

2. **Phase 2: Statements** (Est. 1-2 hours)
   - Add :goto if missing
   - Verify/enhance :dialogue

3. **Phase 3: Operators** (Est. 1 hour)
   - Add :- if missing
   - Add :asr if missing

4. **Phase 4: Testing** (Est. 3-4 hours)
   - Create 35+ tests

5. **Phase 5: Documentation** (Est. 2-3 hours)
   - Create/update sci_frontend.texi

**Total: 10-14 hours (1-2 days)**

### Code Changes Required

**File:** `src/frontend-sci/sci-parser.lisp`

- Verify and extend expression node support
- Add/verify statement support
- Ensure operators are canonical

**Files to Create/Update:**
- `tests/eightbol-tests.lisp` - Add :sci-frontend suite
- `doc/chapters/sci_frontend.texi` - Create/update documentation

---

## Implementation Timeline

### Recommended Execution Order

1. **Week 1: GOAL (P1 CRITICAL)**
   - Highest ROI: 5→56 nodes (most dramatic improvement)
   - Smallest scope: ~17 hours
   - Unblocks other priorities

2. **Week 1-2: Forth (P1 HIGH)**
   - Operators are blocking feature
   - ~20 hours, can run parallel with GOAL after GOAL starts

3. **Week 2-3: Pascal & SCI (P2)**
   - Can be parallelized
   - Pascal: 20 hours, SCI: 12 hours

4. **Week 3: Integration & Testing**
   - Run all tests
   - Verify no regressions
   - Final documentation polish

### Parallel Execution (Recommended)

```
Week 1:
  [GOAL Frontend -------|
  [Forth Frontend starts after GOAL foundation ----------|
  [Pascal Frontend starts mid-week  ----------|
  
Week 2:
  Continue Forth/Pascal in parallel
  [SCI Frontend ---|
  
Week 3:
  Integration, testing, documentation finalization
```

---

## Testing Strategy

### Test Coverage Goals

| Frontend | Current Tests | Target Tests | New Tests |
|----------|---------------|--------------|-----------|
| GOAL | 0 | 45 | 45 |
| Forth | 0 | 40 | 40 |
| Pascal | 0 | 40 | 40 |
| SCI | 0 | 40 | 40 |
| **TOTAL** | **0** | **165** | **165** |

### Test Categories (per frontend)

1. **Structure Tests (3):**
   - :program creation
   - :method creation
   - :assembly-entry

2. **Statement Tests (20):**
   - One test per statement type
   - Validate correct AST node structure
   - Test with various argument patterns

3. **Expression Tests (10):**
   - One test per expression node type
   - Verify proper nesting
   - Test edge cases

4. **Operator Tests (12):**
   - Comparison operators
   - Arithmetic operators
   - Bitwise operators
   - Shift operators

5. **Integration Tests (3):**
   - Full program parsing
   - Complex nested structures
   - Regression tests

### Test Implementation

**File:** `tests/eightbol-tests.lisp`

```lisp
(fiveam:def-suite :goal-frontend
  :description "GOAL language frontend AST tests")

(fiveam:def-suite :forth-frontend
  :description "Forth language frontend AST tests")

(fiveam:def-suite :pascal-frontend
  :description "Pascal language frontend AST tests")

(fiveam:def-suite :sci-frontend
  :description "SCI language frontend AST tests")
```

### Run Tests

```lisp
(fiveam:run! :goal-frontend)
(fiveam:run! :forth-frontend)
(fiveam:run! :pascal-frontend)
(fiveam:run! :sci-frontend)
```

---

## Verification Checklist

- [ ] All 56 canonical AST nodes supported in each frontend
- [ ] All operators emitted as canonical keywords (`:`, not symbols)
- [ ] No non-canonical operator forms in generated AST
- [ ] All operators pass roundtrip through backends
- [ ] 165+ unit tests created and passing
- [ ] Documentation complete for all 4 frontends
- [ ] No regressions in existing tests
- [ ] ASDF loads without warnings
- [ ] All backends successfully consume generated AST

---

## Documentation Requirements

### File Structure

```
doc/chapters/
├── goal_frontend.texi      (NEW, ~400-600 lines)
├── forth_frontend.texi     (UPDATE, ~400-600 lines)
├── pascal_frontend.texi    (UPDATE, ~400-600 lines)
└── sci_frontend.texi       (UPDATE, ~400-600 lines)
```

### Documentation Content (per frontend)

1. **Overview** (50-100 lines)
   - Brief description
   - Language background
   - EIGHTBOL compilation approach

2. **AST Node Coverage** (100-150 lines)
   - Table of all 56 nodes
   - Status (supported/not applicable/planned)
   - Rationale for gaps (if any)

3. **Statement Examples** (100-150 lines)
   - One example per statement type
   - Show source syntax + generated AST

4. **Operator Examples** (50-100 lines)
   - Operator canonicalization rules
   - Examples of each operator type

5. **Expression Examples** (50-100 lines)
   - Expression node examples
   - Nesting examples

6. **Limitations & Notes** (30-50 lines)
   - Language-specific limitations
   - Known issues
   - Future improvements

7. **Test Examples** (30-50 lines)
   - How to run tests
   - Example test output

---

## Risk Assessment

### High Risk Items

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|-----------|
| Parenthesis balancing errors | Medium | High | Use paredit, validate before commit |
| Operator canonicalization incomplete | Low | High | Comprehensive operator matrix |
| Regressions in existing frontends | Low | High | Full regression test before commit |
| Difficult to map language constructs | Medium | Medium | Reference existing similar frontends |

### Medium Risk Items

- Complex expression nesting
- Operator precedence issues
- Backend incompatibilities

### Low Risk Items

- Documentation
- Test creation
- Simple node additions

---

## Dependencies & Prerequisites

- [ ] Understand canonical AST node structure
- [ ] Review existing COBOL frontend (reference implementation)
- [ ] Understand backend expectations
- [ ] Have SBCL/ASDF set up locally
- [ ] Able to run test suite

---

## Success Criteria

✅ **Primary Goal:** 56/56 canonical AST nodes in each of 4 frontends

✅ **Secondary Goals:**
1. All operators in canonical keyword form
2. 165+ passing unit tests
3. No regressions in existing code
4. Complete documentation for all 4 frontends
5. Full ASDF build without warnings/errors

✅ **Final Deliverables:**
- Updated source files (4 frontends)
- Comprehensive test suites
- Complete documentation
- Implementation verification report

---

## Estimated Schedule

- **Start Date:** September 9, 2026
- **Target Completion:** September 30, 2026 (3 weeks)
- **Critical Path:** GOAL → Forth → (Pascal + SCI parallel)
- **Buffer:** 1 week for integration and debugging

---

## Next Steps

1. **Immediate:** 
   - Review this plan with team
   - Prioritize execution order
   - Assign roles if team-based

2. **Setup:**
   - Create feature branch
   - Set up test framework
   - Document current state baseline

3. **Execution:**
   - Follow implementation strategy per frontend
   - Run tests after each file change
   - Commit frequently

4. **Validation:**
   - Run full test suite
   - Check for regressions
   - Verify all 56 nodes per frontend

5. **Documentation:**
   - Write frontend chapters
   - Create examples
   - Update main README if needed

6. **Submission:**
   - Create PR with all changes
   - Request code review
   - Merge after approval

---

## References

- `src/ast.lisp` - Canonical AST node definitions
- `src/backend-cobol/cobol-backend.lisp` - Reference implementation
- `tests/eightbol-tests.lisp` - Existing test structure
- `AST_COVERAGE_AUDIT_REPORT.md` - Detailed audit findings
- `AGENTS.md` - Project guidelines and best practices

