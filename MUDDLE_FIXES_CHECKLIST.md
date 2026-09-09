# Muddle Language Integration Fixes - Checklist

## Overview
This checklist tracks the 4 priority levels of fixes identified in the Muddle language audit. See `MUDDLE_RESEARCH.md` for full details.

**Total Effort:** ~2 hours (127 minutes)

---

## Priority 1: CRITICAL BUGS (15 minutes) ⚠️ DO FIRST

These bugs cause **silent method return failure** across ALL backends.

### [ ] P1.1: Fix Muddle Parser Return Statement
- **File:** `src/frontend-muddle/muddle-parser.lisp:73`
- **Change:** Line 73 from `(list :go-back)` to `(list :goback)`
- **Impact:** Fixes Muddle `.RETURN` statement
- **Affected backends:** All 13
- **Effort:** 1 min
- **Test:** Run `(asdf:test-system :eightbol)` after fix

### [ ] P1.2: Fix Lingo Parser Return Statement
- **File:** `src/frontend-lingo/lingo-parser.lisp:187`
- **Change:** Line 187 from `(lambda () (list :go-back))` to `(lambda () (list :goback))`
- **Impact:** Fixes Lingo `RETURN` statement
- **Affected backends:** All 13
- **Effort:** 1 min
- **Test:** Run `(asdf:test-system :eightbol)` after fix

### [ ] P1.3: Fix SCI Parser Return Statement (Part A)
- **File:** `src/frontend-sci/sci-parser.lisp:93`
- **Change:** Line 93 from `(list :go-back)` to `(list :goback)`
- **Impact:** Fixes SCI `return` statement
- **Affected backends:** All 13
- **Effort:** 1 min

### [ ] P1.4: Fix SCI Parser Return Statement (Part B)
- **File:** `src/frontend-sci/sci-parser.lisp:151-153`
- **Change:** Remove the `sci-make-goback-node` function or fix it to emit `:goback`
- **Alternative:** Keep function but call canonical `make-goback-node` from `grammar-build.lisp`
- **Impact:** Eliminates duplicate, incorrect implementation
- **Effort:** 3 min

**Verification:**
```lisp
(defun test-all-backends-with-return ()
  "Verify return statement works in all backends"
  (loop for backend in '(:6502 :65c02 :arm7 :z80 :m68k) do
    (let ((asm (compile-to-backend '(:method :method-id "Test" :statements ((:goback))) backend)))
      (assert (search "rts" asm) nil "Backend ~A missing return instruction" backend))))
```

---

## Priority 2: DOCUMENTATION (2 minutes) 📝

Documentation must be kept in sync with code.

### [ ] P2.1: Fix Muddle Frontend Documentation
- **File:** `doc/chapters/muddle_frontend.texi:413`
- **Current:** `Maps to Eightbol's @code{:go-back} node.`
- **Change to:** `Maps to Eightbol's @code{:goback} node.`
- **Context:** This is in the `.RETURN` keyword section
- **Effort:** 2 min

---

## Priority 3: INCOMPLETE FEATURES (50 minutes) 🔧

These are enhancements and consistency fixes.

### [ ] P3.1: Improve Muddle Call Argument Handling
- **File:** `src/frontend-muddle/muddle-parser.lisp:67-69`
- **Current Issue:** Arguments are silently ignored
- **Options:**
  1. **Implement support:** Map to `:call-acc :using` for single arg
  2. **Add warning:** Emit warning when args are dropped
  3. **Leave as-is:** Document that args are not yet supported
- **Recommendation:** Option 2 (warn user)
- **Effort:** 15 min (debate + implementation + testing)

### [ ] P3.2: Standardize Lingo Return Value Semantics
- **File:** `src/frontend-lingo/lingo-parser.lisp:185-190`
- **Current:** Different behavior for `return` vs `return value`
  - `return` → `(:go-back)` [WRONG]
  - `return value` → `(:move :from value :to "RESULT")`
- **Fix:** Both should emit `:goback` with optional `:returning`
- **After P1.2:** Both emit `:goback`, decide on value handling
- **Effort:** 15 min

### [ ] P3.3: Remove SCI Duplicate Goback Function
- **File:** `src/frontend-sci/sci-parser.lisp:151-153`
- **Issue:** `sci-make-goback-node` duplicates `make-goback-node` from `grammar-build.lisp`
- **Fix:** Either delete or make it call canonical version
- **Effort:** 5 min

### [ ] P3.4: Add Helper for Muddle Call Arguments (Optional)
- **File:** `src/frontend-muddle/muddle-parser.lisp`
- **Add function:** `muddle-parse-call-with-args`
- **Implementation:** Convert `:rest args` into `:call-acc :using` or `:call :args`
- **Effort:** 15 min

---

## Priority 4: TEST COVERAGE (60 minutes) ✓

Ensure regressions are caught by test suite.

### [ ] P4.1: Add Muddle Return Tests
- **File:** `tests/frontends/frontend-muddle-tests/parser-tests.lisp`
- **Add test cases:**
  1. `.RETURN` without value → `(:goback)`
  2. `.RETURN value` → `(:goback)` (value handling)
  3. Verify in method context
- **Effort:** 20 min

### [ ] P4.2: Add Lingo Return Tests
- **File:** `tests/frontends/frontend-lingo-tests/parser-tests.lisp`
- **Add test cases:**
  1. `RETURN` without value → `(:goback)`
  2. `RETURN value` → should produce `:goback` + value handling
  3. Verify in method context
- **Note:** May need to check if this test file exists
- **Effort:** 20 min

### [ ] P4.3: Add SCI Return Tests
- **File:** `tests/frontends/frontend-sci-tests/parser-tests.lisp`
- **Add test cases:**
  1. `return` → `(:goback)`
  2. `return value` → `(:goback)` + value handling
  3. Verify in method context
- **Note:** May need to check if this test file exists
- **Effort:** 20 min

---

## Regression Testing

After completing all priorities, run:

```lisp
;; Full test suite
(asdf:test-system :eightbol)

;; Specific test suites
(fiveam:run! :backend-matrix)           ;; Backend compilation
(fiveam:run! :frontend-muddle)          ;; If exists
(fiveam:run! :compile-regression)       ;; Regression suite
```

**Expected outcome:** No new test failures after fixes.

---

## Implementation Notes

### Order of Implementation
1. **Do P1 fixes first** (they're blocking)
2. **Do P2 immediately after** (quick, keeps docs accurate)
3. **Do P3 when feasible** (non-blocking enhancements)
4. **Do P4 last** (tests should follow implementation)

### Testing Strategy
- After each P1 fix, run `(asdf:test-system :eightbol)`
- After P3 fixes, run specific backend tests
- After P4, run full test suite

### Validation Checklist
- [ ] All `:go-back` references removed (except in this document)
- [ ] All return statements tested
- [ ] Backend tests pass (especially 6502, ARM7, Z80)
- [ ] No warning/error messages from parsers
- [ ] Documentation matches implementation

---

## References

- Full audit: `MUDDLE_RESEARCH.md` (this directory)
- Executive summary: `MUDDLE_RESEARCH_SUMMARY.txt` (this directory)
- AST specification: `src/ast.lisp`
- Canonical AST constructors: `src/grammar-build.lisp`
- Muddle language docs: `doc/chapters/muddle_frontend.texi`

---

## Status Tracking

**Created:** 2026-09-09
**Last Updated:** 2026-09-09
**Status:** Ready for implementation

**Completed:**
- [ ] P1.1
- [ ] P1.2
- [ ] P1.3
- [ ] P1.4
- [ ] P2.1
- [ ] P3.1-P3.4
- [ ] P4.1-P4.3

**Total Completed:** 0 / 15

