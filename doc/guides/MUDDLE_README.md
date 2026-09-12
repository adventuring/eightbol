# Muddle Language Research & Audit

## Quick Navigation

### For Immediate Action
- **START HERE:** [`MUDDLE_FIXES_CHECKLIST.md`](MUDDLE_FIXES_CHECKLIST.md) — 15-minute critical fix guide
- **SUMMARY:** [`MUDDLE_RESEARCH_SUMMARY.txt`](MUDDLE_RESEARCH_SUMMARY.txt) — Executive summary with quick reference table

### For Deep Understanding
- **FULL AUDIT:** [`MUDDLE_RESEARCH.md`](MUDDLE_RESEARCH.md) — Complete 1000+ line research report

---

## TL;DR

**CRITICAL BUG FOUND:** Three frontend parsers emit `:go-back` instead of `:goback`, breaking method returns on ALL 13 backends.

### Files to Fix (15 minutes)
1. `src/frontend-muddle/muddle-parser.lisp:73` — Change `:go-back` to `:goback`
2. `src/frontend-lingo/lingo-parser.lisp:187` — Change `:go-back` to `:goback`
3. `src/frontend-sci/sci-parser.lisp:93` — Change `:go-back` to `:goback`
4. `src/frontend-sci/sci-parser.lisp:151-153` — Remove duplicate function
5. `doc/chapters/muddle_frontend.texi:413` — Update documentation

### Verification
```lisp
(asdf:test-system :eightbol)
(fiveam:run! :backend-matrix)
```

---

## Document Overview

### 1. MUDDLE_FIXES_CHECKLIST.md (6.6 KB, 203 lines)
**Use when:** Implementing fixes
**Contains:**
- 4 priority levels of fixes
- 15 specific tasks with effort estimates
- Before/after code snippets
- Test verification commands
- Status tracking checkboxes

**Time to read:** 10 minutes
**Action items:** 15 tasks (127 minutes total effort)

---

### 2. MUDDLE_RESEARCH_SUMMARY.txt (5.3 KB, 132 lines)
**Use when:** Need quick reference
**Contains:**
- Executive summary of all findings
- AST mapping table (14 constructs)
- Critical bugs highlighted
- Priority 1-4 overview
- Numeric type support details
- Identifier normalization notes
- Backend impact analysis

**Time to read:** 5 minutes
**Audience:** Decision makers, code reviewers

---

### 3. MUDDLE_RESEARCH.md (26 KB, 979 lines)
**Use when:** Need complete technical details
**Contains:**
- Muddle language history & heritage
- Complete AST mapping with implementation status
- All 10 research questions answered
- Detailed bug analysis with code excerpts
- Return semantics comparison (BASIC, Muddle, Lingo, SCI)
- Numeric types & literal formats
- Test coverage gaps
- Implementation complexity estimates
- Full appendices with methodology

**Time to read:** 45 minutes (or reference as needed)
**Audience:** Developers, technical leads, maintainers

---

## Critical Findings at a Glance

### The Bug (High Impact)
```
Muddle, Lingo, SCI frontends emit (:go-back) 
Canonical EIGHTBOL expects (:goback)
Result: Methods compile but never return to caller
Impact: SILENT FAILURE on ALL 13 backends
```

### Affected Backends
- 6502, 65c02, 65c816, HuC6280, RP2A03, cp1610, Z80, SM83, m68k, i286, ARM7, F8, Stack, Forth

### Affected Frontends
- Muddle (`.RETURN` statement)
- Lingo (`RETURN` statement)
- SCI (`return` statement)

### Severity
- **Critical:** Breaks core language functionality
- **Silent:** No error message or warning
- **Scope:** All code using these frontends
- **Testability:** Only detectable via integration tests

---

## Quick Fix Guide

### Priority 1: CRITICAL (15 minutes)
Must be fixed immediately; breaks all method returns.

```lisp
;; Fix 1: muddle-parser.lisp:73
(list :go-back)  →  (list :goback)

;; Fix 2: lingo-parser.lisp:187
(lambda () (list :go-back))  →  (lambda () (list :goback))

;; Fix 3: sci-parser.lisp:93
(list :go-back)  →  (list :goback)

;; Fix 4: sci-parser.lisp:151-153
Remove sci-make-goback-node function (or fix it)
```

### Priority 2: DOCUMENTATION (2 minutes)
Update docs to match code.

```
doc/chapters/muddle_frontend.texi:413
FROM: Maps to Eightbol's @code{:go-back} node.
TO:   Maps to Eightbol's @code{:goback} node.
```

### Priority 3: ENHANCEMENTS (50 minutes)
Non-blocking improvements; can be scheduled later.
- Improve `.CALL` argument handling
- Standardize return semantics
- Remove duplicate code

### Priority 4: TEST COVERAGE (60 minutes)
Add regression tests; should follow implementation.
- Muddle return tests
- Lingo return tests
- SCI return tests

---

## Research Methodology

### Coverage
- ✓ Codebase exploration (158 grep matches reviewed)
- ✓ Canonical AST analysis
- ✓ Frontend parser audit (984 lines of code reviewed)
- ✓ Documentation review (1005 line texi file)
- ✓ Test suite analysis
- ✓ Historical research (Wikipedia, project documentation)

### Findings Quality
- Evidence-based (code citations provided)
- Reproducible (specific line numbers documented)
- Comprehensive (all 10 research questions answered)
- Prioritized (critical vs. optional clearly marked)

---

## Key Muddle Language Facts

### Origins
- **Created:** 1971 at MIT Project MAC
- **Designers:** Gerald Sussman, Carl Hewitt, Chris Reeve, Bruce Daniels
- **Final version:** 105 (1980)
- **Original name:** "Muddle" (later sanitized to "MDL")

### Heritage
- Based on Lisp
- Influenced: Scheme, Common Lisp, Prolog, Smalltalk, actor model
- Famous use: Zork interactive fiction (spawned Infocom's ZIL)

### Technical Features
- S-expression syntax (prefix notation)
- Multiple data types: lists, strings, arrays, user-defined types
- Lexical scoping (NOT dynamic)
- Multithreading and coroutines
- Advanced debugging and introspection

---

## AST Mapping Summary

| Muddle | EIGHTBOL AST | Status |
|--------|--------------|--------|
| `.SET` | `:move` | ✓ Correct |
| `.IF/THEN/ELSE` | `:if` | ✓ Correct |
| `.WHILE` | `:perform :until` | ✓ Correct |
| `.FOR` | `:perform :varying` | ✓ Correct |
| `.DEFINE` | `:method` | ✓ Correct |
| `.RETURN` | `:goback` | ❌ **BROKEN** |
| `.GO` | `:goto` | ✓ Correct |
| `.CALL` | `:call` | ⚠️ Args ignored |
| `.PRINT` | `:print` | ✓ Correct |
| `.INPUT` | `:input` | ✓ Correct |
| `.THROW` | `:log-fault` | ✓ Correct |
| `.EXIT` | `:stop-run` | ✓ Correct |

---

## Test Coverage Status

### Currently Covered
- BASIC frontend (reference implementation) ✓
- All 13 backend code generators ✓
- Muddle lexer ✓

### Missing Coverage
- Muddle return statement ❌
- Lingo return statement ❌
- SCI return statement ❌

---

## Implementation Roadmap

### Phase 1: Critical Fixes (TODAY)
1. Fix three parsers (15 min)
2. Update documentation (2 min)
3. Verify with `(asdf:test-system :eightbol)`

### Phase 2: Enhancements (THIS WEEK)
1. Implement argument handling (15 min)
2. Standardize return semantics (15 min)
3. Code cleanup (5 min)

### Phase 3: Testing (NEXT WEEK)
1. Add regression tests (60 min)
2. Integration testing with all backends
3. Create test case corpus

---

## Questions?

All research questions have been answered. For specific areas, consult:
- **Assignment/variables:** MUDDLE_RESEARCH.md §2.1
- **Control flow:** MUDDLE_RESEARCH.md §2.2
- **Function calls:** MUDDLE_RESEARCH.md §2.5
- **Return semantics:** MUDDLE_RESEARCH.md §5
- **Numeric types:** MUDDLE_RESEARCH.md §6
- **AST mapping:** MUDDLE_RESEARCH.md §4 (table)

---

## Files in This Research

```
/home/brpocock/Projects/eightbol/
├── MUDDLE_README.md              ← You are here
├── MUDDLE_FIXES_CHECKLIST.md     ← Implementation guide
├── MUDDLE_RESEARCH_SUMMARY.txt   ← Quick reference
└── MUDDLE_RESEARCH.md            ← Full audit
```

---

**Research Completed:** 2026-09-09
**Status:** Ready for implementation
**Next Action:** Read MUDDLE_FIXES_CHECKLIST.md and implement Priority 1 fixes

