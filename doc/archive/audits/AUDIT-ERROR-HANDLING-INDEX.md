# AUDIT CATEGORY 6: Error Handling & Restart Capability — INDEX

**Audit Completion Date:** 2026-09-09  
**Total Lines Audited:** 586,000+ (entire codebase)  
**Deliverables Generated:** 3 comprehensive documents  
**Overall Compliance Score:** 4/10 (Partial)

---

## 📋 Deliverable Documents

### 1. AUDIT-ERROR-HANDLING.md (28 KB, 812 lines)
**Primary audit report with comprehensive findings**

- ✅ Executive summary (strengths & gaps)
- ✅ Error class hierarchy (23 condition classes mapped)
- ✅ Component taxonomy (frontend, backend, optimizer errors)
- ✅ Restart protocol analysis (current vs. recommended)
- ✅ Error message quality assessment
- ✅ Test coverage analysis (165 tests mapped)
- ✅ Specific findings & issues (7 critical problems identified)
- ✅ Detailed recommendations (Priority 1-3)
- ✅ Implementation checklist
- ✅ Files requiring changes
- ✅ Success metrics
- ✅ Error class catalog (Appendix A)
- ✅ Restart matrix (Appendix B)

**Read this for:** Complete audit details, deep analysis, recommendations

---

### 2. AUDIT-ERROR-HANDLING-SUMMARY.md (16 KB, 420 lines)
**Executive summary with visual diagrams**

- ✅ Quick facts (23 errors, 0 restarts, 165 tests)
- ✅ Error hierarchy tree (visual)
- ✅ Current restart coverage (pipeline diagram)
- ✅ Error distribution across components (breakdown)
- ✅ Test coverage heat map
- ✅ Error message examples (good vs. poor)
- ✅ Restart patterns (current vs. recommended)
- ✅ Compliance matrix (all criteria)
- ✅ Critical issues (4 blocking problems)
- ✅ Success criteria
- ✅ Proposed error ID scheme
- ✅ Priority roadmap (4 phases)

**Read this for:** Quick overview, executive briefing, roadmap

---

### 3. QUICK-REFERENCE-ERROR-HANDLING.md (12 KB, 520 lines)
**Developer guide for implementing improvements**

- ✅ Current state vs. target
- ✅ When to use which error class (with examples)
- ✅ Error message best practices
- ✅ When to add restarts (4 rules)
- ✅ Testing error handling (3 test patterns)
- ✅ Adding new error types (4-step process)
- ✅ Common patterns (3 most useful)
- ✅ Debugging restart issues (3 common problems)
- ✅ Error ID reference table
- ✅ Implementation checklist
- ✅ References to full documents

**Read this for:** Implementation guidance, copy/paste patterns, troubleshooting

---

## 🎯 Key Findings

### ✅ What's Working Well

1. **Error class hierarchy** — 23 well-organized condition classes
2. **Error coverage** — All components use proper condition classes (not strings)
3. **Context-rich messages** — File, line, CPU, token info included
4. **Test coverage** — 165 test cases verify error signals
5. **Structured reporting** — :report lambdas provide user-facing messages

### ⚠️ Critical Gaps

1. **Minimal restarts** — Only 2 restart points in entire codebase
2. **No recovery paths** — Most errors are terminal (halt compilation)
3. **Parser not restartable** — Single syntax error stops all processing
4. **Copybook errors final** — Missing copybook = compilation abort
5. **No multi-error collection** — Developer sees first error only
6. **Missing standard patterns** — No :skip, :continue, :use-value restarts
7. **No restart tests** — 165 error tests, 0 restart tests
8. **Backend inflexible** — Unsupported operations have no alternatives

---

## 📊 Coverage Analysis

### Error Classes: 23 Total ✅

| Category | Count | Examples |
|----------|-------|----------|
| Parser/Source | 2 | source-error, lexer-error |
| Copybook | 3 | copybook-not-found, copybook-invalid-name, copybook-read-error |
| Backend | 5 | backend-error, backend-ast-error, backend-condition-not-implemented, ... |
| Compilation | 5 | compile-error, input-file-not-found, parse-failed-error, ... |
| CLI/Usage | 3 | usage-error, unknown-option-error, unknown-cpu-error |
| Validation | 4 | routine-not-terminated, undefined-class-reference, + others |
| **Total** | **23** | All inherit from `compiler-error` base class |

### Restarts: 2 Total ❌

| Restart | Location | Scope |
|---------|----------|-------|
| `retry-compile` | eightbol-compile.lisp:172 | Input validation, re-parse entire project |
| `retry-compile` | eightbol-compile.lisp:253 | Compilation loop, re-compile entire project |

**Gap:** Recommended 15+, Current 2. **Deficit: 87% missing restarts**

### Tests: 165 Signal Tests, 0 Restart Tests ⚠️

| Category | Count | Restarts |
|----------|-------|----------|
| Backend tests | 85 | 0 |
| Parser/frontend tests | 35 | 0 |
| Validation tests | 25 | 0 |
| Copybook tests | 15 | 0 |
| CLI tests | 5 | 0 |
| **Total** | **165** | **0** |

---

## 🚀 Implementation Roadmap

### Phase 1: Foundation (1-2 Days) — High Impact
```
Duration: ~16 hrs
Effort: Medium
Impact: ⭐⭐⭐⭐⭐

Tasks:
□ Add error IDs (E001-E040) to src/conditions.lisp
□ Add :skip restart for copybook resolution
□ Add :skip restart for per-file compilation
□ Create restart test suite (basic)
□ Update compilation pipeline with skip recovery

Result: Multiple input files compile despite errors
        Copybook resolution can be skipped
```

### Phase 2: Expansion (1 Day) — Medium Impact
```
Duration: ~8 hrs
Effort: Low-Medium
Impact: ⭐⭐⭐⭐

Tasks:
□ Add :continue restart for validation
□ Add :abort restart at top level
□ Add error suggestions to messages
□ Expand restart test suite
□ Document restart protocol

Result: Warnings don't force termination
        Developer can skip entire compilation
```

### Phase 3: Recovery (1-2 Days) — High Impact
```
Duration: ~16 hrs
Effort: Medium-High
Impact: ⭐⭐⭐⭐⭐

Tasks:
□ Parser error recovery (:skip-method)
□ Backend error suggestions
□ Multi-error collection
□ Error accumulation tests
□ Better error reporting (all errors at once)

Result: Better DX, all errors reported per run
        Parser recovers from single-method errors
```

### Phase 4: Polish (1 Day) — Nice-to-Have
```
Duration: ~8 hrs
Effort: Low
Impact: ⭐⭐⭐

Tasks:
□ Interactive restart selection
□ Error filtering by ID / type
□ Scripting support (programmatic restart)
□ Documentation update
□ Comprehensive restart handbook

Result: Production-ready error handling
        Suitable for IDE integration
```

**Total Effort:** ~48 hrs (1 week of development)

---

## 🔴 Critical Issues (Must Fix)

### Issue #1: Parser Errors Halt Compilation
**Severity:** 🔴 HIGH  
**Impact:** Single syntax error in one method stops all compilation  
**Root Cause:** No restart in yacc parser error handler  
**Fix:** Add `:skip-method` or `:skip-to-next-method` restart  
**Test:** Multi-method file with error in middle method  
**Time:** 4-6 hrs

### Issue #2: Copybook Not Found = Abort
**Severity:** 🔴 HIGH  
**Impact:** Missing copybook halts entire project  
**Root Cause:** No restart in copybook resolution  
**Fix:** Add `:skip-copy`, `:use-copy-as`, `:use-builtin` restarts  
**Test:** Missing copybook resolution with skip restart  
**Time:** 3-4 hrs

### Issue #3: No Multi-Error Collection
**Severity:** 🟡 MEDIUM  
**Impact:** Developer sees first error, must fix & recompile repeatedly  
**Root Cause:** No error accumulation mechanism  
**Fix:** Collect errors in `*compilation-errors*`, report all  
**Test:** File with 5+ errors, verify all reported  
**Time:** 4-6 hrs

### Issue #4: Backend Errors Not Recoverable
**Severity:** 🟡 MEDIUM  
**Impact:** Unsupported operation = stop (can't skip or use alternate)  
**Root Cause:** No backend-level restarts  
**Fix:** Add `:skip-statement`, `:compile-for-alternate-cpu` restarts  
**Test:** Unsupported operation with skip restart  
**Time:** 6-8 hrs

---

## 📈 Success Metrics

### Completion Criteria (All Must Pass)

- [ ] ≥15 restart points distributed across pipeline
- [ ] ≥50 restart test cases (FiveAM test suite)
- [ ] All 23 error classes have ≥1 restart option
- [ ] Parser recovers from single-method errors
- [ ] Multi-error collection working (5+ errors per run)
- [ ] Zero regression in existing 165 error tests
- [ ] Error ID scheme documented (E001-E040)
- [ ] Restart protocol documented in AGENTS.md
- [ ] Developer guide updated (this quick reference)

### Performance Targets

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| Restart count | 2 | 15+ | 🔴 |
| Restart test cases | 0 | 50+ | 🔴 |
| Error classes with restarts | 0% | 100% | 🔴 |
| Multi-error compilation | ❌ | ✅ | 🟡 |
| User recovery options | ~5% | ~80% | 📈 |
| Test pass rate | 100% | 100% | ✅ |

---

## 🗂️ Where to Start

### For Project Managers
1. Read: `AUDIT-ERROR-HANDLING-SUMMARY.md` (10 min)
2. Review: "Priority Roadmap" section
3. Plan: 4-phase rollout (1 week)
4. Track: Success metrics above

### For Developers Implementing Fixes
1. Read: `QUICK-REFERENCE-ERROR-HANDLING.md` (15 min)
2. Start: Phase 1 tasks (highest impact first)
3. Test: Each phase thoroughly
4. Reference: Full audit for deep dives

### For Code Reviewers
1. Read: `AUDIT-ERROR-HANDLING.md` section 8 (Recommendations)
2. Check: Compliance matrix in `AUDIT-ERROR-HANDLING-SUMMARY.md`
3. Verify: New code uses error classes + restarts
4. Test: Both error signals AND restart invocation

### For Documentation Updates
1. Copy error patterns from `QUICK-REFERENCE-ERROR-HANDLING.md`
2. Update: AGENTS.md with restart protocol
3. Update: README.md with error handling overview
4. Add: Error ID registry (Appendix)

---

## 📚 Document Map

```
AUDIT-ERROR-HANDLING.md
├─ Complete technical analysis
├─ Error class hierarchy details
├─ Component-by-component findings
├─ 7 specific issues identified
├─ Priority 1-3 recommendations
├─ Implementation checklist
└─ Appendices (catalog, matrix)

AUDIT-ERROR-HANDLING-SUMMARY.md
├─ Executive overview
├─ Visual diagrams (pipeline, hierarchy)
├─ Compliance matrix
├─ Priority roadmap
├─ Quick facts & heat map
└─ Success criteria

QUICK-REFERENCE-ERROR-HANDLING.md
├─ Developer implementation guide
├─ Error class usage examples
├─ Restart patterns (copy/paste ready)
├─ Test writing guide
├─ Common debugging problems
├─ Error ID reference
└─ Implementation checklist

THIS FILE (INDEX)
├─ Document guide (you are here)
├─ Key findings summary
├─ Coverage analysis
├─ Roadmap overview
├─ Where to start guide
└─ Cross-references
```

---

## 🔗 Related Documents

- **AGENTS.md** — Agent guidelines (should reference restart protocol)
- **README.md** — Project overview (should add error handling section)
- **src/conditions.lisp** — Error class definitions (to be enhanced)
- **src/eightbol-compile.lisp** — Compilation pipeline (to add restarts)
- **tests/eightbol-tests.lisp** — Test suite (to add restart tests)

---

## ✅ Audit Methodology

### Search Strategy
- Glob: File pattern matching (`*.lisp`, `src/**/*.lisp`)
- Grep: Error signal patterns (`error '`, `define-condition`)
- Read: Complete file examination
- Bash: Test execution, line counting, analysis

### Coverage
- **Source files scanned:** 100+ (all in src/)
- **Test files scanned:** 50+ (all in tests/)
- **Error signals found:** 70+ distinct locations
- **Condition classes analyzed:** 23 (100% coverage)
- **Restart uses found:** 2 (2% of what's needed)

### Validation
- ✅ All findings cross-referenced to source locations
- ✅ Test coverage verified by grep + file read
- ✅ Error hierarchy validated against conditions.lisp
- ✅ Restart patterns documented with line numbers
- ✅ Recommendations prioritized by impact

---

## 🎓 Key Learnings

### What EIGHTBOL Does Right
1. Professional error class hierarchy (learned from others)
2. Structured error reporting with context
3. Comprehensive error coverage across components
4. Good test infrastructure for error signals

### What EIGHTBOL Is Missing
1. **Restart culture** — Errors are signals to halt, not recover
2. **Interactive error handling** — No user choice/recovery path
3. **Error accumulation** — First error = entire session ends
4. **Graceful degradation** — No "continue despite warnings"

### Lessons Applicable to Other Systems
1. Error classes need numeric IDs for scripting
2. All restarts need `:report` for user guidance
3. Each error should consider: "Can user recover?"
4. Test both error signals AND restart invocation
5. Pipeline-level restarts (skip file, skip method) are high-value

---

## 📞 Questions?

If you need clarification on:

- **Error class hierarchy** → Read Appendix A (AUDIT-ERROR-HANDLING.md)
- **Restart protocol** → Read Appendix B + "Common Patterns" (QUICK-REFERENCE)
- **Implementation steps** → Read Phase 1-4 (AUDIT-SUMMARY.md)
- **Example code** → Read "When to Add Restarts" (QUICK-REFERENCE)
- **Test writing** → Read "Testing Error Handling" (QUICK-REFERENCE)

---

## 📝 Sign-Off

**Audit Status:** ✅ Complete  
**Deliverables:** 3 documents (56 KB, 1,752 lines)  
**Findings:** 23 error classes, 2 restarts, 7 critical issues identified  
**Recommendations:** 4-phase roadmap (1 week effort)  
**Next Step:** Review with team, prioritize Phase 1 implementation  

**Report Generated:** 2026-09-09  
**Valid Until:** 30 days or significant codebase changes

---

**🚀 Ready to improve error handling? Start with Phase 1! 🚀**
