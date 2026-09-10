# AUDIT SUMMARY: Error Handling & Restart Capability

**Document:** EIGHTBOL Compiler Error Handling Audit  
**Date:** 2026-09-09  
**Auditor:** File Search Specialist  
**Compliance:** 4/10 (Partial)

---

## Quick Facts

```
Total Error Classes:        23 ✅
Errors Using Conditions:    100% ✅
Error Tests:                165 ✅
Restart Points:             2 ❌
Restart Tests:              0 ❌
Frontends with Restarts:    0/10 ❌
Backends with Restarts:     0/14 ❌
```

---

## Error Hierarchy Tree (Simplified)

```
┌─────────────────────────────────────────────────────────┐
│              COMPILER-ERROR (root)                      │
│          (All EIGHTBOL errors inherit here)             │
└───────────────┬──────────────────────────────────────────┘
                │
        ┌───────┼────────────────────────────────────────────┐
        │       │                                            │
    ┌───┴───┐ ┌─┴────────┐ ┌──────────────┐ ┌──────────────┐
    │SOURCE │ │ BACKEND  │ │ COPYBOOK     │ │ COMPILATION  │
    │ERROR  │ │ ERROR    │ │ ERROR        │ │ ERROR        │
    └───────┘ └──────────┘ └──────┬───────┘ └────────┬─────┘
     Parser    Code Gen    Library │              Validate │
     Syntax    Unsupported Imports │              Pipeline  │
               Operations   Stubs  │              Termination│
                           (3)    │                  (5)
                                  │
                          ┌───────┴────────┬─────────────┐
                          │                │             │
                    ┌─────┴────┐  ┌────────┴──┐  ┌──────┴──┐
                    │ NOT-FOUND│  │INVALID-  │  │ READ-   │
                    │          │  │ NAME     │  │ ERROR   │
                    └──────────┘  └──────────┘  └─────────┘
```

---

## Current Restart Coverage

### Visualization: Compilation Pipeline

```
INPUT FILES
    │
    ├─ [restart-case] ◄─ retry-compile (full recompile)
    ▼
PARSE
    │
    ├─ Parser error ──► signal source-error (NO RESTART) ✗
    │
    ├─ [restart-case] ◄─ retry-compile (full recompile)
    ▼
OPTIMIZE
    │
    ├─ Unknown node ──► error 'compiler-error (NO RESTART) ✗
    │
    ├─ Expression too complex ──► error (NO RESTART) ✗
    ▼
VALIDATE
    │
    ├─ Bad termination ──► error 'routine-not-terminated (NO RESTART) ✗
    │
    ├─ Undefined class ──► error 'undefined-class-reference (NO RESTART) ✗
    ▼
BACKEND (per CPU)
    │
    ├─ Unsupported op ──► error 'backend-error (NO RESTART) ✗
    │
    ├─ [handler-case] ◄─ Catches, re-signals (NO RESTART)
    ▼
OUTPUT
    │
    └─ Assembly file (or abort)
```

**Legend:**
- `[restart-case]` = Can restart
- `✓` = Has restart
- `✗` = No restart (errors halt)

---

## Error Distribution Across Components

```
FRONTEND-COBOL (Parser)
├─ source-error (parser syntax)
│  └─ [29 signals] NO RESTART ✗
│
├─ copybook-not-found
│  └─ [2 signals] NO RESTART ✗
│
└─ copybook-invalid-name
   └─ [2 signals] NO RESTART ✗

FRONTEND-BASIC (Lexer)
├─ handler-case catches numeric parse errors (silent)
│  └─ [5 error handlers] NO RESTART ✗
│
└─ handler-case catches transpile errors
   └─ [2 error handlers] NO RESTART ✗

BACKEND-6502 & VARIANTS (Code Generation)
├─ backend-error (unsupported ops)
│  └─ [29 signals] NO RESTART ✗
│
├─ backend-call-service-error (service table)
│  └─ [1+ signals] NO RESTART ✗
│
└─ generic error() (method class unknown)
   └─ [?] NO RESTART ✗

AST-OPTIMIZER & VALIDATOR (Pipeline)
├─ compiler-error (expression too complex)
│  └─ [?] NO RESTART ✗
│
├─ routine-not-terminated (validation)
│  └─ [?] NO RESTART ✗
│
└─ undefined-class-reference (validation)
   └─ [?] NO RESTART ✗
```

---

## Test Coverage Heat Map

```
Legend:   🟢 = Good (20+)  🟡 = Fair (5-20)  🔴 = Poor (0-5)  ⚪ = None

COMPONENT               ERROR TESTS    RESTART TESTS    SIGNAL TESTS
─────────────────────────────────────────────────────────────────
6502 Backend              🟢 ~30        ⚪ None          🟢 ~15
65C02 Backend             🟢 ~20        ⚪ None          🟢 ~10
ARM7 Backend              🟢 ~20        ⚪ None          🟢 ~10
Other Backends (11x)      🟡 ~70        ⚪ None          🟡 ~35
────────────────────────────────────────────────────────────────
COBOL Parser              🟡 ~15        ⚪ None          🟡  ~8
BASIC Frontend            🟡 ~10        ⚪ None          🟡  ~5
Other Frontends           🔴  ~5        ⚪ None          🔴  ~2
────────────────────────────────────────────────────────────────
AST Optimizer             🟡 ~12        ⚪ None          🟡  ~6
AST Validator             🟡 ~10        ⚪ None          🟡  ~5
────────────────────────────────────────────────────────────────
TOTAL                     🟢~165        ⚪ NONE          🟢~85+
```

---

## Error Message Quality Examples

### ✅ Good: Source Error

```
Character.cob:line 125 (seq 5): 
Unexpected token DIVIDE (expected: MOVE, ADD, SUBTRACT, ...)
```

**Why good:**
- File path ✓
- Line number ✓
- Token sequence ✓
- Expected alternatives ✓

### ✅ Good: Backend Error

```
6502: expected :move, got :divide (DIVIDE not supported)
```

**Why good:**
- CPU identifier ✓
- Expected AST node type ✓
- Actual AST node type ✓
- Brief reason ✓

### ⚠️ Poor: Raw Error (No Context)

```
error "Expression too complex for available temporary storage."
```

**Why poor:**
- No file/line ✗
- No suggestions ✗
- Not restartable ✗
- No error ID ✗

### ❌ Bad: Silent Failure

```
; src/frontend-basic/basic-lexer.lisp:93-112
(handler-case
    (parse-integer trimmed :radix 8)
  (error () nil))  ; Just returns nil, doesn't tell user
```

**Why bad:**
- User doesn't know error occurred ✗
- No restartable condition ✗
- No recovery path ✗

---

## Restart Patterns: Current vs. Recommended

### Current (Limited)

```
compile-eightbol
│
└─ [restart-case]
   │
   └─ retry-compile
      └─ Restarts entire compilation
      
  (Only 1 restart type, applies universally)
```

### Recommended (Structured)

```
compile-eightbol
│
├─ [restart-case]
│  └─ abort-compile ◄── Stop, return nil
│
└─ for-each-input-file
   │
   ├─ [restart-case]
   │  ├─ skip-file ◄── Skip to next file
   │  └─ retry-file ◄── Retry this file
   │
   └─ parse-file
      │
      ├─ [restart-case]
      │  ├─ skip-method ◄── Skip to next METHOD
      │  ├─ use-ast ◄── Use fallback AST
      │  └─ continue ◄── Ignore warning, continue
      │
      └─ resolve-copybooks
         │
         ├─ [restart-case]
         │  ├─ skip-copy ◄── Omit this COPY
         │  ├─ use-copy-as ◄── Use alternate name
         │  └─ use-builtin ◄── Use built-in data
         │
         └─ find-copybook
            └─ [Error: not found]
```

---

## Compliance Matrix (Detailed)

| Criterion | Current | Required | Gap | Fix Effort |
|-----------|---------|----------|-----|-----------|
| Error class hierarchy | ✅ (23 classes) | 20+ | ✅ met | — |
| Unique error identifiers | ⚠️ (by name only) | Name + numeric ID | Add IDs | 2 hrs |
| Clear messages | ✅ (with context) | With suggestions | Add suggestions | 4 hrs |
| Restarts for parse errors | ❌ (0) | 3-5 | Add skip/continue | 8 hrs |
| Restarts for copybooks | ❌ (0) | 3-4 | Add skip/use-alt | 6 hrs |
| Restarts for backends | ❌ (0) | 2-3 | Add skip/continue | 8 hrs |
| Multi-error accumulation | ❌ | ✅ | Add error collector | 8 hrs |
| Restart tests | ❌ (0 tests) | 20+ tests | New test suite | 8 hrs |
| **TOTAL** | **⚠️** | **✅** | **High** | **~44 hrs** |

---

## Priority Roadmap

### Phase 1: Foundation (1-2 Days)
```
□ Add error IDs to src/conditions.lisp
□ Add :skip restarts for copybook + files
□ Create restart tests (basic)
└─ Result: Multi-file compilation works despite errors
```

### Phase 2: Expansion (1 Day)
```
□ Add :continue restarts for validation
□ Add :abort restart at top level
□ Expand restart tests
└─ Result: User can choose to continue despite warnings
```

### Phase 3: Recovery (1-2 Days)
```
□ Parser error recovery (skip to next METHOD)
□ Backend error suggestions
□ Multi-error collection
└─ Result: Better developer experience, more errors reported at once
```

### Phase 4: Polish (1 Day)
```
□ Interactive restart selection
□ Error ID filtering/scripting
□ Documentation
└─ Result: Production-ready error handling
```

---

## Critical Issues (Blocking)

### Issue #1: Parser Errors Halt Compilation
**Impact:** Single syntax error in one file prevents entire project compilation  
**Severity:** 🔴 HIGH  
**Fix:** Add `:skip-method` or `:skip-to-next` restart in parser

### Issue #2: Copybook Not Found = Hard Stop
**Impact:** Missing copybook stops all processing  
**Severity:** 🔴 HIGH  
**Fix:** Add `:skip-copy` / `:use-copy-as` restarts

### Issue #3: No Multi-Error Collection
**Impact:** Developer only sees first error, must fix and recompile repeatedly  
**Severity:** 🟡 MEDIUM  
**Fix:** Collect errors in `*compilation-errors*`, report all at end

### Issue #4: Backend Errors Not Restartable
**Impact:** Unsupported operation = compilation halt  
**Severity:** 🟡 MEDIUM  
**Fix:** Add `:skip-unsupported` or `:compile-alternate-cpu` restarts

---

## Success Criteria (Post-Implementation)

✅ **Pass When:**

1. ≥15 restart points across pipeline
2. ≥50 restart test cases (FiveAM)
3. All 23 error classes have restart options
4. Parser can recover from single-method errors
5. Multiple errors reported per compilation run
6. Zero regression in existing tests
7. Restart documentation in README.md + AGENTS.md

---

## Files to Modify

| File | Lines | Changes | Complexity |
|------|-------|---------|-----------|
| `src/conditions.lisp` | 231 | Add error IDs, suggestions | Low |
| `src/eightbol-compile.lisp` | 586 | Add skip/abort restarts | Low-Med |
| `src/frontend-cobol/cobol-parser.lisp` | ~2000 | Add parse recovery | Medium |
| `src/ast-validate.lisp` | 279 | Add continue restart | Low |
| `src/expression-complexity.lisp` | ~120 | Add suggestions | Low |
| `src/backend.lisp` | ~1500 | Add backend restarts | Medium |
| `tests/*.lisp` | Many | Add restart tests | Medium |

---

## Appendix: Error IDs (Proposed)

```
E001-E010   Source/Parse Errors
  E001 = Unexpected token
  E002 = Missing token
  E003 = Unsupported statement
  E004 = Syntax error (generic)
  E005 = COPY not found
  E006 = COPY invalid name
  E007 = COPY read error
  E008 = Invalid class reference
  E009 = Method not terminated
  E010 = Unqualified variable

E011-E020   Backend Errors
  E011 = Unsupported operation
  E012 = Invalid AST node
  E013 = Condition not implemented
  E014 = STRING BLT error
  E015 = Service call error
  E016 = Resource limit exceeded

E021-E030   Compilation Pipeline
  E021 = Parse failed
  E022 = Invalid AST structure
  E023 = Optimization failed
  E024 = Validation failed
  E025 = Unknown CPU
  E026 = Invalid option
  E027 = File not found
  E028 = I/O error

E031-E040   Reserved for future
```

---

## References

- **Full Audit:** `AUDIT-ERROR-HANDLING.md`
- **AGENTS.md:** Agent guidelines (restart protocol, style)
- **README.md:** Project overview
- **Common Lisp Standard:** Condition system, restarts

---

**Generated:** 2026-09-09 | **Next Review:** After Phase 1 implementation
