# EIGHTBOL Numeric Type Support Audit: EXECUTIVE SUMMARY

**Date:** 2026-09-09
**Audit Scope:** All 15 EIGHTBOL backends
**Total Documentation:** 2,372 lines (audit, checklist, matrix, readme)

---

## QUICK FACTS

| Metric                      | Value                  |
|-----------------------------|------------------------|
| Total Backends Audited      | 15                     |
| Backends with Mature Support | 3 (6502, Z80, CP1610) |
| Backends with Partial Support | 6 (M68K, I286, ARM7, F8, RP2A03, SM83) |
| Backends with Minimal Support | 6 (65C02, 65C816, HUC6280, M6800, STACK, FORTH) |
| Numeric Types Identified    | 5 (Binary U/S, BCD U/S, Display) |
| Current Average Coverage    | 56%                    |
| Total Codebase (backends)   | ~12,600 lines          |
| **Estimated Total Fix Effort** | **375-525 hours**     |
| **Recommended Timeline**    | **16-20 weeks**        |

---

## KEY FINDINGS

### Numeric Type Support by Category

#### ✓ WELL SUPPORTED
- **BINARY Unsigned 8/16-bit:** All 15 backends
- **BINARY Signed 8/16-bit:** 14/15 backends
- **Bitwise Operations:** All 15 backends
- **Shift Operations:** 14/15 backends
- **Basic ADD/SUBTRACT:** 14/15 backends

#### ⚠ PARTIALLY SUPPORTED
- **BINARY 24/32-bit:** 12/15 backends
- **BCD Unsigned:** 6/15 backends
- **BCD Signed:** 2/15 backends
- **MULTIPLY:** 3/15 backends
- **DIVIDE:** 1/15 backends

#### ✗ NOT SUPPORTED
- **BCD Conversion:** 8/15 backends
- **MULTIPLY (all):** 12/15 backends
- **DIVIDE (all):** 14/15 backends
- **DISPLAY Numbers:** 15/15 backends
- **Fixed-Point Scaling (misaligned):** 14/15 backends

---

## CRITICAL ISSUES (Must Fix)

### Issue 1: BCD 2-Byte In-Place SUBTRACT (6502 Family)
**File:** `backend-6502-part1.lisp:124`
**Severity:** HIGH
**Impact:** SUBTRACT fails on 2-byte signed BCD
**Fix Effort:** 3-5 hours

### Issue 2: Misaligned PIC Decimal Scaling (6502 Family)
**Files:** `backend-6502-part5.lisp:246, 299`
**Severity:** HIGH
**Impact:** ADD/SUBTRACT fails when scales differ
**Fix Effort:** 4-6 hours

### Issue 3: No BCD Conversion (8 backends)
**Severity:** HIGH
**Impact:** Cannot convert BINARY ↔ DECIMAL
**Fix Effort:** 50-100 hours total

### Issue 4: No MULTIPLY/DIVIDE (Most backends)
**Severity:** HIGH
**Impact:** MULTIPLY/DIVIDE statements fail
**Fix Effort:** 150-200 hours total

### Issue 5: No DISPLAY Number Support (All backends)
**Severity:** MEDIUM
**Impact:** DISPLAY cannot be converted or used
**Fix Effort:** 50-100 hours total

---

## BACKEND MATURITY RANKING

```
MATURE (75%+)
  6502       ████████░ 75% (2 known issues)
  65C02      ████████░ 75% (inherits from 6502)
  65C816     ████████░ 75% (inherits from 6502)
  HUC6280    ████████░ 75% (inherits from 6502)

SUBSTANTIAL (60-70%)
  Z80        ██████░░░ 65% (no BCD conversion)
  CP1610     ██████░░░ 62% (no BCD conversion)

PARTIAL (50-60%)
  M68K       █████░░░░ 55% (native ABCD/SBCD; blocked)
  RP2A03     █████░░░░ 55% (6502 variant)
  F8         █████░░░░ 52% (BCD deferred)
  I286       █████░░░░ 50% (has MUL/DIV; blocked)
  ARM7       █████░░░░ 50% (has MUL/DIV; blocked)
  SM83       █████░░░░ 50% (no BCD)

MINIMAL (<50%)
  M6800      ████░░░░░ 40% (incomplete)
  STACK      ████░░░░░ 45% (stack-based)
  FORTH      ████░░░░░ 45% (stack-based)
```

---

## PRIORITY MATRIX

### 🔴 RED (CRITICAL)
1. **6502 BCD SUBTRACT** (3-5h) — Blocks current code
2. **6502 PIC Scaling** (4-6h) — Blocks ADD/SUBTRACT
3. **Numeric Specification** (15-20h) — Blocks all work

### 🟠 ORANGE (HIGH)
1. **6502 MULTIPLY/DIVIDE** (40-60h) — Core functionality
2. **Z80 BCD Support** (50-70h) — Second backend
3. **CP1610 BCD Support** (45-65h) — Third backend

### 🟡 YELLOW (MEDIUM)
1. **M68K/I286/ARM7** (20-30h each) — Quick wins
2. **Other Backends** (100-150h) — Can parallelize

### 🟢 GREEN (LOW)
1. **DISPLAY Support** (50-100h) — Nice to have
2. **Optimization** (20-30h) — Final polish

---

## RECOMMENDED ROADMAP

### Phase 1: Foundation (Week 1)
- [ ] Approve numeric type specification (2h)
- [ ] Fix 6502 BCD SUBTRACT (5h)
- [ ] Fix 6502 PIC scaling (6h)
- [ ] Create test suite (15h)

### Phase 2: 6502 Core (Weeks 2-7)
- [ ] Implement MULTIPLY (20h)
- [ ] Implement DIVIDE (25h)
- [ ] Implement BCD conversion (20h)
- [ ] Test & verify (10h)

### Phase 3: Non-6502 (Weeks 8-16, parallel)
- [ ] Z80 track (70h)
- [ ] CP1610 track (65h)
- [ ] M68K/I286/ARM7 track (75h)
- [ ] Other backends (150h)

### Phase 4: QA & Release (Weeks 16-20)
- [ ] Regression testing (20h)
- [ ] Benchmarking (15h)
- [ ] Documentation (15h)
- [ ] Release (5h)

**Total: 375-525 hours | 16-20 weeks with 2-3 engineers**

---

## DELIVERABLES

### 1. EIGHTBOL_NUMERIC_AUDIT_EXECUTIVE_SUMMARY.md (This File)
- Quick facts, findings, priority matrix
- Recommended roadmap and next steps
- Read time: 15-20 minutes

### 2. EIGHTBOL_NUMERIC_TYPES_AUDIT.md (54 KB, 1,532 lines)
- Complete numeric type specification
- Current state audit for all 15 backends
- CPU-specific implementation plans
- Effort estimates and risk assessment
- Read time: 60-90 minutes

### 3. EIGHTBOL_NUMERIC_IMPLEMENTATION_CHECKLIST.md (16 KB, 590 lines)
- Detailed task breakdown by phase and backend
- Dependency graph and success criteria
- Resource allocation
- Read time: 30-45 minutes

### 4. EIGHTBOL_NUMERIC_MATRIX.csv (2.6 KB, 16 lines)
- Quick-reference matrix of all backends
- Coverage %, support by type, priorities
- Read time: 5 minutes

### 5. NUMERIC_AUDIT_README.md (8 KB, 234 lines)
- Guide to all documentation
- Usage instructions per role
- Read time: 10 minutes

**Total: 2,372 lines | 111-160 minutes to read all**

---

## IMMEDIATE ACTIONS

### This Week
1. **Review Specification** (Lead architect, 2h)
   - Read Part 1 of EIGHTBOL_NUMERIC_TYPES_AUDIT.md
   - Approve or provide feedback

2. **Assign 6502 Specialist** (PM, 1h)
   - Start on backend-6502-part1.lisp:124 (BCD SUBTRACT)
   - Start on backend-6502-part5.lisp:246,299 (PIC scaling)

3. **Create Test Suite** (QA, 20h)
   - Build unit tests per EIGHTBOL_NUMERIC_MATRIX.csv
   - Verify known failures

---

## QUESTIONS FOR STAKEHOLDERS

1. **Specification:** Are you satisfied with the BCD/fixed-point/DISPLAY specification?
2. **Priority:** Is MULTIPLY/DIVIDE critical, or can we defer?
3. **Timeline:** Is 16-20 weeks acceptable, or do you need faster?
4. **Resources:** Can you assign 2-3 full-time engineers?
5. **Testing:** What test coverage do you require (100%? 95%)?

---

**Status:** Complete and ready for implementation sprint planning
**Confidence:** 85-95% on findings, 75-80% on effort estimates

See NUMERIC_AUDIT_README.md for detailed usage guide.
