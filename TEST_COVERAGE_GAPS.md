# EIGHTBOL Test Coverage Gaps - Priority Checklist

Generated: 2026-09-09  
Current Overall Coverage: **82%**

---

## HIGH PRIORITY GAPS (Start Immediately)

### 1. Backend INSPECT Test Suite
- **Current Coverage**: 1-3 core tests
- **Target Coverage**: 60+ tests (5 tests × 13 backends)
- **Effort**: 2 days
- **Files to Create**:
  - `tests/backends/backend-6502-tests/inspect-tests.lisp`
  - `tests/backends/backend-65c02-tests/inspect-tests.lisp`
  - `tests/backends/backend-65c816-tests/inspect-tests.lisp`
  - ... (repeat for all 13 backends)
- **Test Scenarios**:
  - [ ] INSPECT TALLYING with single character
  - [ ] INSPECT TALLYING with multiple characters
  - [ ] INSPECT REPLACING with single character
  - [ ] INSPECT REPLACING with multiple characters
  - [ ] INSPECT with substring patterns
- **Expected Test Increase**: +60 tests

### 2. Backend EVALUATE Test Suite
- **Current Coverage**: 1 core test
- **Target Coverage**: 65+ tests (5 tests × 13 backends)
- **Effort**: 2 days
- **Files to Create**:
  - `tests/backends/backend-*/evaluate-tests.lisp` (×13)
- **Test Scenarios**:
  - [ ] EVALUATE WHEN with equality
  - [ ] EVALUATE WHEN with ranges
  - [ ] EVALUATE WHEN with multiple conditions
  - [ ] EVALUATE WHEN OTHER clause
  - [ ] Nested EVALUATE statements
- **Expected Test Increase**: +65 tests

### 3. Frontend Statement Support Matrix
- **Current Coverage**: Unclear which statements each language supports
- **Target**: Document support for all 23 statement types across all 17 languages
- **Effort**: 1-2 days
- **Deliverable**: `FRONTEND_STATEMENT_SUPPORT_MATRIX.md`
- **Contents**:
  - [ ] Create table: 17 languages × 23 statements
  - [ ] Mark each as: ✓ (supported), ⚠️ (partial), ✗ (unsupported)
  - [ ] Add notes for partial support
  - [ ] Identify missing tests
- **Expected Test Increase**: +30-50 tests (filling gaps)

---

## HIGH PRIORITY GAPS (Next 2 Weeks)

### 4. Reference Modification Edge Cases
- **Current Coverage**: Basic tests only
- **Target Coverage**: Boundary condition tests
- **Effort**: 1-2 days
- **Test Scenarios**:
  - [ ] Reference modification with start=1
  - [ ] Reference modification with start=length
  - [ ] Reference modification with length=0
  - [ ] Reference modification with length > remaining
  - [ ] Reference modification with negative indices (error handling)
  - [ ] Reference modification off-by-one errors
  - [ ] Reference modification with subscripted base
- **Files**: Add to `tests/backend-comprehensive-ast-tests.lisp`
- **Expected Test Increase**: +20-30 tests

### 5. Multiply/Divide Implementation (IF PLANNED)
- **Current Coverage**: 8 limited tests (signals error)
- **Target Coverage**: 130+ tests (if implementation starts)
- **Effort**: 3-4 days
- **Files to Create**:
  - `tests/backends/backend-*/multiply-divide-tests.lisp` (×13)
- **Test Scenarios**:
  - [ ] 8-bit multiply
  - [ ] 16-bit multiply
  - [ ] 8-bit divide
  - [ ] 16-bit divide
  - [ ] Multiplication overflow
  - [ ] Division by zero error
  - [ ] Fixed-point multiply/divide
  - [ ] BCD multiply/divide
- **Expected Test Increase**: +130 tests (if implemented)

---

## MEDIUM PRIORITY GAPS (This Sprint)

### 6. Dynamic Allocation Tests (IF SUPPORTED)
- **Current Coverage**: 0 tests
- **Status**: Unknown if ALLOCATE/DEALLOCATE is supported
- **Decision Required**: Yes/No support?
- **If YES**:
  - Effort: 3 days
  - Target: 50+ tests
  - Files: `tests/backend-*-tests/dynamic-allocation-tests.lisp`
- **If NO**:
  - Document as unsupported
  - Update AST reference

### 7. Exception Handling Expansion
- **Current Coverage**: 1 test per backend (LOG-FAULT, DEBUG-BREAK)
- **Target**: 40+ tests
- **Effort**: 2 days
- **Test Scenarios**:
  - [ ] LOG-FAULT with various error codes
  - [ ] DEBUG-BREAK trap generation
  - [ ] Exception propagation through method stack
  - [ ] Nested exception handling
  - [ ] Exception in conditional branches
  - [ ] Exception in loops
- **Expected Test Increase**: +40 tests

### 8. Copybook Advanced Scenarios
- **Current Coverage**: 47 tests (basic copybook handling)
- **Target**: 65+ tests
- **Effort**: 2 days
- **Test Scenarios**:
  - [ ] Nested copybook includes
  - [ ] Forward references in copybooks
  - [ ] Circular copybook detection
  - [ ] Conditional includes (if supported)
  - [ ] Copybook with library paths
  - [ ] Copybook name collision handling
- **Files**: Expand `tests/copybook-generation-tests.lisp`
- **Expected Test Increase**: +20 tests

### 9. Numeric Precision Edge Cases
- **Current Coverage**: 50 tests (general precision)
- **Target**: 80+ tests
- **Effort**: 2 days
- **Test Scenarios**:
  - [ ] Overflow detection (8-bit, 16-bit, 32-bit)
  - [ ] Underflow detection
  - [ ] Precision loss in conversions
  - [ ] BCD overflow/underflow
  - [ ] Fixed-point rounding modes
  - [ ] Scale factor edge cases
  - [ ] NaN/Infinity handling (if supported)
- **Files**: Create `tests/numeric-edge-cases-all-backends.lisp`
- **Expected Test Increase**: +30 tests

---

## MEDIUM PRIORITY GAPS (After Current Sprint)

### 10. Assembly Directive Injection Tests
- **Current Coverage**: 1 test (ASSEMBLY-ENTRY)
- **Target**: 15+ tests
- **Effort**: 1-2 days
- **Test Scenarios**:
  - [ ] Inline assembly blocks
  - [ ] Custom label insertion
  - [ ] Architecture-specific directives
  - [ ] Inline assembly variable access
  - [ ] Inline assembly register preservation
- **Files**: Create `tests/backend-*-tests/assembly-injection-tests.lisp`
- **Expected Test Increase**: +15 tests

---

## LOW PRIORITY GAPS (Future)

### 11. Performance Regression Tests
- **Effort**: 3-5 days
- **Scope**: Compile-time performance benchmarks
- **Target**: 10-15 tests

### 12. Code Size Optimization Tests
- **Effort**: 2-3 days
- **Scope**: Verify optimizations produce smaller code
- **Target**: 10-15 tests

### 13. Cross-Language Semantic Equivalence
- **Effort**: 5+ days
- **Scope**: Same logic in different languages produces identical output
- **Target**: 20-30 tests

---

## Test Coverage Roadmap

### Week 1 (Immediate)
- [ ] Create INSPECT test files (6 files, ~5 tests each)
- [ ] Create EVALUATE test files (6 files, ~5 tests each)
- [ ] Start Frontend Statement Support Matrix

### Week 2
- [ ] Finish INSPECT tests across all backends (13 backends)
- [ ] Finish EVALUATE tests across all backends (13 backends)
- [ ] Add Reference Modification edge cases

### Week 3
- [ ] Expand Exception Handling tests
- [ ] Expand Copybook Advanced Scenarios
- [ ] Expand Numeric Precision Edge Cases

### Week 4+
- [ ] Implement Multiply/Divide tests (if support added)
- [ ] Add Dynamic Allocation tests (if supported)
- [ ] Performance/Code-size regression tests

---

## Summary of Recommended Additions

### By Effort Size
| Effort | Count | Tests | Files |
|--------|-------|-------|-------|
| 1 day | 3 tasks | ~35 | 1 |
| 2 days | 6 tasks | ~190 | 18 |
| 3 days | 2 tasks | ~90 | 26 |
| 5+ days | 2 tasks | ~50 | TBD |
| **TOTAL** | **13 tasks** | **365+ new tests** | **45+ new files** |

### By Priority
| Priority | Count | Tests | Timeline |
|----------|-------|-------|----------|
| HIGH | 5 tasks | ~155 tests | Weeks 1-2 |
| MEDIUM | 5 tasks | ~140 tests | Weeks 2-4 |
| LOW | 3 tasks | ~70 tests | Week 5+ |
| **TOTAL** | **13 tasks** | **365+ tests** | **4-8 weeks** |

---

## Coverage Impact

### Current State
- Total Tests: 1,321
- Coverage: 82%
- Gaps: 10 major, 10+ minor

### After HIGH Priority
- Total Tests: 1,476 (+155)
- Coverage: ~88%
- Gaps: 5 major, 8 minor

### After MEDIUM Priority
- Total Tests: 1,616 (+140)
- Coverage: ~92%
- Gaps: 2-3 major, 5 minor

### After LOW Priority
- Total Tests: 1,686 (+70)
- Coverage: ~95%
- Gaps: <5 minor

---

## Testing Best Practices Applied

1. **Consistent Test Structure**
   - Lexer → Parser → Functions → Numeric Types → Variables → Integration

2. **Cross-Backend Matrix Testing**
   - Each feature tested on all 13 CPUs (where applicable)

3. **Optimizer Edge Cases**
   - Transformation tests + Edge case tests + Regression tests

4. **Frontend Coverage**
   - Same test categories per language (replicable pattern)

5. **Integration Testing**
   - Real-world scenarios, not just unit tests

---

## Sign-Off

**Audit Category**: 9 (Test Coverage Analysis)  
**Audit Date**: 2026-09-09  
**Total Test Definitions Found**: 1,321  
**Current Coverage**: 82%  
**Target Coverage**: 95%+ (with recommendations)  
**Estimated Timeline**: 4-8 weeks  
**Estimated New Tests**: 365+  

**Status**: Ready for Priority Review & Sprint Planning
