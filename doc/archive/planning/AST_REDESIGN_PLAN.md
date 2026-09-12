# EIGHTBOL AST Redesign: Operator Nodes as Top-Level AST Types

**Date:** September 9, 2026  
**Status:** AUTHORIZED - Critical architectural change  
**Impact:** Affects all 17 frontends + 13 backends + 6 optimizers

---

## Executive Summary

The comprehensive AST coverage audit (September 9, 2026) revealed **critical gaps** in operator support across all frontends. The user has authorized a **major architectural redesign**:

### Authorization
✅ **Arithmetic & bitwise operators must be top-level AST nodes**
```lisp
;; OLD (embedded, current):
(:compute :target x :expression (+ a b))

;; NEW (canonical):
(:+ (:- a b) c)
```

✅ **All 6 bitwise operators must be in all 17 frontends**
- Coverage goal: 17/17 (100%) for `:¬ :∧ :∨ :⊻ :⊼ :⊽`
- Current: 0/17 (0%)

✅ **Complete Burgermistress to >70% coverage**
- Current: 8/56 (14%)
- Target: 40+ nodes

---

## Current State (Pre-Redesign)

### Operator Coverage Before Redesign

| Operator | Coverage | Status |
|----------|----------|--------|
| `:+` | 0/17 (0%) | **🚨 CRITICAL** |
| `:-` | 2/17 (11%) | **🚨 CRITICAL** |
| `:\u00d7` | 4/17 (23%) | **🚨 CRITICAL** |
| `:\u00f7` | 4/17 (23%) | **🚨 CRITICAL** |
| `:¬` | 0/17 (0%) | **🚨 CRITICAL** |
| `:\u2227` | 0/17 (0%) | **🚨 CRITICAL** |
| `:\u2228` | 0/17 (0%) | **🚨 CRITICAL** |
| `:\u22bb` | 0/17 (0%) | **🚨 CRITICAL** |
| `:\u22bc` | 0/17 (0%) | **🚨 CRITICAL** |
| `:\u22bd` | 0/17 (0%) | **🚨 CRITICAL** |
| `:=` | 10/17 (58%) | Partial |
| `:\u2260` | 6/17 (35%) | Partial |
| `:<` | 6/17 (35%) | Partial |
| `::` (shift) | 1/17 (5%) | **🚨 CRITICAL** |

### Problem Statement
- **Arithmetic operators are embedded in `:add`/`:subtract`/`:compute` nodes**
  - Frontends emit composite nodes, not operator keywords
  - Backends cannot process operators independently
  - Optimizers cannot rewrite arithmetic expressions

- **Bitwise operators are completely absent**
  - Zero frontends emit bitwise operators
  - No native syntax in any target language
  - Blocks all systems relying on bitwise operations

- **Inconsistent operator representation**
  - Comparisons: keyword operators (`:=`, `:<`, etc.)
  - Arithmetic: embedded in nodes
  - Bitwise: missing entirely
  - Creates incompatibility across AST producers/consumers

---

## Proposed Redesign

### 1. Operator Nodes as Top-Level AST Types

#### Structure
```lisp
;; Comparison operators (existing, unchanged):
(:= left-expr right-expr)
(:<  left-expr right-expr)
(:≠  left-expr right-expr)

;; Arithmetic operators (NEW - top-level):
(:+ left-expr right-expr)
(:- left-expr right-expr)
(:× left-expr right-expr)
(:÷ left-expr right-expr)

;; Bitwise operators (NEW - top-level):
(:¬ expr)           ;; NOT
(:∧ left-expr right-expr)   ;; AND
(:∨ left-expr right-expr)   ;; OR
(:⊻ left-expr right-expr)   ;; XOR
(:⊼ left-expr right-expr)   ;; NAND
(:⊽ left-expr right-expr)   ;; NOR

;; Shift operators (NEW - top-level):
(:ash expr shift-count)     ;; Arithmetic shift
```

#### Equivalence
```lisp
;; Before:
(:compute :target result :expression (+ (* a 2) (- b c)))

;; After (canonical):
(:+ (:× a 2) (:- b c))
```

### 2. Updated AST Node Definitions

#### In `src/ast.lisp`

```lisp
;; Operator node constructors
(defun make-operator-node (op-keyword left &optional right)
  "Build an operator node.
   
   OP-KEYWORD: :+ :- :× :÷ :¬ :∧ :∨ :⊻ :⊼ :⊽ := :≠ :< :≤ :> :≥ :ash
   LEFT:       left operand (expression)
   RIGHT:      right operand (optional, nil for unary :¬)
   
   Returns: (:op-keyword left) or (:op-keyword left right)"
  (if right
      (list op-keyword left right)
      (list op-keyword left)))

;; Specializations
(defun make-add-node (left right)
  (make-operator-node :+ left right))

(defun make-subtract-node (left right)
  (make-operator-node :- left right))

(defun make-multiply-node (left right)
  (make-operator-node :× left right))

(defun make-divide-node (left right)
  (make-operator-node :÷ left right))

(defun make-bitwise-and-node (left right)
  (make-operator-node :∧ left right))

;; ... etc for all operators
```

### 3. AST Validation Updates

#### Operator Node Validation
```lisp
;; All operator nodes must have:
;; - Valid operator keyword (from canonical list)
;; - Operand count matching operator (1 for :¬, 2 for binary)
;; - Each operand is valid expression (literal, symbol, nested operator, etc.)
;; - No invalid nesting (:of inside :+, etc.)
```

---

## Implementation Plan

### Phase 1: AST Layer (Week 1-2)
**Effort:** HIGH  
**Risk:** HIGH (affects all downstream)

- [ ] Update `src/ast.lisp` with new operator constructors
- [ ] Add validation for operator nodes
- [ ] Define canonical operator keyword list
- [ ] Create operator node documentation
- [ ] Add tests for operator node construction

### Phase 2: Frontend Updates (Week 3-8)
**Effort:** CRITICAL  
**Risk:** MEDIUM

For each of 17 frontends:
- [ ] Add support for arithmetic operator keyword emission (`:+`, `:-`, `:×`, `÷`)
- [ ] Add support for all 6 bitwise operators (`:¬`, `:∧`, `:∨`, `:⊻`, `:⊼`, `:⊽`)
- [ ] Add support for shift operators (`:ash`)
- [ ] Provide **native language syntax examples** for each operator
- [ ] Update tests for each operator type

**Frontend Priority:**
1. **COBOL** (reference, 84% coverage already)
2. **Lua** (66% coverage, modern)
3. **SCI, Lingo, Pascal** (48-50% coverage)
4. **All others** (ranked by current coverage)

### Phase 3: Backend Updates (Week 9-10)
**Effort:** HIGH  
**Risk:** MEDIUM

For each of 13 backends (6502, 65c02, 65c816, Z80, etc.):
- [ ] Add code generation for all operator nodes
- [ ] Emit valid assembly for each operator per CPU architecture
- [ ] Handle expression precedence correctly
- [ ] Allocate temporaries for complex expressions
- [ ] Validate operand widths match CPU capabilities

### Phase 4: Optimizer Updates (Week 11-12)
**Effort:** MEDIUM  
**Risk:** LOW

For each of 6 optimizers:
- [ ] Preserve operator nodes if unknown
- [ ] Rewrite operator expressions (e.g., constant folding)
- [ ] Simplify nested operators
- [ ] Apply strength reduction (e.g., `(:× x 2)` → `(:ash x 1)`)

### Phase 5: Testing & Validation (Week 13-14)
**Effort:** MEDIUM  
**Risk:** LOW

- [ ] Create comprehensive operator coverage tests
- [ ] Verify all 17 frontends → all operator types
- [ ] Verify all 13 backends accept all operator types
- [ ] Regression testing across entire system
- [ ] Performance benchmarking

### Phase 6: Burgermistress Completion (Parallel, Week 1-14)
**Effort:** MEDIUM  
**Risk:** LOW

- [ ] Audit current Burgermistress implementation
- [ ] Implement missing nodes to reach >70% coverage
- [ ] Add bitwise operator support
- [ ] Add Prolog-specific features
- [ ] Test suite (60+ tests)

---

## Native Language Syntax Reference

### Arithmetic Operators

| Language | Addition | Subtraction | Multiply | Divide | Status |
|----------|----------|-------------|----------|--------|--------|
| COBOL | `ADD A TO B` | `SUBTRACT B FROM A` | `MULTIPLY A BY B` | `DIVIDE A BY B` | ✅ |
| BASIC | `A + B` | `A - B` | `A * B` | `A / B` | ✅ |
| C/Objective-C | `a + b` | `a - b` | `a * b` | `a / b` | ✅ |
| Lua | `a + b` | `a - b` | `a * b` | `a / b` | ✅ |
| Forth | `A B +` | `A B -` | `A B *` | `A B /` | ✅ |
| Prolog/BurgerMistress | `is(X, A+B)` | `is(X, A-B)` | `is(X, A*B)` | `is(X, A/B)` | ✅ |

### Bitwise Operators

| Language | AND | OR | XOR | NOT | NAND | NOR | Status |
|----------|-----|----|----|-----|------|-----|--------|
| COBOL | `B-AND` | `B-OR` | `B-XOR` | `B-NOT` | — | — | ✅ |
| BASIC | `AND` | `OR` | `XOR` | `NOT` | — | — | ✅ |
| C/Objective-C | `&` | `\|` | `^` | `~` | — | — | ✅ |
| Lua | `&` | `\|` | `~` | `~` (unary) | — | — | ⚠️ (6-bit limit) |
| Forth | `AND` | `OR` | `XOR` | `INVERT` | — | — | ✅ |
| Prolog/BurgerMistress | `/\` | `\/` | `xor` | `\+` | — | — | ✅ |

---

## Risk Assessment

### High Risk
- **Precedence:** Operator precedence varies by language; must normalize in AST
- **Backends:** AST change requires updates to all 13 code generators
- **Regression:** Risk of breaking existing test suite

### Medium Risk
- **Frontend parsing:** Adding 10+ new operators to each parser
- **Operator disambiguation:** Some symbols (`*`, `/`, etc.) used for multiple purposes

### Mitigation
- Create comprehensive test suite FIRST
- Update AST layer → test → update frontends → test → etc.
- Maintain backward compatibility layer (old nodes map to new)
- Extensive regression testing after each phase

---

## Success Criteria

### Phase 1 Complete When
- [ ] All operator node constructors work
- [ ] Validation passes for valid operators
- [ ] Documentation complete

### Phase 2 Complete When
- [ ] All 17 frontends emit all operator types
- [ ] Coverage: 100% (17/17) for each operator
- [ ] All tests pass (frontend-specific)
- [ ] Native syntax examples verified

### Phase 3 Complete When
- [ ] All 13 backends accept all operator types
- [ ] Assembly output correct for each operator/CPU combination
- [ ] Temp allocation correct
- [ ] All tests pass (backend-specific)

### Phase 4 Complete When
- [ ] Optimizers preserve unknown nodes
- [ ] Constant folding works
- [ ] Strength reduction implemented
- [ ] All tests pass

### Overall Complete When
- [ ] Coverage audit shows 17/17 for all operators
- [ ] No regressions in existing tests
- [ ] Documentation updated
- [ ] Ready for 1.0 release

---

## Timeline & Effort

| Phase | Duration | Effort | Risk |
|-------|----------|--------|------|
| Phase 1: AST Layer | 2 weeks | HIGH | HIGH |
| Phase 2: Frontends | 6 weeks | CRITICAL | MEDIUM |
| Phase 3: Backends | 2 weeks | HIGH | MEDIUM |
| Phase 4: Optimizers | 2 weeks | MEDIUM | LOW |
| Phase 5: Testing | 2 weeks | MEDIUM | LOW |
| Phase 6: Burgermistress | Parallel | MEDIUM | LOW |
| **TOTAL** | **14 weeks** | **CRITICAL** | **MEDIUM** |

---

## Next Steps

**IMMEDIATE (This Week)**
1. User confirms this plan
2. Spawn worker agent for Phase 1 (AST layer redesign)
3. Begin operator node constructor implementation
4. Create comprehensive operator test suite

**WEEK 2**
5. Complete AST layer changes
6. Begin frontend updates (COBOL first)
7. Verify backward compatibility

**WEEKS 3-8**
8. Systematic frontend updates (all 17)
9. Continuous integration and testing

**WEEKS 9-12**
10. Backend code generation
11. Optimizer updates
12. Burgermistress completion (parallel)

**WEEKS 13-14**
13. Comprehensive testing
14. Regression detection
15. Performance validation
16. Release readiness

---

## Questions for User

Before proceeding, confirm:

1. **Is this redesign authorized?** ✅ (YES from user)
2. **Timeline acceptable** (14 weeks to production-ready)?
3. **Backward compatibility required** (bridge old :add/:subtract nodes)?
4. **Priority order** (frontends: COBOL → Lua → SCI/Lingo/Pascal → others)?
5. **Burgermistress effort** (how complete must Prolog be before 1.0)?

