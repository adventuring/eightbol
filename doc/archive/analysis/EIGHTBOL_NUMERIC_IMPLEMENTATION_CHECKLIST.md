# EIGHTBOL Numeric Types Implementation Checklist

## PART 1: CRITICAL PATH (Must Complete First)

### Phase 1A: Specification & Documentation (Week 1)
- [ ] **Formalize BCD Format Specification**
  - [ ] Define canonical nybble layout (high/low order)
  - [ ] Document padding rules (single digit, odd nybbles)
  - [ ] Specify sign representation (0x0F vs 0x0C, etc.)
  - [ ] Create reference implementation (Lisp)
  - [ ] Document per-platform character codes

- [ ] **Formalize Fixed-Point Scaling Rules**
  - [ ] Scale matching semantics for ADD/SUBTRACT
  - [ ] Widening vs. truncation rules
  - [ ] Fractional bit tracking in *pic-frac-bits-table*
  - [ ] MULTIPLY/DIVIDE scaling behavior

- [ ] **Define DISPLAY Format Specification**
  - [ ] Sign character handling (+/-)
  - [ ] Decimal point placement
  - [ ] Padding/alignment rules
  - [ ] Platform-specific character codes (ASCII, PETSCII, etc.)

- [ ] **Create Comprehensive Test Suite**
  - [ ] Test matrix (all numeric types × all widths × all operations)
  - [ ] Backend-specific tests
  - [ ] Regression test suite
  - [ ] Interoperability tests (BINARY ↔ DECIMAL)

**Effort: 20-25 hours | Owner: Lead architect**

---

### Phase 1B: Fix 6502 Blocking Issues (Week 1-2)
- [ ] **Fix BCD 2-Byte In-Place SUBTRACT**
  - [ ] File: `backend-6502-part1.lisp:118-142`
  - [ ] Remove compile error block (line 123-126)
  - [ ] Implement software BCD subtraction
  - [ ] Test with both positive and negative operands
  - [ ] Add to regression suite

- [ ] **Fix Misaligned PIC Decimal Scaling**
  - [ ] File: `backend-6502-part5.lisp:246, 299`
  - [ ] Remove compile error blocks
  - [ ] Implement scale widening logic
  - [ ] Handle bit-shift sequences for alignment
  - [ ] Test mixed-scale ADD/SUBTRACT operations

- [ ] **Create 6502 Numeric Test Harness**
  - [ ] Unit tests for each numeric type
  - [ ] Integration tests with real EIGHTBOL programs
  - [ ] Coverage for all identified gaps

**Effort: 10-15 hours | Owner: 6502 specialist**

---

## PART 2: 6502 FAMILY COMPLETION (Weeks 2-7)

### Phase 2A: MULTIPLY Implementation (Weeks 2-4)
- [ ] **8-bit MULTIPLY (UNSIGNED)**
  - [ ] File: Create `backend-6502-multiply.lisp`
  - [ ] Algorithm: Shift-and-add method
  - [ ] Emit code for unsigned 8×8 → 16 bit
  - [ ] Test cases: 0×0, 255×255, 10×20, etc.
  - [ ] Verify result precision

- [ ] **16-bit MULTIPLY (UNSIGNED)**
  - [ ] Emit code for 16×16 → 32 bit
  - [ ] Handle carry propagation correctly
  - [ ] Optimize for common cases (power of 2, small constants)
  - [ ] Test: 0×0, 65535×65535, 256×256, etc.

- [ ] **Signed MULTIPLY**
  - [ ] Pre-compute signs via BIT/BMI
  - [ ] Convert operands to unsigned
  - [ ] Perform unsigned MULTIPLY
  - [ ] Restore sign on result (NEG if needed)
  - [ ] Test negative × positive, negative × negative

- [ ] **BCD MULTIPLY**
  - [ ] Document: BCD multiplication not supported (compile error OK)
  - [ ] Alternative: User must convert to BINARY first
  - [ ] Add documentation to AGENTS.md

**Effort: 15-20 hours | Owner: 6502 specialist**

---

### Phase 2B: DIVIDE Implementation (Weeks 4-5)
- [ ] **8-bit DIVIDE (UNSIGNED)**
  - [ ] File: Create `backend-6502-divide.lisp`
  - [ ] Algorithm: Bit-by-bit long division
  - [ ] Emit code for 16 ÷ 8 → quotient + remainder
  - [ ] Handle division by zero (error or trap?)
  - [ ] Test: 100÷5, 255÷1, 7÷2, etc.

- [ ] **16-bit DIVIDE (UNSIGNED)**
  - [ ] Emit code for 32 ÷ 16 → 16-bit quotient + remainder
  - [ ] Handle multi-bit shifts correctly
  - [ ] Optimize hot paths
  - [ ] Test: 65535÷1, 65535÷256, 10000÷100, etc.

- [ ] **Signed DIVIDE**
  - [ ] Sign handling (dividend/divisor)
  - [ ] Convert to unsigned, divide, restore sign
  - [ ] Test negative ÷ positive, negative ÷ negative

- [ ] **Fixed-Point DIVIDE**
  - [ ] Define semantics (how to handle fractional results?)
  - [ ] Scaling rules (e.g., 1.5 ÷ 0.5 = 3.0)
  - [ ] Rounding/truncation behavior

**Effort: 20-25 hours | Owner: 6502 specialist**

---

### Phase 2C: BCD Conversion (Weeks 6-7)
- [ ] **BCD → BINARY Conversion**
  - [ ] File: Create `backend-6502-bcd-to-binary.lisp`
  - [ ] Unpack nybbles from BCD storage
  - [ ] Compute positional values (10^n)
  - [ ] Accumulate into binary result
  - [ ] Handle multi-byte BCD (up to 8 nybbles)
  - [ ] Test: 1→0x01, 99→0x63, 9999→0x270F, etc.

- [ ] **BINARY → BCD Conversion**
  - [ ] File: Create `backend-6502-binary-to-bcd.lisp`
  - [ ] Algorithm: Repeated division by 10
  - [ ] Extract digit, store as nybble
  - [ ] Continue with quotient
  - [ ] Handle multi-byte results
  - [ ] Test: 0x01→1, 0x63→99, 0x270F→9999, etc.

- [ ] **Signed BCD Conversion**
  - [ ] Handle sign bit in BCD format
  - [ ] Preserve sign through conversion
  - [ ] Test: -99 → BCD, BCD → -99

**Effort: 15-20 hours | Owner: 6502 specialist**

---

### Phase 2D: DISPLAY Number Support (Optional for 6502)
- [ ] **DISPLAY → BINARY Conversion**
  - [ ] Parse character string
  - [ ] Extract sign, digits, decimal point
  - [ ] Convert to binary value
  - [ ] Test: "+1234" → 0x04D2, "-99.99" → scaled value, etc.

- [ ] **BINARY → DISPLAY Conversion**
  - [ ] Format as character string
  - [ ] Insert sign character, decimal point
  - [ ] Handle padding/alignment
  - [ ] Test: 0x04D2 → "+1234", etc.

**Effort: 15-20 hours (DEFERRED to later) | Owner: 6502 specialist**

---

## PART 3: 6502 FAMILY VARIANTS (Weeks 3-4, parallel with Phase 2)

### 65C02 & HUC6280 Optimizations
- [ ] **STZ Instruction Usage**
  - [ ] Replace LDA#0/STA with STZ where appropriate
  - [ ] Test for code size reduction

- [ ] **BRA Instruction Optimization**
  - [ ] Use BRA for local branches (already done)
  - [ ] Verify correct usage

**Effort: 2-5 hours | Owner: Junior engineer**

---

### 65C816 Enhancements (FUTURE)
- [ ] **16-bit Native Mode Support**
  - [ ] REP/SEP instruction mapping
  - [ ] 16-bit A register operations
  - [ ] Optimization for 16-bit native

**Effort: 5-10 hours (DEFERRED) | Owner: TBD**

---

## PART 4: NON-6502 BACKENDS (Weeks 8-16, Parallel)

### Z80 Implementation Track

#### Phase 4Z1: BCD Conversion (Hours 1-8)
- [ ] **BCD → BINARY**
  - [ ] File: `backend-z80.lisp` (extend existing 1203 lines)
  - [ ] Unpack nybbles (BCD bytes contain two digits each)
  - [ ] Compute positional values
  - [ ] Use HL, BC, DE register pairs for 16-bit values
  - [ ] Test multi-byte conversion

- [ ] **BINARY → BCD**
  - [ ] Repeated division by 10
  - [ ] Store results as nybbles
  - [ ] Pack nybbles into bytes

#### Phase 4Z2: BCD Arithmetic (Hours 8-14)
- [ ] **BCD ADD**
  - [ ] No hardware ADC/DAA on Z80
  - [ ] Manual nybble-by-nybble addition
  - [ ] BCD correction (ADD 6 if result > 9)

- [ ] **BCD SUBTRACT**
  - [ ] Nybble-by-nybble subtraction with borrow
  - [ ] BCD correction needed

#### Phase 4Z3: MULTIPLY/DIVIDE (Hours 14-20)
- [ ] 8-bit and 16-bit implementations

**Total Z80 Effort: 50-70 hours | Owner: Z80 specialist**

---

### CP1610 Implementation Track

#### Phase 4CP1: BCD Support
- [ ] **BCD Conversion for CP1610**
  - [ ] Adjust for 10-bit word size
  - [ ] Handle word-addressed memory
  - [ ] Nybble manipulation in 16-bit registers

#### Phase 4CP2: Scaling & MULTIPLY/DIVIDE
- [ ] Fixed-point scaling alignment
- [ ] MULTIPLY/DIVIDE operations

**Total CP1610 Effort: 45-65 hours | Owner: CP1610 specialist**

---

### M68K Fast-Track (Quick Wins)

#### Phase 4M1: Unblock BCD (Hours 1-2)
- [ ] **Remove BCD Operation Blocks**
  - [ ] File: `backend-m68k.lisp`
  - [ ] Remove compile error for MULTIPLY with BCD (line 173)
  - [ ] Remove compile error for DIVIDE with BCD (line 198)

#### Phase 4M2: Implement ABCD/SBCD (Hours 2-5)
- [ ] **Use native ABCD/SBCD instructions**
  - [ ] Map to EIGHTBOL BCD ADD/SUBTRACT
  - [ ] Test: BCD addition, subtraction

#### Phase 4M3: Leverage MUL/DIV (Hours 5-8)
- [ ] **Emit MULU/MULS/DIVU/DIVS**
  - [ ] Already exist; just need code emission
  - [ ] Test MULTIPLY/DIVIDE operations

#### Phase 4M4: DISPLAY & Conversions (Hours 8-15)
- [ ] BCD conversion, DISPLAY support

**Total M68K Effort: 20-30 hours | Owner: M68K specialist**

---

### I286 Fast-Track

#### Phase 4I1: Unblock & Emit MUL/DIV (Hours 1-5)
- [ ] Remove compile error blocks
- [ ] Emit IMUL, IDIV, MUL, DIV instructions

#### Phase 4I2: BCD Support (Hours 5-12)
- [ ] Map AAA, AAD instructions
- [ ] Full BCD conversion

#### Phase 4I3: DISPLAY & Completion (Hours 12-20)

**Total I286 Effort: 20-27 hours | Owner: i286 specialist**

---

### ARM7 Fast-Track

#### Phase 4A1: Unblock MUL/DIV (Hours 1-3)
- [ ] Remove compile error blocks
- [ ] Emit MUL, DIV instructions

#### Phase 4A2: BCD Support (Hours 3-10)
- [ ] Complete BCD correction sketches
- [ ] Full BCD conversion

#### Phase 4A3: DISPLAY & Completion (Hours 10-18)

**Total ARM7 Effort: 22-29 hours | Owner: ARM specialist**

---

### F8, SM83, M6800, RP2A03, STACK, FORTH (Lower Priority)

- [ ] Each backend follows similar pattern:
  1. Unblock compile errors
  2. Implement BCD (if needed)
  3. Implement MULTIPLY/DIVIDE
  4. Implement DISPLAY (if needed)
  5. Test & verify

**Collective Effort: 150-200 hours | Owner: Multiple engineers (parallel)**

---

## PART 5: QUALITY ASSURANCE

### QA Phase 1: Regression Testing (Weeks 16-17)

- [ ] **Run Existing Test Suite**
  - [ ] `(asdf:test-system :eightbol)`
  - [ ] `:backend-matrix` tests
  - [ ] `:compile-regression` tests
  - [ ] Verify no regressions

- [ ] **Extended Test Coverage**
  - [ ] `:numeric-precision-all-backends` suite
  - [ ] All 15 backends passing
  - [ ] Coverage: 100% of numeric paths

- [ ] **Interoperability Testing**
  - [ ] BINARY ↔ DECIMAL conversions
  - [ ] Cross-platform consistency
  - [ ] Edge cases (0, min, max values)

**Effort: 15-20 hours | Owner: QA engineer**

---

### QA Phase 2: Performance Benchmarking (Week 17-18)

- [ ] **Create Benchmark Suite**
  - [ ] ADD/SUBTRACT: 100K operations
  - [ ] MULTIPLY: 10K operations (slower)
  - [ ] DIVIDE: 10K operations (slower)
  - [ ] BCD conversion: 10K operations

- [ ] **Baseline Measurements**
  - [ ] Record performance on all 15 backends
  - [ ] Document execution time per operation

- [ ] **Performance Analysis**
  - [ ] Identify bottlenecks
  - [ ] Compare to expected performance
  - [ ] Flag regressions > 10%

**Effort: 10-15 hours | Owner: Performance engineer**

---

## PART 6: DOCUMENTATION & RELEASE

### Documentation Phase (Week 18-19)

- [ ] **Update AGENTS.md**
  - [ ] Document numeric type support matrix
  - [ ] List known limitations per backend
  - [ ] Performance characteristics

- [ ] **Update README.md**
  - [ ] Numeric types summary
  - [ ] Supported operations per backend

- [ ] **Create Numeric Types Guide**
  - [ ] File: `doc/NUMERIC_TYPES.texi`
  - [ ] BCD format specification
  - [ ] Fixed-point scaling rules
  - [ ] DISPLAY format specification
  - [ ] Platform-specific notes

- [ ] **Create Developer Guide**
  - [ ] File: `doc/NUMERIC_IMPLEMENTATION.texi`
  - [ ] Backend-specific numeric implementation patterns
  - [ ] Testing approach
  - [ ] Common pitfalls

**Effort: 10-15 hours | Owner: Technical writer**

---

### Release Phase (Week 19-20)

- [ ] **Create Release Notes**
  - [ ] List all fixes/features
  - [ ] Known limitations
  - [ ] Performance improvements

- [ ] **Final Testing**
  - [ ] All test suites passing
  - [ ] No regressions
  - [ ] Documentation complete

- [ ] **Release Commit**
  - [ ] Tag version (e.g., v0.9-numeric-audit-complete)
  - [ ] Push to origin/main

**Effort: 5 hours | Owner: Release manager**

---

## PART 7: DEPENDENCY GRAPH

```
Specification (20h)
    ↓
6502 Core Fixes (15h) → 6502 MULTIPLY (20h) → 6502 DIVIDE (25h) → 6502 BCD Conv (20h)
    ↓
65C02/65C816/HUC6280 (5h)
RP2A03 (40h) [depends on 6502]

Z80 Parallel Track (70h)
    ├─ BCD Conversion (8h)
    ├─ BCD Arithmetic (6h)
    └─ MULTIPLY/DIVIDE (20h)

CP1610 Parallel Track (65h)
    ├─ BCD Support (12h)
    ├─ Scaling (10h)
    └─ MULTIPLY/DIVIDE (20h)

M68K Fast-Track (30h) [Quick wins; unblock existing ops]
I286 Fast-Track (27h) [Quick wins]
ARM7 Fast-Track (29h) [Quick wins]

F8, SM83, M6800, STACK, FORTH (140h) [Lower priority; parallel]

Quality Assurance (25h)
    ├─ Regression Testing (20h)
    └─ Performance Benchmarking (15h)

Documentation (15h)
Release (5h)
```

---

## PART 8: SUCCESS CRITERIA

### Code Quality
- [ ] All code follows EIGHTBOL style guide (see best-behavior.md)
- [ ] No compiler warnings
- [ ] Proper error handling for divide-by-zero, overflow, etc.
- [ ] Comments on complex algorithms

### Test Coverage
- [ ] All 15 backends passing numeric test suite
- [ ] 100% code path coverage for numeric operations
- [ ] Interoperability tests passing
- [ ] No regressions from prior releases

### Performance
- [ ] No > 10% performance regressions on ADD/SUBTRACT
- [ ] MULTIPLY/DIVIDE performance acceptable (within 2-5× of native)
- [ ] BCD operations within 1.5-2× of binary operations

### Documentation
- [ ] Numeric types specification complete
- [ ] Per-backend implementation notes documented
- [ ] Known limitations clearly listed
- [ ] Developer guide updated

### Release
- [ ] Version tag created
- [ ] Release notes published
- [ ] All deliverables in GitHub

---

## PART 9: RESOURCE ALLOCATION

**Estimated Total Team:** 2-3 engineers + 1 QA + 1 tech writer

**Timeline:** 16-20 weeks (4-5 months) with parallel work

**Critical Path:**
1. Specification (Week 1): 1 person
2. 6502 Core (Weeks 1-7): 1 person (full-time)
3. Non-6502 Backends (Weeks 8-16): 2+ people (parallel)
4. QA & Documentation (Weeks 16-19): 2 people
5. Release (Week 19-20): 1 person

---

## APPENDIX: BACKEND-BY-BACKEND TASK BREAKDOWN

### 6502 (3,300 lines)
- [ ] Fix 2-byte BCD SUBTRACT (3h)
- [ ] Fix misaligned PIC scaling (4h)
- [ ] Implement MULTIPLY 8/16-bit (15h)
- [ ] Implement DIVIDE 8/16-bit (20h)
- [ ] BCD conversion (20h)
- [ ] Testing & verification (5h)
- **Subtotal: 67 hours**

### 65C02 (11 lines)
- [ ] STZ optimization (1h)
- [ ] BRA optimization (1h)
- [ ] Testing (1h)
- **Subtotal: 3 hours**

### 65C816 (11 lines)
- [ ] 16-bit native mode (5h)
- [ ] Testing (2h)
- **Subtotal: 7 hours**

### Z80 (1,203 lines)
- [ ] BCD conversion (8h)
- [ ] BCD arithmetic (6h)
- [ ] MULTIPLY/DIVIDE (20h)
- [ ] DISPLAY (15h)
- [ ] Testing (15h)
- **Subtotal: 64 hours**

### CP1610 (1,157 lines)
- [ ] BCD support (12h)
- [ ] Scaling fixes (10h)
- [ ] MULTIPLY/DIVIDE (20h)
- [ ] DISPLAY (15h)
- [ ] Testing (15h)
- **Subtotal: 72 hours**

### M68K (882 lines)
- [ ] Unblock & emit ABCD/SBCD (5h)
- [ ] Emit MULU/MULS/DIVU/DIVS (5h)
- [ ] BCD conversion (8h)
- [ ] DISPLAY (8h)
- [ ] Testing (5h)
- **Subtotal: 31 hours**

### I286 (914 lines)
- [ ] Unblock & emit MUL/DIV (5h)
- [ ] BCD support (7h)
- [ ] DISPLAY (8h)
- [ ] Testing (5h)
- **Subtotal: 25 hours**

### ARM7 (912 lines)
- [ ] Unblock & emit MUL/DIV (3h)
- [ ] BCD support (10h)
- [ ] DISPLAY (8h)
- [ ] Testing (5h)
- **Subtotal: 26 hours**

### F8 (1,245 lines)
- [ ] BCD operations (16h)
- [ ] MULTIPLY/DIVIDE (20h)
- [ ] DISPLAY (12h)
- [ ] Testing (10h)
- **Subtotal: 58 hours**

### Others (RP2A03, SM83, M6800, STACK, FORTH)
- [ ] RP2A03 (40h)
- [ ] SM83 (35h)
- [ ] M6800 (40h)
- [ ] STACK (18h)
- [ ] FORTH (18h)
- **Subtotal: 151 hours**

### QA, Documentation, Release
- [ ] Testing (20h)
- [ ] Benchmarking (15h)
- [ ] Documentation (15h)
- [ ] Release (5h)
- **Subtotal: 55 hours**

---

## TOTAL PROJECT ESTIMATE

| Phase           | Hours | Weeks |
|-----------------|-------|-------|
| Specification   | 20    | 1     |
| 6502 Core       | 67    | 3     |
| 6502 Variants   | 10    | 1     |
| Z80             | 64    | 3     |
| CP1610          | 72    | 3     |
| M68K/I286/ARM7  | 82    | 3     |
| F8              | 58    | 2     |
| Others          | 151   | 4     |
| QA/Docs/Release | 55    | 2     |
| **GRAND TOTAL** | **579** | **22** |

**With 2-3 engineers, parallel work: 16-20 weeks**

---

**Checklist Owner:** OpenCode Agent
**Last Updated:** 2026-09-09
**Status:** Ready for implementation sprint planning
