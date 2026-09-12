# EIGHTBOL COMPREHENSIVE TEST PLAN

**Version:** 1.0  
**Last Updated:** 2026-09-09  
**Status:** In Development

---

## TABLE OF CONTENTS

1. [Backend Test Implementation Framework](#backend-test-framework)
2. [Copybook Generation Test Specifications](#copybook-tests)
3. [Optimizer Test Specifications](#optimizer-tests)
4. [Command-Line Interface Tests](#cli-tests)
5. [Integration Test Scenarios](#integration-tests)
6. [Edge Cases & Error Handling](#edge-cases)
7. [Performance Benchmarks](#performance)
8. [Test Execution Environment](#execution-environment)

---

## BACKEND TEST FRAMEWORK

### Overview

Each backend (6502, 65c02, 65c816, HuC6280, RP2A03, cp1610, z80, SM83, m68k, i286, ARM7, F8, stack VM) requires systematic testing of AST-to-assembly code generation.

### Test Organization

```
tests/backends/backend-{name}-tests/
├── arithmetic-node-tests.lisp        (6 tests)
├── call-invoke-tests.lisp            (5 tests)
├── control-flow-tests.lisp           (5 tests)
├── integration-tests.lisp            (4 tests)
├── move-node-tests.lisp              (4 tests)
├── special-nodes-tests.lisp          (4-5 tests)
├── string-operations-tests.lisp      (4 tests)
├── package.lisp                      (suite definitions)
└── IMPLEMENTATION_PLAN.md            (this document)
```

### Test Harness Architecture

```lisp
(defun test-backend-codegen (ast backend &key expected-patterns error-patterns)
  "Generate assembly from AST and validate against expected output.
   
   Parameters:
   - AST: EIGHTBOL AST node (from parsing)
   - BACKEND: Keyword (e.g., :6502, :z80)
   - EXPECTED-PATTERNS: List of regex patterns to match in output
   - ERROR-PATTERNS: List of patterns that must NOT appear
   
   Returns: T if all validations pass, else throws test failure")
```

### Standard Test Template

```lisp
(defun make-backend-move-test (backend from-type to-type)
  "Generate test for MOVE operation on BACKEND.
   
   Example:
   (test 6502-move-immediate-to-register
     \"MOVE: Generate correct 6502 LDA #immediate\"
     (let ((ast (make-move-node (make-literal 42) (make-register \"A\")))
           (result (compile-ast-to-backend ast :6502)))
       (is (scan \"LDA\\s+#42\" result) \"Should emit: LDA #42\")
       (is (not (scan \"STA\" result)) \"Should not use STA for input\"))))
```

### 6502 Backend: Detailed Test Plan

**Target:** 100% coverage of 6502 instruction subset used by EIGHTBOL

#### A. Move Operations (4 tests)

**Test 6502_move_1: Register-to-Register (LDA, STA)**
```
Input AST:  (move-node (reg A) (reg X))
Expected:   "LDA... STA..."
Validates:  - Correct source register (A)
            - Correct destination register (X)
            - No spurious operations
Status:     PENDING (template exists, needs implementation)
```

**Test 6502_move_2: Memory-to-Register (LDA $addr)**
```
Input AST:  (move-node (memory \"HP\") (reg A))
Expected:   "LDA $HpAddr"
Validates:  - Correct memory address resolution
            - Addressing mode correct
            - Register preserved after operation
Status:     PENDING
```

**Test 6502_move_3: Immediate-to-Register (LDA #imm)**
```
Input AST:  (move-node (literal 42) (reg A))
Expected:   "LDA #42"
Validates:  - Immediate value correctly embedded
            - No address resolution
Status:     PENDING
```

**Test 6502_move_4: Multi-byte Move (16-bit values)**
```
Input AST:  (move-node (literal 10000) (memory \"X\"))
Expected:   "LDA #... STA $X"
            "LDA #... STA $X+1"
Validates:  - Correct byte ordering (little-endian on 6502)
            - Both bytes transferred
            - Address calculation
Status:     PENDING
```

#### B. Arithmetic Operations (6 tests)

**Test 6502_arith_1: Simple Addition (CLC, ADC)**
```
Input AST:  (add (reg A) (literal 5))
Expected:   "CLC", "ADC #5"
Validates:  - Carry flag cleared before addition
            - Correct operand
            - Result in accumulator
Status:     PENDING
```

**Test 6502_arith_2: Subtraction (SEC, SBC)**
```
Input AST:  (subtract (reg A) (literal 3))
Expected:   "SEC", "SBC #3"
Validates:  - Carry set before subtraction
            - Correct subtraction (inverted carry logic)
Status:     PENDING
```

**Test 6502_arith_3: Comparison (CMP)**
```
Input AST:  (compare (reg A) (literal 10))
Expected:   "CMP #10", flags set
Validates:  - Comparison preserves accumulator
            - Flags correctly indicate result
            - Zero flag set if A == 10
Status:     PENDING
```

**Test 6502_arith_4: Logical AND**
```
Input AST:  (and (reg A) (literal 0x0F))
Expected:   "AND #$0F"
Validates:  - Bitwise AND
            - Correct mask value
Status:     PENDING
```

**Test 6502_arith_5: Logical OR**
```
Input AST:  (or (reg A) (literal 0x80))
Expected:   "ORA #$80"
Validates:  - Bitwise OR
            - Correct value combined
Status:     PENDING
```

**Test 6502_arith_6: Bit Shift Operations (LSR, ASL)**
```
Input AST:  (shift-left (reg A) 2)
Expected:   "ASL", "ASL"  (repeated for shift count)
Validates:  - Multiple shifts generated for shift count > 1
            - Direction correct (left vs right)
Status:     PENDING
```

#### C. Call & Invoke Operations (5 tests)

**Test 6502_call_1: Local Nullary Call (JSR)**
```
Input AST:  (call :target \"MyRoutine\")
Expected:   "JSR MyRoutine"
Validates:  - Correct subroutine name
            - Return address on stack
Status:     PENDING
```

**Test 6502_call_2: Local Unary Call (parameter passing)**
```
Input AST:  (call-acc :target \"Calculate\" :using (literal 42))
Expected:   "LDA #42", "JSR Calculate"
Validates:  - Parameter correctly placed in accumulator
            - Subroutine called after parameter setup
Status:     PENDING
```

**Test 6502_call_3: Method Invoke (INVOKE)**
```
Input AST:  (invoke :object \"Self\" :method \"Think\")
Expected:   "JSR MethodSelfThink"
Validates:  - Correct mangled name
            - Object reference (if needed)
Status:     PENDING
```

**Test 6502_call_4: Library Call**
```
Input AST:  (call :target \"StrLen\" :library t)
Expected:   "JSR StrLen"
Validates:  - Library function located correctly
            - No object reference needed
Status:     PENDING
```

**Test 6502_call_5: Return Value Handling (RTS, value in A)**
```
Input AST:  (call :target \"GetValue\" :expecting-return t)
Expected:   "JSR GetValue"
Validates:  - Return value extracted from accumulator
            - Caller code uses returned value correctly
Status:     PENDING
```

#### D. Control Flow Operations (5 tests)

**Test 6502_ctrl_1: IF/THEN/ELSE (BEQ, BNE, JMP)**
```
Input AST:  (if (compare A 0) (move 1 X) (move 2 X))
Expected:   "CMP #0", "BNE Else_Label", "LDA #1", "JMP After_Label", 
            "Else_Label: LDA #2", "After_Label:"
Validates:  - Condition evaluated
            - Branches correctly labeled
            - No fall-through execution
Status:     PENDING
```

**Test 6502_ctrl_2: Simple PERFORM (JSR to label)**
```
Input AST:  (perform \"UpdateScreen\")
Expected:   "JSR UpdateScreen"
Validates:  - Correct label target
Status:     PENDING
```

**Test 6502_ctrl_3: PERFORM VARYING (loop)**
```
Input AST:  (perform-varying \"i\" 1 10 (move i x))
Expected:   "LDA #1", loop with CMP, increment, BNE
Validates:  - Loop counter initialized
            - Comparison at each iteration
            - Counter incremented
            - Branch to loop start
Status:     PENDING
```

**Test 6502_ctrl_4: GOTO (unconditional JMP)**
```
Input AST:  (goto \"label-name\")
Expected:   "JMP LabelName"
Validates:  - Correct label name conversion
            - No condition checking
Status:     PENDING
```

**Test 6502_ctrl_5: Tail Call Optimization (no JSR/RTS pair)**
```
Input AST:  (call :target \"LastRoutine\" :tail-call t)
Expected:   "JMP LastRoutine"  (NOT "JSR ... RTS")
Validates:  - JSR not used for tail call
            - Direct jump to subroutine
            - Reuses caller's return address
Status:     PENDING
```

#### E. String Operations (4 tests)

**Test 6502_string_1: String MOVE (BLT - Block Transfer)**
```
Input AST:  (move-string (ref \"SourceStr\") (ref \"DestStr\") 20)
Expected:   "LDA... STA... LDX... (loop with DEX, BNE)"
Validates:  - Source address loaded
            - Destination address loaded
            - Byte count set
            - Loop copies bytes
Status:     PENDING
```

**Test 6502_string_2: STRING Concatenation**
```
Input AST:  (concatenate-string \"Hello\" \" World\")
Expected:   Assembly that combines strings
Validates:  - First string copied
            - Second string appended
            - Total length correct
Status:     PENDING
```

**Test 6502_string_3: UNSTRING Parsing**
```
Input AST:  (unstring \"Name-Value\" :delimiter \"-\" :into (name value))
Expected:   Assembly that splits string at delimiter
Validates:  - Delimiter detection
            - Substring extraction
            - Correct field assignment
Status:     PENDING
```

**Test 6502_string_4: Character Inspection (INSPECT)**
```
Input AST:  (inspect \"MyString\" :replacing '((\"a\" \"@\")))
Expected:   Assembly loop that replaces 'a' with '@'
Validates:  - Each character compared
            - Matching characters replaced
            - Other characters unchanged
Status:     PENDING
```

#### F. Special Nodes (4-5 tests)

**Test 6502_special_1: DEBUG BREAK**
```
Input AST:  (debug-break :message \"Debug point\")
Expected:   Comment or special marker
Validates:  - Debug output generated
            - Program continues after
Status:     PENDING
```

**Test 6502_special_2: INSPECT TALLYING**
```
Input AST:  (inspect \"String\" :tallying '((\"a\" count)))
Expected:   Assembly loop that counts occurrences
Validates:  - Loop over string
            - Counter incremented for matches
            - Final count available
Status:     PENDING
```

**Test 6502_special_3: EVALUATE/WHEN**
```
Input AST:  (evaluate x :when 1 (move 100 y) :when 2 (move 200 y))
Expected:   "CMP #1", branch to case 1, "CMP #2", branch to case 2, default
Validates:  - Multiple conditions tested
            - Correct branch taken
            - Default case if no match
Status:     PENDING
```

**Test 6502_special_4: Exit Program (GOBACK)**
```
Input AST:  (goback)
Expected:   "RTS"  (at top level) or runtime exit
Validates:  - Program terminates correctly
Status:     PENDING
```

**Test 6502_special_5: Exit Method**
```
Input AST:  (exit-method)
Expected:   "RTS"  (within method context)
Validates:  - Method returns to caller
Status:     PENDING
```

#### G. Integration Tests (4 tests)

**Test 6502_integ_1: Character.Think() Method**
```
Input AST:  Full "Think" method from Character.cob
Expected:   Valid 6502 assembly with correct structure
Validates:  - Real-world compilation works
            - All node types cooperate
Status:     PENDING
```

**Test 6502_integ_2: Loop with Conditionals**
```
Input AST:  Loop that increments counter, checks condition
Expected:   Assembly with correct flow
Validates:  - Complex control flow
            - Counter management
Status:     PENDING
```

**Test 6502_integ_3: Method Invocation Chain**
```
Input AST:  Method A calls B calls C
Expected:   Correct JSR sequencing
Validates:  - Call stack management
            - Return addresses
Status:     PENDING
```

**Test 6502_integ_4: String Manipulation Sequence**
```
Input AST:  String MOVE, concatenation, inspection
Expected:   Correct assembly for sequence
Validates:  - String state maintained across operations
Status:     PENDING
```

---

## COPYBOOK GENERATION TESTS

### Purpose

Validate that EIGHTBOL correctly generates COBOL copybooks for class definitions and platform globals.

### Test Structure

**File:** `tests/copybook-generation-tests.lisp`  
**Coverage:** 46 tests across 7 categories

### Category A: Name Transformations (5 tests)

**Test 1: PascalCase to UPPERCASE-HYPHENATED**
```
Input:    "TestClass"
Output:   "TEST-CLASS"
Validates: Basic case conversion and hyphenation
Status:   PENDING (0%)
```

**Test 2: Multiple Consecutive Capitals**
```
Input:    "URLParser"
Output:   "URL-PARSER"
Validates: Correct hyphen insertion at cap transitions
Status:   PENDING (0%)
```

**Test 3: Single Word**
```
Input:    "Test"
Output:   "TEST"
Validates: Single word handling
Status:   PENDING (0%)
```

**Test 4: Underscores**
```
Input:    "Test_Class"
Output:   "TEST-CLASS"
Validates: Underscore to hyphen conversion
Status:   PENDING (0%)
```

**Test 5: Numbers**
```
Input:    "Test2Class"
Output:   "TEST2-CLASS"
Validates: Number boundary handling
Status:   PENDING (0%)
```

### Category B: Comment Detection (3 tests)

**Test 6: Lisp-style Comments**
```
Input:    "; This is a comment"
Validates: Returns true for comment
Status:   PENDING (0%)
```

**Test 7: COBOL-style Comments**
```
Input:    "* This is a comment"
Validates: Returns true for comment
Status:   PENDING (0%)
```

**Test 8: Non-comment Content**
```
Input:    "  05 Variable PIC 9(4)."
Validates: Returns false (not a comment)
Status:   PENDING (0%)
```

### Category C: Annotation Parsing (5 tests)

**Test 9: Object Reference Annotation**
```
Input:    "@ClassName"
Expected: (:object-ref "ClassName")
Status:   PENDING (0%)
```

**Test 10: PIC Notation**
```
Input:    "= PIC X(20)"
Expected: (:pic "PIC X(20)")
Status:   PENDING (0%)
```

**Test 11: VARCHAR with Dependency**
```
Input:    "= VARCHAR(n) DEPENDING ON SizeField"
Expected: (:varchar n size-field)
Status:   PENDING (0%)
```

**Test 12: Nil Case (plain comment)**
```
Input:    "Just a comment"
Expected: nil
Status:   PENDING (0%)
```

**Test 13: Invalid Format**
```
Input:    "= INVALID FORMAT"
Expected: Error or nil
Status:   PENDING (0%)
```

### Category D: Assembly Line Parsing (12 tests)

**Test 14: .byte Directive**
```
Input:    "Label: .byte $42, $FF"
Expected: (:byte 66 255)
Status:   PENDING (0%)
```

**Test 15: .word Directive (16-bit)**
```
Input:    "Data: .word $1234, $5678"
Expected: (:word 4660 22136)
Status:   PENDING (0%)
```

**Test 16: .fill Directive**
```
Input:    "Pad: .fill 8, $00"
Expected: (:fill 8 0)
Status:   PENDING (0%)
```

**Test 17: .const Directive**
```
Input:    "Constant: .const $42"
Expected: (:const 66)
Status:   PENDING (0%)
```

**Test 18: .align Directive**
```
Input:    ".align 256"
Expected: (:align 256)
Status:   PENDING (0%)
```

**Test 19: Label Recognition**
```
Input:    "MyLabel:"
Expected: (:label "MyLabel")
Status:   PENDING (0%)
```

**Test 20: Comment Lines**
```
Input:    "; .byte comment should not parse as directive"
Expected: nil (ignored)
Status:   PENDING (0%)
```

**Test 21: Whitespace-only Lines**
```
Input:    "    "
Expected: nil (ignored)
Status:   PENDING (0%)
```

**Test 22: Invalid Directive Format**
```
Input:    ".invalid bad format"
Expected: Error or nil
Status:   PENDING (0%)
```

**Test 23: Mixed Content**
```
Input:    "Label: .byte $42 ; comment"
Expected: (:byte 66) with comment stripped
Status:   PENDING (0%)
```

**Test 24: Hex vs Decimal**
```
Input:    ".byte 42, $FF"
Expected: Correct parsing of both formats
Status:   PENDING (0%)
```

**Test 25: Large Values**
```
Input:    ".word 65535"
Expected: (:word 65535)
Status:   PENDING (0%)
```

### Category E: Roundtrip Integration (8 tests)

**Test 26: Parse real Character-Slots.cpy**
```
Status:   PENDING (0%)
```

**Test 27: Parse real Phantasia-Globals.cpy**
```
Status:   PENDING (0%)
```

**Test 28: Generate and re-parse**
```
Process: Generate copybook → parse it back
Expected: Identical structure
Status:   PENDING (0%)
```

**Test 29: Validate name transformations in real files**
```
Status:   PENDING (0%)
```

**Test 30: Error detection (malformed file)**
```
Status:   PENDING (0%)
```

**Test 31: Performance (large copybook)**
```
Status:   PENDING (0%)
```

**Test 32: Character encoding (special chars)**
```
Status:   PENDING (0%)
```

**Test 33: Platform variations (7800 vs future platforms)**
```
Status:   PENDING (0%)
```

---

## OPTIMIZER TEST SPECIFICATIONS

### Overview

Optimizers transform AST nodes while preserving program semantics.

### Test Categories

#### A. Constant Folding (4-5 tests per optimizer)

**Test Purpose:** Verify that compile-time constants are correctly combined

**Test 1: Simple Binary Operation**
```
Input AST:  (add (literal 2) (literal 3))
Expected:   (literal 5)  -- Folded at compile time
Validates:  - Arithmetic correctness
            - No runtime code generated
Status:     PENDING
```

**Test 2: Nested Operations**
```
Input AST:  (add (multiply (literal 2) (literal 3)) (literal 4))
Expected:   (literal 10)  -- (2*3)+4 = 10
Status:     PENDING
```

**Test 3: Type Preservation**
```
Input AST:  Constants of different numeric types
Expected:   Correct type after folding
Status:     PENDING
```

**Test 4: Non-foldable Cases**
```
Input AST:  (add (variable "X") (literal 1))
Expected:   NOT folded (variable present)
Status:     PENDING
```

**Test 5: Edge Values**
```
Input AST:  Operations with max/min values
Expected:   Correct overflow handling
Status:     PENDING
```

#### B. Strength Reduction (4-5 tests)

**Test Purpose:** Replace expensive operations with cheaper equivalents

**Test 1: Multiply by Power of 2**
```
Input AST:  (multiply (variable "X") (literal 8))
Expected:   (shift-left (variable "X") 3)  -- Shift is cheaper
Status:     PENDING
```

**Test 2: Divide by Power of 2**
```
Input AST:  (divide (variable "X") (literal 4))
Expected:   (shift-right (variable "X") 2)
Status:     PENDING
```

**Test 3: Multiply by 1 (elimination)**
```
Input AST:  (multiply (variable "X") (literal 1))
Expected:   (variable "X")  -- Removed entirely
Status:     PENDING
```

**Test 4: Addition vs Shifting**
```
Input AST:  (add (variable "X") (variable "X"))
Expected:   (shift-left (variable "X") 1)
Status:     PENDING
```

**Test 5: Non-reducible Operations**
```
Input AST:  (multiply (variable "X") (variable "Y"))
Expected:   NOT reduced (both variables)
Status:     PENDING
```

#### C. Common Subexpression Elimination (4-5 tests)

**Test Purpose:** Avoid recomputing identical expressions

**Test 1: Identical Expressions**
```
Input AST:  Two (add (var X) (literal 1)) nodes
Expected:   Computed once, result reused
Status:     PENDING
```

**Test 2: Expression Chains**
```
Input AST:  Multiple uses of same complex expression
Expected:   Single computation, multiple uses
Status:     PENDING
```

**Test 3: Non-identical Expressions**
```
Input AST:  Similar but different expressions
Expected:   NOT eliminated (not identical)
Status:     PENDING
```

**Test 4: Side Effects**
```
Input AST:  Expressions with side effects (calls)
Expected:   NOT eliminated (order matters)
Status:     PENDING
```

**Test 5: Loop-Invariant Code Motion**
```
Input AST:  Expression constant within loop
Expected:   Moved outside loop
Status:     PENDING
```

#### D. Loop Unrolling (4-5 tests)

**Test Purpose:** Reduce loop overhead for small loops

**Test 1: Fixed Iteration Count**
```
Input AST:  (perform-varying i 1 3 body)  -- 3 iterations
Expected:   body, body, body (unrolled)
Status:     PENDING
```

**Test 2: Unrolling Factor**
```
Input AST:  Loop with 8 iterations
Expected:   Unrolled by factor (4x or similar)
Status:     PENDING
```

**Test 3: Loop Too Large**
```
Input AST:  Loop with 100+ iterations
Expected:   NOT unrolled (too large)
Status:     PENDING
```

**Test 4: Variable Loop Count**
```
Input AST:  (perform-varying i start end body)  -- Runtime count
Expected:   NOT unrolled (count unknown)
Status:     PENDING
```

**Test 5: Nested Loops**
```
Input AST:  Unrollable inner loop in larger loop
Expected:   Inner loop unrolled selectively
Status:     PENDING
```

#### E. Register Allocation (4-5 tests)

**Test Purpose:** Optimize variable-to-register assignment

**Test 1: Hot Variables**
```
Input AST:  Frequently used variables
Expected:   Assigned to registers (not memory)
Status:     PENDING
```

**Test 2: Register Pressure**
```
Input AST:  More variables than available registers
Expected:   Optimal assignment considering usage
Status:     PENDING
```

**Test 3: Callee-Save Register Use**
```
Input AST:  Cross-function variable usage
Expected:   Callee-save registers used appropriately
Status:     PENDING
```

**Test 4: Spilling**
```
Input AST:  Pressure exceeds registers
Expected:   Automatic spilling to stack
Status:     PENDING
```

**Test 5: Backend-Specific Registers**
```
Input AST:  Different backends with different register sets
Expected:   Correct allocation per backend
Status:     PENDING
```

---

## COMMAND-LINE INTERFACE TESTS

### Basic Shell Tests (10 tests)

**File:** `tests/other/basic-shell-tests.lisp`

### Copybook Command Tests (10 tests)

**File:** `tests/other/cobol-copybook-tests.lisp`

### General Command-Line Tests (10 tests)

**File:** `tests/other/command-line-tests.lisp`

**Test Categories:**
- Help output validation
- Version display
- File argument processing
- Error handling
- Exit codes
- Output formatting
- Configuration file parsing
- Platform selection
- Optimization levels
- Debug output

---

## INTEGRATION TEST SCENARIOS

### Scenario 1: Character.Think() Full Compilation

**Objective:** Compile complete Character class method across all backends

**Steps:**
1. Parse Character.cob (or minimal equivalent)
2. Generate AST
3. For each backend:
   - Compile to assembly
   - Validate assembly syntax
   - Verify expected patterns present

**Expected Outcomes:**
- All backends compile successfully
- Assembly contains required instructions
- No spurious code generated

---

### Scenario 2: Cross-Platform Numeric Computation

**Objective:** Verify numeric precision maintained across platforms

**Steps:**
1. Define numeric operations (ADD, SUBTRACT, MULTIPLY, DIVIDE)
2. Compile for different numeric types (BINARY, DECIMAL, BCD)
3. For each backend:
   - Generate assembly
   - Execute or simulate
   - Verify results

**Expected Outcomes:**
- Correct numeric precision
- Proper handling of overflow/underflow
- Platform-specific behaviors validated

---

### Scenario 3: Method Call Chain

**Objective:** Validate call stack and return value propagation

**Steps:**
1. Define 3 methods: A → B → C
2. Each returns a modified value
3. Compile full call chain
4. Validate assembly structure

**Expected Outcomes:**
- Return addresses correct
- Values passed correctly
- Return values propagated back

---

## EDGE CASES & ERROR HANDLING

### Boundary Conditions

1. **Zero-length strings**
2. **Maximum field width (PIC 9(18))**
3. **Minimum field width (PIC 9)**
4. **Empty program (no procedures)**
5. **Single-statement method**
6. **Deeply nested conditionals (10+ levels)**

### Error Cases

1. **Undefined variable references**
2. **Type mismatches**
3. **Invalid operand combinations**
4. **Stack overflow in deeply nested calls**
5. **Recursive method definitions**
6. **Circular method invocations**

---

## PERFORMANCE BENCHMARKS

### Compilation Speed

| Metric | Target | Status |
|--------|--------|--------|
| Simple method | <100ms | PENDING |
| Complex method (200 lines) | <1s | PENDING |
| Full Character class | <5s | PENDING |
| 10 parallel compilations | <6s | PENDING |

### Generated Code Size

| Program | Target Size | Status |
|---------|------------|--------|
| Minimal method | <200 bytes | PENDING |
| Complex method | <2KB | PENDING |
| Full Character | <10KB | PENDING |

### Optimization Speedup

| Optimizer | Target Speedup | Status |
|-----------|----------------|--------|
| Constant Folding | 5-10% | PENDING |
| Strength Reduction | 3-8% | PENDING |
| CSE | 2-5% | PENDING |
| Loop Unrolling | 10-20% (on loops) | PENDING |
| Register Allocation | 5-15% | PENDING |

---

## TEST EXECUTION ENVIRONMENT

### System Requirements

- SBCL (Steel Bank Common Lisp) >= 2.1.0
- Common Lisp: ANSI compliant
- FiveAM test framework
- File I/O permissions for temp files

### Test Harness Functions

```lisp
(defun run-all-backend-tests ()
  "Execute all 1,000+ backend tests across all architectures.")

(defun run-backend-tests (backend)
  "Execute tests for specific backend (e.g., :6502).")

(defun run-copybook-tests ()
  "Execute 46 copybook generation tests.")

(defun run-optimizer-tests ()
  "Execute 62 optimizer validation tests.")

(defun generate-coverage-report ()
  "Generate test coverage metrics and HTML report.")
```

### Continuous Integration

- Run on every commit
- Generate coverage reports
- Fail build if any tests skip without reason
- Track performance metrics

---

## ACCEPTANCE CRITERIA

### Phase 1 (Audit)
- [ ] All 1,103 pending tests catalogued
- [ ] Skip reasons documented
- [ ] Blocking dependencies identified
- [ ] 0 generic "Implementation pending" reasons

### Phase 2 (Copybooks)
- [ ] 46/46 copybook tests implemented
- [ ] 90%+ code coverage for copybook generation
- [ ] All name transformations validated

### Phase 3 (Backends)
- [ ] 1,036/1,036 backend template tests implemented
- [ ] 80%+ code coverage per backend
- [ ] All backends compile test methods correctly

### Phase 4 (Optimization)
- [ ] 62/62 optimizer tests pass
- [ ] Each optimizer measurably improves output
- [ ] No regression on non-optimized code

### Final
- [ ] <10 pending tests remaining (all justified)
- [ ] 95%+ pass rate
- [ ] All coverage targets met
- [ ] Performance within benchmarks

---

**Document Version:** 1.0  
**Last Updated:** 2026-09-09  
**Owner:** EIGHTBOL QA Team  
**Requires Approval:** Project Lead  
