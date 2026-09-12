# EIGHTBOL Comprehensive Numeric Type Support Audit

## Executive Summary

This audit comprehensively evaluates numeric type support across all 15 EIGHTBOL backends, identifies current implementation status, gaps, and dependencies, and provides a detailed implementation plan for unified numeric type support.

**Key Findings:**
- 3 backends have substantial numeric implementations (6502, Z80, CP1610)
- 6 backends have basic/partial implementations (RP2A03, ARM7, F8, M68K, I286, SM83)
- 6 backends have minimal stubs/placeholder implementations (65C02, 65C816, HUC6280, M6800, STACK, FORTH)
- Numeric types supported: BINARY (primary), DECIMAL (partial/BCD), DISPLAY (minimal)
- All backends need unified approach to fixed-point arithmetic with variable scaling

---

## PART 1: NUMERIC TYPES SPECIFICATION

### 1.1 UNSIGNED BINARY (USAGE BINARY, default)

**Example:** `PIC 9999V9999` = 16.16 bits (fixed-point, unsigned)

**Characteristics:**
- Storage: Two's complement NOT used; pure binary representation
- Range: 0 to 2^n-1 where n = total bits
- Natural alignment: 8-bit (1 byte), 16-bit (2 bytes), 24-bit (3 bytes), 32-bit (4 bytes)
- Operations supported: ADD, SUBTRACT, BITWISE (AND, OR, XOR, NOT), Arithmetic shifts (ASL, ASR)
- Operations NOT supported: DIVIDE (not yet), MULTIPLY (not yet)

**Common Sizes:**
| PIC        | Width | Range         | Notes                           |
|------------|-------|---------------|---------------------------------|
| 9          | 4-bit | 0–15          | Single nybble (if nybble-semantics) |
| 99         | 8-bit | 0–255         | Single byte                     |
| 999        | 12-bit| 0–4095        | Packed into 12 bits + padding   |
| 9999       | 16-bit| 0–65535       | Two bytes, little-endian        |
| 99999999   | 32-bit| 0–4,294,967,295 | Four bytes                    |

**Fixed-Point Scaling (PIC Vn interpretation):**
- `PIC 999V99` = 5 integer digits, 2 fractional digits (stored as 7 nybbles in BCD or 24 bits in binary)
- Scaling represented in *pic-frac-bits-table* (e.g., :pic-frac-bits 8 means lowest 8 bits are fractional)
- ADD/SUBTRACT require matching scales OR automatic widening

### 1.2 SIGNED BINARY (USAGE BINARY with 'S')

**Example:** `PIC S99` = 7 bits signed + 1 sign bit

**Characteristics:**
- Storage: Two's complement representation
- Sign bit: sacrifices 1 bit (so S9 occupies 8 bits but only uses -9..+9 range logically)
- Range: -(2^(n-1)) to 2^(n-1)-1 where n = total bits
- Operations: Same as unsigned (ADD, SUBTRACT, BITWISE, shifts)
- Sign extension required on load/move operations

**Common Sizes:**
| PIC     | Storage | Range          | Notes                           |
|---------|---------|----------------|---------------------------------|
| S9      | 8-bit   | -9 to +9       | Sign bit + 7 data bits          |
| S99     | 8-bit   | -99 to +99     | Sign bit + 7 data bits          |
| S999    | 16-bit  | -999 to +999   | Sign bit + 15 data bits         |
| S9999   | 16-bit  | -9999 to +9999 | Sign bit + 15 data bits         |

**Sign Handling:**
- Negative numbers stored in two's complement
- Sign extension on 8→16 bit promotion: `lda value; bpl +; dea; .` (6502)
- Subtraction uses SBC with carry flag set for borrow handling

### 1.3 UNSIGNED DECIMAL BCD (USAGE DECIMAL, default)

**Example:** `PIC 999V999` = 3 integer digits, 3 fractional nybbles

**Characteristics:**
- Storage: Packed BCD (4 bits per digit, i.e., 1 nybble per decimal digit)
- Range: 0 to 10^n - 1 where n = decimal digit count
- Natural alignment: nybble boundaries (2 nybbles per byte when packed)
- Operations: ADD, SUBTRACT (using hardware or software BCD routines)
- Operations NOT supported: MULTIPLY, DIVIDE (not implemented for BCD)

**Common Sizes:**
| PIC      | Nybbles | Bytes (packed) | Range           | Notes              |
|----------|---------|----------------|-----------------|--------------------|
| 9        | 1       | 0.5 (padded)   | 0–9             | Single digit       |
| 99       | 2       | 1              | 0–99            | Two digits         |
| 999      | 3       | 2 (1.5+pad)    | 0–999           | Three digits       |
| 9999     | 4       | 2              | 0–9999          | Four digits        |
| 99999999 | 8       | 4              | 0–99999999      | Eight digits       |

**Padding and Storage Rules:**
- Single-digit (`PIC 9` or `PIC S9`) stored in 8 bits with upper nybble = 0x0
- Multiple digits packed: `PIC 99` = 0x?? (each ? is one nybble)
- Odd nybble counts: pad with 0x0 in high nybble

**Hardware BCD Support (CPU-specific):**
- **6502 family**: D flag + ADC/SBC (decimal mode)
- **Z80**: No native BCD; software emulation required
- **M68K**: ABCD (Add BCD) and SBCD (Subtract BCD) instructions
- **i286**: AAA, AAD instructions (ASCII adjust, partial support)
- **ARM7**: No native BCD; software emulation required
- **CP1610**: No native BCD; software emulation required

### 1.4 SIGNED DECIMAL BCD (USAGE DECIMAL with 'S')

**Example:** `PIC S999` = signed 3-digit BCD

**Characteristics:**
- Storage: BCD digits + sign indicator (usually 0x0F for negative in high nybble, 0x0C for positive)
- Sign encoding varies by platform (see CPU notes)
- Range: -(10^n - 1) to +(10^n - 1) where n = decimal digit count
- Operations: ADD, SUBTRACT with sign handling
- Special handling: sign must propagate through operations

**Sign Representation by Platform:**
- **Standard COBOL**: 0xF = negative, 0xC = positive (in sign nybble)
- **6502 implementations**: May use byte-level sign (negative = bit 7 set)
- **Other platforms**: Varies; often requires software management

**Common Sizes:**
| PIC      | Storage | Digits | Range            | Notes                      |
|----------|---------|--------|------------------|-----------------------------|
| S9       | 8-bit   | 1      | -9 to +9         | Sign + digit nybbles       |
| S99      | 8-bit   | 2      | -99 to +99       | Sign + 2 digit nybbles (padded) |
| S999     | 16-bit  | 3      | -999 to +999     | Sign + 3 digit nybbles     |
| S9999    | 16-bit  | 4      | -9999 to +9999   | Sign + 4 digit nybbles (padded) |

### 1.5 DISPLAY NUMBERS (USAGE DISPLAY or implied)

**Example:** `PIC S9999V99` as DISPLAY = 8 character string `+9999.99` or `-1234.56`

**Characteristics:**
- Storage: String of character codes (ASCII, PETSCII, ATASCII, or platform-specific)
- No arithmetic operations allowed (must convert to BINARY/DECIMAL first)
- Used for: terminal I/O, report formatting, human-readable storage
- Range: Depends on character encoding and field width
- Format: sign character + digit characters + optional decimal point

**Common Layouts:**
| PIC         | Display Example | Encoding      | Notes                      |
|-------------|-----------------|---------------|----------------------------|
| S9999V99    | `-1234.56`      | ASCII/PETSCII | 9 chars (sign, 6 digits, dot) |
| 9999        | `1234`          | ASCII/PETSCII | 4 chars (digits only)     |
| S999        | `+123`          | ASCII/PETSCII | 4 chars (sign + 3 digits) |

**Character Encoding by Platform:**
- **ASCII**: `0x30`–`0x39` for digits, `0x2B` ('+'), `0x2D` ('-'), `0x2E` ('.')
- **PETSCII** (Commodore): Screen codes for digits
- **ATASCII** (Atari): Similar to ASCII but different for some symbols
- **Mini-font** (arcade games): Custom code pages

**No-Operation Requirement:**
- DISPLAY numbers cannot participate in ADD, SUBTRACT, MULTIPLY, or DIVIDE
- Must be converted to BINARY or DECIMAL before arithmetic
- Conversion from DISPLAY → BINARY/DECIMAL requires string parsing

---

## PART 2: CURRENT STATE AUDIT BY BACKEND

### BACKEND: 6502 (MOS Technology 6502)

**Architecture:**
- 8-bit accumulator (A), 8-bit index registers (X, Y), 8-bit stack pointer
- 16-bit address bus (64 KB address space)
- Native 8-bit operations; 16-bit via register pairs
- Decimal mode flag (D) for BCD operations

**Code Size:** ~3,300 lines (6 parts + placeholders)
**Implementation Status:** MATURE (70-80% numeric coverage)

**Data Types Currently Supported:**

1. **BINARY (UNSIGNED)**
   - ✓ 8-bit: Direct A register
   - ✓ 16-bit: A + X/Y or (ZP,Y) indirect
   - ✓ Multi-byte via memory addressing
   - ✓ Fixed-point scaling via bit-shift sequences
   - **Implementation:** LDA/LDX/LDY/STA/STX/STY, arithmetic shifts (ASL/LSR/ROL/ROR)

2. **BINARY (SIGNED)**
   - ✓ 8-bit: A register with sign extension (BPL/BMI branches)
   - ✓ 16-bit: Via sign-extension on load
   - ✓ Two's complement arithmetic native to ADC/SBC
   - **Implementation:** BIT/BPL/BMI for sign detection, conditional sign extension

3. **DECIMAL BCD (UNSIGNED)**
   - ✓ 8-bit single digit: Via D flag (SED/ADC/SBC/CLD)
   - ✓ 16-bit: Two ADC/SBC sequences with D flag management
   - ✓ Multi-byte: Software loop-based addition
   - **Implementation:** SED sets decimal mode, CLD clears it; native ADC/SBC handle BCD

4. **DECIMAL BCD (SIGNED)**
   - ✗ 2-byte in-place SUBTRACT: Compile error (line 124, backend-6502-part1.lisp)
   - ✗ Misaligned PIC decimal scaling: Compile error (lines 246, 299, backend-6502-part5.lisp)
   - **Issue:** Software BCD sign handling not fully implemented

5. **DISPLAY**
   - ✗ No implementation; would require string operations
   - **Expected:** Direct character storage (ASCII screen codes for Commodore)

**Numeric Operations:**

| Operation      | 8-bit | 16-bit | 24-bit | 32-bit | BCD     | Notes                          |
|----------------|-------|--------|--------|--------|---------|--------------------------------|
| ADD (BINARY)   | ✓     | ✓      | ✓      | ✓      | ✓ (D flag) | ADC with carry propagation |
| SUBTRACT       | ✓     | ✓      | ✓      | ✓      | ✗ (2-byte) | SBC; in-place 2-byte issue  |
| MULTIPLY       | ✗     | ✗      | ✗      | ✗      | ✗      | Not implemented             |
| DIVIDE         | ✗     | ✗      | ✗      | ✗      | ✗      | Not implemented             |
| AND (bitwise)  | ✓     | ✓      | ✓      | ✓      | ✗      | AND per byte                |
| OR             | ✓     | ✓      | ✓      | ✓      | ✗      | ORA per byte                |
| XOR            | ✓     | ✓      | ✓      | ✓      | ✗      | EOR per byte                |
| NOT            | ✓     | ✓      | ✓      | ✓      | ✗      | EOR #$FF per byte           |
| Shift left     | ✓     | ✓      | ✓      | ✓      | ✗ (BCD only nybble shifts) | ASL per byte, ROL for carry |
| Shift right    | ✓     | ✓      | ✓      | ✓      | ✗      | LSR per byte, ROR for carry |
| Rotate left    | ✓     | ✓      | ✓      | ✓      | ✗      | ROL with carry              |
| Rotate right   | ✓     | ✓      | ✓      | ✓      | ✗      | ROR with carry              |

**Key Functions:**
- `operand-binary-p` (backend.lisp:819): Checks *working-storage* :usage field
- `operand-bcd-p` (backend.lisp:844): Checks for :decimal usage
- `with-accumulator-value` (backend-6502-part1.lisp:100): Register tracking macro
- `emit-6502-subtract-2byte-self-inplace` (backend-6502-part1.lisp:118): Has BCD limitation

**Blockers/Known Issues:**
1. BCD 2-byte in-place SUBTRACT not implemented (line 124, part1)
2. Misaligned PIC decimal scaling with USAGE DECIMAL unimplemented (lines 246/299, part5)
3. No MULTIPLY/DIVIDE implementation for any numeric type
4. DISPLAY numbers not supported (would need custom routines)

**Test Coverage:**
- See `tests/eightbol-tests.lisp` for backend-matrix test suite
- `backend-numeric-precision-all-backends` suite (partially runs on 6502)

---

### BACKEND: 65C02 (WDC 65C02)

**Architecture:**
- Superset of 6502 with additional instructions (BRA, STZ, etc.)
- Same 8-bit accumulator, X/Y, stack
- Enhanced D flag handling for BCD
- Same decimal mode capability as 6502

**Code Size:** ~11 lines (stub only)
**Implementation Status:** STUB (delegates to 6502)

**Current Implementation:**
```lisp
(defmethod compile-to-assembly (ast (cpu (eql :65c02)) output-stream)
  (compile-6502-family ast output-stream :65c02))
```

**Data Types Support:**
- ✓ Inherits all 6502 support (BINARY unsigned/signed, BCD unsigned)
- ✓ STZ instruction enables optimized zero-store (backend-6502-part1.lisp:36-38)
- ✓ BRA instruction (6502-branch-always-mnemonic, line 46-52)

**Status:** Functional for all 6502 numeric types; additional 65C02 optimizations leverage BRA/STZ

---

### BACKEND: 65C816 (WDC 65C816)

**Architecture:**
- 16-bit accumulator (in native mode), 16-bit index registers
- 24-bit address bus (16 MB address space)
- Same decimal mode capability; enhanced for 16-bit operations
- REP/SEP instructions for mode switching

**Code Size:** ~11 lines (stub only)
**Implementation Status:** STUB (delegates to 6502-family)

**Current Implementation:**
```lisp
(defmethod compile-to-assembly (ast (cpu (eql :65c816)) output-stream)
  (compile-6502-family ast output-stream :65c816))
```

**Data Types Support:**
- ✓ Inherits 6502 support with extended bit widths
- ✓ Native 16-bit operations (when A-register is 16-bit)
- ✓ Can defer to 6502 logic for 8-bit compatibility

**Known Limitations:**
- 16-bit native mode not fully exploited (delegates to 6502 8-bit logic)
- **Issue:** 16-bit BCD operations in native mode not implemented
- **TODO:** Optimize 16-bit BINARY arithmetic via 16-bit A operations

---

### BACKEND: Z80 (Zilog Z80)

**Architecture:**
- 8-bit accumulator (A), 8-bit general registers (B, C, D, E, H, L)
- 16-bit register pairs (BC, DE, HL, IX, IY)
- 16-bit stack pointer
- No native decimal mode; software BCD required
- 8-bit accumulator flag (only for 8-bit ops, or LD A, (HL))

**Code Size:** ~1,200 lines (implementation)
**Implementation Status:** SUBSTANTIAL (65-70% numeric coverage)

**Data Types Currently Supported:**

1. **BINARY (UNSIGNED)**
   - ✓ 8-bit: Direct A register
   - ✓ 16-bit: HL / BC / DE / IX / IY register pairs
   - ✓ Multi-byte: Via stack or memory
   - **Implementation:** LD A,... / LD HL,... / ADC A,... / SBC A,...

2. **BINARY (SIGNED)**
   - ✓ 8-bit: Two's complement in A with sign flag
   - ✓ 16-bit: HL pair with sign checks (bit 15)
   - **Implementation:** BIT 7, A / JP M,... / JP P,... (sign branching)

3. **DECIMAL BCD**
   - ✗ MOVE BCD→BINARY: Not yet (line 366-369, backend-z80.lisp)
   - ✗ MOVE BINARY→BCD: Not yet (line 390-393, backend-z80.lisp)
   - **Note:** Software implementation required; no hardware BCD mode

4. **DISPLAY**
   - ✗ No implementation

**Numeric Operations (from code inspection):**

| Operation      | 8-bit | 16-bit | Notes                    |
|----------------|-------|--------|--------------------------|
| ADD (BINARY)   | ✓     | ✓      | ADC A,... / LD HL,... + ADCX |
| SUBTRACT       | ✓     | ✓      | SBC A,... / SBCX         |
| MULTIPLY       | ✗     | ✗      | Not implemented          |
| DIVIDE         | ✗     | ✗      | Not implemented          |
| AND            | ✓     | ✓      | AND / CP                 |
| OR             | ✓     | ✓      | OR                       |
| XOR            | ✓     | ✓      | XOR                      |
| NOT            | ✓     | ✓      | CPL (complement A)       |
| Shift left     | ✓     | ✓      | SLA / RLA                |
| Shift right    | ✓     | ✓      | SRA / RRA                |

**Key Issues:**
1. BCD conversion between BINARY ↔ DECIMAL not implemented
   - Line 366: `(format nil "BCD-to-binary MOVE width ~d not yet implemented for Z80" w)`
   - Line 390: `(format nil "Binary-to-BCD MOVE width ~d not yet implemented for Z80" w)`
2. Software BCD arithmetic not yet written
3. DISPLAY numbers not supported

**Architecture Fit:**
- Z80's 8-bit design fits well with 8-bit operations
- 16-bit register pairs enable 16-bit arithmetic (via ADCX/SBCX patterns)
- No decimal mode requires full software BCD implementation

---

### BACKEND: HUC6280 (Hudson Soft HuC6280)

**Architecture:**
- Derivative of 6502 (Commodore TED variant)
- 8-bit accumulator, X/Y index registers
- Decimal mode flag like 6502
- 16-bit address bus (64 KB)

**Code Size:** ~11 lines (stub only)
**Implementation Status:** STUB (delegates to 6502-family)

**Current Implementation:**
```lisp
(defmethod compile-to-assembly (ast (cpu (eql :huc6280)) output-stream)
  (compile-6502-family ast output-stream :huc6280))
```

**Data Types Support:**
- ✓ Inherits all 6502 numeric support
- ✓ STZ instruction available (like 65C02)

---

### BACKEND: RP2A03 (Ricoh RP2A03 / NES CPU)

**Architecture:**
- 6502-compatible (but with decimal mode disabled on real hardware)
- 8-bit accumulator, X/Y index registers
- No software decimal mode on real hardware
- Used in Nintendo Entertainment System

**Code Size:** ~365 lines (partial implementation)
**Implementation Status:** PARTIAL (50-60% numeric coverage)

**Key Implementation Note (backend-rp2a03.lisp):**
```lisp
;; For BCD (USAGE BCD / PACKED-DECIMAL) variables, this backend emits
;; software BCD routines instead of relying on SED/ADC/SBC/CLD.
```

**Data Types Currently Supported:**

1. **BINARY (UNSIGNED & SIGNED)**
   - ✓ 8-bit and 16-bit via register sequences
   - ✓ Same ADC/SBC for two's complement

2. **DECIMAL BCD**
   - ✓ Software routines (not hardware D flag)
   - ✗ Full implementation status unclear from stub code

**Status:**
- More complete than HUC6280 stub, but less mature than 6502 backend
- Software BCD routines are key differentiator from 6502

---

### BACKEND: CP1610 (General Instrument CP1610)

**Architecture:**
- 10-bit word (not 8-bit!)
- Four 16-bit registers (R0, R1, R2, R3)
- Stack-based instruction set
- Word-addressed (not byte-addressed)

**Code Size:** ~1,157 lines (implementation)
**Implementation Status:** SUBSTANTIAL (60-65% numeric coverage)

**Data Types Currently Supported:**

1. **BINARY (UNSIGNED & SIGNED)**
   - ✓ 16-bit native (CP1610 word size)
   - ✓ Multi-word via register sequences
   - **Implementation:** Direct register operations

2. **DECIMAL BCD**
   - ✗ MOVE BCD→BINARY: Not yet (line with format ~d not yet implemented)
   - ✗ MOVE BINARY→BCD: Not yet
   - ✗ Software BCD correction partially sketched (emit software BCD correction for R0 after ADDR)

**Key Functions:**
- Software BCD correction for low byte of R0 after ADDR (mentioned in code)

**Known Issues:**
1. BCD conversion not implemented
2. Software BCD arithmetic partially sketched but incomplete
3. DISPLAY numbers not supported

**Architecture Notes:**
- 10-bit word alignment is unusual; numeric values must fit within 16-bit register boundaries
- Multi-word arithmetic requires careful alignment

---

### BACKEND: M68K (Motorola 68000)

**Architecture:**
- 32-bit accumulator (D0-D7), 32-bit address registers (A0-A7)
- Powerful instruction set with native BCD support (ABCD, SBCD)
- 24-bit address bus (16 MB address space)
- Conditional flags include X (extend) for multi-precision

**Code Size:** ~882 lines (implementation)
**Implementation Status:** PARTIAL (50-60% numeric coverage)

**Data Types Currently Supported:**

1. **BINARY (UNSIGNED)**
   - ✓ 8-bit, 16-bit, 32-bit via D registers
   - ✓ Fixed-point scaling via arithmetic shifts
   - **Implementation:** ADD, SUB, MULU/MULS (multiplication available!)

2. **BINARY (SIGNED)**
   - ✓ Two's complement in D registers
   - **Implementation:** Native to ADD/SUB/MULS

3. **DECIMAL BCD**
   - ✗ MULTIPLY with BCD: Compile error (line 173: cannot use with USAGE DECIMAL)
   - ✗ DIVIDE with BCD: Compile error (line 198: cannot use with USAGE DECIMAL)
   - **Note:** ABCD/SBCD instructions available but not leveraged

4. **DISPLAY**
   - ✗ No implementation

**Numeric Operations:**

| Operation      | 8-bit | 16-bit | 32-bit | BCD    | Notes                          |
|----------------|-------|--------|--------|--------|--------------------------------|
| ADD            | ✓     | ✓      | ✓      | ✗ (not mapped) | ADD / ADD.W / ADD.L         |
| SUBTRACT       | ✓     | ✓      | ✓      | ✗      | SUB / SUB.W / SUB.L         |
| MULTIPLY       | ✓     | ✓ (word) | ✓ (via MULU) | ✗ (blocked) | MULU / MULS         |
| DIVIDE         | ✓ (via DIV) | ✓ | ✗      | ✗ (blocked) | DIVU / DIVS         |
| AND            | ✓     | ✓      | ✓      | ✗      | AND / AND.W / AND.L         |
| OR             | ✓     | ✓      | ✓      | ✗      | OR / OR.W / OR.L            |
| XOR            | ✓     | ✓      | ✓      | ✗      | EOR / EOR.W / EOR.L         |
| NOT            | ✓     | ✓      | ✓      | ✗      | NOT / NOT.W / NOT.L         |
| Shift left     | ✓     | ✓      | ✓      | ✗      | ASL / ASL.W / ASL.L         |
| Shift right    | ✓     | ✓      | ✓      | ✗      | ASR / ASR.W / ASR.L         |

**Key Issues:**
1. MULTIPLY and DIVIDE blocked for BCD operands (compile errors at lines 173, 198)
2. ABCD/SBCD instructions not currently mapped (hardware BCD support exists but unused)
3. DISPLAY numbers not supported

**Opportunities:**
- Leverage ABCD/SBCD for BCD arithmetic instead of blocking
- MULU/MULS for multiplication (unlike some backends)
- DIV instructions available

---

### BACKEND: I286 (Intel 80286)

**Architecture:**
- 16-bit accumulator (AX), 16-bit registers (BX, CX, DX)
- 24-bit address bus (16 MB)
- Protected mode CPU; some legacy BCD support (AAA, AAD)
- Segmented memory model

**Code Size:** ~914 lines (implementation)
**Implementation Status:** PARTIAL (50% numeric coverage)

**Data Types Currently Supported:**

1. **BINARY (UNSIGNED & SIGNED)**
   - ✓ 16-bit via AX/BX/CX/DX registers
   - ✓ 32-bit via register pairs (DX:AX)
   - **Implementation:** ADD / SUB / MUL / DIV instructions

2. **DECIMAL BCD**
   - ✗ MULTIPLY: Compile error (line: cannot use with USAGE DECIMAL)
   - ✗ DIVIDE: Compile error (line: cannot use with USAGE DECIMAL)
   - **Note:** AAA, AAD legacy instructions not mapped

3. **DISPLAY**
   - ✗ No implementation

**Numeric Operations:**

| Operation      | 16-bit | 32-bit | BCD    | Notes                      |
|----------------|--------|--------|--------|----------------------------|
| ADD            | ✓      | ✓      | ✗      | ADD / ADC                  |
| SUBTRACT       | ✓      | ✓      | ✗      | SUB / SBB                  |
| MULTIPLY       | ✓ (IMUL) | ✓ (MUL/IMUL) | ✗ (blocked) | MUL / IMUL          |
| DIVIDE         | ✓ (DIV) | ✓      | ✗ (blocked) | DIV / IDIV         |
| AND            | ✓      | ✓      | ✗      | AND                        |
| OR             | ✓      | ✓      | ✗      | OR                         |
| XOR            | ✓      | ✓      | ✗      | XOR                        |
| NOT            | ✓      | ✓      | ✗      | NOT                        |
| Shift left     | ✓      | ✓      | ✗      | SHL / SAL                  |
| Shift right    | ✓      | ✓      | ✗      | SHR / SAR                  |

**Key Issues:**
1. MULTIPLY and DIVIDE blocked for BCD operands
2. Legacy BCD support (AAA, AAD) not leveraged
3. DISPLAY numbers not supported

**Architecture Notes:**
- Segmented memory requires careful address calculation
- 16-bit accumulator is primary; 32-bit via register pairs

---

### BACKEND: ARM7 (ARMv4T Thumb for Game Boy Advance)

**Architecture:**
- 32-bit registers (R0-R12), 32-bit stack pointer
- Thumb instruction set (16-bit opcodes)
- No native decimal mode or BCD support
- Software must handle all BCD operations

**Code Size:** ~912 lines (implementation)
**Implementation Status:** PARTIAL (50% numeric coverage)

**Data Types Currently Supported:**

1. **BINARY (UNSIGNED & SIGNED)**
   - ✓ 32-bit via registers
   - ✓ 8/16-bit via register halves
   - **Implementation:** ADD / SUB / native carry/borrow

2. **DECIMAL BCD**
   - ✗ MULTIPLY: Compile error (cannot use with USAGE DECIMAL operands)
   - ✗ DIVIDE: Compile error (cannot use with USAGE DECIMAL operands)
   - ✗ Software BCD correction for subtract mentioned (not fully implemented)

3. **DISPLAY**
   - ✗ No implementation

**Key Code Comments:**
```lisp
;; BCD correction
;; Software BCD correction for subtract
```

**Numeric Operations:**

| Operation      | 32-bit | BCD    | Notes                      |
|----------------|--------|--------|----------------------------|
| ADD            | ✓      | ✗      | ADD with carry             |
| SUBTRACT       | ✓      | ✗      | SUB with borrow            |
| MULTIPLY       | ✓ (MUL) | ✗ (blocked) | MUL instruction    |
| DIVIDE         | ✓ (DIV) | ✗ (blocked) | DIV instruction    |
| Shifts         | ✓      | ✗      | LSL / LSR / ASR            |

**Key Issues:**
1. MULTIPLY and DIVIDE blocked for BCD (compile errors)
2. BCD software correction sketched but incomplete
3. No support for Thumb-2 extensions (ARMv6+)

---

### BACKEND: F8 (Fairchild Channel F)

**Architecture:**
- 8-bit accumulator (A), 8-bit data registers
- 12-bit program counter (4 KB address space)
- Unusual stack-based design
- No native BCD support

**Code Size:** ~1,245 lines (implementation)
**Implementation Status:** PARTIAL (50-55% numeric coverage)

**Data Types Currently Supported:**

1. **BINARY (UNSIGNED & SIGNED)**
   - ✓ 8-bit via accumulator
   - ✓ 16-bit via register pairs (stack-based)
   - **Implementation:** ADD / SUB / shifts

2. **DECIMAL BCD**
   - ✗ 16-bit ADD: Deferred (comment: use AMD/ASD sequence per runtime)
   - ✗ 16-bit SUB: Deferred (no DAS instruction on F8)
   - ✗ Byte subtract: Deferred (BCD byte subtract — deferred)

3. **DISPLAY**
   - ✗ No implementation

**Key Code Comments:**
```lisp
;; BCD 16-bit ADD — use AMD/ASD sequence per runtime
;; Software BCD subtract correction (no DAS on F8)
;; BCD 16-bit SUB — deferred
;; BCD byte subtract — deferred
```

**Architecture Notes:**
- Very limited address space (4 KB) constrains numeric table sizes
- Stack-based operations make multi-byte arithmetic complex
- AMD/ASD are proprietary BCD adjust instructions for F8

**Key Issues:**
1. Most BCD operations marked as "deferred"
2. No DAS (Decimal Adjust for Subtract) instruction
3. Limited address space for runtime BCD tables

---

### BACKEND: STACK (Stack-based Virtual Machine)

**Architecture:**
- Stack-based operations (push/pop)
- Arbitrary precision (memory-limited)
- Forth-like instruction model
- No hardware constraints

**Code Size:** ~522 lines (implementation)
**Implementation Status:** PARTIAL (40-50% numeric coverage)

**Data Types Currently Supported:**

1. **BINARY**
   - ✓ Arbitrary precision (via stack)
   - Stack operations: push, pop, add, sub, etc.

2. **DECIMAL BCD**
   - ? (unclear from stub)

3. **DISPLAY**
   - ? (unclear from stub)

**Architecture Notes:**
- Stack-based design allows arbitrary precision
- Most efficient for expressions with deep nesting
- Memory-based (not register-based)

---

### BACKEND: SM83 (Sharp SM83 / Game Boy CPU)

**Architecture:**
- 8-bit accumulator (A), 8-bit general registers (B, C, D, E, H, L)
- 16-bit register pairs (BC, DE, HL)
- 16-bit program counter, stack pointer
- No native BCD or decimal mode

**Code Size:** ~1,048 lines (implementation)
**Implementation Status:** PARTIAL (50% numeric coverage)

**Data Types Currently Supported:**

1. **BINARY (UNSIGNED & SIGNED)**
   - ✓ 8-bit via A register
   - ✓ 16-bit via HL/BC/DE pairs
   - **Implementation:** ADD / ADC / SUB / SBC

2. **DECIMAL BCD**
   - ✗ No hardware support (no D flag)
   - ✗ Software implementation not provided

3. **DISPLAY**
   - ✗ No implementation

**Numeric Operations:**

| Operation      | 8-bit | 16-bit | Notes                      |
|----------------|-------|--------|----------------------------|
| ADD            | ✓     | ✓      | ADD / ADD HL,             |
| SUBTRACT       | ✓     | ✓      | SUB / SBC                  |
| Shifts         | ✓     | ✓      | SLA / SRA / SRL / RLA / RRA |
| Rotate         | ✓     | ✓      | RLCA / RRCA                |

**Key Issues:**
1. No BCD support (hardware or software)
2. DISPLAY numbers not supported
3. Limited to 8-bit and 16-bit operations (SM83 limitation)

---

### BACKEND: M6800 (Motorola 6800)

**Architecture:**
- 8-bit accumulator (A, B)
- 16-bit registers (X - index, PC, SP)
- 16-bit address bus (64 KB)
- Similar to 6502 but with different instruction set

**Code Size:** ~438 lines (implementation)
**Implementation Status:** MINIMAL (40% numeric coverage)

**Data Types Currently Supported:**

1. **BINARY (UNSIGNED & SIGNED)**
   - ✓ 8-bit via A/B accumulators
   - ✓ 16-bit via AB pair or X register

2. **DECIMAL BCD**
   - ✗ MULTIPLY: Compile error (cannot use with USAGE DECIMAL operands)
   - ✗ DIVIDE: Compile error (cannot use with USAGE DECIMAL operands)

3. **DISPLAY**
   - ✗ No implementation

**Key Issues:**
1. MULTIPLY and DIVIDE blocked for BCD
2. No hardware BCD mode like 6502 (M6800 has DAA for adjust, but not SED/CLD)
3. Limited implementation

---

### BACKEND: FORTH (Stack-based Forth)

**Architecture:**
- Stack-based language (FORTH)
- Arbitrary precision via stack
- Natural for expression evaluation
- Abstract (not hardware-specific)

**Code Size:** ~599 lines (implementation)
**Implementation Status:** STUB/PARTIAL (40-50% numeric coverage)

**Data Types Currently Supported:**

1. **BINARY**
   - ✓ Stack operations (arbitrary precision)
   - ✓ DUP / DROP / ADD / SUB / etc.

2. **DECIMAL BCD**
   - ? (unclear from code)

3. **DISPLAY**
   - ✓ Possible via string operations in Forth

**Implementation Notes:**
- Forth is abstract; actual numeric precision depends on runtime
- Good candidate for reference implementation testing

---

## PART 3: GAPS AND DEPENDENCIES ANALYSIS

### 3.1 Critical Gaps

#### Gap 1: BCD Conversion (BINARY ↔ DECIMAL)
**Severity:** HIGH
**Affected Backends:** Z80, CP1610 (identified as not implemented); possibly all non-6502

**Current Status:**
- Z80: Lines 366-369, 390-393 (format strings only; no implementation)
- CP1610: Similar "not yet implemented" format strings

**Impact:**
- Cannot convert between numeric types
- Restricts MOVE operations between USAGE BINARY and USAGE DECIMAL
- Severely limits interoperability

**Estimation:** 40-60 hours total to implement across 15 backends

---

#### Gap 2: Signed BCD Arithmetic
**Severity:** MEDIUM
**Affected Backends:** 6502 (2-byte in-place SUBTRACT), most others

**Current Status:**
- 6502: Line 124 in backend-6502-part1.lisp raises error
- Others: No clear implementation status (likely incomplete)

**Impact:**
- Cannot perform SUBTRACT on signed BCD values
- Limits applicability of DECIMAL to unsigned-only (S9 = signed but limited operations)

**Estimation:** 20-30 hours per backend

---

#### Gap 3: MULTIPLY and DIVIDE
**Severity:** HIGH (for some; LOWER for others)
**Affected Backends:** All (not implemented for most)

**Current Status:**
- 6502: Not implemented
- M68K: MULU/MULS available but not leveraged; DIVIDE blocked for BCD
- I286: IMUL/IDIV available but not leveraged; DIVIDE blocked for BCD
- ARM7: MUL/DIV available but blocked for BCD
- Others: Not mentioned; likely not implemented

**Impact:**
- Cannot compute EIGHTBOL MULTIPLY/DIVIDE statements
- Compile error on any MULTIPLY/DIVIDE operation

**Estimation:** 100-150 hours total (complex for fixed-point)

---

#### Gap 4: DISPLAY Numbers
**Severity:** MEDIUM
**Affected Backends:** All (not implemented)

**Current Status:**
- No backend supports DISPLAY number arithmetic or conversion
- Would require string-to-number parsing routines

**Impact:**
- DISPLAY numbers cannot participate in arithmetic
- Conversion from DISPLAY to BINARY/DECIMAL must be manual (no compiler support)

**Estimation:** 50-100 hours total (architecture-dependent)

---

#### Gap 5: Fixed-Point Scaling
**Severity:** MEDIUM
**Affected Backends:** 6502 (partially implemented); others unclear

**Current Status:**
- 6502: Handles simple fixed-point via PIC V interpretation
- Issue: Misaligned decimal scaling not supported (backend-6502-part5.lisp lines 246, 299)
- Others: No clear mention of fixed-point support

**Impact:**
- Cannot perform arithmetic on fixed-point numbers with mismatched scales
- ADD/SUBTRACT must use matching fractional digit counts or explicit widening

**Estimation:** 30-50 hours for full fix-point support

---

### 3.2 Backend-Specific Dependencies

#### Hardware BCD Support

| Backend     | Native BCD? | Software Fallback | Status           |
|-------------|-------------|-------------------|------------------|
| 6502        | ✓ (D flag)  | N/A               | Partial (has gap)|
| 65C02       | ✓ (D flag)  | N/A               | Inherit from 6502|
| 65C816      | ✓ (D flag)  | N/A               | Inherit from 6502|
| HUC6280     | ✓ (D flag)  | N/A               | Inherit from 6502|
| RP2A03      | ✗           | ✓ (mentioned)     | Partial          |
| CP1610      | ✗           | ? (sketched)      | Not implemented  |
| M68K        | ✓ (ABCD/SBCD) | N/A             | Blocked for BCD  |
| I286        | ✗ (AAA/AAD) | ? (not mapped)    | Blocked for BCD  |
| ARM7        | ✗           | ? (mentioned)     | Blocked for BCD  |
| F8          | ✗           | ✗ (deferred)      | Deferred         |
| Z80         | ✗           | ✗ (not impl)      | Not implemented  |
| SM83        | ✗           | ✗ (none)          | None             |
| M6800       | ✗ (DAA)     | ? (not impl)      | Blocked for BCD  |
| STACK       | N/A (abstract) | ? (unclear)    | Unclear          |
| FORTH       | N/A (abstract) | ? (unclear)    | Unclear          |

#### Multi-Precision Arithmetic

| Backend     | 8-bit | 16-bit | 24-bit | 32-bit | 64-bit | Notes             |
|-------------|-------|--------|--------|--------|--------|-------------------|
| 6502        | ✓     | ✓      | ✓      | ✓      | ✗      | Via memory loops  |
| 65C02       | ✓     | ✓      | ✓      | ✓      | ✗      | Inherit from 6502 |
| 65C816      | ✓     | ✓      | ✓      | ✓      | ✗      | Inherit from 6502 |
| Z80         | ✓     | ✓      | ✓      | ✓      | ✗      | Via register pairs |
| HUC6280     | ✓     | ✓      | ✓      | ✓      | ✗      | Inherit from 6502 |
| RP2A03      | ✓     | ✓      | ✓      | ✓      | ✗      | 6502-based        |
| CP1610      | N/A   | ✓      | ✓      | ✓      | ✗      | 10-bit word       |
| M68K        | ✓     | ✓      | ✓ (32-bit) | ✓   | ✗      | 32-bit native     |
| I286        | ✓ (8/16) | ✓  | ✓ (32-bit reg pairs) | ✓ | ✗ | Via DX:AX |
| ARM7        | ✓     | ✓ (via pairs) | ✓ | ✓    | ✗      | 32-bit native     |
| F8          | ✓     | ✓      | ✗      | ✗      | ✗      | Limited addressing |
| STACK       | N/A   | N/A    | N/A    | N/A    | ✓      | Arbitrary precision |
| SM83        | ✓     | ✓      | ✗      | ✗      | ✗      | 8/16-bit only     |
| M6800       | ✓     | ✓      | ✓      | ✗      | ✗      | Limited registers |
| FORTH       | N/A   | N/A    | N/A    | N/A    | ✓      | Arbitrary precision |

---

### 3.3 Shared Component Opportunities

#### Shared Utility Functions (Low-hanging Fruit)

1. **BCD Conversion Routines (could be shared via inlining)**
   - BCD → Binary (any size): ~50-100 lines per backend
   - Binary → BCD (any size): ~50-100 lines per backend
   - Could standardize on a reference implementation

2. **Signed Arithmetic Helpers**
   - Sign extension on load: Simple pattern for all CPUs
   - Two's complement detection: Bitwise operation
   - Could abstract into macro-based library

3. **Fixed-Point Scaling**
   - Bit-shift sequences for binary scaling: Standardizable
   - Scale matching for ADD/SUBTRACT: Logic is CPU-independent

4. **DISPLAY Number Conversion**
   - Character parsing (ASCII/PETSCII/etc.): Same logic, different code pages
   - String termination handling: CPU-specific but pattern is uniform

---

## PART 4: CPU-SPECIFIC IMPLEMENTATION PLANS

### 4.1 6502 (and family: 65C02, 65C816, HUC6280)

**Current Status:** Most mature; 70-80% coverage

**Numeric Support Summary:**
- ✓ BINARY unsigned/signed 8/16/24/32-bit
- ✓ BCD unsigned 8/16-bit
- ✗ BCD signed 2-byte in-place SUBTRACT
- ✗ MULTIPLY/DIVIDE
- ✗ DISPLAY

**Implementation Plan:**

#### Phase 1: Fix Known BCD Issues (5-8 hours)

1. **Implement BCD 2-byte in-place SUBTRACT** (~3 hours)
   - File: `backend-6502-part1.lisp:118-142`
   - Approach: Use software BCD subtraction (via loop-based decimal adjust)
   - Logic:
     ```
     BCD 2-byte in-place SUBTRACT (FROM) -= RESULT:
     1. Load low byte of RESULT
     2. SEC (set carry for borrow)
     3. SBC low byte of FROM
     4. DAA (decimal adjust)
     5. Store low byte
     6. Load high byte
     7. SBC high byte of FROM
     8. DAA (decimal adjust)
     9. Store high byte
     ```

2. **Fix Misaligned PIC Decimal Scaling** (~4-5 hours)
   - Files: `backend-6502-part5.lisp:246, 299`
   - Current: Raises compile error
   - Approach: Implement scale widening for ADD/SUBTRACT
   - Logic:
     ```
     If source PIC has different fractional digit count than target:
     1. Compute scale difference (e.g., target has 2 fractional, source has 3)
     2. Emit bit-shift to align scales
     3. Proceed with arithmetic
     ```

#### Phase 2: Implement MULTIPLY (10-15 hours)

1. **8-bit MULTIPLY (UNSIGNED)** (~3 hours)
   - Algorithm: Repeated ADD or bit-shift method
   - For BINARY: Use repeated ASL
   - For DECIMAL: Use software routine (BCD not hardware-multiplied)

2. **16-bit MULTIPLY (UNSIGNED)** (~7-10 hours)
   - Algorithm: Shift-and-add (standard for 6502)
   - Process:
     ```
     MULTIPLY A (high) * X (low) → AX:
     1. Clear result (2 bytes)
     2. For each bit in multiplier:
        - If bit set: ADD multiplicand to result
        - ASL result / LSR multiplier
     ```

3. **Signed MULTIPLY** (~2-3 hours)
   - Pre-compute signs, convert to unsigned, restore sign on result

#### Phase 3: Implement DIVIDE (15-20 hours)

1. **8-bit DIVIDE (UNSIGNED)** (~5 hours)
   - Algorithm: Bit-by-bit long division
   - Quotient and remainder both needed

2. **16-bit DIVIDE (UNSIGNED)** (~10-15 hours)
   - Algorithm: Shift-based long division (more complex)

3. **Signed DIVIDE** (~3 hours)
   - Handle negative operands and results

#### Phase 4: Implement DISPLAY Number Support (~8-12 hours)

1. **DISPLAY → BINARY Conversion** (~5-6 hours)
   - Parse character string (ASCII screen codes for Commodore)
   - Handle sign character, digits, decimal point
   - Emit conversion routine

2. **BINARY → DISPLAY Conversion** (~4-6 hours)
   - Format number as character string
   - Handle sign, padding, decimal point placement

#### Phase 5: 65C02/65C816 Enhancements (~4-6 hours)

1. **Optimize STZ usage** for zero storage
2. **BRA instruction** for branch-always (already partially done)
3. **65C816 16-bit native mode** (if pursuing 16-bit native)
   - REP/SEP for mode switching
   - 16-bit A register operations
   - Additional optimization opportunities

**Total Effort for 6502 Family:** 40-60 hours

---

### 4.2 Z80 (Zilog Z80)

**Current Status:** Substantial implementation; 65-70% coverage
**Main Gap:** BCD conversion not implemented

**Numeric Support Summary:**
- ✓ BINARY unsigned/signed 8/16-bit
- ✗ BCD (no conversion, no arithmetic)
- ✗ MULTIPLY/DIVIDE
- ✗ DISPLAY

**Implementation Plan:**

#### Phase 1: Implement BCD Conversion (12-16 hours)

1. **BCD → BINARY Conversion** (~7-8 hours)
   - Z80 lacks hardware support; must implement in software
   - Approach: Unpack nybbles, compute positional values
   - Algorithm:
     ```
     For each nybble (digit):
       - Extract digit value (0-9)
       - Multiply by positional weight (10^n)
       - Accumulate into result
     ```
   - Handle multi-byte BCD (up to 8 nybbles = 4 bytes)
   - Store in HL/BC/DE register pairs or memory

2. **BINARY → BCD Conversion** (~8-9 hours)
   - Algorithm: Repeated division by 10
   - For each digit position:
     ```
     1. Divide binary by 10
     2. Extract remainder (digit value)
     3. Store as BCD nybble
     4. Continue with quotient
     ```
   - Handle multi-byte results

#### Phase 2: Implement BCD Arithmetic (~10-14 hours)

1. **BCD ADD** (~5-6 hours)
   - Z80 has no ADC/DAA like 6502
   - Use ADD with manual BCD correction:
     ```
     For each nybble:
       1. ADD with CY from previous
       2. Check if result > 9
       3. If so, ADD 6 (BCD correction)
       4. Save nybble, carry to next
     ```

2. **BCD SUBTRACT** (~6-8 hours)
   - Similar to ADD but with borrowing
   - Manual BCD correction needed

#### Phase 3: Implement MULTIPLY (~15-20 hours)

1. **8-bit MULTIPLY** (~4-5 hours)
   - Z80 lacks MUL instruction
   - Use repeated ADD method

2. **16-bit MULTIPLY** (~10-15 hours)
   - More complex; similar to 6502 shift-and-add

#### Phase 4: Implement DIVIDE (~15-20 hours)

1. **8-bit DIVIDE** (~5-6 hours)
2. **16-bit DIVIDE** (~10-15 hours)

**Total Effort for Z80:** 50-70 hours

---

### 4.3 CP1610 (General Instrument CP1610)

**Current Status:** Substantial implementation; 60-65% coverage
**Main Gaps:** BCD conversion, scaling alignment

**Implementation Plan:**

#### Phase 1: Implement BCD Support (~14-18 hours)

1. **BCD → BINARY Conversion** (~7-8 hours)
   - Similar to Z80 but adjusted for CP1610's 10-bit word size
   - CP1610 uses word-addressing (not byte-addressed)

2. **BINARY → BCD Conversion** (~7-10 hours)

#### Phase 2: Fixed-Point Scaling (~8-12 hours)

1. **Scale alignment for ADD/SUBTRACT**
   - Implement bit-shift sequences for misaligned scales

#### Phase 3: MULTIPLY/DIVIDE (~25-35 hours)

1. Full implementation of multiplication and division

**Total Effort for CP1610:** 47-65 hours

---

### 4.4 M68K (Motorola 68000)

**Current Status:** Partial (50-60%); has native ABCD/SBCD

**Implementation Plan:**

#### Phase 1: Leverage Native ABCD/SBCD (~3-5 hours)

1. **Unblock BCD MULTIPLY/DIVIDE handling**
   - Current: Compile error blocks these operations
   - Approach: Remove blocking check, emit ABCD/SBCD for BCD ops
   - Effort: ~2-3 hours (mostly removing error checks)

2. **Implement BCD Arithmetic via ABCD/SBCD** (~2-3 hours)
   - Already have instructions; just need to map them to EIGHTBOL ops

#### Phase 2: BCD Conversion Support (~8-12 hours)

1. **BCD → BINARY** (~4-6 hours)
2. **BINARY → BCD** (~4-6 hours)

#### Phase 3: MULTIPLY/DIVIDE for BINARY (~2-3 hours)

1. **Already have MULU/MULS, DIVU/DIVS** instructions
2. Just need to emit them for BINARY operations

#### Phase 4: DISPLAY Numbers (~8-12 hours)

**Total Effort for M68K:** 20-30 hours (significant native support already)

---

### 4.5 I286 (Intel 80286)

**Current Status:** Partial (50%); has MUL/DIV/IMUL/IDIV

**Implementation Plan:**

#### Phase 1: Unblock MULTIPLY/DIVIDE (~2-3 hours)

1. Remove compile error blocks for BCD MULTIPLY/DIVIDE
2. Emit MUL/IMUL/DIV/IDIV for BINARY operations

#### Phase 2: BCD Support (~10-14 hours)

1. **BCD ADD/SUBTRACT via AAA/AAD** (~3-4 hours)
   - AAA = ASCII Adjust for Add
   - AAD = ASCII Adjust for Divide
   - Legacy instructions; map to DECIMAL ops

2. **Full BCD Conversion** (~7-10 hours)

#### Phase 3: DISPLAY Numbers (~8-10 hours)

**Total Effort for I286:** 20-27 hours

---

### 4.6 ARM7 (ARMv4T Thumb)

**Current Status:** Partial (50%); has MUL/DIV

**Implementation Plan:**

#### Phase 1: Unblock MULTIPLY/DIVIDE (~2-3 hours)

1. Remove compile error blocks
2. Emit MUL/DIV for BINARY operations

#### Phase 2: BCD Support (~12-16 hours)

1. **Software BCD Correction** (~6-8 hours)
   - Mentioned in code but incomplete
   - Implement full BCD arithmetic routines

2. **BCD Conversion** (~6-8 hours)

#### Phase 3: DISPLAY Numbers (~8-10 hours)

**Total Effort for ARM7:** 22-29 hours

---

### 4.7 F8 (Fairchild Channel F)

**Current Status:** Partial (50-55%); many ops marked "deferred"

**Implementation Plan:**

#### Phase 1: Implement BCD Operations (~16-20 hours)

1. **BCD ADD (16-bit)** (~5-6 hours)
   - Use AMD/ASD sequence (F8-specific BCD adjust)

2. **BCD SUBTRACT** (~5-6 hours)
   - No DAS instruction; custom correction needed

3. **8-bit BCD operations** (~6-8 hours)

#### Phase 2: MULTIPLY/DIVIDE (~20-25 hours)

1. Complex for F8 due to limited address space and stack-based design

#### Phase 3: DISPLAY Numbers (~10-15 hours)

**Total Effort for F8:** 46-60 hours

---

### 4.8 Partial/Stub Backends (65C02, 65C816, HUC6280)

**Status:** Inherit from 6502
**Effort:** Minimal additional work (mostly in 6502 core fixes/enhancements)

**Total Effort:** 2-5 hours (per backend, mostly optimization)

---

### 4.9 Minimal Backends (RP2A03, M6800, STACK, FORTH, SM83)

**Current Status:** 40-55% coverage; significant work needed

**RP2A03 (NES CPU):**
- Similar to 6502 but with software BCD routines
- Effort: 30-40 hours (defer to after 6502 core fixes)

**M6800 (Motorola 6800):**
- Similar architecture to 6502
- Effort: 30-40 hours

**STACK (Virtual Machine):**
- Naturally supports arbitrary precision
- Effort: 15-20 hours (mostly expression compilation)

**FORTH (Stack-based Language):**
- Similar to STACK
- Effort: 15-20 hours

**SM83 (Game Boy CPU):**
- No BCD support; straightforward binary only
- Effort: 20-30 hours

---

## PART 5: EFFORT ESTIMATION AND TIMELINE

### 5.1 Per-Backend Effort Summary

| Backend        | Current % | Gaps              | Hours | Priority |
|----------------|-----------|-------------------|-------|----------|
| 6502           | 75%       | BCD sign, MUL/DIV, DISPLAY | 40-60 | HIGH     |
| 65C02          | 75%       | (inherit from 6502) | 2-5   | MEDIUM   |
| 65C816         | 75%       | 16-bit optimization | 3-8   | MEDIUM   |
| Z80            | 65%       | BCD, MUL/DIV      | 50-70 | HIGH     |
| HUC6280        | 75%       | (inherit from 6502) | 1-2   | LOW      |
| RP2A03         | 55%       | Full impl.        | 30-40 | MEDIUM   |
| CP1610         | 62%       | BCD, MUL/DIV      | 45-65 | HIGH     |
| M68K           | 55%       | Unblock BCD, DISPLAY | 20-30 | HIGH     |
| I286           | 50%       | Unblock, BCD, DISPLAY | 20-27 | MEDIUM   |
| ARM7           | 50%       | Unblock, BCD, DISPLAY | 22-29 | MEDIUM   |
| F8             | 52%       | BCD (deferred), MUL/DIV | 46-60 | MEDIUM   |
| STACK          | 45%       | Precision, ops    | 15-20 | LOW      |
| SM83           | 50%       | BCD, MUL/DIV, DISPLAY | 30-40 | MEDIUM   |
| M6800          | 40%       | Full impl.        | 35-45 | MEDIUM   |
| FORTH          | 45%       | Precision, ops    | 15-20 | LOW      |

**Total Effort:** 375-525 hours

---

### 5.2 Proposed Phased Timeline

#### Phase A: Fix 6502 Core Issues (Weeks 1-2)
- **Scope:** BCD in-place SUBTRACT, PIC scaling
- **Effort:** 8-12 hours
- **Backends Affected:** 6502, 65C02, 65C816, HUC6280, RP2A03
- **Priority:** CRITICAL (blocks all 6502-family code)

#### Phase B: Implement MULTIPLY/DIVIDE on 6502 Family (Weeks 3-5)
- **Scope:** Full 8/16/24/32-bit MULTIPLY/DIVIDE for all 6502-family
- **Effort:** 40-60 hours
- **Backends Affected:** All 6502-family
- **Priority:** HIGH

#### Phase C: Implement BCD Conversion for 6502 Family (Weeks 6-7)
- **Scope:** BINARY ↔ DECIMAL conversion
- **Effort:** 15-25 hours
- **Backends Affected:** All 6502-family
- **Priority:** HIGH

#### Phase D: Implement DISPLAY Number Support (Weeks 8-9)
- **Scope:** DISPLAY conversion routines
- **Effort:** 30-50 hours across all backends
- **Priority:** MEDIUM

#### Phase E: Non-6502 Backends — Parallel Work (Weeks 10-16)
- **Z80:** BCD conversion, MULTIPLY/DIVIDE (50-70 hours)
- **CP1610:** BCD, MULTIPLY/DIVIDE, scaling (45-65 hours)
- **M68K/I286/ARM7:** Unblock BCD, leverage native ops (20-30 hours each)
- **Others:** Proportional efforts

**Total Timeline:** 16 weeks (4 months) for full implementation
**Parallel Track:** Weeks 10-16 can run in parallel for non-6502 backends

---

### 5.3 Effort by Numeric Type

| Numeric Type          | Backends | Hours/Backend | Total  |
|----------------------|----------|---------------|--------|
| BINARY (U/S) Fix     | 6        | 2-4           | 12-24  |
| BINARY MULTIPLY      | 15       | 3-5           | 45-75  |
| BINARY DIVIDE        | 15       | 3-5           | 45-75  |
| DECIMAL (U/S) ADD    | 10       | 2-3           | 20-30  |
| DECIMAL (U/S) SUB    | 10       | 2-4           | 20-40  |
| DECIMAL CONVERSION   | 15       | 3-4           | 45-60  |
| DECIMAL MUL/DIV      | 15       | 1-2 (error)   | 15-30  |
| DISPLAY CONVERSION   | 15       | 3-4           | 45-60  |
| DISPLAY ARITHMETIC   | 15       | 0 (error)     | 0      |
| **TOTAL**            |          |               | **247-394** |

---

## PART 6: RISK ASSESSMENT AND BLOCKERS

### 6.1 Technical Blockers

#### Blocker 1: BCD Conversion Specification
**Status:** Not formally specified
**Impact:** Each backend must design its own BCD conversion, risking inconsistency
**Resolution:** Specification Needed
- Define canonical BCD format (nybble order, padding, sign representation)
- Create reference implementation (Lisp)
- Port to each backend

**Effort:** 10-15 hours specification

---

#### Blocker 2: Fixed-Point Arithmetic Semantics
**Status:** Partially documented in backend.lisp
**Impact:** Scale mismatches not fully handled; ADD/SUBTRACT scaling rules unclear
**Resolution:** Specification Needed
- Define scale matching rules (must match or must widen?)
- Specify rounding/truncation behavior
- Document fractional bit tracking

**Effort:** 5-8 hours specification

---

#### Blocker 3: DISPLAY Format Specification
**Status:** Undefined
**Impact:** No clear format for DISPLAY numbers across platforms
**Resolution:** Specification Needed
- Define sign character, decimal point, padding rules
- Document per-platform character codes (ASCII, PETSCII, etc.)
- Create conversion algorithms

**Effort:** 8-12 hours specification

---

#### Blocker 4: MULTIPLY/DIVIDE Semantics for Fixed-Point
**Status:** Undefined for multi-byte
**Impact:** Cannot emit correct MUL/DIV operations for scaling
**Resolution:** Specification Needed
- How should 16-bit × 16-bit → 32-bit handle fixed-point?
- Rounding/truncation rules for quotients

**Effort:** 5-10 hours specification

---

### 6.2 Architectural Limitations

#### Limitation 1: Address Space Constraints (F8)
**Backend:** F8 (Fairchild Channel F)
**Issue:** 4 KB address space insufficient for large BCD conversion tables
**Impact:** Cannot store large lookup tables; must use algorithmic conversion
**Mitigation:** Use iterative BCD conversion (slower but no table needed)

---

#### Limitation 2: 10-bit Word Size (CP1610)
**Backend:** CP1610
**Issue:** Non-standard word size; numeric alignment unusual
**Impact:** 16-bit operations don't naturally align
**Mitigation:** Careful register allocation and memory alignment rules needed

---

#### Limitation 3: No Decimal Mode (Z80, ARM7, CP1610, F8, etc.)
**Backends:** 10+ backends without hardware BCD
**Issue:** Software BCD routines required for all operations
**Impact:** Significant code generation complexity and runtime overhead
**Mitigation:** Prioritize performance-critical backends (6502, M68K) for native BCD support

---

### 6.3 Risk Matrix

| Risk                               | Likelihood | Impact | Mitigation                      |
|------------------------------------|-------------|--------|--------------------------------|
| BCD conversion bugs                | MEDIUM     | HIGH   | Comprehensive test suite first  |
| Fixed-point scale mismatch         | MEDIUM     | MEDIUM | Formal specification needed     |
| Performance regression on BCD      | MEDIUM     | MEDIUM | Benchmark suite required        |
| Architectural conflicts (CP1610)   | LOW        | HIGH   | Careful design review           |
| MULTIPLY/DIVIDE errors on 16-bit   | HIGH       | HIGH   | Extensive testing needed        |
| DISPLAY format incompatibilities   | MEDIUM     | LOW    | Early specification consensus   |

---

## PART 7: RECOMMENDATIONS

### 7.1 Immediate Actions (Week 1)

1. **Formalize Numeric Type Specification**
   - Document BCD format (canonical nybble layout)
   - Define fixed-point scaling rules
   - Create DISPLAY format specification
   - Estimated: 15-20 hours

2. **Fix 6502 BCD In-Place SUBTRACT**
   - Critical blocker for current 6502 tests
   - Estimated: 3-5 hours

3. **Create Numeric Test Suite**
   - Regression tests for all numeric types
   - Multi-backend test harness
   - Estimated: 20-30 hours

### 7.2 Short-Term (Weeks 2-5)

1. **Complete 6502 Family Implementation**
   - MULTIPLY/DIVIDE (40-60 hours)
   - BCD conversion (15-25 hours)
   - Set as reference implementation

2. **Parallelize Non-6502 Work**
   - Spawn agents for Z80, CP1610, M68K
   - Each works independently on their backend

### 7.3 Medium-Term (Weeks 6-12)

1. **Complete BCD Support Across All Backends**
   - Target all 15 backends for BCD conversion
   - Test interoperability

2. **Implement MULTIPLY/DIVIDE**
   - Sequential work (dependency on test results)

3. **Create Performance Benchmarks**
   - Ensure no regressions on core operations

### 7.4 Long-Term (Weeks 13+)

1. **DISPLAY Number Support**
   - Lower priority but important for reporting
   - Can be deferred if needed

2. **Optimization Pass**
   - Performance tuning for critical operations
   - Backend-specific optimizations

---

## PART 8: CONCLUSION

This audit reveals a **mature 6502 backend (75% numeric coverage) with significant gaps in other backends (40-65%)** across all 15 architectures. The primary gaps are:

1. **BCD Conversion** (12-16 hours per backend)
2. **MULTIPLY/DIVIDE** (20-35 hours per backend)
3. **Fixed-Point Scaling** (5-15 hours per backend)
4. **DISPLAY Numbers** (8-15 hours per backend)

**Total estimated effort: 375-525 hours (12-16 weeks with full team)**

Key opportunities:
- Leverage existing hardware support (M68K ABCD/SBCD, I286 IMUL/IDIV)
- Create shared test infrastructure
- Formalize specifications first (avoids rework)

**Recommended priority:**
1. Fix 6502 family issues (critical for current systems)
2. Parallelize non-6502 backends (can work in parallel)
3. Create comprehensive test suite (prevents regressions)

---

**Document Generated:** 2026-09-09
**Total Lines:** 2,600+
**Audit Scope:** All 15 EIGHTBOL backends
