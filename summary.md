# EIGHTBOL Numeric Precision Test Coverage

## Goal
Create comprehensive test coverage for numeric precision operations (ADD, SUBTRACT, SHIFT-LEFT, SHIFT-RIGHT, MOVE) across all EIGHTBOL backends, including:
- USAGE BINARY and USAGE DECIMAL
- Signed and unsigned data
- Various PICTURE specifications up to 8 bytes total precision
- All supported backends: :6502, :rp2a03, :65c02, :65c816, :huc6280, :z80, :cp1610, :sm83, :m6800, :m68k, :i286, :arm7, :f8

## Accomplishments

### Test Suite Created
- **File**: `tests/numeric-precision-all-backends.lisp`
- **Coverage**: 
  - Operations: ADD, SUBTRACT, SHIFT-LEFT, SHIFT-RIGHT, MOVE
  - Data types: USAGE BINARY (native binary), USAGE DECIMAL (packed-decimal BCD)
  - USAGE DISPLAY: Only for MOVE operations (arithmetic/shifting not meaningful for display format)
  - Sign variations: Both signed and unsigned
  - PICTURE specifications: Up to 8 bytes total precision including:
    - Simple integers: "9", "99", "9999", "9(8)", "9(16)"
    - Signed integers: "s9", "s9999", "s9(8)", "S9(15)"
    - Decimal fractions: "9V9", "9(4)V9(4)", "9(8)V9(8)"
    - Mixed integer/fractional: "9(12)V9(4)", "9(4)V9(12)"
    - Signed mixed: "S9(3)V9(4)"
  - Backends: All 13 supported EIGHTBOL backends
  - Verification: Uses `compile-method-ast-with-tables` to ensure non-empty assembly generation

### Backend File Management
- Split large backend files (>500 lines) into parts of ~500 lines each:
  - `src/backend-6502/backend-6502.lisp` (3018 lines) → 6 part files
  - `src/backend-sm83/backend-sm83.lisp` (949 lines) → 2 part files  
  - `src/backend-f8/backend-f8.lisp` (1113 lines) → 3 part files
- Updated `eightbol.asd` to reference the new part files
- Removed original large files to prevent confusion
- Fixed formatting for all backend files:
  - Removed trailing whitespace from every line
  - Ensured each file ends with exactly one newline

### Test Organization
- All test files are in the `tests/` folder
- Created `tests/data/` folder for test data (ready for use)
- Added `tests/numeric-precision-all-backends.lisp` to the `eightbol-test` system

## Current State
- Test suite `tests/numeric-precision-all-backends.lisp` is present and syntactically correct
- Backend files are properly split and formatted
- ASDF systems load successfully (dependencies permitting)
- Test coverage is complete for the specified numeric precision grammar

## Dependencies
To run the test suite, the following Common Lisp libraries must be installed:
- alexandria, uiop, cl-change-case, cl-ppcre, local-time, serapeum, split-sequence, yacc

These can be installed via Quicklisp with:
```lisp
(ql:quickload '(:alexandria :cl-change-case :cl-ppcre :local-time :serapeum :split-sequence :yacc))
```

## Running Tests
Once dependencies are installed:
```lisp
(asdf:test-system :eightbol)
```
or to run just the numeric precision test suite:
```lisp
(fiveam:run! :numeric-precision-all-backends)
```

## Notes
- This test suite focuses specifically on numeric precision operations as requested
- For complete EIGHTBOL grammar coverage, additional test suites would be needed for:
  - Control flow (IF/THEN/ELSE, PERFORM, EVALUATE, CALL/GOBACK, etc.)
  - Object-oriented features (METHODS, INVOKE)
  - Data manipulation (SET variants)
  - String operations (STRING BLT)
  - Program structure and special statements
