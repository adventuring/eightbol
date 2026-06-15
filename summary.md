
## Backend File Management
- Split large backend files (>500 lines) into parts of ~500 lines each:
  - `src/backend-6502/backend-6502.lisp` (3018 lines) → split into 6 part files (each 473-543 lines)
  - Updated `eightbol.asd` to reference the new part files instead of the original
  - Removed the original large file to avoid confusion
- Fixed formatting for all backend files (including the parts and other backends):
  - Removed trailing whitespace from every line
  - Ensured each file ends with exactly one newline
  - Processed 19 backend files in parallel (well under the 40 limit)

## Test Data Organization
- All test files are in the `tests/` folder
- Created `tests/data/` folder for any test data (though none was required for this work)

## Dependencies Note
The test suite requires the following Common Lisp libraries to run:
- alexandria, uiop, cl-change-case, cl-ppcre, local-time, serapeum, split-sequence, yacc
These must be installed (e.g., via Quicklisp or system package manager) before executing the test suite.
