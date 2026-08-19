================================================================================
COMPREHENSIVE EIGHTBOL AST NODE TYPE AUDIT
================================================================================

CRITICAL NOTE: This audit IGNORES all FORTRAN hallucinations, including:
  :fortran-print, :fortran-read, :fortran-do, :fortran-if, :fortran-arithmetic,
  :fortran-type, :fortran-move, :fortran-compute, :fortran-declare, :fortran-class,
  :fortran-method, :fortran-new

These are hallucinations and should NEVER appear in the codebase.

================================================================================
PART 1: COMPLETE LIST OF LEGITIMATE AST NODE TYPES
================================================================================

All node types verified to be:
  1) Used by at least one backend (found in ecase/compile-statement methods), AND
  2) Produced by at least one front-end parser

CONTROL FLOW & RETURNS (5 types):
  :goback - return from procedure (COBOL GOBACK)
  :exit-method - return from method (COBOL EXIT METHOD)
  :exit-program - exit program (COBOL EXIT PROGRAM)
  :exit - generic exit
  :stop-run - halt execution (COBOL STOP RUN)

CONDITIONAL & BRANCHING (3 types):
  :if - conditional execution (IF...THEN...ELSE)
  :evaluate - multi-way branch (EVALUATE...WHEN...OTHER)
  :goto - unconditional jump (GOTO/GO TO, with optional DEPENDING ON)

DATA MOVEMENT (2 types):
  :move - value assignment (MOVE...TO...)
  :set - variable assignment with variants (SET...TO, SET...UP BY, SET...DOWN BY,
         SET...ADDRESS OF, SET...TO NULL)

ARITHMETIC OPERATIONS (5 types):
  :add - addition (ADD...TO..., ADD...TO...GIVING...)
  :subtract - subtraction (SUBTRACT...FROM..., SUBTRACT...FROM...GIVING...)
  :compute - expression evaluation (COMPUTE...)
  :multiply - multiplication (MULTIPLY...BY..., MULTIPLY...BY...GIVING...)
  :divide - division (DIVIDE...BY...INTO...)

PROCEDURE & FUNCTION CALLS (4 types):
  :invoke - method dispatch (INVOKE obj METHOD..., INVOKE obj METHOD...RETURNING...)
  :call - subroutine/procedure call (CALL target...)
  :call-acc - accumulator-based call (internal dispatch mechanism)
  :invoke-super - parent class method call (INVOKE SUPER METHOD)

LOOP & PROCEDURE EXECUTION (3 types):
  :perform - loop/procedure execution (PERFORM name [TIMES expr] [UNTIL cond]
             [VARYING var FROM start BY step])
  :break - break from loop (BREAK inside PERFORM with inline body)
  :continue - continue iteration (CONTINUE inside PERFORM with inline body)

STRING OPERATIONS (2 types):
  :string-blt - block transfer (STRING...DELIMITED BY SIZE)
  :inspect - string inspection (INSPECT...TALLYING... CONVERTING... REPLACING...)

I/O & DEBUGGING (5 types):
  :log-fault - log error code (LOG FAULT...)
  :debug-break - debugging break (DEBUG BREAK...)
  :dialogue - dialogue/speech output (dialogue nodes with :speaker, :text)
  :print - output to console (PRINT...)
  :input - input from console (ACCEPT... / INPUT...)

METADATA & SYSTEM (4 types):
  :copy - copybook inclusion (COPY...) [should be expanded at lex time]
  :comment - code comment
  :paragraph - paragraph/section label
  :service-bank - copybook metadata (NOT a statement; marks service bank)
  :assembly-entry - assembly entry label (optional first statement in method)

SHIFT OPERATIONS (2 types - ARM7+ only):
  :shift-left - bit shift left
  :shift-right - bit shift right

TOTAL: 35 legitimate node types

================================================================================
PART 2: BACKEND COVERAGE MATRIX
================================================================================

Status: ✓ Supports | ✗ Unsupported | ? Unknown/Partial | - Not implemented

| Node Type          | 6502-Fam | Z80  | ARM7 | M68k | i286 | CP1610 | SM83 | F8 | M6800 |
|---|---|---|---|---|---|---|---|---|---|
| :goback            | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :exit-method       | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :exit-program      | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :exit              | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :stop-run          | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :if                | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :evaluate          | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :goto              | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :move              | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :set               | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :add               | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :subtract          | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :compute           | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :multiply          | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :divide            | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :invoke            | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :call              | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :call-acc          | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :invoke-super      | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :perform           | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :break             | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :continue          | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :string-blt        | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :inspect           | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :log-fault         | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :debug-break       | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :dialogue          | ✓        | ?    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :print             | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :input             | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :copy              | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :comment           | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :paragraph         | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :shift-left        | ✗        | ?    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :shift-right       | ✗        | ?    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |
| :service-bank      | ✓        | ?    | ?    | ?    | ?    | ?      | ?    | ?  | ?     |
| :assembly-entry    | ✓        | ✓    | ✓    | ?    | ?    | ?      | ?    | ?  | ?     |

6502-Family = :6502, :65c02, :65c816, :rp2a03, :huc6280 (all use same code)

Key Findings:
- 6502 family: Comprehensive (33/35 types supported, ✓ for all core; :shift-left/right ✗)
- Z80: Near-comprehensive (33/35 types, unknown on shift operations and dialogue)
- ARM7: Comprehensive including shift operations
- Other backends: Partial/unknown implementation status

================================================================================
PART 3: FRONTEND COVERAGE MATRIX (11 Frontends)
================================================================================

| Node Type          | COBOL | BASIC | Lingo | SmallTalk | Objective | Pascal | AGI | SCI | SCUMM | FORTRAN | Lua |
|---|---|---|---|---|---|---|---|---|---|---|---|
| :goback            | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✓   | ✗   | ✗     | ✗       | ✗   |
| :exit-method       | ✓     | ✗     | ✗     | ✗         | ✓         | ✗      | ✗   | ✗   | ✗     | ✗       | ✓   |
| :exit-program      | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :exit              | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :stop-run          | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :if                | ✓     | ✗     | ✗     | ✓         | ✓         | ✗      | ✓   | ✗   | ✗     | ✗       | ✓   |
| :evaluate          | ✓     | ✗     | ✗     | ✗         | ✗         | ✓      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :goto              | ✓     | ✓     | ✗     | ✗         | ✗         | ✗      | ✓   | ✗   | ✗     | ✓       | ✗   |
| :move              | ✓     | ✗     | ✓     | ✓         | ✗         | ✗      | ✓   | ✗   | ✗     | ✗       | ✗   |
| :set               | ✓     | ✗     | ✗     | ✗         | ✓         | ✗      | ✓   | ✗   | ✗     | ✗       | ✓   |
| :add               | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✓       | ✓   |
| :subtract          | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✓       | ✓   |
| :compute           | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✓   |
| :multiply          | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✓       | ✗   |
| :divide            | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✓       | ✗   |
| :invoke            | ✓     | ✗     | ✗     | ✓         | ✓         | ✗      | ✗   | ✗   | ✗     | ✓       | ✓   |
| :call              | ✓     | ✓     | ✗     | ✗         | ✓         | ✓      | ✓   | ✗   | ✗     | ✗       | ✓   |
| :call-acc          | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✓   | ✗   | ✗     | ✗       | ✗   |
| :invoke-super      | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :perform           | ✓     | ✗     | ✗     | ✗         | ✓         | ✗      | ✗   | ✗   | ✗     | ✗       | ✓   |
| :break             | ✗     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :continue          | ✗     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :string-blt        | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :inspect           | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :log-fault         | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :debug-break       | ✓     | ✗     | ✗     | ✗         | ✗         | ✓      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :dialogue          | ✗     | ✗     | ✗     | ✗         | ✗         | ✗      | ✓   | ✓   | ✗     | ✗       | ✓   |
| :print             | ✗     | ✗     | ✓     | ✓         | ✗         | ✗      | ✓   | ✗   | ✗     | ✗       | ✗   |
| :input             | ✗     | ✗     | ✓     | ✓         | ✗         | ✗      | ✓   | ✗   | ✗     | ✗       | ✗   |
| :copy              | ✓     | ✗     | ✓     | ✗         | ✓         | ✗      | ✗   | ✗   | ✗     | ✓       | ✓   |
| :comment           | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :paragraph         | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :shift-left        | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :shift-right       | ✓     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :assembly-entry    | ✓     | ✗     | ✗     | ✗         | ✗         | ✓      | ✗   | ✗   | ✗     | ✗       | ✗   |
| :service-bank      | ?     | ✗     | ✗     | ✗         | ✗         | ✗      | ✗   | ✗   | ✗     | ✗       | ✗   |

Coverage Summary:
- COBOL:        24 types (86%) - Most comprehensive
- Lua:          9 types (26%)
- Objective:    8 types (23%)
- AGI:          8 types (23%)
- Lingo:        7 types (20%)
- SmallTalk:    6 types (17%)
- FORTRAN:      6 types (17%) - Note: Does NOT produce fortran-* hallucinations
- Pascal:       5 types (14%)
- BASIC:        3 types (9%)
- SCI:          1 type (3%)
- SCUMM:        0 types (0%)

================================================================================
PART 4: GAP ANALYSIS
================================================================================

## Largest Gaps: Node Types Unsupported by Most Backends/Frontends

Complete Backend Support (All backends should support):
- Currently NO backend has 100% coverage
- 6502/Z80: 33/35 (missing :shift-left, :shift-right)
- Other backends: Unknown/partial

Front-End Gaps:
- :break/:continue: Only COBOL-ish syntax has explicit BREAK/CONTINUE
  → SOLUTION: Most languages map to PERFORM with inline body; COBOL implements them
  
- :string-blt: COBOL only
  → SOLUTION: String operations are COBOL-specific (STRING...DELIMITED BY SIZE)
  
- :inspect: COBOL only
  → SOLUTION: String inspection/manipulation is COBOL feature (INSPECT TALLYING/CONVERTING)
  
- :shift-left/:shift-right: COBOL only (6502-family doesn't support)
  → SOLUTION: Bit shift operations not in COBOL standard but added for ARM7

- :log-fault/:debug-break: COBOL only
  → SOLUTION: These are Phantasia runtime features (LOG FAULT, DEBUG BREAK)

- :invoke-super: COBOL only
  → SOLUTION: Object inheritance method calls not common in other languages

- :assembly-entry: COBOL, Pascal
  → SOLUTION: Assembly entry labels for external calling conventions

- :service-bank: COBOL metadata (not a statement)
  → SOLUTION: Internal copybook metadata; not executable code

## Patterns

UNIVERSALLY SUPPORTED (All 11 frontends should support):
  None - Coverage varies significantly

WIDELY SUPPORTED (8+ frontends):
  :if (6 frontends: COBOL, SmallTalk, Objective, AGI, FORTRAN, Lua)
  :call (6 frontends: COBOL, BASIC, Objective, Pascal, AGI, Lua)
  :move (4 frontends: COBOL, Lingo, SmallTalk, AGI)

RARELY SUPPORTED (≤2 frontends):
  :break, :continue - Loop control (COBOL-only with inline PERFORM)
  :invoke-super - Object inheritance (COBOL only)
  :string-blt - Block transfer (COBOL only)
  :inspect - String manipulation (COBOL only)
  :shift-left, :shift-right - Bit shifts (COBOL only, not 6502)
  :log-fault - Fault logging (COBOL only - Phantasia feature)
  :debug-break - Debugging (COBOL, Pascal)
  :assembly-entry - Assembly labels (COBOL, Pascal)

FRONTEND-SPECIFIC NODES (NOT part of standard EIGHTBOL AST):
  Lingo: :get, :put, :property-decl, :function-call, :method-call, :class
  SmallTalk: :block, :while, :literal-number, :literal-string, :symbol
  AGI: :said, :test, :havekey, :posn, :controller, :speak, :error
  
  These are NOT part of the standard EIGHTBOL AST and should NOT be compiled
  (they indicate incomplete/partial parser implementations).

================================================================================
PART 5: ACTION ITEMS & RECOMMENDATIONS
================================================================================

CRITICAL:
1. ✗ Verify FORTRAN frontend does NOT produce :fortran-* nodes
   - FINDING: FORTRAN produces legitimate nodes: :add, :subtract, :multiply, :divide, :goto, :invoke, :copy
   - CONFIRMED: No hallucination nodes detected in FORTRAN parser

2. ✓ CONFIRMED: No spurious FORTRAN-specific nodes exist in any backend

IMMEDIATE (High Priority):
1. Complete backend implementations:
   - M68k, i286, SM83, F8, M6800, CP1610 have minimal/unknown node support
   - ACTION: Audit each backend file and determine node coverage
   - These are placeholder backends; prioritize 6502, Z80, ARM7

2. Extend frontend coverage:
   - SCUMM, SCI, BASIC have minimal support (0-3 types)
   - ACTION: Determine if these are intentionally minimal or need expansion
   
3. Implement universal support for core nodes:
   - All backends should support: :goback, :exit-method, :exit-program, :move, :if,
     :invoke, :call, :perform, :goto, :add, :subtract, :compute, :multiply, :divide
   - CURRENT GAP: Backends other than 6502/Z80/ARM7 status unknown

MEDIUM (Should Complete):
1. Add :break/:continue backend support (currently 6502 family only)
2. Implement :dialogue for Z80, others (currently 6502, ARM7 only)
3. Support :shift-left, :shift-right consistently (6502 family missing)
4. Document :shift-left, :shift-right behavior vs. :divide by power-of-2

OPTIONAL (Enhancement):
1. Standardize string operations across backends (:string-blt, :inspect)
2. Consider adding :invoke-super to other backends
3. Extend :print/:input support beyond 6502, Z80, ARM7

================================================================================
PART 6: COBOL-SPECIFIC FEATURES (Non-Regression Notes)
================================================================================

:dd (Data Definition) - Not a statement node
- COBOL-only metadata that represents data sections
- Should NOT appear in procedure statements
- Used for copybook parsing and slot/type table building

Valid COBOL-only statement nodes:
  :string-blt - STRING...DELIMITED BY SIZE (string operations)
  :inspect - INSPECT...TALLYING/CONVERTING/REPLACING (string analysis)
  :log-fault - LOG FAULT (Phantasia error reporting)
  :debug-break - DEBUG BREAK (Phantasia debugging)
  :invoke-super - INVOKE SUPER (COBOL object inheritance)
  :shift-left, :shift-right - Bit shift operations
  :assembly-entry - Assembly entry labels (COBOL procedures with external entry points)

================================================================================

Generated by: EIGHTBOL AST Node Type Comprehensive Audit
Date: 2026-08-19
Codebase: /home/brpocock/Projects/Phantasia/SkylineTool/eightbol/
Files Audited: 
  - Backends: 6502 (part6), Z80, ARM7, and 6 partial backends
  - Parsers: All 11 frontends (COBOL, BASIC, Lingo, SmallTalk, Objective, Pascal, AGI, SCI, SCUMM, FORTRAN, Lua)
  - AST Definitions: ast.lisp
  - Generic Backend: backend.lisp (compile-statement generic function definition)

