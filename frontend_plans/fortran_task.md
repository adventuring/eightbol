# FORTRAN — autmatic FORmula TRANslator Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-fortran

## Files to Create/Update
- fortran-lexer.lisp
- fortran-parser.lisp  
- fortran-shell.lisp
- fortran-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the FORTRAN — autmatic FORmula TRANslator chapter in EIGHTBOL.texi:

- Fixed format or free format
- Keywords: PROGRAM, SUBROUTINE, FUNCTION, END, IF, THEN, ELSE, ENDIF, DO, CONTINUE, ENDDO, CALL, RETURN, PAUSE, STOP, READ, WRITE, PRINT, FORMAT, DIMENSION, COMMON, EQUIVALENCE, DATA, INTEGER, REAL, DOUBLE PRECISION, COMPLEX, LOGICAL, CHARACTER, PARAMETER, EXTERNAL, INTRINSIC
- Standard operators: +, -, *, /, **, =, .EQ., .NE., .LT., .LE., .GT., .GE., .NOT., .AND., .OR., .EQV., .NEQV.
- Variable naming: case-insensitive (typically UPPER_CASE or mixed_case)
- Strong typing with variable declarations
- DO loops for iteration
- IF/THEN/ELSE/ENDIF for conditionals
- Arrays with parentheses indexing
- FUNCTION and SUBROUTINE procedures

## Implementation Tasks
1. Create/update lexer to tokenize FORTRAN — autmatic FORmula TRANslator source code
2. Create/update parser to convert tokens to AST
3. Create/update shell for AST preprocessing (macros, semantic checks)
4. Create/update transpiler to convert AST to internal EIGHTBOL AST format
5. Create make-parser.lisp for parser construction
6. Update package.lisp to export new functions
7. Update main.lisp to add language dispatch
8. Ensure proper handling of:
   - Variable assignment and scoping
   - Arithmetic operations (+, -, bit shifts)
   - Boolean expressions and comparisons
   - Control structures (if/then/else, loops)
   - Function/subroutine calls
   - Object-oriented features (where applicable)
   - COPY statement handling
   - Accumulator-based calls (:call-acc)
   - Loop constructs with :perform :body
   - Break/continue statements

## Testing
- Create test cases in tests/ directory
- Verify AST generation matches expected patterns
- Ensure compatibility with all backends
