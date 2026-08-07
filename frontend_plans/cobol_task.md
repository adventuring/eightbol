# COBOL — COmmon Business Oriented Language Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-cobol

## Files to Create/Update
- cobol-lexer.lisp
- cobol-parser.lisp  
- cobol-shell.lisp
- cobol-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the COBOL — COmmon Business Oriented Language chapter in EIGHTBOL.texi:

- Four divisions: Identification, Environment, Data, Procedure
- Verbose English-like syntax
- Keywords: ACCEPT, ADD, CALL, CANCEL, COMPUTE, DELETE, DISPLAY, DIVIDE, ENTRY, EVALUATE, IF, MOVE, MULTIPLY, OPEN, PERFORM, READ, RETURN, REWRITE, SEARCH, SET, SORT, START, STOP, STRING, SUBTRACT, UNSTRING, WRITE
- Standard operators: +, -, *, /, **, =, >, <, >=, <=, <> (or NOT=), AND, OR, NOT
- Data Division with PIC clauses for variable definitions
- Procedural division with statements like MOVE, ADD, SUBTRACT, PERFORM
- PERFORM loops with various forms (TEST BEFORE/AFTER, VARYING, etc.)
- COPY statement for copybook inclusion
- Object-oriented support with CLASS division

## Implementation Tasks
1. Create/update lexer to tokenize COBOL — COmmon Business Oriented Language source code
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
