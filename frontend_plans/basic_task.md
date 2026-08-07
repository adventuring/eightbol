# BASIC —- Beginners' All-purpose Symbolic Instruction Code Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-basic

## Files to Create/Update
- basic-lexer.lisp
- basic-parser.lisp  
- basic-shell.lisp
- basic-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the BASIC —- Beginners' All-purpose Symbolic Instruction Code chapter in EIGHTBOL.texi:

- Line numbers required for GOTO/GOSUB
- Keywords: IF, THEN, ELSE, ENDIF, WHILE, WEND, FOR, TO, STEP, NEXT, DO, LOOP, UNTIL, EXIT, SUB, END SUB, FUNCTION, END FUNCTION, CALL, RETURN, GOSUB
- Standard infix operators: +, -, *, /, ^, =, <>, <, >, <=, >=, AND, OR, NOT
- String delimiters: Double or single quotes
- Variable naming: case-insensitive (typically UPPER_CASE)
- SUB and FUNCTION procedures for modular code
- WHILE/WEND, FOR/NEXT, DO/LOOP constructs
- Array handling with parentheses indexing

## Implementation Tasks
1. Create/update lexer to tokenize BASIC —- Beginners' All-purpose Symbolic Instruction Code source code
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
