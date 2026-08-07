# AGI — Adventure Game Interpreter language Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-agi

## Files to Create/Update
- agi-lexer.lisp
- agi-parser.lisp  
- agi-shell.lisp
- agi-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the AGI — Adventure Game Interpreter language chapter in EIGHTBOL.texi:

- Line-based syntax (no semicolons required)
- Keywords: IF, ELSE, THEN, GOSUB, GOTO, NEWROOM, LOADLOGICS, RETURN, QUIT, ASSIGN, INCREMENT, DECREMENT, SET, RESET, SAID, TEST, POSN, CONTROLLER, HAVEKEY
- String handling with double quotes
- Variable naming: lower_snake_case
- Function naming: dotted.snake.case
- CALL-ACC accumulator-based calls
- GOSUB/GOTO for subroutines
- IF/THEN/ELSE/END-IF conditionals
- SET, INCREMENT, DECREMENT for variable manipulation
- Object manipulation via SET with dotted notation (object.property)

## Implementation Tasks
1. Create/update lexer to tokenize AGI — Adventure Game Interpreter language source code
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
