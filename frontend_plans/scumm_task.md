# SCUMM — Script Creation Utility for Maniac Mansion Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-scumm

## Files to Create/Update
- scumm-lexer.lisp
- scumm-parser.lisp  
- scumm-shell.lisp
- scumm-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the SCUMM — Script Creation Utility for Maniac Mansion chapter in EIGHTBOL.texi:

- Script language for Maniac Mansion and LucasArts adventures
- Keywords: if, else, endif, while, endwhile, for, endfor, break, continue, return, define, set, get, put, call, exit, goto, label, assert, wait, stop, start, fade, restore, save, restart, version
- Operators: +, -, *, /, %, =, ==, !=, <, >, <=, >=, &&, ||, !
- String delimiters: Double quotes
- Variable naming: case-insensitive (typically lower_case_with_underscores)
- C-like syntax with semicolons
- Function definitions with to/end
- Control structures: if/else/endif, while/endwhile, for/endfor
- Arrays and lists with square bracket indexing
- Actor-based object manipulation

## Implementation Tasks
1. Create/update lexer to tokenize SCUMM — Script Creation Utility for Maniac Mansion source code
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
