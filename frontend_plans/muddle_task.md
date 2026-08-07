# Muddle — MIT Design Language “MDL” Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-muddle

## Files to Create/Update
- muddle-lexer.lisp
- muddle-parser.lisp  
- muddle-shell.lisp
- muddle-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the Muddle — MIT Design Language “MDL” chapter in EIGHTBOL.texi:

- Lisp-like language from MIT
- Keywords: .if, .else, .then, .endif, .while, .endwhile, .for, .endfor, .exit, .return, .define, .set, .get, .put, .call, .go, .throw, .catch, .finally
- Operators: +, -, *, /, ^, =, /=, <, >, <=, >=, and, or, not
- String delimiters: Double quotes or vertical bars
- Variable naming: case-insensitive (typically lower-case-with-hyphens or camelCase)
- Prefix notation for operations
- List and vector data structures
- Recursive function definitions
- Conditional expressions with .if/.then/.else/.endif
- Loop constructs with .while/.endwhile and .for/.endfor

## Implementation Tasks
1. Create/update lexer to tokenize Muddle — MIT Design Language “MDL” source code
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
