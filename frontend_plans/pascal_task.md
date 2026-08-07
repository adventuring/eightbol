# Pascal Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-pascal

## Files to Create/Update
- pascal-lexer.lisp
- pascal-parser.lisp  
- pascal-shell.lisp
- pascal-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the Pascal chapter in EIGHTBOL.texi:

- Structured programming language
- Keywords: and, array, begin, case, const, destructor, division, do, downto, else, end, file, for, function, goto, if, implementation, in, label, mod, nil, not, object, of, or, packed, procedure, program, record, repeat, set, shl, shr, string, then, to, type, unit, until, uses, var, while, with, xor
- Operators: +, -, *, /, div, mod, =, <>, <, >, <=, >=, and, or, not, <<, >>
- String delimiters: Single or double quotes
- Variable naming: case-insensitive (typically CamelCase or snake_case)
- Strong typing with explicit variable declarations
- Nested procedures and functions
- Control structures: if/then/else, case, for, while, repeat..until
- Pointers and dynamic memory allocation
- Sets and subranges
- Records (structures)

## Implementation Tasks
1. Create/update lexer to tokenize Pascal source code
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
