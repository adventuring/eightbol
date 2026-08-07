# SCI — Sierra Creative Interpreter language Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-sci

## Files to Create/Update
- sci-lexer.lisp
- sci-parser.lisp  
- sci-shell.lisp
- sci-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the SCI — Sierra Creative Interpreter language chapter in EIGHTBOL.texi:

- Lisp-like language for Sierra adventures
- Keywords: (if), (else), (then), (while), (for), (foreach), (break), (continue), (return), (define), (setq), (set), (get), (put), (call), (proc), (method), (class), (instance), (send), (super), (self), (print), (format), (strcat), (strlen), (substr), (upcase), (downcase), (numtostr), (strtonum), (+), (-), (*), (/), (=), (<>), (<), (>), (<=), (>=), (and), (or), (not)
- Operators: Same as keywords for arithmetic and logical operations
- String delimiters: Double quotes for strings, single quotes for symbols
- Variable naming: case-insensitive (typically lowerCase or upperCase for globals)
- Prefix notation (parentheses)
- Object-oriented programming with classes and instances
- Procedures (procs) and methods
- send keyword for object messaging
- Superclass method calls with super
- Conditional expressions with if/then/else
- Loop constructs with while, for, foreach

## Implementation Tasks
1. Create/update lexer to tokenize SCI — Sierra Creative Interpreter language source code
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
