# Lingo — Macromedia Director Lingo script Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-lingo

## Files to Create/Update
- lingo-lexer.lisp
- lingo-parser.lisp  
- lingo-shell.lisp
- lingo-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the Lingo — Macromedia Director Lingo script chapter in EIGHTBOL.texi:

- Event-driven scripting language
- Keywords: if, else, then, end if, repeat, while, for, to, step, end repeat, exit, return, on, end, global, property, set, get, put, putprop, getprop, delete, send, call, invoke, new, free, copy, rename, lock, unlock, member, sprite, frame, movie, sound, keyboard, mouse, rect, point
- Standard operators: +, -, *, /, ^, =, <>, <, >, <=, >=, and, or, not, &&, ||
- String delimiters: Double or single quotes
- Variable naming: case-insensitive (typically lowerCase or camelCase)
- Handlers (functions) triggered by events
- Property-based object manipulation
- repeat loops for iteration
- if/then/else/end if for conditionals
- Object instantiation with new()

## Implementation Tasks
1. Create/update lexer to tokenize Lingo — Macromedia Director Lingo script source code
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
