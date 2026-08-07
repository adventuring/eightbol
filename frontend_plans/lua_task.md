# Lua Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-lua

## Files to Create/Update
- lua-lexer.lisp
- lua-parser.lisp  
- lua-shell.lisp
- lua-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the Lua chapter in EIGHTBOL.texi:

- Lightweight, embeddable scripting language
- Keywords: and, break, do, else, elseif, end, false, for, function, if, in, local, nil, not, or, repeat, return, then, true, until, while
- Standard operators: +, -, *, /, ^, %, ==, ~=, <, >, <=, >=, and, or, not, # (length)
- String delimiters: Double quotes, single quotes, or long brackets
- Variable naming: case-sensitive (typically snake_case or camelCase)
- Tables as primary data structure (arrays, dictionaries, objects)
- Functions as first-class values
- for, while, repeat loops
- if/then/else/end conditionals
- Local and global variable scoping
- Object-oriented programming via tables and metatables

## Implementation Tasks
1. Create/update lexer to tokenize Lua source code
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
