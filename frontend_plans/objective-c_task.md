# Objective-C — like C, but with objects Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-objective-c

## Files to Create/Update
- objective-c-lexer.lisp
- objective-c-parser.lisp  
- objective-c-shell.lisp
- objective-c-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the Objective-C — like C, but with objects chapter in EIGHTBOL.texi:

- Object-oriented extension of C
- Keywords: @interface, @implementation, @end, @public, @protected, @private, @package, if, else, switch, case, default, for, while, do, break, continue, return, goto, etc.
- Standard operators: +, -, *, /, %, =, ==, !=, <, >, <=, >=, &&, ||, !, &, |, ^, ~, <<, >>, etc.
- String delimiters: Double quotes or wide string literals
- Variable naming: case-sensitive (camelCase for variables, UpperCamelCase for classes)
- Class-based object orientation
- Message passing syntax: [receiver message]
- Pointer arithmetic and manual memory management
- Header (.h) and implementation (.m) file separation
- Protocols for defining interfaces

## Implementation Tasks
1. Create/update lexer to tokenize Objective-C — like C, but with objects source code
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
