# SmallTalk Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-smalltalk

## Files to Create/Update
- smalltalk-lexer.lisp
- smalltalk-parser.lisp  
- smalltalk-shell.lisp
- smalltalk-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the SmallTalk chapter in EIGHTBOL.texi:

- Pure object-oriented language
- Keywords: self, super, true, false, nil, thisContext, ifTrue:, ifFalse:, ifTrue:ifFalse:, whileTrue:, whileFalse:, timesRepeat:, to:do:, to:by:do:
- Operators: +, -, *, /, //, \\, =, ~=, >, <, >=, <=, and, or, not
- String delimiters: Single quotes for strings, double quotes for comments
- Variable naming: case-sensitive (typically camelCase for variables, UpperCamelCase for classes)
- Everything is an object
- Computation via message passing
- Blocks (closures) for control structures
- Dynamic typing
- Image-based persistence
- Metaobject protocol

## Implementation Tasks
1. Create/update lexer to tokenize SmallTalk source code
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
