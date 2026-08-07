# ZIL — Zork Implementation Language Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-zil

## Files to Create/Update
- zil-lexer.lisp
- zil-parser.lisp  
- zil-shell.lisp
- zil-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the ZIL — Zork Implementation Language chapter in EIGHTBOL.texi:

- Zork Implementation Language (Lisp-like)
- Keywords: <IF>, <ELSE>, <THEN>, <WHILE>, <FOR>, <BREAK>, <CONTINUE>, <RETURN>, <DEFINE>, <SET>, <SETG>, <GET>, <PUT>, <CALL>, <GO>, <EXIT>, <LABEL>, <ASSERT>, <WAIT>, <STOP>, <START>, <FADE>, <RESTORE>, <SAVE>, <RESTART>, <VERSION>, <OBJECT>, <PROPERTY>, <GLOBAL>, <LOCAL>
- Operators: +, -, *, /, =, <>, <, >, <=, >=, AND, OR, NOT
- String delimiters: Double quotes for strings, single quotes for atoms/characters
- Variable naming: case-insensitive (typically lower-case-with-hyphens)
- Prefix notation with angle brackets
- Object-oriented programming with objects and properties
- Z-machine execution environment
- Routine-based procedural programming
- Global (<SETG>) and local (<SET>) variables
- Object manipulation with <OBJECT>, <PROPERTY>, <P> (property access)

## Implementation Tasks
1. Create/update lexer to tokenize ZIL — Zork Implementation Language source code
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
