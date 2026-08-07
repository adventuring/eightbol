# Burgermistress — the Thief of Fate scripting language by Burger Becky Frontend Implementation

## Directory
/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/src/frontend-burgermistress

## Files to Create/Update
- burgermistress-lexer.lisp
- burgermistress-parser.lisp  
- burgermistress-shell.lisp
- burgermistress-transpile.lisp
- make-parser.lisp

## Key Features to Implement
Based on the Burgermistress — the Thief of Fate scripting language by Burger Becky chapter in EIGHTBOL.texi:

- Similar to BASIC but with different keywords
- Keywords: if, else, then, endif, while, wend, for, to, step, next, do, loop, until, exit, sub, end sub, function, end function, call, return, gosub, return
- Standard infix operators
- Variable naming: lowerCamelCase
- Function naming: UpperCamelCase
- Similar control structures to BASIC

## Implementation Tasks
1. Create/update lexer to tokenize Burgermistress — the Thief of Fate scripting language by Burger Becky source code
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
