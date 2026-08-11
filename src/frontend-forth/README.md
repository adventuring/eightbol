% Forth Frontend for EIGHTBOL

## Overview

The Forth frontend for EIGHTBOL enables compilation of Forth stack-based source code to EIGHTBOL intermediate representation and ultimately to assembly language for 8-bit and 16-bit platforms.

### Features

- **Complete Forth tokenization**: Stack operations, arithmetic, logic, comparison, control flow
- **Multiple number formats**: Decimal, hexadecimal ($ or 0x), octal (0o), binary (% or 0b), dword (d'WORD')
- **Identifier normalization**: Automatic conversion to SHOUT-CASE for symbol consistency
- **Dialogue/narrative support**: Extension words for SAY, NARRATE, SHOW, HIDE, FADE, MUSIC, SOUND
- **Memory operations**: Variable definitions, fetch (@), store (!), return stack ops (>R, R>, R@)
- **I/O operations**: Print (.), string output (.""), character input (KEY), line input (ACCEPT)
- **Word definitions**: Colon definitions (:...;) and constants
- **Comprehensive AST generation**: All Forth operations map to EIGHTBOL AST nodes

## File Structure

```
src/frontend-forth/
├── lexer.lisp         - Tokenization and lexical analysis
├── parser.lisp        - Token-to-AST parsing and compilation
├── forth-tests.lisp   - Unit tests and examples
├── lexer.texf         - Lexer documentation (TeXinfo)
└── parser.texf        - Parser documentation (TeXinfo)
```

## Usage

### Tokenizing Forth Source

```lisp
(forth-tokenize-source "DUP SWAP + .")
;; Returns: (((:word . "DUP") (:word . "SWAP") (:word . "+") (:word . ".")))
```

### Parsing to AST

```lisp
(let* ((source "DUP SWAP +")
       (tokens (forth-tokenize-source source)))
  (forth-parse-tokens tokens))
;; Returns: (:forth-program :definitions [...AST nodes...])
```

### Compiling from Source String

```lisp
(forth-compile-source "42 EMIT CR")
```

### Compiling from File

```lisp
(forth-compile-file "path/to/program.forth")
```

## Forth Word Reference

### Stack Operations

| Word | Stack Effect | Purpose |
|------|--------------|---------|
| DUP | (x -- x x) | Duplicate top |
| SWAP | (x y -- y x) | Exchange top two |
| DROP | (x -- ) | Discard top |
| OVER | (x y -- x y x) | Copy second to top |
| ROT | (x y z -- y z x) | Rotate |
| 2DUP | (x y -- x y x y) | Duplicate top two |
| 2SWAP | (x1 x2 y1 y2 -- y1 y2 x1 x2) | Swap pairs |
| 2DROP | (x y -- ) | Discard top two |

### Arithmetic

| Word | Stack Effect | Purpose |
|------|--------------|---------|
| + | (x y -- x+y) | Addition |
| - | (x y -- x-y) | Subtraction |
| * | (x y -- x*y) | Multiplication |
| / | (x y -- x/y) | Division |
| MOD | (x y -- x mod y) | Modulo |
| ABS | (x -- \|x\|) | Absolute value |
| NEGATE | (x -- -x) | Negate |

### Comparison

| Word | Stack Effect | Purpose |
|------|--------------|---------|
| = | (x y -- f) | Equal? |
| < | (x y -- f) | Less than? |
| > | (x y -- f) | Greater than? |
| <= | (x y -- f) | Less or equal? |
| >= | (x y -- f) | Greater or equal? |
| <> | (x y -- f) | Not equal? |
| 0= | (x -- f) | Zero? |
| 0< | (x -- f) | Negative? |
| 0> | (x -- f) | Positive? |

### Bitwise

| Word | Stack Effect | Purpose |
|------|--------------|---------|
| AND | (x y -- x&y) | Bitwise AND |
| OR | (x y -- x\|y) | Bitwise OR |
| XOR | (x y -- x^y) | Bitwise XOR |
| NOT | (x -- ~x) | Bitwise NOT |
| << | (x n -- x<<n) | Left shift |
| >> | (x n -- x>>n) | Right shift |

### Memory

| Word | Stack Effect | Purpose |
|------|--------------|---------|
| @ | (addr -- value) | Fetch from memory |
| ! | (value addr -- ) | Store to memory |
| +! | (incr addr -- ) | Add to memory |
| C@ | (addr -- byte) | Fetch byte |
| C! | (byte addr -- ) | Store byte |

### Return Stack

| Word | Stack Effect | R-Stack Effect | Purpose |
|------|--------------|----------------|---------|
| >R | (x -- ) | ( -- x) | Push to return stack |
| R> | ( -- x) | (x -- ) | Pop from return stack |
| R@ | ( -- x) | (x -- x) | Copy return stack top |

### I/O and Output

| Word | Stack Effect | Purpose |
|------|--------------|---------|
| . | (x -- ) | Print top of stack |
| ." text" | ( -- ) | Print string literal |
| EMIT | (char -- ) | Output character |
| CR | ( -- ) | Output newline |
| SPACE | ( -- ) | Output single space |
| SPACES | (n -- ) | Output N spaces |
| PAGE | ( -- ) | Clear screen |

### Input

| Word | Stack Effect | Purpose |
|------|--------------|---------|
| KEY | ( -- char) | Read single key |
| ACCEPT | ( -- count) | Read line into buffer |

### Variables and Constants

```forth
VARIABLE counter          \ Create variable named counter
0 counter !               \ Store 0 to counter
counter @ .               \ Fetch and print counter
10 counter +!             \ Add 10 to counter

CONSTANT MAX-SIZE         \ Create constant
42 CONSTANT ANSWER        \ Constants can have initialization
```

### Word Definitions

```forth
: DOUBLE                  \ Define word DOUBLE
  DUP + ;                 \ Duplicate and add (2x)

: SQUARE                  \ Define word SQUARE
  DUP * ;                 \ Duplicate and multiply (x^2)

: EMIT-ASCII              \ Custom character output
  65 EMIT ;               \ Output 'A' (ASCII 65)
```

### Dialogue/Narrative Extensions

```forth
SAY "Welcome to the adventure!"
NARRATE "The forest stretches before you."
SHOW character-sprite
HIDE background-sprite
FADE out
MUSIC "theme.mus"
SOUND "sword-clash.snd"
```

## Number Literal Formats

```forth
42                        \ Decimal
0xFF or $FF               \ Hexadecimal
0o777                     \ Octal
0b1010 or %1010           \ Binary
d'CHAR'                   \ Dword (4-character literal)
```

## Example Programs

### Simple Arithmetic

```forth
5 3 +                     \ Push 5, push 3, add (result: 8)
.                         \ Print result
CR                        \ Newline
```

### Using Variables

```forth
VARIABLE x
10 x !                    \ Set x = 10
VARIABLE y
20 y !                    \ Set y = 20
x @ y @ +                 \ Add x and y
.                         \ Print sum
```

### Word Definition

```forth
: TWICE                   \ Define word
  DUP + ;                 \ Duplicate and add

7 TWICE .                 \ Should print 14
```

### Dialogue Example

```forth
SAY "Hello, adventurer!"
CR
NARRATE "You stand before a fork in the road."
KEY DROP                  \ Wait for key press
SHOW map
MUSIC "exploration.mus"
```

## AST Node Types

The parser generates these AST node types:

- `:forth-push-literal` - Push numeric/string literal to stack
- `:forth-push-identifier` - Fetch variable and push to stack
- `:forth-stack-op` - Stack manipulation (DUP, SWAP, DROP, etc.)
- `:forth-arithmetic` - Arithmetic operation (+, -, *, /, MOD, etc.)
- `:forth-comparison` - Comparison operation (=, <, >, <=, >=, <>)
- `:forth-conditional` - IF/THEN/ELSE structure
- `:forth-loop` - Loop structure (BEGIN/UNTIL, BEGIN/WHILE, DO/LOOP)
- `:forth-variable` - Variable definition
- `:forth-fetch` - Memory fetch (@)
- `:forth-store` - Memory store (!)
- `:forth-print-value` - Print (.)
- `:forth-print-string` - Print string (.")
- `:forth-emit` - Emit character
- `:forth-print-cr` - Print newline
- `:forth-input-key` - Read key
- `:forth-input-line` - Read line
- `:forth-dialogue` - Narrative/dialogue output
- `:forth-word-def` - Word definition (:...;)
- `:forth-constant` - Constant definition
- `:forth-program` - Top-level program

## Implementation Notes

### Identifier Normalization

Forth identifiers are automatically normalized to SHOUT-CASE for consistency:

```
my-word       → MY_WORD
myVar         → MYVAR
counter       → COUNTER
hello_world   → HELLO_WORLD
```

### Comment Support

The lexer supports two comment styles:

```forth
\ This is a line comment (backslash to end of line)

( This is a parenthetical comment )
```

### String Handling

Strings can be delimited by either single or double quotes:

```forth
." This is a string"
." "Hello, World!""
```

## API Functions

### Lexer Functions

- `forth-normalize-identifier(s)` - Normalize identifier to SHOUT-CASE
- `forth-valid-identifier-p(s)` - Check if string is valid identifier
- `forth-lex-number(chars, pos)` - Lex number starting at position
- `forth-lex-line(line)` - Tokenize single line
- `forth-tokenize-source(source)` - Tokenize entire source file
- `forth-token-type(lexeme)` - Get token type classification
- `forth-get-keyword-token(lexeme)` - Get keyword symbol
- `forth-get-dialogue-token(lexeme)` - Get dialogue word symbol

### Parser Functions

- `forth-parse-tokens(tokens-per-line)` - Parse token lists to AST
- `forth-parse-line(ctx, tokens)` - Parse single line
- `forth-parse-token(ctx, token, idx, tokens)` - Parse single token
- `forth-compile-source(source)` - Compile source string to AST
- `forth-compile-file(filename)` - Compile .forth file to AST

### AST Constructors

- `make-forth-push-literal(value)`
- `make-forth-push-identifier(name)`
- `make-forth-stack-op(op-name, :args args)`
- `make-forth-arithmetic(op, :left l, :right r)`
- `make-forth-comparison(op, :left l, :right r)`
- `make-forth-conditional(cond, then, else)`
- `make-forth-loop(type, init, condition, body)`
- `make-forth-variable-def(name, :initial-value val)`
- `make-forth-fetch(address)`
- `make-forth-store(address, value)`
- `make-forth-print-value()`
- `make-forth-print-string(string)`
- `make-forth-print-char()`
- `make-forth-print-cr()`
- `make-forth-input-key()`
- `make-forth-input-line(max-length)`
- `make-forth-dialogue(type, :speaker s, :text t, :options o)`
- `make-forth-word-def(name, params, body)`
- `make-forth-constant-def(name, value)`
- `make-forth-program(definitions)`

## Testing

Unit tests are provided in `forth-tests.lisp`. Run with:

```lisp
(run-all-forth-tests)
```

Tests cover:
- Basic tokenization
- Number literal parsing (all formats)
- Identifier normalization
- String tokenization
- Comment handling
- Identifier validation
- Program parsing
- Stack operation parsing
- Arithmetic parsing
- Dialogue parsing

## Bugs and TODOs

- TODO: Structured control flow (BEGIN/UNTIL, BEGIN/WHILE, DO/LOOP +LOOP)
- TODO: Tail-call optimization for word definitions
- TODO: Constant folding during parsing
- XXX: String escape sequences not yet implemented
- FIXME: User-defined word recursion validation pending

## Documentation

- `lexer.texf` - Lexer API documentation (TeXinfo format)
- `parser.texf` - Parser API documentation (TeXinfo format)

These .texf files can be included in the EIGHTBOL developer guide.
