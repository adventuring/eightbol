# Fountain Frontend Lexer & Parser

## Overview

This directory contains a complete lexer and parser for the Fountain screenplay format as used in Phantasia game scripting. The implementation provides:

- **Tokenization** of Fountain screenplay files (.fountain, .screenplay, .teleplay, .play)
- **Identifier normalization** to PascalCase conventions
- **Number literal support** for decimal, hexadecimal (0x), octal (0o), binary (0b), and dword (0d"WORD") formats
- **AST generation** with specialized nodes for screenplay elements
- **Dialogue, action, and transition** handlers for interactive storytelling

## Files

- **`package.lisp`** (761 bytes) - Package definition and exports
- **`lexer.lisp`** (19 KB) - Tokenizer for Fountain screenplay format
- **`parser.lisp`** (19 KB) - Parser producing AST nodes from token stream
- **`tests.lisp`** (11 KB) - Comprehensive unit tests

## Key Features

### 1. Lexer (`lexer.lisp`)

The lexer tokenizes Fountain source code into a sequence of tokens:

```lisp
(lex-fountain-source "INT TAVERN - MAIN ROOM")
;; => ((:INT "INT" 1 0) (:IDENTIFIER "Tavern" 1 4) ...)
```

#### Number Literal Support

- **Decimal**: `42` → `:decimal-number` token with value `42`
- **Hex**: `0xFF` → `:hex-number` with value `255`
- **Octal**: `0o77` → `:octal-number` with value `63`
- **Binary**: `0b1111` → `:binary-number` with value `15`
- **Dword**: `0d"TEST"` → `:dword-number` with computed value

#### Identifier Normalization

All identifiers are automatically converted to PascalCase:

```lisp
(to-pascal-case "my-variable-name") ;; => "MyVariableName"
(to-pascal-case "my_variable_name") ;; => "MyVariableName"
(to-pascal-case "myVariableName")   ;; => "MyVariableName"
```

#### Special Tokens

- **Keywords**: INT, EXT, ENTER, SET, WHEN, UNLESS, PRINT, INPUT, DIALOGUE, etc.
- **Variables**: `$gold` tokenized as `:variable` with value `"Gold"` (PascalCased)
- **String literals**: Double and single quoted strings
- **Comments**: Line comments (;;) and block comments ([[ ]])
- **Operators**: +, -, *, /, =, <, >, (, ), [, ], {, }, comma, dot, etc.

### 2. Parser (`parser.lisp`)

The parser transforms token streams into Abstract Syntax Tree (AST) nodes:

```lisp
(parse-fountain-source "INT TAVERN - MAIN ROOM")
;; => (:PROGRAM :SCENES NIL :STATEMENTS
;;     ((:SCENE :LOCATION "Tavern" :MAP "Main Room" :SCENE-TYPE :NORMAL ...)))
```

#### Supported AST Node Types

- **`:scene`** - Scene headers (sluglines) defining locations and maps
- **`:dialogue`** - Character dialogue with optional parentheticals
- **`:action`** - Stage directions and gameplay actions
- **`:transition`** - Fade, cut, and other scene transitions
- **`:character-entry`** - Character appearances with initial modifiers (looks, faces, equips)
- **`:variable-assignment`** - SET $variable TO expression statements
- **`:conditional`** - WHEN/UNLESS conditional blocks
- **`:print`** - PRINT statement for output
- **`:input`** - INPUT statement for user interaction
- **`:expression`** - Binary/unary expressions and operations
- **`:program`** - Top-level program node containing scenes and statements

#### Screenplay Element Handlers

- **Dialogue**: Extracts speaker, dialogue text, and parenthetical actions
- **Print**: Handles output of strings, variables, and expressions
- **Input**: Processes user input with optional prompts
- **Character Entry**: Parses character appearances with location and state modifiers
- **Conditionals**: Supports WHEN and UNLESS conditions with nested statements

### 3. AST Node Constructors

All AST nodes are constructed using dedicated functions following Phantasia conventions:

```lisp
(make-scene-node "Tavern" "MainRoom" :scene-type :normal)
(make-dialogue-node "PLAYER" "Welcome!" :parenthetical "(cheerfully)")
(make-character-entry-node "INNKEEPER" "Bar Counter" :modifiers '((:looks "worried")))
(make-variable-assignment-node "Gold" 100)
(make-conditional-node condition then-block :else-block else-block)
(make-print-node (list "You have: " "$Gold" " gold pieces"))
(make-input-node "Enter name: " '("PlayerName"))
```

## API Reference

### Lexer Functions

```lisp
(lex-fountain-source source)
  ;; Tokenize SOURCE string, return token list
  ;; Tokens are (type value line column) lists

(lex-fountain-file filepath)
  ;; Read and tokenize file from FILEPATH

(make-token type value line column)
  ;; Create individual token

(token-type token)
(token-value token)
(token-line token)
(token-column token)
  ;; Token accessors
```

### Parser Functions

```lisp
(parse-fountain-source source)
  ;; Returns (values ast error-list)

(parse-fountain-file filepath)
  ;; Returns (values ast error-list)

(parse-fountain-tokens tokens)
  ;; Returns (values ast error-list)
```

### Utility Functions

```lisp
(to-pascal-case string)
  ;; Normalize identifier to PascalCase

(parse-number-literal string)
  ;; Returns (values numeric-value token-type success-p)

(valid-identifier-p string)
  ;; Check if string is valid Fountain identifier
```

## Example Usage

```lisp
;; Load the system
(load "/path/to/src/frontend-fountain/package.lisp")
(load "/path/to/src/frontend-fountain/lexer.lisp")
(load "/path/to/src/frontend-fountain/parser.lisp")

;; Parse a simple screenplay
(multiple-value-bind (ast errors)
    (parse-fountain-source
      "INT TAVERN - MAIN ROOM
       
       Enter INNKEEPER at \"Bar Counter\"
       
       PLAYER
       Welcome to the tavern!
       
       INNKEEPER
       Hello, traveler! What brings you here?
       
       Set $conversation-started to 1
       
       FADE OUT")
  (when errors
    (format t "Parse errors: ~A~%" errors))
  (format t "AST: ~S~%" ast))
```

## Testing

Unit tests cover:

1. **Identifier Normalization** (8 tests)
   - Hyphen/underscore conversion
   - Mixed case handling
   - Complex multi-word identifiers

2. **Number Literals** (5 tests)
   - Decimal, hex, octal, binary parsing
   - Invalid format detection

3. **Lexer** (15 tests)
   - Token type recognition
   - String/variable/keyword tokenization
   - Comment handling
   - Multi-line scripts
   - Operators and punctuation

4. **Parser** (10 tests)
   - Scene header parsing
   - Dialogue block parsing
   - Character entry parsing
   - Variable assignment
   - Conditional parsing
   - Print/Input statements
   - BLOB scenes
   - Complex multi-element scenes
   - Transitions

To run tests (when loaded with lexer/parser):
```lisp
(run-all-tests)           ;; All tests
(run-normalization-tests) ;; Identifier normalization
(run-number-literal-tests);; Number literal parsing
(run-lexer-tests)         ;; Lexer tokenization
(run-parser-tests)        ;; Parser AST generation
```

## Design Principles

Following Phantasia project conventions:

- **PascalCase identifiers** for functions, classes, and node types
- **Lisp-style documentation** with doc-strings
- **Keyword arguments** for optional parameters
- **S-expression AST** using lists with keyword prefixes
- **Error handling** with clear error messages and line/column tracking
- **Modular design** with separate lexer/parser/AST layers

## Lexer Token Types

```
Keywords: :INT :EXT :FADE :OUT :CUT :TO :ENTER :AT :SET :WHEN :UNLESS
         :PRINT :INPUT :DIALOGUE :GREATER :LESS :EQUAL :THEN :AND :OR :NOT
         :NORTH :SOUTH :EAST :WEST :OF :LOOKS :FACING :EQUIPS :IS
         :BLOB :FLOOR :CEILING :ROUND :MODULO :ABS :MIN :MAX

Literals: :decimal-number :hex-number :octal-number :binary-number :dword-number
         :string-literal :variable :identifier

Punctuation: :lparen :rparen :lbracket :rbracket :lbrace :rbrace
            :comma :dot :equal :plus :minus :star :slash :gt :lt :bang

Operators:  :ampersand :pipe :caret :transition :ellipsis

Special:   :newline :line-comment :block-comment :unknown
```

## Performance Notes

- Lexer operates in O(n) time where n = source length
- Parser uses recursive descent with O(n) token stream processing
- Supports large screenplay files (tested with multi-thousand-line scripts)
- Memory efficient token stream (minimal copying)

## Known Limitations & Future Enhancements

- Macros and template expansion not yet supported
- No semantic analysis (type checking, scope validation)
- Limited error recovery in parser
- No optimization of AST nodes
- Comments currently stripped during lexing (could preserve in future)

## Contributing

When modifying:

1. Update corresponding `.texf` documentation file (future)
2. Add test cases for new features
3. Follow existing code style (Lisp conventions from SkylineTool)
4. Ensure `make test` passes
5. Update this README with significant changes

## See Also

- **Fountain Scripting Manual**: `Manual/FountainScripting.tex`
- **SkylineTool Frontend Examples**: `SkylineTool/eightbol/src/frontend-basic/`
- **EIGHTBOL Parser Reference**: `SkylineTool/eightbol/src/frontend-basic/basic-parser.lisp`
- **Phantasia Developer Guide**: `Source/Documentation/PhantasiaDevGuide.texi`
