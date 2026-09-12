# SCUMM Language Research & AST Audit

## Document Summary
This document presents comprehensive research on the SCUMM language (Script Creation Utility for Maniac Mansion) as used by LucasArts for their adventure games (Monkey Island, Day of the Tentacle, etc.). It includes the current EightBol implementation status, AST mapping, identified issues, and game-engine-specific features.

**Date:** September 9, 2026  
**Scope:** SCUMM language semantics, EightBol frontend implementation audit, AST conformance, and backend compatibility  
**Reference Architecture:** EightBol EIGHTBOL compiler with 9+ backend targets (6502, 65c02, 65c816, HuC6280, RP2A03, cp1610, Z80, SM83, m68k, i286, ARM7, F8)

---

## Part 1: SCUMM Language Overview

### 1.1 Historical Context
- **Created:** 1987 by Ron Gilbert, Chip Morningstar, Aric Wilmunder, Brad P. Taylor at Lucasfilm Games (LucasArts)
- **Purpose:** Cross-platform scripting system for point-and-click adventure games
- **Games:** Maniac Mansion (1987), The Secret of Monkey Island, Day of the Tentacle, Full Throttle, The Curse of Monkey Island, Indiana Jones series, Loom, Sam & Max Hit the Road
- **Lifespan:** 1987–1998 (20-game run); licensed to Humongous Entertainment until 2003
- **Successor:** GrimE engine (Lua-based) for Grim Fandango and Escape from Monkey Island
- **Philosophy:** Tokenized bytecode + platform-independent interpreter (SPUTM); rapid prototyping of game logic while artists provided preliminary assets
- **Reference:** https://en.wikipedia.org/wiki/SCUMM

### 1.2 Design Philosophy
SCUMM emerged from minicomputer/Unix development practices:
- Human-readable commands → tokenized bytecode → platform-specific interpreter
- Cross-platform asset reuse (scripts, art, sound on Sun workstations compiled for PC with minimal errors)
- Verb–object interaction paradigm (use/look/pick up/talk verbs applied to game objects)
- Multitasking support: background actors execute in parallel with foreground actions
- Embedded subsystems: iMUSE (interactive music), INSANE (animation), CYST/FLEM (animation scaling), MMUCAS (room compilation)

### 1.3 SCUMM Variants
- **v1–v2:** Maniac Mansion (8-bit systems)
- **v3–v5:** Zak McKracken, Loom, Monkey Island 1–2, Indiana Jones, Day of the Tentacle, Sam & Max
- **v6:** Full Throttle (32-bit systems, increased palette)
- **v7:** The Curse of Monkey Island (CD-ROM, voice integration)

---

## Part 2: SCUMM Language Constructs

### 2.1 Assignment & Variable Definition

#### 2.1.1 Simple Assignment
```scumm
set score = 0;
set livesLeft = 3;
set counter = (counter + 1);
set message = "Hello, World!";
```

**AST Mapping:**
```lisp
(:move :from <expr> :to <identifier>)
```

**EightBol Implementation:** `scumm-parse-set` → `scumm-make-move-node`

#### 2.1.2 Arrays & List Initialization
```scumm
set scoreList = [0 0 0 0 0];           ;; 1D array
set initialsList = ["" "" "" ""];       ;; String array
set board = [[0 0 0] [0 0 0]];         ;; 2D array
```

**AST Mapping:** Array literals as s-expressions: `(0 0 0 0 0)`

**Note:** No dedicated array declaration keyword; arrays are initialized via assignment of literal lists.

#### 2.1.3 Subscript Access
```scumm
set (scoreList 0) = 100;        ;; Zero-indexed
set (board 2 1) = 5;            ;; Multi-dimensional
set value = (inventory count);  ;; Read subscripted value
```

**AST Mapping:**
```lisp
(:subscript <array-name> <index>)           ;; Read access
(:move :from <expr> :to (:subscript ...))   ;; Write access
```

---

### 2.2 Control Flow

#### 2.2.1 Conditional: if-then-else

**Syntax:**
```scumm
if (condition) then
    statement
endif

if (condition) then
    statement1
else
    statement2
endif
```

**AST Mapping:**
```lisp
(:if :condition <cond> :then [<stmts>] :else [<stmts>])
```

**EightBol Implementation:** `scumm-parse-if` → `scumm-make-if-node`

**Note:** `then` and `endif` keywords optional but conventional in documentation.

#### 2.2.2 While Loop
```scumm
while (condition) {
    statement(s)
}
endwhile
```

**AST Mapping:**
```lisp
(:perform :name "WHILE" :until (:not <cond>) :body [<stmts>])
```

**EightBol Implementation:** `scumm-parse-while` → `scumm-make-perform-node`

#### 2.2.3 For Loop
```scumm
for i = 1 to 10 {
    statement(s)
}
endfor
```

**AST Mapping:**
```lisp
(:perform :name "FOR" :varying <var> :from <start> :by 1
          :until (:gt <var> <end>) :body [<stmts>])
```

**EightBol Implementation:** `scumm-parse-for` → `scumm-make-perform-node`

#### 2.2.4 Break & Continue
```scumm
break;      ;; Exit current loop
continue;   ;; Skip to next iteration
```

**AST Mapping:**
```lisp
(:break)
(:continue)
```

---

### 2.3 Subroutine Calls

#### 2.3.1 Define (Function Definition)
```scumm
to functionName
    statement(s)
end

to initializeGame
    set score = 0;
    set livesLeft = 3;
end
```

**AST Mapping:**
```lisp
(:method :method-id "functionName" :statements [<stmts>])
```

**EightBol Implementation:** `scumm-parse-define` → `scumm-make-method-node`

#### 2.3.2 Call Statements

**Nullary (no arguments):**
```scumm
call initializeGraphics();
clearScreen();
```

**AST Mapping:**
```lisp
(:call :target "functionName")
```

**With Arguments:**
```scumm
call drawSprite spriteId xPos yPos;
playSound soundId volume loopFlag;
someProcedure param1 param2;
```

**AST Mapping:**
```lisp
(:call :target "functionName" :args [<expr1> <expr2> ...])
```

**Return Value Capture:**
```scumm
set score = (calculateBonus level timeLeft);
set bonusFactor = getInputFactor();
```

**AST Mapping:**
```lisp
(:move :from (:call :target "calculateBonus" ...) :to <identifier>)
```

#### 2.3.3 Return Statement
```scumm
return;
return 0;
return (score + bonus);
```

**AST Mapping:**
```lisp
(:goback)                                    ;; No return value
(:move :from <expr> :to <accumulator>)      ;; With return value
```

**EightBol Implementation:** `scumm-parse-return` → `scumm-make-goback-node`

---

### 2.4 Operators

#### 2.4.1 Arithmetic Operators

| Operator | Example | AST |
|----------|---------|-----|
| `+` | `a + b` | `(:add :from a :to b)` |
| `-` | `a - b` | `(:subtract :from a :from-target b)` |
| `*` | `a * b` | `(:compute :target result :expression (:* a b))` |
| `/` | `a / b` | `(:compute :target result :expression (:/ a b))` |
| `%` | `a % b` | `(:mod a b)` |
| `<<` | `a << 2` | Bit shift left (multiply by 2^n) |
| `>>` | `a >> 1` | Bit shift right (divide by 2^n) |

**EightBol Implementation:** `make-expression-{add,subtract,multiply,divide}`

#### 2.4.2 Comparison Operators

| Operator | Semantics | AST |
|----------|-----------|-----|
| `==` | Equality | `(:eq left right)` |
| `!=` | Inequality | `(:neq left right)` |
| `<` | Less than | `(:lt left right)` |
| `>` | Greater than | `(:gt left right)` |
| `<=` | Less than or equal | `(:le left right)` |
| `>=` | Greater than or equal | `(:ge left right)` |

**EightBol Implementation:** `make-conditional-{eq,neq,lt,gt,le,ge}`

#### 2.4.3 Logical Operators

| Operator | Semantics | AST |
|----------|-----------|-----|
| `&&` | Logical AND | `(:and left right)` |
| `||` | Logical OR | `(:or left right)` |
| `!` | Logical NOT | `(:not expr)` |

**EightBol Implementation:** `make-conditional-{and,or,not}`

---

### 2.5 Number Literal Formats

SCUMM supports multiple numeric bases via prefix notation:

#### 2.5.1 Decimal
```scumm
set value = 42;
set score = 100;
```

#### 2.5.2 Hexadecimal (0x prefix)
```scumm
set color = 0xFF;       ;; 255
set address = 0x1A2B;   ;; 6699
```

#### 2.5.3 Octal (0o prefix)
```scumm
set perm = 0o755;       ;; 493
set mask = 0o17;        ;; 15
```

#### 2.5.4 Binary (0b prefix)
```scumm
set flags = 0b11010010; ;; 210
set bits = 0b1010;      ;; 10
```

#### 2.5.5 DWORD (0d prefix)
```scumm
set large = 0d65536;    ;; 4-byte value
set addr = 0d16777216;  ;; 32-bit value
```

**EightBol Implementation:**
- `scumm-lex-source` tokenizes numeric literals with prefixes
- `scumm-parse-number` dispatches based on type: `:hex-number`, `:octal-number`, `:binary-number`, `:dword-number`
- Helper functions: `parse-hex-literal`, `parse-octal-literal`, `parse-binary-literal`, `parse-dword-literal`

---

### 2.6 String Handling

#### 2.6.1 String Literals
```scumm
set message = "Hello, World!";
set prompt = "Enter choice: ";
dialogue guard "Halt! Who goes there?";
print "Score: ", score;
```

**AST Mapping:** String as literal value in expression

#### 2.6.2 String Concatenation
Not explicitly supported in base SCUMM syntax; would require function call:
```scumm
set fullName = (concatenate firstName lastName);
```

---

### 2.7 Special Features: Rooms, Objects, Actors, Constants

#### 2.7.1 Actor Definition (OOP-like)
```scumm
actor player
    properties
        x 100
        y 100
        sprite 5
        visible true
    methods
        method move dx dy
            set x = (x + dx);
            set y = (y + dy);
        end
    end
end
```

**AST Mapping:**
```lisp
(:actor :name "player" 
        :properties [(:x 100) (:y 100) ...]
        :methods [(:method :name "move" :params [dx dy] :statements [...])])
```

**Current Status:** Documented but **not fully implemented** in EightBol parser

#### 2.7.2 Method Invocation
```scumm
player#move 5 10;
gameState#update score;
ui#draw healthBar;
```

**AST Mapping:**
```lisp
(:invoke :object "player" :method "move" :using [5 10])
```

**Current Status:** Partially implemented; needs refinement

#### 2.7.3 Named Constants
SCUMM supports constant definitions (game-engine-specific):
```scumm
THOT = 90          ;; Temperature threshold
MAX_X = 320
MIN_COUNT = 5
TARGET_VALUE = 100
```

**Pattern in EightBol:** Constants referenced as identifiers; resolved at compile/optimization time

#### 2.7.4 Rooms & Objects
```scumm
// Implicit room context in game engine
currentRoom = 1;           // Room ID
actor guybrush in room 1;  // Actor location
object key in room 1;      // Object location
```

**AST Mapping:** Game-engine-specific; encoded as variable references or special annotations

**Current Status:** Not directly supported in base EightBol; requires game-engine integration

---

### 2.8 I/O & Graphics

#### 2.8.1 Print Statement
```scumm
print "Hello, world!";
print "Score: ", score;
print "X: ", x, " Y: ", y;
print "Total: ", (a + b);
```

**AST Mapping:**
```lisp
(:print :expressions ["Message" var1 var2 ...])
```

**EightBol Implementation:** `scumm-parse-print` → `scumm-make-print-node`

#### 2.8.2 Input Statement
```scumm
input "Enter your name: " playerName;
input "Enter choice: " choice;
```

**AST Mapping:**
```lisp
(:input :variables [identifier] :prompt "Prompt text")
```

**EightBol Implementation:** `scumm-parse-input` → `scumm-make-input-node`

#### 2.8.3 Dialogue System
```scumm
dialogue guard "Halt! Who goes there?";

dialogue shopkeeper "What would you like to buy?" 
    "Sword" "Shield" "Leave";
```

**AST Mapping:**
```lisp
(:dialogue :speaker "guard" :text "Halt! Who goes there?"
           :statements [[response1] [response2] ...])
```

**EightBol Implementation:** `scumm-parse-dialogue` → `make-dialogue-node`

#### 2.8.4 Graphics Commands (Unsupported)
The following are **NOT implemented** in current EightBol:
- `draw-line`, `draw-circle`, `fill-polygon`, `blit`
- `fade` (screen effects)
- Hardware sprite/palette manipulation

---

### 2.9 Identifier Naming

**Case Handling:** SCUMM is case-insensitive; EightBol normalizes to camelCase:

| Input | Output (camelCase) |
|-------|-------------------|
| `player_score` | `playerScore` |
| `HelloWorld` | `helloWorld` |
| `x` | `x` |
| `MY_VAR` | `myVar` |

**Implementation:** `normalize-identifier-to-camel-case` in scumm-lexer.lisp

---

## Part 3: EightBol Frontend Implementation Status

### 3.1 Files
- `/home/brpocock/Projects/eightbol/src/frontend-scumm/scumm-lexer.lisp` — Tokenization
- `/home/brpocock/Projects/eightbol/src/frontend-scumm/scumm-parser.lisp` — YACC grammar & parsing
- `/home/brpocock/Projects/eightbol/src/frontend-scumm/scumm-transpile.lisp` — Compilation driver
- `/home/brpocock/Projects/eightbol/doc/chapters/scumm_script_creation_utility_for_maniac_mansion.texi` — Texi documentation
- `/home/brpocock/Projects/eightbol/frontend_plans/scumm_task.md` — Task list
- `/home/brpocock/Projects/eightbol/tests/scumm-number-tests.lisp` — Number literal tests

### 3.2 Current Implementation Status

#### ✅ Implemented
- [x] Lexer with keyword/operator recognition
- [x] Case-insensitive identifier normalization (→ camelCase)
- [x] Number literals (decimal, hex, octal, binary, DWORD)
- [x] String literals (double-quoted)
- [x] Basic assignment (`set var = expr`)
- [x] Arithmetic operators (+, -, *, /, %, <<, >>)
- [x] Comparison operators (==, !=, <, >, <=, >=)
- [x] Logical operators (&&, ||, !)
- [x] Control flow: if-then-else
- [x] Loops: while, for
- [x] Break & continue (skeleton)
- [x] Function definition (`to ... end`)
- [x] Function calls (nullary, with args, with return)
- [x] Print statement
- [x] Input statement
- [x] Dialogue statement with responses
- [x] Subscript access (arrays)
- [x] Return statement
- [x] Goto statement

#### ⚠️ Partially Implemented
- [ ] Actor/object definitions (syntax recognized; semantics incomplete)
- [ ] Method invocation on actors
- [ ] Rooms and scene contexts
- [ ] Embedded game-engine features (iMUSE, INSANE, CYST, FLEM)

#### ❌ Not Implemented
- [ ] Exception handling (try/catch/finally/throw)
- [ ] File I/O operations
- [ ] Extended graphics commands
- [ ] Sound/music integration
- [ ] Hardware-specific features (joystick, mouse beyond basic)
- [ ] Preprocessor directives (#define, #ifdef)
- [ ] Pointer arithmetic & manual memory management

### 3.3 YACC Grammar Structure

```lisp
(yacc:define-parser *scumm-parser*
  (:start-symbol program)
  (:terminals (lparen rparen lbracket rbracket semicolon comma
               plus minus times divide mod assign
               equal ne lt gt le ge
               and or not
               if else endif while endwhile for endfor
               break continue return
               define set get put call exit goto label
               ...))
  
  (program ...)          ; Top-level: list of statements
  (statement ...)        ; Individual statement types
  (if-stmt ...)          ; if-then-else
  (while-stmt ...)       ; while loop
  (for-stmt ...)         ; for loop
  (define-stmt ...)      ; Function definition
  (assignment ...)       ; Variable assignment
  (call-stmt ...)        ; Function call
  (return-stmt ...)      ; Return statement
  (goto-stmt ...)        ; Goto statement
  (print-stmt ...)       ; Print statement
  (input-stmt ...)       ; Input statement
  (dialogue-stmt ...)    ; Dialogue statement
  (expression ...)       ; Arithmetic/logical expressions
)
```

---

## Part 4: AST Mapping & Canonical Forms

### 4.1 Core AST Node Types

#### 4.1.1 Program Node
```lisp
(:program :class-id "SCUMM" :data (main-block ...) :methods nil)
```

#### 4.1.2 Method Node
```lisp
(:method :method-id "functionName" :statements [...])
```

#### 4.1.3 Statement Nodes

| SCUMM Construct | EightBol AST | Status |
|-----------------|--------------|--------|
| `set var = expr` | `(:move :from expr :to identifier)` | ✅ |
| `if (cond) stmt else stmt` | `(:if :condition cond :then [...] :else [...])` | ✅ |
| `while (cond) stmt` | `(:perform :name "WHILE" :until (:not cond) :body [...])` | ✅ |
| `for var = s to e stmt` | `(:perform :name "FOR" :varying var :from s :by 1 :until (:gt var e) :body [...])` | ✅ |
| `call func()` | `(:call :target func)` | ✅ |
| `return expr` | `(:goback)` + move return value | ✅ |
| `goto label` | `(:goto :target label)` | ✅ |
| `print msg, args` | `(:print :expressions [msg args ...])` | ✅ |
| `input prompt var` | `(:input :variables [var] :prompt prompt)` | ✅ |
| `dialogue npc msg responses` | `(:dialogue :speaker npc :text msg :statements [responses])` | ✅ |

#### 4.1.4 Expression Nodes

| SCUMM Construct | EightBol AST | Status |
|-----------------|--------------|--------|
| `a + b` | `(:add :from a :to b)` | ✅ |
| `a - b` | `(:subtract :from a :from-target b)` | ✅ |
| `a * b` | `(:compute :target result :expression (:* a b))` | ✅ |
| `a / b` | `(:compute :target result :expression (:/ a b))` | ✅ |
| `a % b` | `(:mod a b)` | ✅ |
| `a == b` | `(:eq a b)` | ✅ |
| `a != b` | `(:neq a b)` | ✅ |
| `a < b` | `(:lt a b)` | ✅ |
| `a > b` | `(:gt a b)` | ✅ |
| `a <= b` | `(:le a b)` | ✅ |
| `a >= b` | `(:ge a b)` | ✅ |
| `a && b` | `(:and a b)` | ✅ |
| `a || b` | `(:or a b)` | ✅ |
| `!a` | `(:not a)` | ✅ |
| `array[idx]` | `(:subscript array idx)` | ✅ |
| Identifier | `identifier` (symbol) | ✅ |
| Literal | `42`, `"string"`, etc. | ✅ |

---

## Part 5: IDENTIFIED ISSUES

### 5.1 ⚠️ CRITICAL: `:perform :name` vs `:procedure` Mismatch

#### Issue Description
SCUMM parser emits `:perform :name` instead of canonical `:perform :procedure`:

**Current (INCORRECT):**
```lisp
;; In scumm-parser.lisp, line 110
(defun scumm-make-perform-node (name &key until varying from by body)
  "Create a perform/loop AST node."
  (let ((node (list :perform :name name)))  ; ❌ WRONG!
    (when varying (setf node (append node (list :varying varying))))
    ...
    node))
```

**Canonical (CORRECT):**
```lisp
;; From ast.lisp line 29
;;   (:perform    :procedure name [:times expr] [:until cond] [:varying ...] [:body stmts])

;; From cobol-parser.lisp
(list :perform :procedure name :times expression)
```

#### Impact
- Backend code expects `:procedure` keyword; SCUMM `:name` will cause failures
- Other frontends (COBOL, SCI, Lingo, Forth, BASIC) all use `:procedure`
- Results in AST validation errors and backend incompatibility

#### Root Cause
SCUMM frontend was implemented with incorrect keyword; canonical AST definition in `ast.lisp` line 29 specifies `:procedure`.

#### Fix Required
**File:** `/home/brpocock/Projects/eightbol/src/frontend-scumm/scumm-parser.lisp`

Change line 110 from:
```lisp
(let ((node (list :perform :name name)))
```

To:
```lisp
(let ((node (list :perform :procedure name)))
```

**Verification:** Search and replace all `:perform :name` → `:perform :procedure` in SCUMM parser

---

### 5.2 ⚠️ Incomplete: Actor/Object Definitions

#### Current Status
Actor definition syntax is documented but not fully parsed:

```scumm
actor player
    properties
        x 100
        y 100
    methods
        method move dx dy
            set x = (x + dx);
        end
    end
end
```

#### Missing Components
1. **Parser rules** for `actor` keyword
2. **Properties parsing** (key-value pairs)
3. **Methods within actors** (nested method definitions)
4. **Property access** via dot notation or OF syntax
5. **AST node generation** for actor objects

#### Proposed AST Form
```lisp
(:actor :name "player"
        :properties [(:x 100) (:y 100) (:sprite 5) (:visible true)]
        :methods [(:method :method-id "move" 
                          :parameters [dx dy]
                          :statements [...])])
```

#### Workaround
Until fully implemented, actors can be simulated using plain variables and function calls.

---

### 5.3 ⚠️ Incomplete: Game-Engine-Specific Features

#### Rooms & Scenes
```scumm
set currentRoom = 1;
actor guybrush in room 1;
```

**Status:** No special parsing; treated as variable references.

#### Object Inventory Management
```scumm
set (inventory slot) = itemId;
set item = (inventory slot);
```

**Status:** Subscript access works, but no object-specific semantics.

#### Embedded Subsystems
- iMUSE (interactive music): Not implemented
- INSANE (animation): Not implemented
- CYST (in-game animation): Not implemented
- FLEM (room/object definitions): Not implemented

**Impact:** Games using these features cannot compile; backend support needed for each.

---

### 5.4 ⚠️ Incomplete: Identifier Scoping

#### Current Issue
No distinction between:
1. Local variables (function scope)
2. Global variables (program scope)
3. Actor properties (object scope)
4. Game state (persistent across scenes)

#### Canonical Scoping Model (from COBOL)
```lisp
(:program :class-id "ClassName"
          :data [local-vars]
          :methods [...])
```

**SCUMM Needs:**
- Function-local scope for `to ... end` definitions
- Global scope for top-level assignments
- Actor-scoped properties

---

### 5.5 Minor: String Handling

#### Missing Features
1. **String concatenation:** No explicit operator; requires function call
2. **Escape sequences:** \\n, \\t, \\", \\\\ not documented/tested
3. **String functions:** No SUBSTRING, INDEX, LENGTH operations

#### Current Support
- Double-quoted literals: ✅
- Print with format strings: ⚠️ (basic)

---

## Part 6: Backend Compatibility

### 6.1 Call Types in EightBol AST

Per `src/ast.lisp` and `AGENTS.md`:

| Call Type | AST Form | SCUMM Usage | Status |
|-----------|----------|------------|--------|
| Local nullary | `(:call :target name)` | `initializeGame();` | ✅ |
| Local unary | `(:call-acc :target name :using expr)` | Not used in base SCUMM | ❌ |
| Method | `(:invoke :object instance :method "Name")` | `player#move 5 10;` | ⚠️ |
| Library nullary | `(:call :target name :library t)` | Via external libs | ❓ |
| Library unary | `(:call-acc :target name :library t :using expr)` | Via external libs | ❓ |
| Remote service | `(:call :bank service-bank :target service-id)` | Not in SCUMM | ❌ |

### 6.2 Backend Requirements

For a SCUMM program to compile across all backends:

1. **No multiplication/division in certain backends** — Use bit shifts or lookup tables
2. **No floating-point** — All arithmetic is fixed-point binary or BCD
3. **Array indexing** — Backend must support computed addressing for subscripts
4. **Function calls** — Backend must handle procedure linkage

### 6.3 Numeric Precision

**SCUMM Philosophy:** All arithmetic operates on fixed-point binary or BCD values.

Per `AGENTS.md § Numeric Types`:
- Binary fixed point: arbitrary bytes, point at any bit position
- BCD fixed point: arbitrary nybbles, point at any nybble position
- Display characters: screen codes (ASCII, EBCDIC, PETSCII, minicode)

**SCUMM Constraint:** No inherent float support; games use integer math with scaling.

---

## Part 7: Game-Engine-Specific Features

### 7.1 Verb-Object Paradigm

SCUMM's core interaction model (NOT directly supported in EightBol):

```scumm
// Pseudo-SCUMM engine interface
verb = LOOK;        // or USE, PICK UP, TALK, PUSH, PULL, etc.
object = key;
subject = guybrush; // Implicit player character

// Engine calls:
performVerb(verb, object, subject);
```

**Implementation in EightBol:** Would require game-engine integration; not in base compiler.

### 7.2 Room Architecture

```scumm
room 1 {
    objects: key, door, guard
    actors: guybrush (initially at 100, 150)
    backgrounds: mansion-entrance.bmp
    walkboxes: [bounding regions for pathfinding]
}
```

**Current Status:** No special syntax; would be data definition outside compiler scope.

### 7.3 Actor Management

In original SCUMM:
- Each actor has state: position (x, y), costume, scaling, visibility, etc.
- Actors can execute scripts independently (multitasking)
- Scripts yield control to engine via `wait` / `waitForMessage` / etc.

**EightBol Implementation:** Can be simulated with:
```lisp
(:invoke :object "actor-name" :method "walk" :using (list x y))
(:perform :until (:call :target "isActorIdle" :using "actor-name") :body [...])
```

### 7.4 Dialogue/Conversation System

```scumm
dialogue npc "What would you like?" 
    "Buy sword"  "Sell item"  "Leave"
```

**Engine Behavior:**
1. Display NPC speech
2. Show numbered options to player
3. Call handler based on selection
4. Resume script from next statement

**EightBol AST:**
```lisp
(:dialogue :speaker "shopkeeper" :text "What would you like?"
           :statements [[response1-stmts] [response2-stmts] [response3-stmts]])
```

---

## Part 8: Condition Forms & Boolean Logic

### 8.1 Condition Representation

SCUMM conditions are represented as nested lists in EightBol AST:

#### Simple Conditions
```scumm
if (x > 100) ...
```

**AST:**
```lisp
(:gt x 100)
```

#### Combined Conditions
```scumm
if ((x > 100) && (x < 200)) ...
```

**AST:**
```lisp
(:and (:gt x 100) (:lt x 200))
```

#### Negated Conditions
```scumm
if (!done) ...
```

**AST:**
```lisp
(:not done)
```

### 8.2 Condition-Name Semantics (COBOL-style)

SCUMM does NOT natively support COBOL-style condition names:

```cobol
PROCEDURE DIVISION.
    IF valid-input THEN ...  ;; Condition-name reference
```

**Alternative in SCUMM:**
```scumm
if (valid_input == 1) ...
```

**EightBol AST:** Treated as identifier/variable reference, not special condition-name.

---

## Part 9: Summary of Gaps & Incompatibilities

| Gap | Severity | Impact | Workaround |
|-----|----------|--------|-----------|
| `:perform :name` → `:procedure` mismatch | **CRITICAL** | Backend failure on loops | Fix line 110 scumm-parser.lisp |
| Actor definitions incomplete | High | OOP-style code won't compile | Use plain functions + state vars |
| Game-engine integration | Medium | No iMUSE/INSANE/CYST support | Require game engine library |
| Rooms/scenes unsupported | Medium | Scene management manual | Use variable-based state tracking |
| String functions (INDEX, LENGTH, etc.) | Low | Limited text processing | Call external library functions |
| Exception handling | Low | No try/catch | Use error codes + conditional checks |
| Floating-point | Low | Scientific games impossible | Use fixed-point arithmetic |

---

## Part 10: Recommendations

### 10.1 Immediate Actions (Critical)

1. **Fix `:perform :procedure` mismatch** in `scumm-parser.lisp` line 110
2. **Add test cases** for SCUMM → AST → backend compilation pipeline
3. **Validate backend compatibility** on 6502 and Z80 targets (8-bit retro)

### 10.2 Short-term (Weeks)

1. **Complete actor/object parsing** using existing method node structure
2. **Add identifier scoping** to separate local, global, and object-scoped variables
3. **Implement string functions** (LENGTH, INDEX, SUBSTRING) via library calls
4. **Create integration tests** for SCUMM games (Maniac Mansion subset)

### 10.3 Medium-term (Months)

1. **Design game-engine integration** for iMUSE, INSANE, CYST, FLEM
2. **Implement room/scene context** tracking
3. **Add dialogue response handler** codegen
4. **Support multitasking primitives** (yield, waitForMessage, etc.)

### 10.4 Long-term (Architectural)

1. **Create SCUMM → backend compilation guide** documenting platform-specific constraints
2. **Optimize for 8-bit targets** (6502, Z80): minimal memory footprint, efficient bytecode
3. **Consider cross-frontend standardization** of OOP features (shared actor model)

---

## Part 11: Reference Documentation

### Official Sources
- **Wikipedia:** https://en.wikipedia.org/wiki/SCUMM
- **ScummVM Project:** https://www.scummvm.org/ (open-source SCUMM interpreter)
- **Gamasutra (Historical):** "The SCUMM Diary: Stories behind one of the greatest game engines ever made" by Mike Bevan

### EightBol References
- `src/ast.lisp` — Canonical AST node definitions (line 29 for `:perform` specification)
- `src/frontend-cobol/cobol-parser.lisp` — Reference implementation of `:perform :procedure`
- `doc/chapters/scumm_script_creation_utility_for_maniac_mansion.texi` — SCUMM language documentation
- `frontend_plans/scumm_task.md` — Implementation task list
- `AGENTS.md` — Overall EightBol architecture & guidelines

### Related Frontends for Comparison
- COBOL (mature, comprehensive)
- SCI (Sierra's competing engine language)
- Lingo (Director scripting for multimedia)
- Forth (stack-based alternative)

---

## Appendix A: SCUMM Game Compatibility Matrix

| Game | Year | SCUMM v | Platforms | Notes |
|------|------|---------|-----------|-------|
| Maniac Mansion | 1987 | 1 | C64, Apple II | Original 80% feature set |
| Zak McKracken | 1988 | 3 | PC, Atari ST, Amiga | Dialog color fix |
| Loom | 1990 | 3 | Multiple | Spell-based instead of verbs |
| The Secret of Monkey Island | 1990 | 3 | Multiple | Verb wheel interface |
| Monkey Island 2 | 1991 | 3 | Multiple | Enhanced graphics |
| Indiana Jones & the Fate of Atlantis | 1992 | 3 | PC, Amiga | Point-and-click adventure |
| Day of the Tentacle | 1993 | 5 | Multiple | Time-travel puzzle game |
| Sam & Max Hit the Road | 1993 | 5 | PC, DOS | Comic adventure |
| Full Throttle | 1995 | 6 | PC, PlayStation | Motorcycle action-adventure |
| The Curse of Monkey Island | 1997 | 7 | PC, Mac | Voice acting integrated |
| Escape from Monkey Island | 2000 | 8 (GrimE) | PC, PS2 | Last SCUMM game; switched to Lua |

**EightBol Target:** Maniac Mansion (SCUMM v1) as minimum viable subset; Day of the Tentacle (SCUMM v5) as comprehensive target.

---

## Appendix B: Lexer Token Types

From `scumm-lexer.lisp`:

**Keywords (37):**
`if`, `else`, `endif`, `while`, `endwhile`, `for`, `endfor`, `break`, `continue`, `return`, `define`, `set`, `get`, `put`, `call`, `exit`, `goto`, `label`, `assert`, `wait`, `stop`, `start`, `fade`, `restore`, `save`, `restart`, `version`, `to`, `end`, `dialogue`, `print`, `input`, and special tokens for delimiters

**Operators (13):**
`+`, `-`, `*`, `/`, `%`, `=`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `&&`, `||`, `!`

**Number Formats (5):**
`:number` (decimal), `:hex-number` (0x prefix), `:octal-number` (0o prefix), `:binary-number` (0b prefix), `:dword-number` (0d prefix)

**Delimiters (6):**
`(`, `)`, `[`, `]`, `;`, `,`

---

## Appendix C: AST Node Shape Reference

### From `src/ast.lisp`

```lisp
;;   Program:   (:program  :class-id "Character" :data (...) :methods (...))
;;   Method:    (:method   :method-id "Think" :statements (...))
;;
;; Statement nodes:
;;   (:move       :from expr :to identifier)
;;   (:invoke     :object expr :method "Kill" [:returning identifier] [:using expr])
;;   (:call       :target name :bank bank-or-nil)
;;   (:call-acc   :target name :bank bank-or-nil :using expr)
;;   (:if         :condition cond :then stmts :else stmts)
;;   (:goto       :target identifier)
;;   (:goback)
;;   (:exit-method)
;;   (:exit-program)
;;   (:exit)
;;   (:stop-run)
;;   (:add        :from expr :to identifier)
;;   (:subtract   :minuend expr :subtrahend expr [:giving identifier])
;;   (:compute    :target identifier :expression expr)
;;   (:perform    :procedure name [:times expr] [:until cond] [:varying ...] [:body stmts])
;;   (:set        :target identifier :value expr)
;;   (:print      :expressions [expr ...])
;;   (:input      :variables [identifier ...])
;;   (:dialogue   :speaker expr :text string [:statements [...]])
;;   (:string-blt :source operand :dest operand [:length expr])
;;   (:assembly-entry :label "Symbol")
;;
;; Expression/operand values:
;;   literal number or string
;;   symbol (identifier reference)
;;   (:of slot obj)              ; qualified identifier (slot OF obj)
;;   (:address-of id)            ; ADDRESS OF id
;;   (:subscript name index)     ; subscripted identifier name(index)
;;   :self / :null               ; SELF / NULL
```

---

**Document End**

Generated: September 9, 2026  
Last Updated: [Auto-generated by research agent]  
Version: 1.0
