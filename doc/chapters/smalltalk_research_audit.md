# Comprehensive Research: Smalltalk Language vs EIGHTBOL Canonical AST

## Executive Summary

Smalltalk is a **pure object-oriented language** (everything is an object/message) where **ALL computation happens via message passing**. This fundamentally differs from EIGHTBOL's imperative procedural AST model. The Smalltalk frontend has **critical issues** with non-canonical AST generation, specifically:

1. **Literal wrappers** (`:literal-string`, `:literal-number`) that don't exist in canonical AST
2. **Non-canonical `:move` keys** (`:variables`, `:expressions` instead of `:from`, `:to`)
3. **Missing or incomplete OO features** that don't map cleanly to imperative backends

---

## Part 1: Smalltalk Language Features

### 1.1 **Historical Context**
- **Invented:** 1972 (Alan Kay, Xerox PARC)
- **Philosophy:** "Everything is an object" and "messaging is the most important concept"
- **Paradigm:** Pure OO with message passing, reflection, live coding
- **Major implementations:** Squeak, Pharo, GNU Smalltalk, VisualWorks, VA Smalltalk

### 1.2 **Core Principles**

**A Smalltalk object can do exactly three things:**
1. Hold state (references to other objects)
2. Receive messages from itself or another object
3. Send messages to itself or other objects

**Everything is an object:**
- Numbers, strings, booleans, characters → objects of Integer, String, Boolean, Character classes
- Classes → objects (instances of Metaclass)
- Methods → objects (instances of CompiledMethod)
- Code blocks → objects (BlockClosure instances)
- Variables → NOT objects (only values are objects)

### 1.3 **Assignment/Variable Definition**

**Smalltalk Syntax:**
```smalltalk
myVariable := 100
count := count + 1
player := Player new
self x := 50
```

**Canonical EIGHTBOL AST:**
```lisp
(:move :from 100 :to myVariable)
(:move :from (:add :from count :to 1 :giving nil) :to count)
(:move :from (:invoke :object Player :method "new") :to player)
```

**Current Smalltalk Frontend:**
```lisp
(:move :variables (list myVariable) :expressions (list 100))  ✗ NON-CANONICAL
```

**Issue:** Uses `:variables` and `:expressions` keys instead of canonical `:from`/`:to`

### 1.4 **Message Sends (Method Calls)**

**Smalltalk Syntax:**
```smalltalk
obj := Player new.                        " Nullary message "
obj initialize.                           " Nullary message "
obj moveDX: 5 dy: 10.                    " Keyword message (selector: moveDX:dy:) "
3 + 4                                     " Binary message (selector: +) "
obj ifTrue: [statements] ifFalse: [stmts] " Keyword message with blocks "
player health > 0                         " Comparison message "
(obj1, obj2, obj3) do: [:x | x print]    " Unary message with block "
```

**Three message forms:**
1. **Unary:** `receiver message` → `obj print`
2. **Binary:** `receiver op operand` → `3 + 4`, `x > 10`
3. **Keyword:** `receiver key1: arg1 key2: arg2` → `obj moveDX: 5 dy: 10`

**Canonical EIGHTBOL AST:**
```lisp
(:invoke :object Player :method "new")
(:invoke :object obj :method "initialize")
(:invoke :object obj :method "moveDX:dy:" :using (list 5 10))
(:add :from 3 :to 4 :giving nil)         " For arithmetic expressions "
(:if :condition (...) :then [...] :else [...])
```

**Current Smalltalk Frontend Issue:**
```lisp
(list :invoke :receiver receiver :selector selector)  ✗ NON-CANONICAL KEYS
```

Uses `:receiver` and `:selector` instead of `:object` and `:method`

### 1.5 **Control Flow: Blocks and Conditionals**

**Smalltalk Blocks (closures):**
```smalltalk
[ x + 1 ]                               " Block literal (no params) "
[ :x | x + 1 ]                          " Block with parameter x "
[ :x :y | x + y ]                       " Block with multiple parameters "

condition ifTrue: [ statements ]         " Conditional (block as argument) "
condition ifFalse: [ statements ]
condition ifTrue: [ ... ] ifFalse: [...]

[condition] whileTrue: [ statements ]   " While loop (condition in block) "
[condition] whileFalse: [ statements ]
```

**How blocks work:**
- Blocks are objects (BlockClosure instances)
- They capture their lexical environment
- They're passed as **message arguments** to control structures

**Canonical EIGHTBOL AST:**
- Blocks → `:perform :body [statements]`
- Conditionals → `:if :condition expr :then stmts :else stmts`

### 1.6 **Loop Constructs**

**Smalltalk Syntax:**
```smalltalk
1 to: 10 do: [ :i | statements ]                " Numeric loop (1..10) "
1 to: 100 by: 5 do: [ :i | statements ]         " With step size "
10 downTo: 1 do: [ :i | statements ]            " Countdown "

array do: [ :item | statements ]                " Iterate collection "
array select: [ :item | condition ]             " Filter collection "
array collect: [ :item | expression ]           " Map collection "
array inject: 0 into: [ :sum :item | ... ]      " Fold/reduce "

[ condition ] whileTrue: [ statements ]
[ condition ] whileFalse: [ statements ]
[ statements ] repeat                           " Infinite loop "
```

**Canonical EIGHTBOL AST:**
```lisp
(:perform :from 1 :to 10 :body [...])
(:perform :from 1 :to 100 :by 5 :body [...])
(:perform :body [...] :until (:not condition))  " While True→Until Not "
```

**Current Frontend Status:** ⚠ **NOT FULLY IMPLEMENTED**

### 1.7 **Collections**

**Smalltalk Collections:**
```smalltalk
#(1 2 3)                    " Array (immutable) "
Array with: 1 with: 2 with: 3
{ 1 . 2 . 3 }              " Dynamic array "
OrderedCollection new add: 1

{ 'x' -> 1 . 'y' -> 2 }     " Dictionary (key-value pairs) "
Dictionary new at: 'x' put: 1

'hello' at: 1               " String indexing "
'hello' size                " String length "
'hello' , 'world'           " String concatenation "

array at: 1                 " Element access "
array at: 1 put: value      " Element assignment "
```

**Canonical EIGHTBOL AST:** (LIMITED OO SUPPORT)
- Collections are NOT first-class in EIGHTBOL AST
- Only subscripted access: `(:subscript name index)`
- No native dictionary/map support

### 1.8 **Strings and Symbols**

**Smalltalk Syntax:**
```smalltalk
'hello world'               " String (mutable byte array) "
'it''s'                     " String with apostrophe (doubled) "
#hello                      " Symbol (immutable, interned) "
#'hello world'              " Symbol with spaces "
'hello' asSymbol            " Convert string to symbol "
#hello asString             " Convert symbol to string "
'hello' at: 1               " Character access "
'hello' size                " String length "
'hello' , 'world'           " Concatenation "
'hello' copyFrom: 1 to: 3   " Substring "
```

**Canonical EIGHTBOL AST:**
```lisp
"hello world"               " Bare string literal (not wrapped) "
42                          " Bare number literal (not wrapped) "
```

**Current Smalltalk Frontend Issue:**
```lisp
(:literal-string :value "hello")   ✗ WRAPPER NODE
(:literal-number :value 42)        ✗ WRAPPER NODE
```

### 1.9 **Arithmetic and Operators**

**Smalltalk Arithmetic:**
```smalltalk
3 + 4                       " Addition (binary message) "
10 - 3
5 * 6
20 / 5                      " Floating point division "
20 // 3                     " Integer division "
20 \\ 3                     " Modulo (remainder) "
2 ** 8                      " Exponentiation "
-5                          " Unary negation "

10 > 5, 5 < 10, 5 = 5      " Comparison "
10 >= 5, 5 <= 10, 5 \= 5   " More comparisons "
true & false                " Boolean AND "
true | false                " Boolean OR "
true not                    " Boolean NOT "
```

**Key insight:**
- **ALL arithmetic is message passing** (e.g., `3 + 4` sends `+` message to integer 3 with argument 4)
- EIGHTBOL backends cannot execute arbitrary method dispatch
- Must be compiled down to primitive operations

**Canonical EIGHTBOL AST:**
```lisp
(:add :from 3 :to 4 :giving nil)           " Expression form "
(:add :from 10 :to 3)                      " Statement form "
(:multiply :by 5 :multiplier 6 :giving nil)
(:divide :numerator 20 :denominator 3 :giving nil)
(:subtract :minuend 20 :subtrahend 3 :giving nil)
```

### 1.10 **I/O Operations**

**Smalltalk Syntax:**
```smalltalk
Transcript show: 'Hello'                " Print to transcript "
obj inspect                              " Interactive inspection "
obj printOn: stream                      " Print to stream "
'message' asString                       " Convert to string "
aNumber asString
Transcript cr                            " Newline "

Stdin nextLine                           " Read line "
Stdin nextWord                           " Read word "
```

**Canonical EIGHTBOL AST:**
```lisp
(:print :expressions ("Hello"))
(:input :variables (variable-list) :prompt optional-prompt)
```

**Current Frontend Status:** ⚠ **BASIC SUPPORT ONLY** (via `:print` and `:input`)

### 1.11 **Reflection and Metaprogramming**

**Smalltalk Reflection:**
```smalltalk
obj class                               " Get object's class "
obj class name                          " Get class name "
MyClass methods                         " List all methods "
MyClass instVarNames                    " Instance variable names "
MyClass new                             " Create instance "
(MyClass methodNamed: 'foo') source     " Get method source "
obj respond: #foo                       " Check if object has method "
obj perform: #methodName                " Send message by name (late binding) "
obj perform: #methodName: with: arg     " With argument "
```

**Key OO Issues for EIGHTBOL:**
1. **Late binding:** Method name determined at runtime → backends can't resolve at compile time
2. **Reflection:** Querying object structure at runtime → requires runtime tables
3. **Metaclasses:** Classes are objects → infinite regression in purely compiled backend
4. **Polymorphism:** Same message sent to different object types → requires vtables or dispatch code

**Backend Challenge:**
EIGHTBOL backends generate **static assembly code** (6502, Z80, etc.) which cannot support:
- Dynamic method lookup
- Runtime type checking
- Arbitrary message dispatch
- Reflection queries

### 1.12 **Classes and Object Creation**

**Smalltalk Class Definition:**
```smalltalk
Object subclass: #Player
    instanceVariableNames: 'x y health sprite'
    classVariableNames: 'MaxPlayers'
    poolDictionaries: ''
    category: 'Game-Entities'

Player>>initialize
    x := 0.
    y := 0.
    health := 100.
    sprite := 0.

Player>>moveDX: dx dy: dy
    x := x + dx.
    y := y + dy.

Player>>isAlive
    ^ health > 0
```

**Canonical EIGHTBOL AST:**
```lisp
(:program :class-id "Player"
    :data (field x field y field health field sprite)
    :methods (
        (:method :method-id "initialize"
            :statements (...))
        (:method :method-id "moveDX:dy:"
            :statements (...))
        (:method :method-id "isAlive"
            :statements (...))))
```

---

## Part 2: Audit Findings - Non-Canonical Issues

### Issue #1: Literal Wrappers

**Canonical Form (from grammar-build.lisp):**
```lisp
(defun make-literal-number (n)
  n)                                    " Returns BARE number "

(defun make-literal-string (s)
  s)                                    " Returns BARE string "
```

**Non-Canonical Form (Smalltalk Frontend):**
```lisp
(defun smalltalk-make-literal-string (s)
  (list :literal-string :value s))      " Wraps in AST node "

(defun smalltalk-make-literal-number (n &key format)
  (if format
      (list :literal-number :value n :format format)
      (list :literal-number :value n)))  " Wraps in AST node "
```

**Why This is Wrong:**
- **Canonical AST permits raw values** as operands (numbers, strings, symbols)
- **Wrapper nodes add an extra layer** that backends don't expect
- **Other frontends (BASIC, etc.) return bare values**, not wrapped
- **Inconsistency:** Only Smalltalk creates these wrappers

**Impact on Backends:**
```lisp
; Canonical (what backends expect):
(:move :from 42 :to myVar)              " Simple "
(:move :from "hello" :to greeting)

; Broken (what Smalltalk generates):
(:move :from (:literal-number :value 42) :to myVar)   " Extra traversal needed "
(:move :from (:literal-string :value "hello") :to greeting)
```

### Issue #2: Non-Canonical Move Keys

**Canonical Form:**
```lisp
(:move :from expr :to identifier)
```

**Non-Canonical Form (Smalltalk):**
```lisp
(defun smalltalk-make-move-node (target value)
  (list :move
        :variables (list (smalltalk-normalize-identifier target))
        :expressions (list value)))
```

Produces:
```lisp
(:move :variables (list myVar) :expressions (list value))  ✗ WRONG
```

**Where else this appears:**
- **ZIL Frontend** also uses non-canonical keys: `(:move :expressions (list val) :variables (list prop))`

**Why This is Wrong:**
- **AST canonicalization rules** (documented in `src/ast.lisp`) define specific keys
- **Backends expect `:from` and `:to`**, not `:variables` and `:expressions`
- **Consistency matters:** All other frontends use `:from` and `:to`

### Issue #3: Non-Canonical Invoke Keys

**Current Smalltalk Code (Line 193, 197):**
```lisp
(list :invoke :receiver receiver :selector selector)  ✗ WRONG KEYS
```

**Canonical Form:**
```lisp
(:invoke :object obj :method "MethodName")
```

---

## Part 3: Proposed Fixes

### Fix #1: Correct Literal Generation

**File:** `/home/brpocock/Projects/eightbol/src/frontend-smalltalk/smalltalk-parser.lisp`

**Current (Lines 44-53):**
```lisp
(defun smalltalk-make-literal-string (s)
  (list :literal-string :value s))

(defun smalltalk-make-literal-number (n &key format)
  (if format
      (list :literal-number :value n :format format)
      (list :literal-number :value n)))
```

**Fixed:**
```lisp
(defun smalltalk-make-literal-string (s)
  s)                                    ; Return bare string

(defun smalltalk-make-literal-number (n &key format)
  (declare (ignore format))             ; Ignore format for now
  n)                                    ; Return bare number
```

### Fix #2: Correct Move Node Generation

**File:** `/home/brpocock/Projects/eightbol/src/frontend-smalltalk/smalltalk-parser.lisp`

**Current (Lines 32-36):**
```lisp
(defun smalltalk-make-move-node (target value)
  (list :move
        :variables (list (smalltalk-normalize-identifier target))
        :expressions (list value)))
```

**Fixed:**
```lisp
(defun smalltalk-make-move-node (target value)
  (list :move
        :from value
        :to (smalltalk-normalize-identifier target)))
```

### Fix #3: Correct Invoke Node Generation

**File:** `/home/brpocock/Projects/eightbol/src/frontend-smalltalk/smalltalk-parser.lisp`

**Current Parser Issue (Lines 191-197):**
```lisp
(message
  (expression :PERIOD expression
    (lambda (receiver selector)
      (list :invoke :receiver receiver :selector selector)))  ✗ WRONG KEYS
  (expression :PERIOD expression :LPAREN expression-list :RPAREN
    (lambda (receiver selector args)
      (declare (ignore args))
      (list :invoke :receiver receiver :selector selector))))    ✗ WRONG KEYS
```

**Fixed:**
```lisp
(message
  (expression :PERIOD expression
    (lambda (receiver selector)
      (list :invoke :object receiver :method selector)))  ✓ CANONICAL
  (expression :PERIOD expression :LPAREN expression-list :RPAREN
    (lambda (receiver selector args)
      (declare (ignore args))
      (list :invoke :object receiver :method selector))))    ✓ CANONICAL
```

---

## Part 4: Smalltalk Features vs EIGHTBOL AST Limitations

### Feature Support Matrix

| Feature | Smalltalk Support | EIGHTBOL AST | Backend Support | Notes |
|---------|------------------|--------------|-----------------|-------|
| **Assignment** | `:=` operator | `:move` node | ✓ Full | Works fine |
| **Arithmetic** | Binary messages (`+`, `-`, `*`, `/`) | `:add`, `:subtract`, `:multiply`, `:divide` | ✓ Full | Must compile message to operation |
| **Comparisons** | Binary messages (`>`, `<`, `=`, etc.) | Conditional nodes (`=`, `/=`, `<`, etc.) | ✓ Full | Works fine |
| **Boolean Logic** | Binary messages (`&`, `\|`, `not`) | `:and`, `:or`, `:not` nodes | ✓ Full | Works fine |
| **Conditionals** | `ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:` | `:if` node | ✓ Full | Blocks map to statements |
| **Loops (numeric)** | `to:do:`, `downTo:do:` | `:perform :from :to :by :body` | ✓ Full | Works fine |
| **Loops (collection)** | `do:`, `select:`, `collect:` | `:perform` with body | ⚠ Partial | Collection iteration not supported |
| **Blocks/Closures** | `[x \| expr]` | `:perform :body` | ⚠ Partial | Limited to control flow |
| **Method Calls** | Message sends | `:invoke` and `:call` | ✓ Full | Works for simple cases |
| **Method Dispatch** | Late binding (polymorphic) | Static invoke | ✗ None | Cannot dispatch at runtime |
| **Reflection** | Query/inspect objects | No reflection support | ✗ None | Not applicable in compiled backends |
| **Collections** | Arrays, Dictionaries, OrderedCollections | `:subscript` only | ⚠ Minimal | No native dict/set support |
| **Strings** | String objects with methods | Bare string literals | ⚠ Partial | `STRING DELIMITED BY SIZE` only |
| **I/O** | `Transcript show:`, `stdin`, etc. | `:print`, `:input` | ✓ Full | Limited to console I/O |
| **Classes** | Full OO with inheritance | `:program :methods` | ⚠ Limited | No inheritance support |
| **Instance Variables** | Private object state | `:data` section | ✓ Full | Maps to record/struct fields |
| **Metaclasses** | Classes are objects | Not applicable | ✗ None | Incompatible with compiled code |
| **Symbols** | Interned strings (#name) | No symbol type | ✗ None | Can simulate with strings |
| **Exceptions** | Exception hierarchy | `:log-fault`, `:debug-break` | ✗ None | No try/catch support |

---

## Part 5: Backend Code Generation Challenges

### Challenge 1: Pure Message-Based Computation

**Problem:**
- Smalltalk: `result := (obj1 foo) + (obj2 bar) * 3`
  - Sends `foo` message to obj1
  - Sends `bar` message to obj2
  - Sends `+` and `*` messages to numbers

- EIGHTBOL backend (6502, Z80):
  - Must generate direct arithmetic instructions
  - Cannot support arbitrary method dispatch
  - Must "inline" all message sends at compile time

**Solution:** Compile only a **whitelist of known messages:**
- `Integer>>+`, `Integer>>-`, `Integer>>*`, `Integer>>—`
- `Integer>>compare`, `Boolean>>ifTrue:ifFalse:`, etc.
- Reject user-defined method dispatch with compile error

### Challenge 2: Lack of Virtual Method Tables (vtables)

**Problem:**
- Smalltalk supports polymorphism: same message to different types
- Backends cannot allocate runtime vtables
- 8-bit systems have limited memory

**Solution:**
- Only allow **monomorphic** code paths
- Compile each type separately
- Use dispatch manually (e.g., via explicit if/else on type)

### Challenge 3: No Reflection at Runtime

**Problem:**
- Smalltalk's reflection (`.class`, `.methods`, `.inspect`) requires runtime introspection
- Backends generate static code with no symbol table

**Solution:**
- Compile-time reflection only
- Reject `.class`, `.methods`, etc. with error
- Use COBOL-style declarations instead

---

## Part 6: Summary of Smalltalk's Unique Characteristics

### Pure OO Nature
- **Everything is an object** (except variables)
- **Computation via message passing** (no primitive functions)
- **Late binding** (method lookup at runtime)
- **Reflection** (runtime introspection of code structure)
- **Live coding** (modify code while running)

### Impact on AST Generation
1. **Literals are bare values** (not wrapped nodes) — EIGHTBOL backends expect this
2. **Move nodes use canonical keys** (`:from`/`:to`) — consistency across frontends
3. **Message sends → invoke/call nodes** — backends must know what method is being called
4. **Polymorphism not supported** — backends need static resolution at compile time
5. **Reflection must be rejected** — cannot inspect runtime structure in 8-bit code

### Issues Found
1. ✗ Literal wrappers (breaks canonical AST)
2. ✗ Non-canonical move keys (breaks AST contract)
3. ✗ Non-canonical invoke keys (inconsistent with other frontends)
4. ⚠ Incomplete loop support (collection iteration)
5. ⚠ No reflection support (will need error handling)

---

## References

- **Wikipedia:** [Smalltalk](https://en.wikipedia.org/wiki/Smalltalk)
- **Squeak/Smalltalk:** [Official Site](https://squeak.org/)
- **Squeak by Example:** [GitHub - 6.0 Edition](https://github.com/hpi-swa-lab/SqueakByExample-english/)
- **Related Audit:** `doc/lingo_research_audit.md` (similar issues found in Lingo frontend)
- **AST Definition:** `src/ast.lisp` (canonical node shapes)
- **Grammar Build:** `src/grammar-build.lisp` (AST constructor patterns)
