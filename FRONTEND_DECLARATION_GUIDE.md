# Frontend Declarations Implementation Guide

## Overview

All 17 frontends must be updated to:
1. Scan for `(declare ...)` annotations in comments preceding procedures/methods/programs
2. Call `parse-declare-annotation(comment-text)` to extract declarations
3. Pass extracted declarations to `make-program-node`, `make-procedure-node`, or `make-method-node` via `:declare` keyword

## Supported Declaration Forms

```lisp
(declare
  (optimize (speed N) (space N) (safety N))    ;; N ∈ [0,3]
  (temp Var1 Var2 Var3)                        ;; Additional temporaries
)
```

## Comment Patterns by Language

### BASIC
```basic
1000 REM (declare (optimize (speed 3) (space 1)) (temp TempX TempY))
1010 LET X = ComplexExpr
```

**Parser location:** `src/frontend-basic/basic-parser.lisp`

**Update pattern:**
- In statement parsing rule, check for comment preceding procedure/method  
- Call `parse-declare-annotation` on comment text
- Pass result to `make-procedure-node(:declare declarations)`

### COBOL  
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyProg.
      *> (declare (optimize (space 3) (speed 1)))
       PROCEDURE DIVISION.
           MOVE X TO Y.
```

**Parser location:** `src/frontend-cobol/cobol-parser.lisp`

**Update pattern:**
- Scan preceding comments before PROCEDURE DIVISION or paragraph
- Call `parse-declare-annotation` on comment
- Pass to `make-method-node(:declare declarations)`

### Prolog / BurgerMistress
```prolog
%% (declare (optimize (speed 3) (safety 2)) (temp IntA IntB))
complex_calc :- X is Y * Z + Foo.
```

**Parser location:** `src/frontend-burgermistress/burger-parser.lisp`

**Update pattern:**
- Scan `%%` comment lines preceding clause definition
- Call `parse-declare-annotation`
- Pass to `make-procedure-node(:declare declarations)`

### Forth
```forth
( declare (optimize (speed 3)) (temp temp-val) )
: complex-calc ... ;
```

**Parser location:** `src/frontend-forth/forth-parser.lisp`

**Update pattern:**
- Scan `( ... )` comment blocks preceding `:` definition
- Call `parse-declare-annotation`
- Pass to `make-procedure-node(:declare declarations)`

### Fortran
```fortran
C     (declare (optimize (speed 2) (safety 1)))
      SUBROUTINE CALCULATE(X, Y)
```

**Parser location:** `src/frontend-fortran/fortran-parser.lisp`

**Update pattern:**
- Scan `C` comment lines preceding subroutine
- Call `parse-declare-annotation`
- Pass to `make-procedure-node(:declare declarations)`

### Objective / Smalltalk
```smalltalk
"(declare (optimize (speed 3) (space 1)))"
method: #calculate ^result
```

**Parser location:** `src/frontend-objective/objective-parser.lisp`

**Update pattern:**
- Scan string literal comments preceding method
- Call `parse-declare-annotation`
- Pass to `make-method-node(:declare declarations)`

### Pascal
```pascal
{ (declare (optimize (speed 3) (safety 2))) }
procedure Calculate;
begin
```

**Parser location:** `src/frontend-pascal/pascal-parser.lisp`

**Update pattern:**
- Scan `{ ... }` comment blocks preceding procedure declaration
- Call `parse-declare-annotation`
- Pass to `make-procedure-node(:declare declarations)`

### Lua
```lua
-- (declare (optimize (speed 3) (space 1)))
function calculate(x)
end
```

**Parser location:** `src/frontend-lua/lua-parser.lisp`

**Update pattern:**
- Scan `--` comment lines preceding function definition
- Call `parse-declare-annotation`
- Pass to `make-procedure-node(:declare declarations)`

### ZIL
```zilch
"(declare (optimize (speed 2) (safety 1)))"
<ROUTINE CALCULATE (X)>
```

**Parser location:** `src/frontend-zil/zil-parser.lisp`

**Update pattern:**
- Scan string comment before `<ROUTINE>` definition
- Call `parse-declare-annotation`
- Pass to `make-procedure-node(:declare declarations)`

### AGI
```agi
; (declare (optimize (speed 3)))
[ load.v v0 v0 ]
```

**Parser location:** `src/frontend-agi/agi-parser.lisp`

**Update pattern:**
- Scan `;` comment lines preceding statement/routine
- Call `parse-declare-annotation`
- Pass appropriate `:declare` to node maker

### SCUMM
```scumm
; (declare (optimize (speed 3)))
:Actor {
```

**Parser location:** `src/frontend-scumm/scumm-parser.lisp`

### Lingo (Director)
```lingo
-- (declare (optimize (speed 2) (safety 1)))
on mouseUp
end mouseUp
```

**Parser location:** `src/frontend-lingo/lingo-parser.lisp`

### Fountain (Markdown-like)
```fountain
# (declare (optimize (speed 3)))
## Scene: Interior Tavern

HERO
(dialog text)
```

**Parser location:** `src/frontend-fountain/fountain-parser.lisp`

### Goal (Game AI)
```goal
; (declare (optimize (speed 3)))
(method character-play-animation ...)
```

**Parser location:** `src/frontend-goal/goal-parser.lisp`

### SCI (Adventure Game Language)
```sci
; (declare (optimize (space 3) (speed 1)))
(procedure calculate (x y)
```

**Parser location:** `src/frontend-sci/sci-parser.lisp`

### Muddle (MOD/Lisp-like)
```muddle
;; (declare (optimize (speed 3) (safety 2)))
<DEFINE CALCULATE (X)
```

**Parser location:** `src/frontend-muddle/muddle-parser.lisp`

## Implementation Steps

### Step 1: Update Parser
For each frontend, identify where procedures/methods/programs are constructed:

```lisp
;; BEFORE:
(make-procedure-node name :statements stmts)

;; AFTER:
(let ((decl (parse-declare-annotation preceding-comment)))
  (make-procedure-node name 
    :statements stmts
    :declare decl))
```

### Step 2: Capture Preceding Comment
Each frontend must capture the comment immediately before the procedure/method/program:

```lisp
;; In lexer or parser:
(defvar *last-comment* nil
  "Most recent comment text, for declaration extraction")

;; When processing comment token:
(setf *last-comment* comment-text)

;; When processing procedure:
(let ((declarations (when *last-comment* 
                      (parse-declare-annotation *last-comment*))))
  (setf *last-comment* nil)  ;; Clear after use
  (make-procedure-node name
    :statements stmts
    :declare declarations))
```

### Step 3: Validate
- Test that `parse-declare-annotation` correctly extracts forms
- Test that `make-procedure-node` accepts `:declare` keyword
- Verify AST contains `:declare` keyword with list of forms

## Testing Template

For each frontend `xxx`:

```lisp
(defun test-xxx-declarations ()
  "Test that XXX frontend parses declarations from comments."
  (let* ((source "(declare (optimize (speed 3) (space 1)))
procedure test_proc
...
end")
         (ast (parse-xxx source)))
    ;; Verify :declare keyword present
    (assert (safe-getf (rest ast) :declare))
    ;; Verify declaration forms
    (let ((decls (safe-getf (rest ast) :declare)))
      (assert (member 'optimize decls :key #'first))
      (assert (member 'temp decls :key #'first)))))
```

## Verification Checklist

For each frontend:
- [ ] Lexer/parser captures comments
- [ ] `parse-declare-annotation` called before procedure/method/program
- [ ] `:declare` keyword passed to node maker
- [ ] AST nodes include `:declare` in plist
- [ ] Unit test passes
- [ ] Example with declarations compiles successfully

## Files to Update

1. `src/frontend-basic/basic-parser.lisp`
2. `src/frontend-burgermistress/burger-parser.lisp`
3. `src/frontend-cobol/cobol-parser.lisp`
4. `src/frontend-forth/forth-parser.lisp`
5. `src/frontend-fortran/fortran-parser.lisp`
6. `src/frontend-fountain/fountain-parser.lisp`
7. `src/frontend-goal/goal-parser.lisp`
8. `src/frontend-lingo/lingo-parser.lisp`
9. `src/frontend-lua/lua-parser.lisp`
10. `src/frontend-muddle/muddle-parser.lisp`
11. `src/frontend-objective/objective-parser.lisp`
12. `src/frontend-pascal/pascal-parser.lisp`
13. `src/frontend-sci/sci-parser.lisp`
14. `src/frontend-scumm/scumm-parser.lisp`
15. `src/frontend-smalltalk/smalltalk-parser.lisp`
16. `src/frontend-zil/zil-parser.lisp`
17. `src/frontend-agi/agi-parser.lisp`
