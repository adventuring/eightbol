# QUICK REFERENCE: Error Handling Guide for EIGHTBOL Contributors

**Purpose:** Understand error handling requirements and restart patterns  
**Audience:** EIGHTBOL developers, contributors  
**Status:** After audit completion (2026-09-09)

---

## TL;DR: Current State vs. Target

### Current (2026-09-09)
- ✅ 23 error classes (well-organized)
- ✅ 165 error tests
- ❌ 2 restarts (insufficient)
- ❌ 0 restart tests
- ❌ Parser errors = halt
- ❌ Multi-file compilation fragile

### Target (Post-Implementation)
- ✅ 23 error classes (with numeric IDs)
- ✅ 50+ restart tests
- ✅ 15+ restarts across pipeline
- ✅ Parser recovers per-method
- ✅ Copybook resolution restartable
- ✅ Skip/continue options throughout

---

## When to Use Which Error Class

### For Parser/Syntax Errors

```lisp
;; ✓ CORRECT: Use source-error
(error 'source-error
       :source-file (current-file)
       :source-line (current-line)
       :source-sequence (current-seq)
       :message "Unexpected token DIVIDE")

;; ✗ WRONG: Generic error
(error "Unexpected token")

;; ✗ WRONG: No context
(error 'compiler-error :message msg)
```

### For Code Generation Failures

```lisp
;; ✓ CORRECT: Use backend-error with CPU
(error 'backend-error
       :cpu :6502
       :message "DIVIDE not supported"
       :detail "Use bit shift or target 65C816")

;; ✗ WRONG: No CPU context
(error "DIVIDE not supported")

;; ✗ WRONG: Wrong error type
(error 'source-error :message msg)
```

### For Copybook Issues

```lisp
;; ✓ CORRECT: Use specific copybook error
(error 'copybook-not-found
       :copybook-name "Classes"
       :library "Source/Classes"
       :message "Cannot find copybook")

;; ✗ WRONG: Generic error
(error "Can't find Classes")

;; ✗ WRONG: Wrong subtype
(error 'compiler-error :message msg)
```

### For Validation Issues

```lisp
;; ✓ CORRECT: Use specific validation error
(error 'undefined-class-reference
       :class-name "NPC"
       :defined-set defined-classes
       :message "Class NPC not defined")

;; ✓ ALSO OK: Use routine-not-terminated
(error 'routine-not-terminated
       :method-id "Think"
       :message "Method lacks terminating GOBACK")

;; ✗ WRONG: Bare compiler-error
(error 'compiler-error :message msg)
```

---

## Error Message Best Practices

### Format: Location + Context + Message + Suggestion

```
✓ GOOD:
  Character.cob:125 (seq 5): Expected MOVE, got DIVIDE (unsupported on 6502)

✗ POOR:
  Unexpected token

✓ GOOD:
  6502: DIVIDE not supported (try bit shifts or use 65C816)

✗ POOR:
  error in backend
```

### Include Context Where Relevant

```lisp
;; ✓ With file/line
(error 'source-error
       :source-file "Character.cob"
       :source-line 125
       :source-sequence 5
       :message "Expected MOVE, got DIVIDE"
       :terminal :DIVIDE
       :expected '(:MOVE :ADD :SUBTRACT))

;; ✗ Without context
(error 'source-error
       :message "Unexpected token")
```

### Actionable Messages

```lisp
;; ✓ Tells developer what to do
(error 'backend-error
       :cpu :6502
       :message "DIVIDE not supported on 6502"
       :detail "Use bit shift right/left for division")

;; ✗ Just says "no"
(error 'backend-error
       :message "Not supported")
```

---

## When to Add Restarts

### Rule 1: Add `:skip` for Optional Operations

```lisp
;; Example: Copybook resolution
(restart-case
    (find-copybook name)
  (skip-copy ()
    :report (format nil "Skip COPY ~s" name)
    nil))

;; Example: Per-file compilation
(dolist (file input-files)
  (restart-case
      (compile-file file)
    (skip-file ()
      :report "Skip this file, continue to next."
      (continue))))
```

### Rule 2: Add `:continue` for Non-Fatal Issues

```lisp
;; Validation warning
(restart-case
    (validate-program ast)
  (continue ()
    :report "Continue despite validation warnings."
    ast))
```

### Rule 3: Add `:use-value` for Resolution Fallbacks

```lisp
;; Copybook alternate names
(restart-case
    (find-copybook-file name)
  (use-copy-as (alternative)
    :report "Use alternative copybook name."
    :interactive (lambda () (list (read)))
    (find-copybook-file alternative)))
```

### Rule 4: Always provide `:report`

```lisp
;; ✓ GOOD: User sees this in debugger
(restart-case
    (risky-operation)
  (skip-operation ()
    :report "Skip this operation and continue.")  ; ← User sees this

;; ✗ BAD: No user guidance
(restart-case
    (risky-operation)
  (skip-operation ()
    nil)))  ; ← User doesn't know what this does
```

---

## Testing Error Handling

### Test 1: Verify Error is Signaled

```lisp
(test backend/divide-signals-error
  "DIVIDE on 6502 should signal backend-error."
  (signals eightbol::backend-error
    (compile-to-asm :6502 '(:divide :from "A" :into "B"))))
```

### Test 2: Verify Restart Exists

```lisp
(test restart/skip-copy-available
  "skip-copy restart should be available for missing copybook."
  (handler-bind
      ((copybook-not-found
        (lambda (e)
          (declare (ignore e))
          (is (find-restart 'skip-copy))  ; ← Restart exists
          (invoke-restart 'skip-copy))))
    (compile-eightbol "test.cob")))
```

### Test 3: Verify Restart Works

```lisp
(test restart/skip-file-continues
  "Skipping one file should compile others."
  (let ((files '("bad.cob" "good.cob" "also-good.cob"))
        (compiled '()))
    (handler-bind
        ((source-error
          (lambda (e)
            (when (string-equal (file e) "bad.cob")
              (invoke-restart 'skip-file)))))
      (dolist (f files)
        (restart-case
            (progn
              (compile-eightbol f)
              (push f compiled))
          (skip-file ()
            (continue)))))
    (is (equal (reverse compiled) '("good.cob" "also-good.cob")))))
```

---

## Adding a New Error Type

### Step 1: Define Condition Class

```lisp
;; In src/conditions.lisp

(define-condition my-new-error (backend-error)
  ((context :initarg :context :reader eightbol-my-context :initform nil))
  (:documentation "Signalled when ...")
  (:report (lambda (c s)
             (format s "~@[~a: ~]my error: ~a~@[ (~a)~]"
                     (eightbol-backend-cpu c)
                     (eightbol-error-message c)
                     (eightbol-my-context c)))))
```

### Step 2: Signal It

```lisp
;; In backend code

(error 'my-new-error
       :cpu :6502
       :message "Description of what went wrong"
       :context "Additional details"
       :error-id 42)  ; New: numeric ID
```

### Step 3: Add Test

```lisp
;; In tests/backend-6502-tests.lisp

(test backend-6502/my-new-error
  "My new error should signal with correct details."
  (signals eightbol::my-new-error
    (compile-to-asm :6502 '(:problematic-ast))))
```

### Step 4: Consider Restart

```lisp
;; Should user be able to recover?

(restart-case
    (risky-backend-operation)
  (skip-operation ()
    :report "Skip this operation."
    nil)
  (use-fallback (value)
    :report "Use alternative code sequence."
    value))
```

---

## Common Patterns

### Pattern 1: Multi-File with Skip

```lisp
;; src/eightbol-compile.lisp

(let ((results '()))
  (dolist (input-file input-files)
    (restart-case
        (progn
          (let ((ast (parse-eightbol-file input-file)))
            (compile-to-assembly ast cpu)))
          (push input-file results))
      (skip-file ()
        :report "Skip this file and continue."
        (continue))))  ; Loop to next file
  results)
```

### Pattern 2: Fallback Values

```lisp
;; Copybook resolution

(restart-case
    (find-copybook-in-path name search-paths)
  (use-copy-as (alt-name)
    :report "Use alternative copybook name."
    :interactive (lambda ()
                   (format *query-io* "Alternative name: ")
                   (list (read *query-io*)))
    (find-copybook-in-path alt-name search-paths))
  (use-builtin (data)
    :report "Use built-in copybook data."
    :interactive (lambda () (list (read *query-io*)))
    data))
```

### Pattern 3: Validation with Continue

```lisp
;; AST validation

(restart-case
    (progn
      (validate-object-reference-classes ast classes)
      (validate-method-terminations ast)
      ast)
  (continue ()
    :report "Continue despite validation warnings."
    ast))
```

---

## Debugging Restart Issues

### Problem: Restart Not Available

```lisp
;; ✗ Error: No restart with name :my-restart

;; Check 1: Is restart-case wrapping the code?
(restart-case
    (do-work)
  (my-restart () nil))  ; ← Must be here

;; Check 2: Is error being signaled INSIDE restart-case?
(restart-case
    (progn
      (error 'my-error)  ; ← Must be inside
      ))
  (my-restart () nil))

;; Check 3: Is handler invoking restart correctly?
(handler-bind
    ((my-error (lambda (e)
                 (invoke-restart 'my-restart))))  ; ← Correct name
  (restart-case
      (error 'my-error)
    (my-restart () :done)))
```

### Problem: Restart Not Invoked

```lisp
;; ✗ Error is thrown but restart isn't taken

;; Check: Did you invoke-restart?
(handler-bind
    ((my-error (lambda (e)
                 (invoke-restart 'my-restart)))) ; ← Don't forget!
  (restart-case
      (error 'my-error)
    (my-restart () :done)))

;; Check: Is loop continuing after restart?
(dolist (file files)
  (restart-case
      (compile file)
    (skip-file ()
      (continue))))  ; ← IMPORTANT: explicit continue
```

### Problem: Wrong Restart Invoked

```lisp
;; ✗ Invoked :skip-file but got :skip-method

;; Check: Correct restart name in invoke-restart
(invoke-restart 'skip-file)  ; ← Must match restart case name

;; Check: Nested restart-cases have right names
(restart-case         ; ← OUTER: skip-file
    (dolist (f files)
      (restart-case   ; ← INNER: skip-method
          (parse-methods f)
        (skip-method () ...)))
  (skip-file () ...))
```

---

## Error IDs (Proposed Reference)

```
E001  Unexpected token
E002  Missing required token
E003  Unsupported statement
E004  Syntax error (generic)
E005  Copybook not found
E006  Copybook invalid name
E007  Copybook read error
E008  Undefined class reference
E009  Method not terminated
E010  Unqualified variable

E011  Unsupported operation (backend)
E012  Invalid AST node
E013  Condition not implemented
E014  STRING BLT error
E015  Service call error
E016  Resource limit exceeded

E021  Parse failed
E022  Invalid AST structure
E023  Optimization failed
E024  Validation failed
E025  Unknown CPU
E026  Invalid CLI option
E027  Input file not found
E028  I/O error
```

---

## Checklist: Adding Error + Restart

- [ ] Define condition class (src/conditions.lisp)
- [ ] Add error ID (E0XX)
- [ ] Write signal call with all context
- [ ] Add :report message (user-facing)
- [ ] Consider restart(s) for recovery
- [ ] Write error test (signals ...)
- [ ] Write restart test (invoke-restart ...)
- [ ] Update error ID registry (this file)
- [ ] Document in AGENTS.md / README.md
- [ ] Run test suite: `(asdf:test-system :eightbol)`

---

## References

- **Full Audit:** `/AUDIT-ERROR-HANDLING.md`
- **Summary:** `/AUDIT-ERROR-HANDLING-SUMMARY.md`
- **Conditions:** `src/conditions.lisp`
- **Pipeline:** `src/eightbol-compile.lisp`
- **Tests:** `tests/eightbol-tests.lisp` and others

---

## Contact / Questions

If you're adding error handling to EIGHTBOL:

1. ✅ Read this guide first
2. ✅ Check error class hierarchy in full audit
3. ✅ Use appropriate condition class
4. ✅ Include context (file, line, CPU, etc.)
5. ✅ Consider restart opportunities
6. ✅ Write tests for both error AND restart
7. ✅ Run full test suite before PR

**Happy error handling! 🚀**

---

**Document:** Quick Reference for EIGHTBOL Error Handling  
**Generated:** 2026-09-09  
**Status:** Post-Audit Guide  
**Version:** 1.0
