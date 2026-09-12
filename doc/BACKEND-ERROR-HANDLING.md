# EIGHTBOL Backend Error Handling Standardization

## Overview

This document describes the standardized error handling system for EIGHTBOL backends. Instead of using plain string errors, all backends now use structured condition classes that provide:

1. **Type safety** - Errors are properly typed conditions
2. **Semantic information** - Errors include context (CPU, statement, etc)
3. **Programmatic handling** - Catch and handle specific error types
4. **Recovery mechanisms** - Support for restart-case error recovery

## Condition Class Hierarchy

```
COMPILER-ERROR (base)
├── BACKEND-ERROR
│   ├── backend-ast-error
│   ├── backend-expression-error
│   ├── backend-byte-access-error
│   ├── backend-procedure-error
│   ├── backend-loop-control-error
│   ├── backend-symbol-not-found
│   ├── backend-unsupported-feature
│   ├── backend-internal-error
│   ├── backend-string-blt-error
│   ├── backend-call-service-error
│   ├── backend-copy-not-expanded
│   ├── backend-condition-not-implemented
│   └── backend-ast-error
└── ... (other error types)
```

## Condition Classes and Usage

### 1. backend-ast-error

**Purpose:** Signaled when AST node structure is invalid

**Slots:**
- `:cpu` - Target CPU (e.g., `:6502`, `:z80`)
- `:message` - Human-readable error message
- `:expected` - Expected AST type (usually `:program`)
- `:actual` - Actual value received

**Example:**
```lisp
(error 'backend-ast-error
  :cpu :6502
  :message "expected :program AST node, got :method"
  :expected :program
  :actual :method)
```

**Location:** `src/conditions.lisp:92-99`

---

### 2. backend-expression-error

**Purpose:** Expression evaluation fails or expression type is unknown

**Slots:**
- `:cpu` - Target CPU
- `:message` - Error description
- `:reason` - Why the expression failed (optional)
- `:expression` - The problematic expression

**Example:**
```lisp
(error 'backend-expression-error
  :cpu :6502
  :message "Unknown expression type in constant check"
  :reason "not in expression handlers table"
  :expression '(:unknown-op 42))
```

**Used in:**
- `backend-6502/backend-6502-part2.lisp` - constant evaluation
- Expression handler lookups where type is unrecognized

**Location:** `src/conditions.lisp:101-116`

---

### 3. backend-byte-access-error

**Purpose:** Byte index is out of range for target value width

**Slots:**
- `:cpu` - Target CPU
- `:message` - Error description
- `:byte-index` - Index being accessed (0-based)
- `:width` - Actual width of value in bytes

**Example:**
```lisp
(error 'backend-byte-access-error
  :cpu :6502
  :message "Attempted to access byte 5 of a 4 byte object"
  :byte-index 5
  :width 4)
```

**Used in:**
- `backend-6502/backend-6502-part3.lisp` - load/store byte operations
- Multi-byte arithmetic operations

**Location:** `src/conditions.lisp:118-131`

---

### 4. backend-procedure-error

**Purpose:** Procedure/statement structure is invalid or requirements not met

**Slots:**
- `:cpu` - Target CPU
- `:message` - Error description
- `:statement` - The problematic statement (optional)

**Example:**
```lisp
(error 'backend-procedure-error
  :cpu :6502
  :message "PERFORM with inline body requires UNTIL, TIMES, or VARYING"
  :statement nil)
```

**Used in:**
- PERFORM statement validation
- Malformed procedure statements
- Missing required clauses

**Location:** `src/conditions.lisp:133-145`

---

### 5. backend-loop-control-error

**Purpose:** BREAK or CONTINUE used outside of loop context

**Slots:**
- `:cpu` - Target CPU
- `:message` - Error description
- `:statement-type` - Either "BREAK" or "CONTINUE"

**Example:**
```lisp
(error 'backend-loop-control-error
  :cpu :6502
  :message "BREAK statement outside of PERFORM loop"
  :statement-type "BREAK")
```

**Used in:**
- BREAK statements outside PERFORM
- CONTINUE statements outside loops

**Location:** `src/conditions.lisp:147-161`

---

### 6. backend-symbol-not-found

**Purpose:** Variable, constant, or class cannot be found

**Slots:**
- `:cpu` - Target CPU
- `:message` - Error description
- `:symbol-name` - Name of the undefined symbol
- `:symbol-type` - Type: `:variable`, `:constant`, or `:class`

**Example:**
```lisp
(error 'backend-symbol-not-found
  :cpu :6502
  :message "Cannot determine parent class for MyClass"
  :symbol-name 'MyClass
  :symbol-type :class)
```

**Used in:**
- Undefined variable references
- Unknown class lookups
- Missing constant definitions

**Location:** `src/conditions.lisp:163-180`

---

### 7. backend-unsupported-feature

**Purpose:** Language feature is not implemented or supported on this CPU

**Slots:**
- `:cpu` - Target CPU
- `:message` - Error description
- `:feature-name` - Name of unsupported feature
- `:reason` - Why not supported (optional)

**Example:**
```lisp
(error 'backend-unsupported-feature
  :cpu :6502
  :message "Condition type not supported"
  :feature-name "condition"
  :reason "unhandled condition :signed-comparison")
```

**Used in:**
- Signed comparison (not yet implemented on 6502)
- Unimplemented EXIT statement
- Unknown condition types

**Location:** `src/conditions.lisp:182-198`

---

### 8. backend-internal-error

**Purpose:** Internal compiler bug or consistency check failure

**Slots:**
- `:cpu` - Target CPU
- `:message` - Error description
- `:context` - Where in compilation this occurred (optional)

**Example:**
```lisp
(error 'backend-internal-error
  :cpu :6502
  :context 'emit-alu-byte
  :message "ALU byte index 5 >= width 4")
```

**Used in:**
- FIXME placeholders
- Corrupted AST checks
- Sanity check failures

**Location:** `src/conditions.lisp:200-217`

---

### Existing Condition Classes

Three condition classes were already defined and are used as-is:

#### backend-string-blt-error
Used for STRING DELIMITED BY SIZE errors

#### backend-call-service-error
Used for CALL SERVICE statement errors

#### backend-copy-not-expanded
Used when COPY wasn't expanded at lex time

**Location:** `src/conditions.lisp:109-137` (existing definitions)

---

## Standardization Progress

### Completed Backends (41 errors replaced)

| Backend | File(s) | Errors | Status |
|---------|---------|--------|--------|
| 6502 | part1-6 | 29 | ✓ Complete |
| Z80 | backend-z80.lisp | 6 | ✓ Complete |
| M68K | backend-m68k.lisp | 6 | ✓ Complete |
| 65C02 | backend-65c02.lisp | 0 | ✓ Already done |
| 65C816 | backend-65c816.lisp | 0 | ✓ Already done |

### Pending Backends (54 errors to replace)

| Backend | Errors | Notes |
|---------|--------|-------|
| ARM7 | 9 | Thumb instruction set |
| CP1610 | 5 | Intellivision processor |
| F8 | 6 | Fairchild Channel F |
| FORTH | 7 | Stack-based language |
| I286 | 8 | Intel 16-bit |
| M6800 | 1 | Motorola 6800 |
| RP2A03 | 7 | NES processor |
| SM83 | 7 | Game Boy Z80 variant |
| STACK | 4 | Stack VM backend |

---

## Error Handling Patterns

### Pattern 1: Validation Error

```lisp
(unless (valid-value-p value)
  (error 'backend-expression-error
    :cpu *backend-cpu*
    :message "Invalid expression value"
    :reason "value must be numeric constant"
    :expression value))
```

### Pattern 2: Bounds Check Error

```lisp
(when (>= index width)
  (error 'backend-byte-access-error
    :cpu *backend-cpu*
    :message (format nil "Byte ~d out of bounds" index)
    :byte-index index
    :width width))
```

### Pattern 3: Missing Symbol

```lisp
(let ((handler (gethash symbol *handlers*)))
  (unless handler
    (error 'backend-symbol-not-found
      :cpu *backend-cpu*
      :message (format nil "~a not found" symbol)
      :symbol-name symbol
      :symbol-type :handler)))
```

### Pattern 4: Feature Not Implemented

```lisp
(cond
  ((supported-feature-p feature) (handle-feature feature))
  (t (error 'backend-unsupported-feature
       :cpu *backend-cpu*
       :message "Feature not implemented"
       :feature-name feature
       :reason "under development")))
```

---

## Integration with REPL/Testing

### Catching Backend Errors

```lisp
(handler-case
    (compile-to-assembly ast-node :6502 output-stream)
  (backend-expression-error (e)
    (format t "Expression error: ~a~%" (eightbol-error-message e)))
  (backend-byte-access-error (e)
    (format t "Out of bounds at byte ~d/~d~%"
            (eightbol-backend-byte-index e)
            (eightbol-backend-width e)))
  (backend-error (e)
    (format t "Backend error on ~a: ~a~%"
            (eightbol-backend-cpu e)
            (eightbol-error-message e))))
```

### Testing Error Conditions

```lisp
(fiveam:test backend-expression-error-test
  (fiveam:signals backend-expression-error
    (error 'backend-expression-error
      :cpu :6502
      :message "test"
      :expression nil)))
```

---

## Restart Cases (Future Enhancement)

Error recovery mechanisms are planned but not yet implemented. When complete, recommended restart points will include:

- `retry-with-simpler-expression` - Substitute simpler expr
- `use-alternate-temporary` - Use different temp variable
- `skip-optimization` - Skip opt pass and retry
- `use-fallback-procedure` - Use fallback implementation
- `continue-with-warning` - Log warning and continue
- `abort-method` - Skip current method
- `store-temp-result` - Cache intermediate
- `use-register-indirect` - Try indirect addressing

Usage example (when implemented):

```lisp
(restart-case
    (compile-expression expression)
  (retry-with-simpler-expression ()
    :report "Use simplified version of expression"
    (compile-expression (simplify-expression expression)))
  (use-alternate-temporary (temp)
    :report "Use a different temporary variable"
    :interactive (list (prompt-for-temp))
    (compile-with-temp expression temp)))
```

---

## Files Modified

- `src/conditions.lisp` - Condition class definitions
- `src/package.lisp` - Public exports
- `src/backend-6502/backend-6502-part*.lisp` - 6 files
- `src/backend-z80/backend-z80.lisp`
- `src/backend-m68k/backend-m68k.lisp`

---

## See Also

- `EIGHTBOL.texi` - Main compiler documentation
- `src/conditions.lisp` - Source of all condition definitions
- `AGENTS.md` - Build and testing guidelines

