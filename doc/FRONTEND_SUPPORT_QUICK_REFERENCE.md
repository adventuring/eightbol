# QUICK REFERENCE: Frontend AST Node Support
## One-Page Coverage Guide for Developers

Generated: September 9, 2026

---

## WHICH LANGUAGE SHOULD I USE?

### For Maximum Features (Want everything?)
→ **COBOL** (82% coverage)
- All statement types
- Most expressions  
- Good operator support
- All I/O operations

### For Modern Features (Need contemporary syntax?)
→ **Lua** (65% coverage)
- Excellent operator support
- Modern control flow
- Clean expression handling
- String operations (:string-blt)

### For Game Scripting (Building a game?)
→ **SCI** or **Lingo** or **AGI** (40-49% coverage)
- Designed for game development
- Method calling supported
- Game-specific primitives
- Domain-optimized

### For System/Low-Level (Need memory access?)
→ **BASIC** (42% coverage)
- Array subscripting
- Address-of operations
- Reference modification
- Legacy system programming

### For Procedural Classical (Need classic procedural?)
→ **Pascal** (47% coverage)
- Record/structure support  
- Goto/jumps if needed
- Method calling
- Good statement variety

---

## FEATURE SUPPORT QUICK LOOKUP

### ✅ UNIVERSAL (All 17 frontends)
- Program/method structure
- Literals and identifiers
- Basic if/then/else
- Print and input (13/17 - most)

### ⚠️ COMMON (10-15 frontends)
- :move (13/17) 
- :perform loops (11/17)
- :add/:subtract (10-11/17)
- Method calls :invoke (9/17)

### ❌ RARE (< 5 frontends)
- :call-acc (2/17) - only COBOL, AGI
- :string-blt (3/17) - only BASIC, COBOL, Lua
- :debug-break (2/17) - only COBOL, Lua
- :exit-program (2/17) - only COBOL, ZIL
- Bitwise operators (0/17) - NOT AVAILABLE

---

## WHAT'S MISSING EVERYWHERE?

### 🚨 Critical Gaps
| Feature | Status | Workaround |
|---------|--------|-----------|
| Bitwise operators (:¬ :∧ :∨ :⊻ :⊼ :⊽) | 0/17 | None - needs investigation |
| Addition operator (:+) | 0/17 | Use :add statement instead |
| String block transfer (:string-blt) | 3/17 | Limited to BASIC/COBOL/Lua |
| Function calls (:call-acc) | 2/17 | Use :invoke for methods |

---

## LANGUAGE CAPABILITY MATRIX (Quick Version)

```
Feature                COBOL  LUA  SCI  LINGO PASCAL BASIC AGI  FORTRAN
────────────────────────────────────────────────────────────────────────
Statements             ✅    ✅   ✅   ✅    ✅     ✅    ✅   ⚠️
Method calls           ✅    ✅   ✅   ✅    ✅     ❌    ❌   ✅
Loop/perform           ✅    ✅   ✅   ✅    ❌     ❌    ❌   ❌
Operators              ✅    ✅   ✅   ✅    ❌     ❌    ❌   ✅
Expressions            ✅    ✅   ⚠️   ⚠️    ⚠️     ✅    ⚠️   ⚠️
String operations      ✅    ✅   ❌   ❌    ❌     ✅    ❌   ❌
Memory access          ✅    ✅   ❌   ❌    ❌     ✅    ❌   ❌
Control flow           ✅    ✅   ⚠️   ⚠️    ✅     ⚠️    ⚠️   ⚠️
────────────────────────────────────────────────────────────────────────
OVERALL SCORE          82%   65%  49%  47%   47%    42%   40%  31%
```

---

## COMMON FEATURES BY CATEGORY

### Statements Supported
```
HIGHEST (13/17):
  ✅ :move (assignment)
  ✅ :if (conditional)
  ✅ :print (output)
  ✅ :input (input)
  ✅ :dialogue (narrative)

HIGH (11/17):
  ✅ :perform (loops)
  ✅ :add, :subtract
  ✅ :copy (include)

MEDIUM (7-9/17):
  ⚠️ :invoke (method calls)
  ⚠️ :compute (expressions)
  ⚠️ :set (variable assignment)
  ⚠️ :goto (jumps)

LOW (<5/17):
  ❌ :exit-* (early exit)
  ❌ :call-acc (function calls)
  ❌ :string-blt (string ops)
  ❌ :debug-break (debugging)
```

### Operators Available
```
Comparison - GOOD:
  ✅ := (equal)       - 10/17
  ⚠️ :≠ (not equal)   - 6/17
  ⚠️ :< (less than)   - 6/17
  ⚠️ :> (greater)     - 6/17

Arithmetic - POOR:
  ❌ :+               - 0/17 (use :add)
  ❌ :-               - 2/17 (use :subtract)
  ❌ :×               - 4/17 (use :compute)
  ❌ :÷               - 4/17 (use :compute)

Bitwise - MISSING:
  ❌ :¬ :∧ :∨ :⊻ :⊼ :⊽ - 0/17 (NOT AVAILABLE)

Shift - RARE:
  ❌ :ash             - 1/17 (only Lua)
```

### Expressions Supported
```
Basic - UNIVERSAL:
  ✅ Numbers & strings - 17/17
  ✅ Identifiers        - 17/17

Advanced - LIMITED:
  ⚠️ :of (qualified id)      - 7/17
  ❌ :address-of            - 4/17
  ❌ :subscript (arrays)     - 4/17
  ❌ :refmod (substrings)    - 3/17
  ❌ :null                   - 4/17
  ❌ :self (object self)     - 3/17
```

---

## CHOOSING OPERATORS: DO THIS

### ✅ PORTABLE (all or most languages)
```
IF x := 5 THEN          ; Equality - works in 10/17
  ...
```

### ⚠️ AVOID (limited support)
```
IF x ≠ 5 THEN           ; Not equal - only 6/17
  ...

IF x < 5 THEN           ; Less than - only 6/17
  ...
```

### ❌ DON'T USE (missing everywhere)
```
result := a + b         ; Use ADD instead
result := a - b         ; Use SUBTRACT instead
result := a * b         ; Use COMPUTE instead
result := a / b         ; Use COMPUTE instead
result := a AND b       ; Not available at all
result := a OR b        ; Not available at all
result := NOT a         ; Not available at all
```

### ✅ CORRECT ALTERNATIVES
```
ADD a TO b GIVING result        ; Portable
SUBTRACT a FROM b GIVING result ; Portable
COMPUTE result = a * b          ; Portable (when using COMPUTE)
COMPUTE result = a / b          ; Portable (when using COMPUTE)
```

---

## FEATURE AVAILABILITY BY LANGUAGE

### COBOL ✅
```
Missing: Bitwise operators, :self
Use this for: Maximum feature completeness
```

### Lua ✅
```
Missing: :goto, :call-acc, :subscript
Use this for: Modern scripting with good operators
```

### SCI, Lingo, Pascal ⚠️
```
Missing: Varies by language; see full matrix
Use this for: Domain-specific needs (games, multimedia, procedural)
```

### BASIC, AGI, Objective-C ⚠️
```
Missing: Various; incomplete operator support
Use this for: Legacy systems or specialized domains
```

### FORTRAN, Goal, ZIL, Forth, Muddle ⚠️
```
Missing: Many features; sparse support
Use this for: Very specialized needs only
```

### Burgermistress 🔴
```
Status: STUB - Do not use
```

---

## WHAT EACH STATEMENT DOES

| Statement | Purpose | Coverage | Best Languages |
|-----------|---------|----------|---|
| :move | Assign value to variable | 13/17 | All except Forth, Goal |
| :if | Conditional branch | 13/17 | All except ZIL, Forth |
| :invoke | Call method on object | 9/17 | COBOL, Lua, Pascal, ObjC, SCI |
| :add | Add to accumulator | 11/17 | COBOL, Lua, BASIC, SCI, Lingo |
| :subtract | Subtract from accumulator | 10/17 | COBOL, Lua, BASIC, SCI, Lingo |
| :compute | Evaluate complex expression | 7/17 | COBOL, Lua, SCI, Lingo |
| :perform | Loop/repeat | 11/17 | COBOL, Lua, SCI, Lingo, Pascal |
| :goto | Jump to label | 7/17 | COBOL, Pascal, BASIC, AGI, SCI |
| :set | Assign with special semantics | 7/17 | COBOL, Lua, SCI, Lingo |
| :print | Output to console | 13/17 | Most languages |
| :input | Read from console | 12/17 | Most languages |
| :dialogue | Display narrative text | 12/17 | Most languages |
| :copy | Include external file | 11/17 | COBOL, Lua, Pascal, BASIC |
| :call-acc | Function call (accumulator param) | 2/17 | COBOL, AGI only |
| :string-blt | String block transfer | 3/17 | COBOL, Lua, BASIC only |

---

## IMPLEMENTATION STATUS SUMMARY

| Metric | Value | Assessment |
|--------|-------|---|
| Total Frontends | 17 | ✅ Complete lineup |
| Total AST Nodes | 57 | ✅ Comprehensive spec |
| Fully Implemented | 24 (42%) | ⚠️ Moderate |
| Partially Implemented | 26 (46%) | ⚠️ Most features partial |
| Not Implemented | 7 (12%) | ❌ Key gaps exist |
| Average Coverage | 42% | ⚠️ Below target |
| Best Language | COBOL (82%) | ✅ Good |
| Worst Language | Burgermistress (14%) | 🔴 Unusable |
| OVERALL | 6.5/10 | ⚠️ **Needs Work** |

---

## RECOMMENDED USAGE TIERS

### TIER 1: Production (Ready for 1.0)
- **COBOL** - Reference implementation, use as baseline
- **Lua** - Best modern language support

### TIER 2: Acceptable (Known Limitations)
- **SCI, Lingo, Pascal, AGI** - Domain-specific, good within scope
- **BASIC, Objective-C** - Adequate for many use cases

### TIER 3: Limited (Experimental)
- **FORTRAN, Goal, ZIL, Fountain, SCUMM, Muddle, Forth, Objective** - Use only for specialized needs

### TIER 4: Do Not Use
- **Burgermistress** - Incomplete stub, not ready

---

## GETTING MORE DETAILS

Full documentation available in:
- `FRONTEND_AST_COVERAGE_AUDIT_2026.md` - Comprehensive analysis
- `AUDIT_FINDINGS_AND_RECOMMENDATIONS.md` - Detailed findings
- `FRONTEND_AST_COVERAGE_MATRIX_2026.csv` - Machine-readable matrix
- `src/ast.lisp` - AST specification

---

**Last Updated:** September 9, 2026  
**Status:** Current with all 17 frontends audited
