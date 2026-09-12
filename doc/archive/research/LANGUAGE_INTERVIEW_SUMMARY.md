# EIGHTBOL Language Frontend Conformance Interview Summary

**Date:** September 9, 2026  
**Purpose:** Record frontend language conformance decisions for all 17 languages

---

## Canonical AST Forms (Reference)

### Arithmetic Operators
- `:+` (addition)
- `:-` (subtraction)
- `:×` (multiplication, U+00D7)
- `:÷` (division, U+00F7)

### Condition Operators
- `=` (equals)
- `≠` (not equals, U+2260)
- `<` (less than)
- `≤` (less than or equal, U+2264)
- `>` (greater than)
- `≥` (greater than or equal, U+2265)

### Negation/Bitwise
- `¬` (logical NOT, U+00AC)
- `:bit-and`, `:bit-or`, `:bit-xor`, `:bit-not` (bitwise operations)
- `(:ash :value val :shift bits)` (arithmetic shift)

### Variables
**All variables are either:**
- **Globals** (from copybook)
- **Instance slots** (from copybook)
- No implicit variable creation in any frontend

---

## Language Decisions

### 1. COBOL ✓ (COMPLETED)
- `:procedure` nodes for standalone paragraphs
- `:dd` verified for data definitions
- COPY expands inline (no `:copy` node)
- 12 backends updated to handle `:procedure`

### 2. BASIC (PENDING)
- Remove `:set` node (use `:move` instead)
- PCOPY → `:string-blt`
- Subscript/refmod/of/address-of support
- Bit operators (AND/OR/XOR/NOT, shifts)
- NULL/SELF keywords
- RETURN/END → `:goback`
- Fix `*` and `/` to emit `:×` and `:÷`

### 3. AGI ✓ (COMPLETED)
- All 6 loop forms → `:perform`
- CASE → `:evaluate`
- Inspect functions (CHARACTER/COUNT/POSITION/LENGTH)
- `:subscript`, `:refmod`, `:address-of`
- Bitwise & | ^ ~
- NULL/SELF, LOG FAULT, DEBUG BREAK
- RETURN/QUIT → `:goback`

### 4. ZIL ✓ (COMPLETED)
- <TELL> → variable assignments + library calls (no `:dialogue`)
- <COPY> → `:string-blt`
- <LOG-FAULT> → `:log-fault`
- <BREAK> → `:debug-break`
- Inspect functions (CHARACTER/COUNT/POSITION/LENGTH)
- <COND> → `:evaluate`
- <GET>/<AREF>/<SUBSTRING>/<ADDRESSOF>
- Bitwise <BAND>/<BOR>/<BXOR>/<BNOT>
- <RETURN>/<EXIT> → `:goback`

### 5. Forth ✓ (COMPLETED)
- Arithmetic + - * / → canonical `:+`, `:-`, `:×`, `:÷`
- Conditions = < > → standard operators
- @ ! → `:move`
- CMOVE/CMOVE> → `:string-blt`
- All loop forms → `:if`/`:perform`
- : word → `:method` with `:assembly-entry`
- ; EXIT → `:goback`
- S" ... " INCLUDED inline
- CASE/OF/ENDCASE → `:evaluate`
- Stack-based FIELD/ARRAY/SUBSTRING/ADDR
- NULLIFY/SELF/NULL?
- AND/OR/XOR/NOT/LSHIFT/RSHIFT
- EMIT/TYPE/./INPUT → library calls

### 6. Fountain ✓ (COMPLETED)
- Complete frontend created
- Each script → `:procedure`
- # labels → `:goto`
- SUM/DIFFERENCE/PRODUCT/QUOTIENT OF → arithmetic
- SHIFT LEFT/RIGHT OF
- Nullify/Self/Is Null/Is Not Null
- LENGTH OF/CHARACTER AT/POSITION OF/COUNT OF
- FROM/ELEMENT → `:subscript`
- SUBSTRING OF → `:refmod`
- ADDRESS OF → `:address-of`
- BLOCK COPY OF → `:string-blt`
- WHEN/UNLESS → `:if`
- REPEAT → `:perform`
- COND → `:evaluate`

### 7. Lua (PENDING - 80% DONE)
- Fix * / → `:×` `:÷` (not internal symbols)
- and/or/not → `:and/:or/:not`
- goto → `:goto`
- return → `:goback`
- nil → `:null`
- self → `:self`
- obj.field/obj['field'] → `:of`
- arr[index] → `:subscript`
- Still needs: function → `:procedure`, string.sub → `:refmod`, BLT/log_fault/debug_break functions, bitwise ops, nil? test

### 8. Pascal ✓ (INTERVIEWED)
- CASE/OF → `:evaluate` (unified with COBOL/AGI/Fountain)
- LogFault(code) / DebugBreak(code) library functions → `:log-fault` / `:debug-break`
- StringLength(s) / StringCopy(s, start, len) / StringFind(haystack, needle) → `:string-length` / `:string-blt` / custom `:string-find`
- **EXCLUDE:** Sets (omit support, keep AST simple)
- **EXCLUDE:** WITH statement (parser expands to explicit `:of` nodes)

### 9. Lingo ✓ (INTERVIEWED)
- **FIX BUG:** AND/OR/NOT emit canonical `:and/:or/:not` (not Lisp symbols)
- **FIX BUG:** * / emit canonical `:×` `:÷` (not `:compute` nodes)
- **Implement:** while/until loops → `:perform` nodes
- **Add:** Bitwise syntax `a AND-BIT b`, `a OR-BIT b`, `a XOR-BIT b`, `NOT-BIT a` → `:bit-and/:bit-or/:bit-xor/:bit-not`
- **Add:** StringLength() / StringSubstring() library functions
- **EXCLUDE:** Director-specific features (sprites, channels, event handlers) — scripting layer only

### 10. Smalltalk ✓ (INTERVIEWED)
- **FIX BUG:** Remove literal wrappers (emit bare strings/numbers, not `:literal-string` nodes)
- **FIX BUG:** Fix move keys to canonical `:from/:to` (not `:variables/:expressions`)
- **FIX BUG:** Fix invoke keys to canonical `:object/:method` (not `:receiver/:selector`)
- **IMPLEMENT:** Blocks/closures → inline to `:procedure` nodes
- **IMPLEMENT:** Message cascading → expand to sequence of `:invoke` statements

### 11. FORTRAN ✓ (INTERVIEWED)
- Support * / → `:×` `:÷`
- **EXCLUDE:** Exponentiation (**) — no canonical `:exponent` node in AST
- **EXCLUDE:** String concatenation (//) — FORTRAN rarely needs strings on 8-bit systems
- **EXCLUDE:** COMMON blocks — require modern FORTRAN with MODULEs
- **RULE:** All variable declarations generate copybook entries (no implicit variables)

### 12. Muddle ✓ (INTERVIEWED)
- **FIX CRITICAL BUG:** `:go-back` → `:goback` (affects method returns across all backends)
- **IMPLEMENT:** PROG/GO/BACK/AGAIN support
  - PROG with `:assembly-entry` label
  - GO/BACK/AGAIN all map to `:goto`
- **IMPLEMENT:** Both COND and IF forms
  - COND → `:evaluate`
  - IF → `:if`

### 13. SCI ✓ (INTERVIEWED)
- **FIX CRITICAL BUG:** `:go-back` → `:goback`
- **FIX BUG:** and/or/not emit canonical `:and/:or/:not` (not non-canonical symbols)
- **FIX BUG:** Condition operators emit canonical symbols `=`, `≠`, `<`, `≤`, `>`, `≥` (not `:eq/:neq/:lt/:gt/:le/:ge`)

### 14. SCUMM ✓ (INTERVIEWED)
- **FIX CRITICAL BUG:** `:perform :name` → `:perform :procedure` (breaks all loops)
- **IMPLEMENT:** Game-engine features (actors, rooms, objects)
  - Compile to canonical AST: variable assignments + library calls
  - Like AGI/SCI/Fountain approach
- **IMPLEMENT:** All numeric formats (hex, octal, binary, DWORD)

### 15. BurgerMistress ✓ (INTERVIEWED)
- **FIX CRITICAL BUG:** (:* a b) / (:/ a b) → `:×` / `:÷`
- **IMPLEMENT:** Shift operators (<<, >>) → `(:ash :value ... :shift ...)`
- **IMPLEMENT:** Array subscripting `array[index]` → `:subscript`
- **IMPLEMENT:** FOR STEP extraction and use
- **IMPLEMENT:** Field access `object.field` → `:of`

### 16. Goal ✓ (INTERVIEWED)
- **FIX CRITICAL BUG:** (:* a b) / (:/ a b) → `:×` / `:÷`
- **IMPLEMENT:** Loop constructs (while, do, until, loop) → `:perform`
- **IMPLEMENT:** Pattern matching (cond, case) → `:evaluate`
- **IMPLEMENT:** Bitwise operators
  - & | ^ ~ → `:bit-and/:bit-or/:bit-xor/:bit-not`
  - << >> → `(:ash :value ... :shift ...)`

### 17. Objective-C ✓ (INTERVIEWED)
- **CREATE** new frontend `src/frontend-objective/`
- **IMPLEMENT:** Core C + Objective-C extensions
  - C syntax for variables, functions, control flow
  - Objective-C message sends (method invocation)
  - No advanced features yet (categories, protocols)

---

## Summary of Bugs Found

| Language | Bug | Severity | Fix |
|----------|-----|----------|-----|
| Lingo | AND/OR/NOT emit symbols | HIGH | 2-line fix |
| Lingo | * / emit `:compute` | HIGH | 2-line fix |
| Smalltalk | Literal wrappers | HIGH | 3-line fix |
| Smalltalk | Move keys | HIGH | 2-line fix |
| Smalltalk | Invoke keys | HIGH | 2-line fix |
| Muddle | `:go-back` instead of `:goback` | **CRITICAL** | 1-line fix |
| SCI | `:go-back` instead of `:goback` | **CRITICAL** | 1-line fix |
| SCI | and/or/not symbols | HIGH | 2-line fix |
| SCI | Condition operators (:eq/:neq etc.) | HIGH | 6-line fix |
| SCUMM | `:perform :name` instead of `:procedure` | **CRITICAL** | 1-line fix |
| BurgerMistress | (:* (:/ instead of :×:÷ | **CRITICAL** | 2-line fix |
| Goal | (:* (:/ instead of :×:÷ | **CRITICAL** | 2-line fix |

---

## Summary of Missing Features

| Language | Feature | Complexity | Priority |
|----------|---------|-----------|----------|
| Lua | function → `:procedure` | LOW | HIGH |
| Lua | string.sub → `:refmod` | LOW | HIGH |
| Lua | Bitwise operators | MEDIUM | MEDIUM |
| Pascal | StringLength/Copy/Find functions | LOW | MEDIUM |
| Lingo | AND-BIT/OR-BIT syntax | MEDIUM | HIGH |
| Lingo | StringLength/StringSubstring | LOW | MEDIUM |
| Smalltalk | Blocks → `:procedure` | MEDIUM | MEDIUM |
| Smalltalk | Message cascading | LOW | MEDIUM |
| FORTRAN | All features (copybook generation) | HIGH | LOW |
| Muddle | PROG/GO/BACK/AGAIN | MEDIUM | HIGH |
| Muddle | COND and IF | MEDIUM | HIGH |
| SCUMM | Game-engine → canonical AST | MEDIUM | HIGH |
| BurgerMistress | Shifts, arrays, STEP, fields | MEDIUM | HIGH |
| Goal | Loops, patterns, bitwise | MEDIUM | HIGH |
| Objective-C | Core C + Obj-C | HIGH | MEDIUM |

---

## Next Steps

1. **Implement all critical bugs** (Muddle, SCI, SCUMM, BurgerMistress, Goal) — high-impact, low effort
2. **Implement high-priority missing features** (Lua, Lingo, Muddle, SCUMM, BurgerMistress, Goal)
3. **Implement medium-priority features** (Smalltalk, Pascal, Objective-C)
4. **Run comprehensive test suite** to validate all conformance fixes
5. **Fix any regressions** exposed by tests

---

## Commit Strategy

Each language conformance implementation will be committed separately:
- `feat: conform <language> frontend to canonical AST`
- Include summary of bugs fixed and features added
- Reference this document as context

Final comprehensive commit:
- `feat: all language frontends conform to canonical AST`
- All 17 languages complete
- All tests passing
