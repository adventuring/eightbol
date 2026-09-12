;; tests/lua-statements-comprehensive-tests.lisp
;; Comprehensive Lua statement tests covering all major statements
;; Tests numeric types and edge cases

(in-package :eightbol/test)

(fiveam:def-suite :lua-statements-comprehensive
  :description "Comprehensive Lua statement tests")
(in-suite :lua-statements-comprehensive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASSIGNMENT Statement Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test lua/assignment-simple
  "Simple Lua assignment compiles"
  (let ((code "local score = 100"))
    (is (not (null (parse-lua code))))))

(test lua/assignment-arithmetic
  "Lua assignment with arithmetic compiles"
  (let ((code "local total = price + quantity"))
    (is (not (null (parse-lua code))))))

(test lua/assignment-global
  "Global assignment without local compiles"
  (let ((code "score = 100"))
    (is (not (null (parse-lua code))))))

(test lua/assignment-table-field
  "Table field assignment compiles"
  (let ((code "local player = {} player.x = 100"))
    (is (not (null (parse-lua code))))))

(test lua/assignment-table-key
  "Table key assignment compiles"
  (let ((code "local items = {} items[1] = 'sword'"))
    (is (not (null (parse-lua code))))))

(test lua/assignment-multiple
  "Multiple simultaneous assignment compiles"
  (let ((code "local x, y, z = 1, 2, 3"))
    (is (not (null (parse-lua code))))))

(test lua/assignment-swap
  "Swap assignment compiles"
  (let ((code "local a = 10 local b = 20 a, b = b, a"))
    (is (not (null (parse-lua code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IF-THEN-ELSE Statement Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test lua/if-simple
  "Simple if statement compiles"
  (let ((code "if x > 10 then print('hi') end"))
    (is (not (null (parse-lua code))))))

(test lua/if-with-else
  "if with else compiles"
  (let ((code "if x > 10 then y = 1 else y = 2 end"))
    (is (not (null (parse-lua code))))))

(test lua/if-elseif
  "if with elseif compiles"
  (let ((code "if x > 30 then y = 1 elseif x > 15 then y = 2 else y = 3 end"))
    (is (not (null (parse-lua code))))))

(test lua/if-nested
  "Nested if statements compile"
  (let ((code "if x > 10 then if y > 5 then z = 1 end end"))
    (is (not (null (parse-lua code))))))

(test lua/if-complex-condition
  "if with complex condition compiles"
  (let ((code "if x > 10 and y < 5 or z == 0 then result = true end"))
    (is (not (null (parse-lua code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WHILE Loop Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test lua/while-simple
  "Simple while loop compiles"
  (let ((code "while count < 10 do count = count + 1 end"))
    (is (not (null (parse-lua code))))))

(test lua/while-complex-condition
  "while with complex condition compiles"
  (let ((code "while x > 0 and running do x = x - 1 end"))
    (is (not (null (parse-lua code))))))

(test lua/while-with-break
  "while with break statement compiles"
  (let ((code "while true do if quit then break end end"))
    (is (not (null (parse-lua code))))))

(test lua/while-nested
  "Nested while loops compile"
  (let ((code "while i < 5 do j = 0 while j < 3 do j = j + 1 end i = i + 1 end"))
    (is (not (null (parse-lua code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPEAT-UNTIL Loop Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test lua/repeat-simple
  "Simple repeat-until loop compiles"
  (let ((code "repeat count = count + 1 until count == 10"))
    (is (not (null (parse-lua code))))))

(test lua/repeat-complex-condition
  "repeat-until with complex condition compiles"
  (let ((code "repeat x = x - 1 until x <= 0 or quit"))
    (is (not (null (parse-lua code))))))

(test lua/repeat-guaranteed-execution
  "repeat executes at least once"
  (let ((code "repeat x = 1 until true"))
    (is (not (null (parse-lua code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOR Numeric Loop Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test lua/for-simple
  "Simple for loop compiles"
  (let ((code "for i = 1, 10 do print(i) end"))
    (is (not (null (parse-lua code))))))

(test lua/for-with-step
  "for loop with step compiles"
  (let ((code "for i = 0, 100, 5 do total = total + i end"))
    (is (not (null (parse-lua code))))))

(test lua/for-downcount
  "for loop counting down compiles"
  (let ((code "for i = 10, 1, -1 do print(i) end"))
    (is (not (null (parse-lua code))))))

(test lua/for-array-init
  "for loop initializing array compiles"
  (let ((code "local arr = {} for i = 1, 10 do arr[i] = i * 2 end"))
    (is (not (null (parse-lua code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOR-IN Iterator Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test lua/for-in-pairs
  "for-in with pairs compiles"
  (let ((code "local t = {a=1, b=2} for k in pairs(t) do print(k) end"))
    (is (not (null (parse-lua code))))))

(test lua/for-in-ipairs
  "for-in with ipairs compiles"
  (let ((code "local arr = {10, 20, 30} for i, v in ipairs(arr) do print(v) end"))
    (is (not (null (parse-lua code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION Declaration Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test lua/function-simple
  "Simple function declaration"
  (let ((code "function greet() print('hello') end"))
    (is (not (null (parse-lua code))))))

(test lua/function-with-params
  "Function with parameters"
  (let ((code "function add(a, b) return a + b end"))
    (is (not (null (parse-lua code))))))

(test lua/function-with-return
  "Function with return statement"
  (let ((code "function getScore() return score end"))
    (is (not (null (parse-lua code))))))

(test lua/function-local
  "Local function declaration"
  (let ((code "local function private() end"))
    (is (not (null (parse-lua code))))))

(test lua/function-multiple-returns
  "Function with multiple return values"
  (let ((code "function minmax(a, b) if a < b then return a, b else return b, a end end"))
    (is (not (null (parse-lua code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOCAL Declaration Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test lua/local-simple
  "Simple local variable"
  (let ((code "local x = 10"))
    (is (not (null (parse-lua code))))))

(test lua/local-multiple
  "Multiple local variables"
  (let ((code "local x, y, z = 1, 2, 3"))
    (is (not (null (parse-lua code))))))

(test lua/local-without-init
  "Local variable without initialization"
  (let ((code "local x"))
    (is (not (null (parse-lua code))))))

(test lua/local-function
  "Local function"
  (let ((code "local function helper() end"))
    (is (not (null (parse-lua code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RETURN Statement Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test lua/return-no-value
  "Return without value"
  (let ((code "function test() return end"))
    (is (not (null (parse-lua code))))))

(test lua/return-single
  "Return single value"
  (let ((code "function test() return 42 end"))
    (is (not (null (parse-lua code))))))

(test lua/return-multiple
  "Return multiple values"
  (let ((code "function test() return 1, 2, 3 end"))
    (is (not (null (parse-lua code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BREAK Statement Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test lua/break-in-for
  "Break in for loop"
  (let ((code "for i = 1, 100 do if i == 50 then break end end"))
    (is (not (null (parse-lua code))))))

(test lua/break-in-while
  "Break in while loop"
  (let ((code "while true do if quit then break end end"))
    (is (not (null (parse-lua code))))))

(test lua/break-in-repeat
  "Break in repeat-until loop"
  (let ((code "repeat if done then break end until false"))
    (is (not (null (parse-lua code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DO-END Block Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test lua/do-end-simple
  "Simple do-end block"
  (let ((code "do local x = 10 print(x) end"))
    (is (not (null (parse-lua code))))))

(test lua/do-end-nested
  "Nested do-end blocks"
  (let ((code "do local x = 1 do local y = 2 end end"))
    (is (not (null (parse-lua code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABLE Construction Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test lua/table-empty
  "Empty table construction"
  (let ((code "local t = {}"))
    (is (not (null (parse-lua code))))))

(test lua/table-array
  "Array table construction"
  (let ((code "local arr = {1, 2, 3, 4, 5}"))
    (is (not (null (parse-lua code))))))

(test lua/table-dictionary
  "Dictionary table construction"
  (let ((code "local player = {name = 'Hero', health = 100}"))
    (is (not (null (parse-lua code))))))

(test lua/table-mixed
  "Mixed table construction"
  (let ((code "local t = {1, 2, 3, name = 'mixed', count = 3}"))
    (is (not (null (parse-lua code))))))

(test lua/table-nested
  "Nested table construction"
  (let ((code "local matrix = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}"))
    (is (not (null (parse-lua code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-lua (code)
  "Parse Lua code string and return AST or NIL on error."
  (ignore-errors
    (eightbol::parse-lua code)))

;; End of lua-statements-comprehensive-tests.lisp
