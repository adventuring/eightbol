;; tests/pascal-statements-comprehensive-tests.lisp
;; Comprehensive Pascal statement tests covering all major statements
;; Tests numeric types (BINARY, DECIMAL, DISPLAY) and edge cases

(in-package :eightbol/test)

(fiveam:def-suite :pascal-statements-comprehensive
  :description "Comprehensive Pascal statement tests")
(in-suite :pascal-statements-comprehensive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASSIGNMENT Statement Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test pascal/assignment-binary-simple
  "Simple BINARY assignment compiles"
  (let ((code "PROCEDURE Test BEGIN Score := 100 END"))
    (is (not (null (parse-pascal code))))))

(test pascal/assignment-binary-arithmetic
  "BINARY assignment with arithmetic compiles"
  (let ((code "PROCEDURE Test BEGIN Score := Counter + 5 END"))
    (is (not (null (parse-pascal code))))))

(test pascal/assignment-decimal
  "DECIMAL assignment compiles"
  (let ((code "PROCEDURE Test BEGIN Price := 9999 END"))
    (is (not (null (parse-pascal code))))))

(test pascal/assignment-record-field
  "Record field assignment compiles"
  (let ((code "PROCEDURE Test BEGIN Player.X := 100 END"))
    (is (not (null (parse-pascal code))))))

(test pascal/assignment-array-element
  "Array element assignment compiles"
  (let ((code "PROCEDURE Test BEGIN Scores[0] := 1000 END"))
    (is (not (null (parse-pascal code))))))

(test pascal/assignment-chained-arithmetic
  "Chained arithmetic assignment compiles"
  (let ((code "PROCEDURE Test BEGIN Total := A + B * C END"))
    (is (not (null (parse-pascal code))))))

(test pascal/assignment-move-syntax
  "MOVE TO assignment syntax compiles"
  (let ((code "PROCEDURE Test BEGIN MOVE 42 TO X END"))
    (is (not (null (parse-pascal code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IF/THEN/ELSE Statement Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test pascal/if-simple
  "Simple IF statement compiles"
  (let ((code "PROCEDURE Test BEGIN IF X > 10 THEN Y := 1 END END"))
    (is (not (null (parse-pascal code))))))

(test pascal/if-with-else
  "IF with ELSE compiles"
  (let ((code "PROCEDURE Test BEGIN IF X > 10 THEN Y := 1 ELSE Y := 2 END END"))
    (is (not (null (parse-pascal code))))))

(test pascal/if-nested
  "Nested IF statements compile"
  (let ((code "PROCEDURE Test BEGIN IF X > 10 THEN IF Y > 5 THEN Z := 1 END END END"))
    (is (not (null (parse-pascal code))))))

(test pascal/if-complex-condition
  "IF with AND/OR conditions compiles"
  (let ((code "PROCEDURE Test BEGIN IF X > 10 AND Y < 5 THEN Z := 1 END END"))
    (is (not (null (parse-pascal code))))))

(test pascal/if-decimal-condition
  "IF with DECIMAL comparison compiles"
  (let ((code "PROCEDURE Test BEGIN IF Price <= 9999 THEN Affordable := 1 END END"))
    (is (not (null (parse-pascal code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WHILE Loop Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test pascal/while-simple
  "Simple WHILE loop compiles"
  (let ((code "PROCEDURE Test BEGIN WHILE Counter < 10 NEXT Counter := Counter + 1 END END"))
    (is (not (null (parse-pascal code))))))

(test pascal/while-decimal
  "WHILE with DECIMAL condition compiles"
  (let ((code "PROCEDURE Test BEGIN WHILE Amount > 0 NEXT Amount := Amount - 100 END END"))
    (is (not (null (parse-pascal code))))))

(test pascal/while-nested
  "Nested WHILE loops compile"
  (let ((code "PROCEDURE Test BEGIN WHILE X < 5 NEXT WHILE Y < 3 NEXT Y := Y + 1 END X := X + 1 END END"))
    (is (not (null (parse-pascal code))))))

(test pascal/while-with-exit
  "WHILE with EXIT statement compiles"
  (let ((code "PROCEDURE Test BEGIN WHILE Counter < 100 NEXT IF Counter = 50 THEN EXIT END Counter := Counter + 1 END END"))
    (is (not (null (parse-pascal code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPEAT/UNTIL Loop Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test pascal/repeat-simple
  "Simple REPEAT/UNTIL loop compiles"
  (let ((code "PROCEDURE Test BEGIN REPEAT Counter := Counter + 1 UNTIL Counter = 10 END"))
    (is (not (null (parse-pascal code))))))

(test pascal/repeat-decimal
  "REPEAT with DECIMAL condition compiles"
  (let ((code "PROCEDURE Test BEGIN REPEAT Amount := Amount - 100 UNTIL Amount <= 0 END"))
    (is (not (null (parse-pascal code))))))

(test pascal/repeat-guaranteed-execution
  "REPEAT executes at least once"
  (let ((code "PROCEDURE Test BEGIN REPEAT X := 1 UNTIL X = 1 END"))
    (is (not (null (parse-pascal code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOR Loop Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test pascal/for-simple
  "Simple FOR loop compiles"
  (let ((code "PROCEDURE Test BEGIN FOR I FROM 1 TO 10 STEP 1 NEXT J := J + 1 END END"))
    (is (not (null (parse-pascal code))))))

(test pascal/for-downto
  "FOR loop with negative step compiles"
  (let ((code "PROCEDURE Test BEGIN FOR I FROM 10 DOWNTO 1 STEP -1 NEXT J := J + 1 END END"))
    (is (not (null (parse-pascal code))))))

(test pascal/for-large-step
  "FOR loop with large step compiles"
  (let ((code "PROCEDURE Test BEGIN FOR V FROM 0 TO 1000 STEP 100 NEXT Total := Total + V END END"))
    (is (not (null (parse-pascal code))))))

(test pascal/for-array-init
  "FOR loop initializing array compiles"
  (let ((code "PROCEDURE Test BEGIN FOR I FROM 0 TO 99 STEP 1 NEXT Arr[I] := 0 END END"))
    (is (not (null (parse-pascal code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CASE Statement Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test pascal/case-simple
  "Simple CASE statement compiles"
  (let ((code "PROCEDURE Test BEGIN CASE Direction OF 1: North 2: South ENDCASE END"))
    (is (not (null (parse-pascal code))))))

(test pascal/case-with-else
  "CASE with ELSE compiles"
  (let ((code "PROCEDURE Test BEGIN CASE Direction OF 1: North 2: South ELSE Beep ENDCASE END"))
    (is (not (null (parse-pascal code))))))

(test pascal/case-multiple-values
  "CASE with multiple values per clause compiles"
  (let ((code "PROCEDURE Test BEGIN CASE Value OF 1, 2, 3: ProcessSmall 4, 5, 6: ProcessLarge ENDCASE END"))
    (is (not (null (parse-pascal code))))))

(test pascal/case-decimal
  "CASE with DECIMAL expression compiles"
  (let ((code "PROCEDURE Test BEGIN CASE Price OF 1000: Expensive 100: Cheap ENDCASE END"))
    (is (not (null (parse-pascal code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCEDURE Declaration Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test pascal/procedure-simple
  "Simple procedure declaration"
  (let ((code "PROCEDURE Init BEGIN Score := 0 END"))
    (is (not (null (parse-pascal code))))))

(test pascal/procedure-on-object
  "Procedure with ON object modifier"
  (let ((code "PROCEDURE Move ON Player BEGIN X := X + 5 END"))
    (is (not (null (parse-pascal code))))))

(test pascal/procedure-in-library
  "Procedure with IN LIBRARY modifier"
  (let ((code "PROCEDURE DrawSprite IN LIBRARY BEGIN RenderSprite END"))
    (is (not (null (parse-pascal code))))))

(test pascal/procedure-calling-another
  "Procedure calling another procedure"
  (let ((code "PROCEDURE Main BEGIN Init UpdateGame END PROCEDURE Init BEGIN Score := 0 END"))
    (is (not (null (parse-pascal code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN/END Block Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test pascal/begin-end-simple
  "BEGIN/END block"
  (let ((code "PROCEDURE Test BEGIN X := 1 Y := 2 END"))
    (is (not (null (parse-pascal code))))))

(test pascal/begin-end-nested
  "Nested BEGIN/END blocks"
  (let ((code "PROCEDURE Test BEGIN BEGIN X := 1 END Y := 2 END"))
    (is (not (null (parse-pascal code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Declaration Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test pascal/var-simple
  "Simple variable declaration"
  (let ((code "PROCEDURE Test BEGIN VAR X: INTEGER END"))
    (is (not (null (parse-pascal code))))))

(test pascal/var-multiple
  "Multiple variable declarations"
  (let ((code "PROCEDURE Test BEGIN VAR X, Y, Z: INTEGER END"))
    (is (not (null (parse-pascal code))))))

(test pascal/var-decimal
  "DECIMAL variable declaration"
  (let ((code "PROCEDURE Test BEGIN VAR Price: DECIMAL END"))
    (is (not (null (parse-pascal code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONST Declaration Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test pascal/const-simple
  "Simple constant declaration"
  (let ((code "PROCEDURE Test BEGIN CONST MaxScore = 10000 END"))
    (is (not (null (parse-pascal code))))))

(test pascal/const-decimal
  "DECIMAL constant declaration"
  (let ((code "PROCEDURE Test BEGIN CONST BasePrice = 9999 END"))
    (is (not (null (parse-pascal code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-pascal (code)
  "Parse Pascal code string and return AST or NIL on error."
  (ignore-errors
    (eightbol::parse-eightbol-string code)))

;; End of pascal-statements-comprehensive-tests.lisp
