;;; lua-parser.lisp
;;; YACC parser for Lua subset
;;; Maps Lua tokens to EightBol AST nodes

(in-package :eightbol)

;;; Existing AST node types we'll use (no new nodes added):
;;; :dd, :set, :call, :if, :perform, :exit-method, :deref, :method
;;; 

;;; Grammar rules for Lua subset

(lua-parser
 :start-symbol lua-program
 :terminals (keyword symbol number string comment)
 :precedence ((:left + - * /) (:left == /= < > <= >= !=))
 
 ;; Top-level: Lua program
 (lua-program
  (comments*
   lua-chunk
   #'parse/lua-program))
 
 ;; Lua chunk (statements + data defs)
 (lua-chunk
  (data-definition*                   ; Map to :dd nodes
   lua-statement*                     ; Map to :set, :call, etc.
   #'parse/lua-chunk))
 
 ;; Data definition (variable declarations)
 (data-definition
  (lua-var-decl                       ; Simple var: `local x = 5`
   #'parse/data-def)
  (lua-array-decl            ; Array: `local t = {1,2,3}` (simplified)
   #'parse/data-def))
 
 ;; Statements
 (lua-statement
  (if-statement                      ; `if cond then ... else ... end`
   #'parse/if)
  (while-statement                    ; `while cond do ... end`
   #'parse/while)
  (for-statement                      ; `for i=1,5 do ... end`
   #'parse/for)
  (call-statement                     ; `foo(a, b)`
   #'parse/call)
  (method-statement                   ; `obj:method()`
   #'parse/method)
  (set-statement                      ; `x = 5`
   #'parse/set)
  (return-statement                   ; `return expr`
   #'parse/return)))

;;; Parser actions

;;; Data declaration: `local var = value` or `local arr = {1,2,3}`
(parse/data-def (lua-var-decl var value)
                => (list :dd :level 01 :label var :attr ((:usage :binary) (:value value))))

(parse/data-def
 (lua-array-decl arr elements)
 => (:dd :level 01 :label arr :attr ((:occurs (length elements)) :usage :binary)))  ; Assume fixed-size array

;;; If statement
(parse/if
  (if cond then-branch else-branch)
  => (:if :condition cond :then (then-branch) :else (else-branch)))

;;; While loop
(parse/while
  (while cond body)
  => (:perform :until cond :with body))

;;; For loop (simplified)
(parse/for
  (for var from start to end body)
  => (:perform :varying :from start :to end :with body))

;;; Function call
(parse/call
  (func-call func-name args)
  => (:call :target func-name :args args))

;;; Method call
(parse/method
  (obj-method obj method-name args)
  => (:call :target method-name :object obj :args args))

;;; Assignment
(parse/set
  (assign lhs rhs)
  => (:set :target lhs :value rhs))

;;; Return statement
(parse/return
  (return expr)
  => (:exit-method :value expr))

;;; Helper functions

(defun lua-var-decl (symbol value)
  "Parse `local VAR = VALUE`"
  (declare (ignore symbol value))
  symbol)

(defun lua-array-decl (symbol elements)
  "Parse `local ARR = {ELEMENTS}`"
  (declare (ignore symbol elements))
  symbol)

(defun if-statement (cond then-branch else-branch)
  "Parse `if COND then ... else ... end`"
  (declare (ignore cond then-branch else-branch))
  (list cond then-branch else-branch))

(defun while-statement (cond body)
  "Parse `while COND do ... end`"
  (declare (ignore cond body))
  (list cond body))

(defun for-statement (var start end body)
  "Parse `for VAR = START to END do ... end`"
  (declare (ignore var start end body))
  (list var start end body))

(defun call-statement (func-name args)
  "Parse `FUNC(ARG1, ARG2, ...)`"
  (declare (ignore func-name args))
  (list func-name args))

(defun method-statement (obj method-name args)
  "Parse `OBJ:METHOD(ARG1, ...)`"
  (declare (ignore obj method-name args))
  (list obj method-name args))

(defun set-statement (lhs rhs)
  "Parse `LHS = RHS`"
  (declare (ignore lhs rhs))
  (list lhs rhs))

(defun return-statement (expr)
  "Parse `return EXPR`"
  (declare (ignore expr))
  expr)

;;; Export if needed
;;; (export 'parse/lua-parser)
