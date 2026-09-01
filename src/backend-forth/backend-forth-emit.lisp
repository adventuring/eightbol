;; src/backend-forth/backend-forth-emit.lisp — Forth bytecode emission helpers
;;; Copyright © 2026 Interworldly Adventuring, LLC
;;
;; Statement-specific bytecode emission functions.
;; Each function takes statement parameters and emits appropriate bytecodes.
;; These are placeholder implementations that emit bytecode with debug comments.
(in-package :eightbol)

;;; MOVE: copy value from source to destination
(defun forth-emit-move-statement (from to)
  "Emit bytecode for MOVE FROM from TO.

For now, just emit comments. Real implementation would:
  1. If FROM is a constant, emit PUSH_BYTE or PUSH_WORD with value
  2. If FROM is a variable, emit GET_BYTE or GET_WORD to load
  3. Emit SET_BYTE or SET_WORD to store to TO"
  (format *output-stream* "; MOVE ~a -> ~a~%" from to))

;;; SET: assign expression to variable
(defun forth-emit-set-statement (target value)
  "Emit bytecode for SET target TO value."
  (format *output-stream* "; SET ~a = ~a~%" target value))

;;; ADD: add value to accumulator
(defun forth-emit-add-statement (from to)
  "Emit bytecode for ADD from TO to."
  (format *output-stream* "; ADD ~a TO ~a~%" from to))

;;; SUBTRACT: subtract value from accumulator
(defun forth-emit-subtract-statement (subtrahend minuend)
  "Emit bytecode for SUBTRACT subtrahend FROM minuend."
  (format *output-stream* "; SUBTRACT ~a FROM ~a~%" subtrahend minuend))

;;; CALL: invoke subroutine
(defun forth-emit-call-statement (target)
  "Emit bytecode for CALL to TARGET subroutine.

Uses ForthExecute to jump to subroutine by name."
  (forth-emit-opcode +forth-execute+)
  (format *output-stream* "; CALL ~a~%" target))

;;; INVOKE: method call on object
(defun forth-emit-invoke-statement (object method)
  "Emit bytecode for INVOKE object.method.

Constructs method name from object and method, then executes."
  (let ((method-name (format nil "Method~a~a"
                              (forth-symbol (format nil "~a" object))
                              (forth-symbol (format nil "~a" method)))))
    (forth-emit-opcode +forth-execute+)
    (format *output-stream* "; INVOKE ~a~%" method-name)))

;;; IF/THEN/ELSE: conditional branching
(defun forth-emit-if-statement (condition then-stmts else-stmts)
  "Emit bytecode for IF condition THEN ... [ELSE ...] END.

Uses ForthWhen to branch on nonzero condition."
  (let ((else-label (forth-make-label "Else"))
        (end-label (forth-make-label "End")))
    ;; Condition evaluation (placeholder)
    (format *output-stream* "; IF ~a~%" condition)
    (forth-emit-opcode +forth-when+)
    (format *output-stream* "; -> ~a~%" end-label)
    
    ;; Emit THEN statements
    (dolist (stmt then-stmts)
      (compile-statement :forth (first stmt) (rest stmt)))
    
    ;; If ELSE exists, emit it
    (when else-stmts
      (forth-emit-opcode +forth-go+)
      (format *output-stream* "; -> ~a~%" end-label)
      (format *output-stream* "~a:~%" else-label)
      (dolist (stmt else-stmts)
        (compile-statement :forth (first stmt) (rest stmt))))
    
    (format *output-stream* "~a:~%" end-label)))

;;; PERFORM: loop or one-time procedure call
(defun forth-emit-perform-statement (procedure times until)
  "Emit bytecode for PERFORM procedure [TIMES n | UNTIL condition].

Simple version: just call procedure."
  (declare (ignore times until))
  (forth-emit-opcode +forth-execute+)
  (format *output-stream* "; PERFORM ~a~%" procedure))

;;; EVALUATE: computed GOTO using WHEN clauses
(defun forth-emit-evaluate-statement (expression when-clauses)
  "Emit bytecode for EVALUATE expression WHEN clauses."
  (declare (ignore when-clauses))
  (format *output-stream* "; EVALUATE ~a~%" expression))

;;; STRING: string move with length
(defun forth-emit-string-blt-statement (source dest length)
  "Emit bytecode for STRING source DELIMITED BY length MOVE TO dest."
  (declare (ignore source dest length))
  (format *output-stream* "; STRING BLT~%"))

;;; COMPUTE: compute expression and store
(defun forth-emit-compute-statement (target expression)
  "Emit bytecode for COMPUTE target = expression."
  (format *output-stream* "; COMPUTE ~a = ~a~%" target expression))

;;; Expression emission (for values, operands) - placeholder
(defun forth-emit-expression (expr)
  "Emit bytecode to evaluate EXPR and push result onto stack (placeholder)."
  (format *output-stream* "; PUSH ~a~%" expr))
