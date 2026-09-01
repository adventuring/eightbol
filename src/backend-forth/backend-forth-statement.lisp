;; src/backend-forth/backend-forth-statement.lisp — Forth bytecode statement handlers
;;; Copyright © 2026 Interworldly Adventuring, LLC
;;
;; Statement dispatch and handlers for Forth bytecode emission.
;; Each handler receives AST node data and emits corresponding bytecode.
(in-package :eightbol)

;;; Generic statement compilation interface
(defmacro def-forth-statement (statement-type (ast-node-data) &body body)
  "Define compile-statement method for :forth CPU.

STATEMENT-TYPE is the keyword type (e.g., :move, :call).
AST-NODE-DATA is a plist with statement parameters.
BODY emits bytecode to *output-stream*."
  `(defmethod compile-statement ((cpu (eql :forth))
                                  (statement-type (eql ,statement-type))
                                  ,ast-node-data)
     ,@body))

;;; Control flow: return statements
(def-forth-statement :goback (_)
  "Emit return / procedure exit."
  (declare (ignore _))
  (forth-emit-eol))

(def-forth-statement :exit-method (_)
  "Emit method exit (same as goback for bytecode)."
  (declare (ignore _))
  (forth-emit-eol))

(def-forth-statement :exit-program (_)
  "Emit program exit."
  (declare (ignore _))
  (forth-emit-eol))

(def-forth-statement :stop-run (_)
  "Emit stop/halt."
  (declare (ignore _))
  (forth-emit-eol))

(def-forth-statement :exit (_)
  "Emit exit (generic)."
  (declare (ignore _))
  (forth-emit-eol))

;;; Data movement: MOVE
(def-forth-statement :move (ast-node-data)
  "Emit MOVE FROM from TO.

Emits: push source, pop destination as bytecode sequence."
  (let ((from (getf ast-node-data :from))
        (to (getf ast-node-data :to)))
    ;; Emit bytecode: load source value, store to destination
    (forth-emit-move-statement from to)))

;;; Data movement: SET
(def-forth-statement :set (ast-node-data)
  "Emit SET identifier TO expression."
  (let ((target (getf ast-node-data :target))
        (value (getf ast-node-data :value)))
    (forth-emit-set-statement target value)))

;;; Arithmetic: ADD
(def-forth-statement :add (ast-node-data)
  "Emit ADD expression TO identifier.

Stack before: target value
Stack after:  (target + value)"
  (let ((from (getf ast-node-data :from))
        (to (getf ast-node-data :to)))
    (forth-emit-add-statement from to)))

;;; Arithmetic: SUBTRACT
(def-forth-statement :subtract (ast-node-data)
  "Emit SUBTRACT expression FROM identifier."
  (let ((subtrahend (getf ast-node-data :from))
        (minuend (getf ast-node-data :from-target)))
    (forth-emit-subtract-statement subtrahend minuend)))

;;; Subroutine calls: CALL
(def-forth-statement :call (ast-node-data)
  "Emit CALL to subroutine.

Uses ForthExecute to invoke named subroutine."
  (let ((target (getf ast-node-data :target)))
    (forth-emit-call-statement target)))

;;; Method invocation: INVOKE
(def-forth-statement :invoke (ast-node-data)
  "Emit INVOKE object.method.

Dispatches method call using ForthExecute."
  (let ((object (getf ast-node-data :object))
        (method (getf ast-node-data :method)))
    (forth-emit-invoke-statement object method)))

;;; Conditionals: IF/THEN/ELSE
(def-forth-statement :if (ast-node-data)
  "Emit IF condition THEN statements [ELSE statements] END.

Uses ForthWhen/ForthUnless for conditional branching."
  (let ((condition (getf ast-node-data :condition))
        (then-stmts (getf ast-node-data :then))
        (else-stmts (getf ast-node-data :else)))
    (forth-emit-if-statement condition then-stmts else-stmts)))

;;; Looping: PERFORM
(def-forth-statement :perform (ast-node-data)
  "Emit PERFORM procedure [TIMES n | UNTIL condition].

Simple stub: emits procedure call. Complex variations
(TIMES, UNTIL, VARYING) require additional stack management."
  (let ((procedure (getf ast-node-data :procedure))
        (times (getf ast-node-data :times))
        (until (getf ast-node-data :until)))
    (forth-emit-perform-statement procedure times until)))

;;; Computed GOTO: EVALUATE
(def-forth-statement :evaluate (ast-node-data)
  "Emit EVALUATE expression WHEN clauses."
  (let ((expression (getf ast-node-data :expression))
        (when-clauses (getf ast-node-data :when-clauses)))
    (forth-emit-evaluate-statement expression when-clauses)))

;;; String operations: STRING (BLT)
(def-forth-statement :string-blt (ast-node-data)
  "Emit STRING source DELIMITED BY size MOVE TO destination."
  (let ((source (getf ast-node-data :source))
        (dest (getf ast-node-data :destination))
        (length (getf ast-node-data :length)))
    (forth-emit-string-blt-statement source dest length)))

;;; Arithmetic: COMPUTE
(def-forth-statement :compute (ast-node-data)
  "Emit COMPUTE target = expression."
  (let ((target (getf ast-node-data :target))
        (expression (getf ast-node-data :expression)))
    (forth-emit-compute-statement target expression)))

;;; Logging: LOG FAULT
(def-forth-statement :log-fault (ast-node-data)
  "Emit LOG FAULT code.

Placeholder: typically not used in bytecode,
but emitted as comment or debug marker."
  (let ((code (getf ast-node-data :code)))
    (format *output-stream* "; FAULT: ~a~%" code)))

;;; Debugging: INSPECT
(def-forth-statement :inspect (ast-node-data)
  "Emit INSPECT statement (placeholder)."
  (declare (ignore ast-node-data))
  (format *output-stream* "; INSPECT~%"))

;;; Break statement for PERFORM loops
(def-forth-statement :break (_)
  "Emit BREAK (exit PERFORM loop)."
  (declare (ignore _))
  (format *output-stream* "; BREAK~%"))

;;; Fallback for unimplemented statements
(defmethod compile-statement ((cpu (eql :forth))
                               statement-type
                               ast-node-data)
  (declare (ignore ast-node-data))
  (format *output-stream* "; UNIMPLEMENTED: ~a~%" statement-type))
