;;; backend-z80-variables.lisp — Special variables for Z80 backend
(in-package :eightbol)

(defvar *z80-break-label* nil
  "Label to break to from current PERFORM loop, or NIL if not in a loop.")
(defvar *z80-continue-label* nil
  "Label to continue to from current PERFORM loop, or NIL if not in a loop.")
(defvar *z80-accumulator-expression* :trash/init
  "AST for the value currently in A during Z80 emission, or NIL if unknown.")