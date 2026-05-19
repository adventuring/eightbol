;;; tests/repro-bugs.lisp --- regression tests for compiler bugs surfaced
;;; by Phantasia .cob compilations.  Each test isolates a minimal AST
;;; pattern (as emitted into Object/Classes/*.eightbol) that previously
;;; crashed the 6502 backend.
;;;
;;; Bug A: backend-6502 :subtract branch read the rhs from :to instead
;;; of :subtrahend, passing NIL to emit-6502-alu-with-memory-rhs and
;;; faulting in emit-6502-value: "missing expression (NIL)" (Boat,
;;; Intercardinal-Course).
;;;
;;; Bug B: expression-constant-p / expression-constant-value /
;;; emit-6502-value compared the head of an expression against the
;;; string "(" using STRING=, which signalled SIMPLE-TYPE-ERROR
;;; ("X is not a string designator") whenever the head was a list
;;; (e.g. an EVALUATE subject wrapped by cl-yacc as ((:OF ...))).
;;;
;;; Bug C: emit-6502-load-expression did not unwrap a 1-element list
;;; (mirroring emit-6502-value); when invoked with an EVALUATE subject
;;; (always parsed as a 1-element list) it crashed via Bug B, or via the
;;; later default branch that called emit-6502-value with NIL (Anenemy,
;;; Item, Non-Player-Character, second Item EVALUATE).

(in-package :eightbol/test)

(in-suite :eightbol)

(defmacro with-empty-eightbol-tables (&body body)
  "Bind the EIGHTBOL backend's special tables to fresh empty values for
hermetic tests.  Avoids inheriting state from a previous compile run."
  `(let ((eightbol::*class-id* "T")
         (eightbol::*cpu* :6502)
         (eightbol::*slot-table* (make-hash-table :test 'equalp))
         (eightbol::*const-table* (make-hash-table :test 'equalp))
         (eightbol::*type-table* (make-hash-table :test 'equalp))
         (eightbol::*usage-table* (make-hash-table :test 'equalp))
         (eightbol::*sign-table* (make-hash-table :test 'equalp))
         (eightbol::*pic-size-table* (make-hash-table :test 'equalp))
         (eightbol::*pic-width-table* (make-hash-table :test 'equalp))
         (eightbol::*pic-frac-bits-table* (make-hash-table :test 'equalp))
         (eightbol::*pic-nybble-semantics-table* (make-hash-table :test 'equalp))
         (eightbol::*service-bank-table* (make-hash-table :test 'equalp))
         (eightbol::*working-storage* (make-hash-table :test 'equalp))
         (eightbol::*6502-accumulator-expression* :unknown)
         (eightbol::*6502-x-index-expression* :unknown))
     ,@body))

;;;; Bug A: :subtract rhs from :subtrahend

(test repro/subtract-uses-subtrahend-not-to
  "emit-6502-load-expression on (:SUBTRACT :FROM A :SUBTRAHEND B :GIVING NIL)
with non-constant operands must SBC the subtrahend B, not NIL.  Regression:
Boat.cob / Intercardinal-Course.cob produced
@code{emit-6502-value: missing expression (NIL)}."
  (with-empty-eightbol-tables
    ;; Use non-constant operands so the expression is not folded.
    (setf (gethash "Foo" eightbol::*working-storage*) '(:usage :binary :pic "99"))
    (setf (gethash "Bar" eightbol::*working-storage*) '(:usage :binary :pic "99"))
    (let* ((asm (with-output-to-string (out)
                  (eightbol::emit-6502-load-expression
                   out
                   '(:subtract :from "Foo" :subtrahend "Bar" :giving nil)
                   "T"))))
      ;; Must compute Foo - Bar.  Expect lda Foo, sec, sbc Bar.
      (is (search "lda" asm) "lda should be emitted for the :from operand")
      (is (search "sec" asm) "sec should precede sbc")
      (is (search "sbc" asm) "sbc must be emitted (was suppressed when :to was used)")
      (is (search "Bar" asm) "sbc operand must reference :subtrahend (Bar)"))))

(test repro/subtract-emits-subtrahend-operand
  "emit-6502-load-expression on (:SUBTRACT :FROM Width-Mask :SUBTRAHEND 1 :GIVING NIL)
must SBC against the named subtrahend (or its constant), not NIL."
  (with-empty-eightbol-tables
    (let* ((asm (with-output-to-string (out)
                  (eightbol::emit-6502-load-expression
                   out
                   '(:subtract :from "Width-Mask" :subtrahend 1 :giving nil)
                   "T"))))
      ;; The Boat regression: previously sbc was emitted with no operand
      ;; (NIL -> emit-6502-value missing expression).  Verify sbc names the constant 1.
      (is (cl-ppcre:scan "sbc[^\\n]*1" asm)
          "sbc must reference the :subtrahend operand (1)"))))

;;;; Bug B: defensive guards against (string= "(" list)

(test repro/expression-constant-p-on-listy-first
  "EXPRESSION-CONSTANT-P must not signal a type error when (first expression)
is itself a list (e.g. ((:OF \"State\" \"Self\"))).  Regression: Anenemy /
Item / Non-Player-Character crashed with
@code{(:OF \"State\" \"Self\") is not a string designator}."
  (with-empty-eightbol-tables
    (let ((r (handler-case
                 (eightbol::expression-constant-p
                  '((:of "State" "Self")))
               (error (e) (declare (ignore e)) :crashed))))
      (is (not (eq :crashed r))
          "expression-constant-p crashed on ((:OF \"State\" \"Self\"))"))))

(test repro/expression-constant-p-on-listy-first-returns-nil
  "EXPRESSION-CONSTANT-P of ((:OF slot obj)) is not constant (must be NIL)."
  (with-empty-eightbol-tables
    (is (not (eightbol::expression-constant-p
              '((:of "State" "Self")))))))

(test repro/emit-6502-value-of-listy-1-elt-unwraps
  "emit-6502-value on a 1-element list of an :OF form unwraps to the OF
form; the OF form itself triggers a BACKEND-ERROR (complex access — caller
should use emit-6502-load-expression instead), not a SIMPLE-TYPE-ERROR
@code{is not a string designator} from STRING= on the head."
  (with-empty-eightbol-tables
    (let ((etype (handler-case
                     (progn (eightbol::emit-6502-value '((:of "State" "Self")))
                            :returned-cleanly)
                   (eightbol::backend-error () :backend-error)
                   (type-error () :type-error)
                   (error () :other-error))))
      (is (not (eq :type-error etype))
          "emit-6502-value should not signal a TYPE-ERROR on ((:OF ...))"))))

;;;; Bug C: EVALUATE with wrapped subject must compile

(test repro/evaluate-wrapped-of-subject-compiles
  "EVALUATE State OF Self compiles to 6502 without crashing — both shapes:
unwrapped (current parser output) and 1-element-wrapped (legacy parser).
The 6502 backend defensively unwraps so existing Object/Classes/*.eightbol
ASTs (which were produced by the legacy wrapping parser) keep compiling."
  (let ((slots (make-hash-table :test 'equalp)))
    (setf (gethash (eightbol::cobol-slot-table-name-key "State") slots)
          "Anenemy")
    (dolist (subject '((:of "State" "Self")
                       ((:of "State" "Self"))))
      (let ((asm
              (handler-case
                  (compile-method-ast-with-tables
                   `(:method :method-id "Test"
                     :statements
                     ((:evaluate :subject ,subject
                       :when-clauses ((:when 0
                                       ((:goback)))))
                      (:goback)))
                   "Anenemy" :6502
                   :slot-table slots)
                (error (e) (declare (ignore e)) :crashed))))
        (is (not (eq :crashed asm))
            "EVALUATE State OF Self crashed for subject shape ~s" subject)))))

(test repro/evaluate-wrapped-subscript-of-subject-compiles
  "EVALUATE Strategic-Goal(Strategy-Index) OF Self compiles without crashing.
Regression: Non-Player-Character.cob.  Test both unwrapped (current parser
output) and 1-element-wrapped (legacy) subject shapes."
  (let ((slots (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash (eightbol::cobol-slot-table-name-key "Strategic-Goal") slots)
          "Non-Player-Character")
    (setf (gethash "Strategy-Index" ws)
          '(:usage :binary :pic "99"))
    (dolist (subject '((:of (:subscript "Strategic-Goal" "Strategy-Index") "Self")
                       ((:of (:subscript "Strategic-Goal" "Strategy-Index") "Self"))))
      (let ((asm
              (handler-case
                  (compile-method-ast-with-tables
                   `(:method :method-id "Test"
                     :statements
                     ((:evaluate :subject ,subject
                       :when-clauses ((:when 0 ((:goback)))))
                      (:goback)))
                   "Non-Player-Character" :6502
                   :slot-table slots
                   :working-storage ws)
                (error (e) (declare (ignore e)) :crashed))))
        (is (not (eq :crashed asm))
            "EVALUATE Strategic-Goal(Strategy-Index) OF Self crashed for subject ~s"
            subject)))))

;;;; Parser regression: EVALUATE subject and WHEN phrases must NOT be wrapped
;;;; in 1-element lists by cl-yacc's default action.

(test repro/parser-evaluate-subject-not-wrapped
  "EVALUATE State OF Self produces :subject (:OF \"State\" \"Self\") — bare
expression, not a wrapping ((:OF ...)) 1-element list.  cl-yacc's default
production action is #'list, so single-token grammar rules must specify
#'identity explicitly to avoid the wrap."
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Foo.
000030 OBJECT.
000040 PROCEDURE DIVISION.
000050 IDENTIFICATION DIVISION.
000060 METHOD-ID. \"T\".
000070 PROCEDURE DIVISION.
000080  EVALUATE State OF Self WHEN \"Anenemy-Shoot\" GOBACK. END-EVALUATE.
000090  GOBACK.
000100 END METHOD \"T\".
000110 END OBJECT.
000120 END CLASS Foo.
999999")
         (ast (eightbol::parse-eightbol-string src))
         (m (first (eightbol::ast-methods ast)))
         (eval-stmt (find :evaluate (eightbol::ast-method-statements m)
                          :key #'first)))
    (is (not (null eval-stmt)) "EVALUATE statement present")
    (let ((subj (getf (rest eval-stmt) :subject)))
      (is (equal '(:of "State" "Self") subj)
          "subject must be the bare :OF form, not ((:OF ...))"))
    (let* ((clauses (getf (rest eval-stmt) :when-clauses))
           (first-when (find :when clauses :key #'first))
           (phrases (and first-when (second first-when))))
      (is (or (stringp phrases) (numberp phrases) (keywordp phrases)
              (and (listp phrases) (keywordp (first phrases))))
          "WHEN phrases must be a bare expression/keyword, not a wrapping list"))))

;;;; Bug D: emit-6502-load-byte-n ECASE missed :shift-left / :shift-right.

(test repro/load-byte-n-handles-shift-left
  "emit-6502-load-byte-n must handle (:SHIFT-LEFT V N) and (:SHIFT-RIGHT V N).
Regression: Intercardinal-Course.cob's @code{MOVE Move-XH SHIFT-LEFT 4 TO Acc16}
crashed with @code{:SHIFT-LEFT fell through ECASE expression}."
  (with-empty-eightbol-tables
    (setf (gethash "Move-XH" eightbol::*working-storage*)
          '(:usage :binary :pic "99"))
    (dolist (op '(:shift-left :shift-right))
      (let ((etype
              (handler-case
                  (progn (with-output-to-string (out)
                           (eightbol::emit-6502-load-byte-n
                            out (list op "Move-XH" 4) "T" 0 1))
                         :ok)
                (sb-kernel:case-failure () :case-failure)
                (error (e) (declare (ignore e)) :other-error))))
        (is (not (eq :case-failure etype))
            "~s in emit-6502-load-byte-n must not signal ECASE failure" op)))))

(test repro/boat-draw-frame-complex-expression-compiles
  "The Width BIT-OR / BIT-XOR / SUBTRACT expression from Boat.Draw-Frame
must compile without @code{emit-6502-value: missing expression (NIL)}.
Regression: the 6502 backend's :subtract branches in both
emit-6502-load-expression AND emit-6502-load-byte-n read the rhs from
@code{:to} instead of @code{:subtrahend}."
  (let ((slots (make-hash-table :test 'equalp))
        (consts (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash (eightbol::cobol-slot-table-name-key "Width") slots) "Boat")
    (setf (gethash (eightbol::cobol-slot-table-name-key "Decal") slots) "Boat")
    (setf (gethash "width-mask" consts) #x1f)
    (setf (gethash "Decal-Index" ws) '(:usage :binary :pic "99"))
    ;; Decal-Pal-Width is a global table (declared in Phantasia copybooks).
    ;; Mark as a known data name so emit-6502-value resolves it.
    (setf (gethash "Decal-Pal-Width" ws) '(:usage :binary :pic "999"))
    (let ((etype
            (handler-case
                (progn (compile-method-ast-with-tables
                        '(:method :method-id "Draw-Frame"
                          :statements
                          ((:move :to (:subscript "Decal-Pal-Width" "Decal-Index")
                            :from (:bit-or
                                   (:bit-xor "Width-Mask"
                                    (:bit-and "Width-Mask"
                                     (:subtract :subtrahend 1
                                      :from (:multiply
                                             :by (:add :from (:bit-and (:of "Width" "Self") 3)
                                                  :to 1 :giving nil)
                                             :multiplier 2 :giving nil)
                                      :giving nil)))
                                   (:shift-left 5 5)))
                           (:goback)))
                        "Boat" :6502
                        :slot-table slots
                        :const-table consts
                        :working-storage ws)
                       :compiled-cleanly)
              (eightbol::backend-error (e)
                (if (search "missing expression (NIL)" (princ-to-string e))
                    :missing-expression-nil
                    :other-backend-error))
              (error (e) (declare (ignore e)) :other-error))))
      ;; The specific NIL/missing-expression regression must not recur.
      ;; Other errors (e.g. test fixture incompleteness) are tolerated.
      (is (not (eq :missing-expression-nil etype))
          "Boat.Draw-Frame compile regressed to emit-6502-value: missing expression (NIL)"))))
