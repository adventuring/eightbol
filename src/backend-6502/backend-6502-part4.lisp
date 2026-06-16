(in-package :eightbol)
(defun compile-6502-goto (statement)
  (let ((target (safe-getf (rest statement) :target))
        (targets (safe-getf (rest statement) :targets))
        (dep (safe-getf (rest statement) :depending-on)))
    (if dep
        ;; GO TO ... DEPENDING ON expression — trinary search tree (cmp/beq: equal, <, >)
        (let ((target-list (or targets (when target (list target)))))
	  (emit-6502-load-expression *output-stream* dep *class-id*)
	  (emit-6502-goto-tristree *output-stream* target-list 1 (length target-list)
			       *class-id* *method-id*))
        ;; Simple GOTO
        (format *output-stream* "~%~10T~a ~a~%"
                (6502-branch-always-mnemonic)
                (para-label (format nil "~a" (or target (first targets))))))))

(defun emit-6502-goto-tristree (out targets low high class-id method-id)
  "Emit trinary search: cmp #mid; beq target_mid; blt left; bge right.
TARGETS is list of paragraph names (1-based indices). LOW, HIGH are 1-based inclusive."
  (when (or (null targets) (> low high)) (return-from emit-6502-goto-tristree))
  (let* ((mid (floor (+ low high) 2))
         (target-name (nth (1- mid) targets)))
    (when (null target-name) (return-from emit-6502-goto-tristree))
    (let ((label (para-label (format nil "~a" target-name))))
      (if (= low high)
	;; Leaf: single target
	(progn
	  (unless (6502-zero-p low)
	    (format out "~%~10Tcmp # ~a" (expression-constant-value low)))
	  (if (and (numberp *6502-accumulator-expression*)
		 (zerop *6502-accumulator-expression*))
                (format out "~%~10Tjmp ~a~31T ; zero, always taken~%" label)
                (format out "~%~10Tbeq ~a~%" label)))
	(let ((label-less (new-6502-label "GtLess"))
                (label-more (new-6502-label "GtMore")))
	  (unless (6502-zero-p mid)
	    (format out "~%~10Tcmp # ~a" (expression-constant-value mid)))
	  (format out "~%~10Tbeq ~a~%" label)
	  (format out "~%~10Tblt ~a~%" label-less)
	  (format out "~%~10Tbge ~a~%" label-more)
	  (format out "~%~a:" label-less)
	  (setf *6502-accumulator-expression* :trash/label-less
	        *6502-x-index-expression* :trash/label-less)
	  (when (<= low (1- mid))
	    (emit-6502-goto-tristree out targets low (1- mid) class-id method-id))
	  (format out "~%~a:" label-more)
	  (setf *6502-accumulator-expression* :trash/label-more
	        *6502-x-index-expression* :trash/label-more)
	  (when (<= (1+ mid) high)
	    (emit-6502-goto-tristree out targets (1+ mid) high class-id method-id)))))))

(defun 6502-zero-p (expression)
  (let ((n (cond ((numberp expression)
	        expression)
                 ((expression-constant-p expression)
	        (expression-constant-value expression))
                 (t nil))))
    (and (numberp n) (zerop n))))

;;; EVALUATE statement

(defun compile-6502-evaluate (out statement cpu)
  (let ((subject (safe-getf (rest statement) :subject))
        (clauses (safe-getf (rest statement) :when-clauses))
        (label-end (new-6502-label "EvalEnd")))
    (dolist (clause (ensure-list clauses))
      (cond
        ((eq (first clause) :when-other)
         ;; WHEN OTHER — fall-through, execute statements
         (dolist (s (ensure-list (second clause)))
	 (compile-statement cpu (first s) (rest s))))

        ((eq (first clause) :when)
         (let ((phrases (second clause))
	     (statements (third clause))
	     (label-next (new-6502-label "WhenNext")))
	 ;; Compare subject to phrases; if match, execute statements and jump to end
	 (cond

	   ((and (listp phrases) (string-equal (first phrases) "not"))
	    ;; WHEN NOT expression — subject must not equal expression
	    (emit-6502-load-expression out subject *class-id*)
	    (if (expression-constant-p (second phrases))
	        (unless (6502-zero-p (second phrases))
		(format out "~%~10Tcmp # ~a" (expression-constant-value (second phrases))))
	        (format out "~%~10Tcmp ~a" (emit-6502-value (second phrases))))
	    (format out "~%~10Tbeq ~a~%" label-next)
	    (dolist (s (ensure-list statements))
                (compile-statement cpu (first s) (rest s)))
	    (format out "~%~10T~a ~a~%" (6502-branch-always-mnemonic) label-end)
	    (format out "~%~a:" label-next)
	    (setf *6502-accumulator-expression* :trash/label-next
		*6502-x-index-expression* :trash/label-next))

	   ((and (listp phrases) (member (first phrases) '(through thru) :test #'string-equal))
	    ;; WHEN expression1 THROUGH expression2 — subject in [lo, hi] inclusive
	    (let ((low (second phrases)) (high (third phrases)))
                (emit-6502-load-expression out subject *class-id*)
                (if (expression-constant-p low)
		(unless (6502-zero-p low)
		  (format out "~%~10Tcmp # ~a" (expression-constant-value low)))
		(format out "~%~10Tcmp ~a" (emit-6502-value low)))
                (format out "~%~10Tblt ~a~%" label-next)
                ;; Upper bound: skip when subject > high, i.e. subject >= high + 1
                (if (and (expression-constant-p high) (numberp high))
		(unless (6502-zero-p high)
		  (format out "~%~10Tcmp # ~a" (expression-constant-value high)))
		(progn
		  ;; subject - high: if >= 1 then subject > high, skip
		  (format out "~%~10Tsec")
		  (if (expression-constant-p high)
		      (format out "~%~10Tsbc # ~a" (expression-constant-value high))
		      (format out "~%~10Tsbc ~a" (emit-6502-value high)))
		  (setf *6502-accumulator-expression* :trash/sbc)
		  (format out "~%~10Tcmp # 1")))
                (format out "~%~10Tbge ~a~%" label-next)
                (dolist (s (ensure-list statements))
	        (compile-statement cpu (first s) (rest s)))
                (format out "~%~10T~a ~a~%" (6502-branch-always-mnemonic) label-end)
                (format out "~%~a:" label-next)
                (setf *6502-accumulator-expression* :trash/label-next
		  *6502-x-index-expression* :trash/label-next)))

	   ((and (stringp phrases) (string-equal phrases "Any"))
	    ;; WHEN ANY — always match
	    (dolist (s (ensure-list statements))
                (compile-statement cpu (first s) (rest s)))
	    (format out "~%~10T~a ~a~%" (6502-branch-always-mnemonic) label-end))

	   ((and (stringp phrases) (string-equal phrases "TRUE"))
	    (emit-6502-load-expression out subject *class-id*)
	    (format out "~%~10Tbeq ~a~%" label-next)
	    (dolist (s (ensure-list statements))
                (compile-statement cpu (first s) (rest s)))
	    (format out "~%~10T~a ~a~%" (6502-branch-always-mnemonic) label-end)
	    (format out "~%~a:" label-next)
	    (setf *6502-accumulator-expression* :trash/label-next-true
		*6502-x-index-expression* :trash/label-next-true))

	   ((and (stringp phrases) (string-equal phrases "FALSE"))
	    (emit-6502-load-expression out subject *class-id*)
	    (format out "~%~10Tbne ~a~%" label-next)
	    (dolist (s (ensure-list statements))
                (compile-statement cpu (first s) (rest s)))
	    (format out "~%~10T~a ~a~%" (6502-branch-always-mnemonic) label-end)
	    (format out "~%~a:" label-next)
	    (setf *6502-accumulator-expression* :trash/label-next-false
		*6502-x-index-expression* :trash/label-next-false))

	   (t
	    ;; WHEN expression — equality
	    (let ((phrase-expression (if (listp phrases) (second phrases) phrases)))
                (emit-6502-load-expression out subject *class-id*)
                (if (expression-constant-p phrase-expression)
		(let ((n (expression-constant-value phrase-expression)))
		  (unless (6502-zero-p phrase-expression)
                        (format out "~%~10Tcmp # ~a" n)))
		(format out "~%~10Tcmp ~a" (emit-6502-value phrase-expression)))
                (format out "~%~10Tbne ~a~%" label-next))
	    (dolist (s (ensure-list statements))
                (compile-statement cpu (first s) (rest s)))
	    (format out "~%~10T~a ~a~%" (6502-branch-always-mnemonic) label-end)
	    (format out "~%~a:" label-next)
	    (setf *6502-accumulator-expression* :trash/label-next-when=
		*6502-x-index-expression* :trash/label-next-when=)))))))
    (format out "~%~a:" label-end)
    (setf *6502-accumulator-expression* :trash/label-end-when=
	*6502-x-index-expression* :trash/label-end-when=)))

;;; INSPECT statement

(defun compile-6502-inspect (out statement class-id)
  (let ((target (safe-getf (rest statement) :target))
        (tally (safe-getf (rest statement) :tallying))
        (conv-from (safe-getf (rest statement) :converting))
        (conv-to (safe-getf (rest statement) :to))
        (repl-by (safe-getf (rest statement) :by)))
    (let ((target-sym (to-identifier target)))
      (cond
        (tally
         ;; INSPECT id TALLYING tally FOR CHARACTERS — add 1 to tally per character
         (let ((tally-sym (to-identifier tally)))
	 (format out "~%~10T;; INSPECT ~a TALLYING ~a FOR CHARACTERS" target tally)
	 (format out "~%~10Tldy # 0")
	 (let ((label (new-6502-label "TallyLoop"))
                 (label-done (new-6502-label "TallyDone"))
                 (label-skip (new-6502-label "TallySkip")))
	   (format out "~%~a:" label)
	   (setf *6502-accumulator-expression* :trash/label-tally
	         *6502-x-index-expression* :trash/label-tally)
	   (format out "~%~10Tlda ~a, y" target-sym)
	   (format out "~%~10Tbeq ~a~%" label-done)
	   (format out "~%~10Tlda ~a" tally-sym)
	   (format out "~%~10Tclc")
	   (format out "~%~10Tadc # 1")
	   (format out "~%~10Tsta ~a" tally-sym)
	   (format out "~%~10Tblt ~a~%" label-skip)
	   (format out "~%~10Tinc ~a + 1" tally-sym)
	   (format out "~%~a:" label-skip)
	   (setf *6502-accumulator-expression* :trash/label-skip-tally
	         *6502-x-index-expression* :trash/label-skip-tally)
	   (format out "~%~10Tiny")
	   (format out "~%~10Tbne ~a~%" label)
	   (format out "~%~a:" label-done)
	   (setf *6502-accumulator-expression* :trash/label-done-tally
	         *6502-x-index-expression* :trash/label-done-tally))))
        (conv-from
         ;; INSPECT id CONVERTING from TO to — replace chars in string
         (format out "~%~10T;; INSPECT ~a CONVERTING" target)
         (format out "~%~10Tldy # 0")
         (let ((label (new-6502-label "InspLoop"))
	     (label-next (new-6502-label "InspNext"))
	     (label-done (new-6502-label "InspDone")))
	 (format out "~%~a:" label)
	 (setf *6502-accumulator-expression* :trash/inspect-label
                 *6502-x-index-expression* :trash/inspect-label)
	 (format out "~%~10Tlda ~a, y" target-sym)
	 (format out "~%~10Tbeq ~a~%" label-done)
	 (setf *6502-accumulator-expression* :trash/target-inspect)
	 (if (expression-constant-p conv-from)
	     (unless (6502-zero-p conv-from)
                 (format out "~%~10Tcmp # ~a" (expression-constant-value conv-from)))
	     (format out "~%~10Tcmp ~a" (emit-6502-value conv-from)))
	 (format out "~%~10Tbne ~a~%" label-next)
	 (with-accumulator-value (conv-to)
	   (if (expression-constant-p conv-to)
                 (format out "~%~10Tlda # ~a" (expression-constant-value conv-to))
                 (format out "~%~10Tlda ~a" (emit-6502-value conv-to))))
	 (format out "~%~10Tsta ~a, y" target-sym)
	 (format out "~%~a:" label-next)
	 (setf *6502-accumulator-expression* :trash/label-next-conv-to
                 *6502-x-index-expression* :trash/label-next-conv-to)
	 (format out "~%~10Tiny")
	 (format out "~%~10Tbne ~a~%" label)
	 (format out "~%~a:" label-done)
	 (setf *6502-accumulator-expression* :trash/label-done-conv-to
                 *6502-x-index-expression* :trash/label-done-conv-to)))
        (repl-by
         ;; INSPECT id REPLACING CHARACTERS BY expression — fill string with expression
         (let ((len (operand-width target)))
	 (format out "~%~10T;; INSPECT ~a REPLACING CHARACTERS BY ~a" target repl-by)
	 (emit-6502-load-expression out repl-by class-id)
	 (format out "~%~10Tldy # 0")
	 (let ((label (new-6502-label "InspLoop")))
	   (format out "~%~a:" label)
	   (setf *6502-accumulator-expression* :trash/inspect-replacing
	         *6502-x-index-expression* :trash/inspect-replacing)
	   (format out "~%~10Tsta ~a, y" target-sym)
	   (format out "~%~10Tiny")
	   (format out "~%~10Tcpy # ~d" len)
	   (format out "~%~10Tbne ~a~%" label))))))))

;;; INVOKE statement

(defun compile-6502-invoke (out statement &optional class-id)
  (let* ((object (safe-getf (rest statement) :object))
         (method (safe-getf (rest statement) :method))
         (as-class (safe-getf (rest statement) :as))
         (returning (safe-getf (rest statement) :returning)))
    (cond
      ((string-equal method "Class-P")
       (format out "~%~10Tjsr Lib.BasicObjectClassP"))
      ((string-equal object "Self")
       ;; Same  OOPS  path as  INVOKE  on  a named  object:  Phantasia
       ;; CallMethod macro (DoCallMethod).
       (let ((class (or as-class *class-id*)))
         (format out "~%~10T.CallMethod Call~a~a, ~aClass~%"
                 (to-identifier (method-class class method))
	       (to-identifier method)
	       (to-identifier (method-class class method)))))
      (t (let* ((Unknown '#:Unknown)
                (class (or as-class (oops-class-of object) class-id Unknown)))
	 (if (eq class Unknown)
	     (error "Unknown class of method ~a" method)
	     (format out "~%~10T.CallMethod Call~a~a, ~aClass, ~a~%"
		   (to-identifier (method-class class method))
		   (to-identifier method)
		   (to-identifier (method-class class method))
		   (to-identifier object))))))
    (setf *6502-accumulator-expression* :trash/call-method
	*6502-x-index-expression* :trash/call-method)
    (when returning
      (format out "~%~10Tsta ~a" (to-identifier returning))
      (setf *6502-accumulator-expression* returning))))

;;; CALL statement

(defun sym-string (x)
  "Return PascalCase assembly symbol from a literal, identifier, or string (COBOL stabby-case)."
  (to-identifier (if (listp x) (second x) x)))

(defun %service-call-dispatch-symbol (item-sym)
  "Map CALL SERVICE routine stem to assembly dispatch label (ServiceFoo for LUT / .FarJSR).
When ITEM-SYM already has a Service prefix (case-insensitive), return it unchanged."
  (format nil "Service~a" item-sym))

(defun compile-6502-call (out statement)
  (let* ((target (safe-getf (rest statement) :target))
         (service (safe-getf (rest statement) :service))
         (bank (safe-getf (rest statement) :bank))
         (libraryp (safe-getf (rest statement) :library))
         (tail-call-p (safe-getf (rest statement) :tail-call-p))
         (item (or service target))
         (item-sym (sym-string item))
         (dispatch-sym (if service (%service-call-dispatch-symbol item-sym) item-sym))
         (resolved-bank (or bank (service-bank-table-lookup dispatch-sym)))
         (jmp-p tail-call-p)
         (returning (safe-getf (rest statement) :returning))
         (using (safe-getf (rest statement) :using)))
    (when using
      (emit-6502-load-expression out using))
    (cond
      ;; CALL target IN SERVICE bank. / CALL SERVICE target. — service dispatch
      (service
       (assert (not jmp-p))
       (if resolved-bank
	 (let ((bank-sym (sym-string resolved-bank)))
	   (format out "~%~10T.FarJSR ~a, ~a~%" dispatch-sym bank-sym))
	 (error "EIGHTBOL: CALL SERVICE ~a requires bank (not in service-bank table)"
	        service)))
      ;; CALL target IN LIBRARY. — always call LastBank library thunk label.
      ;; Emits jsr Lib.<RoutineName> (e.g. CALL Move-Decal-Y IN LIBRARY
      ;; => jsr Lib.MoveDecalY), regardless of service LUT entries.
      (libraryp
       (format out "~%~10T~a Lib.~a~%" (if jmp-p "jmp" "jsr")
	     item-sym)
       (setf *6502-accumulator-expression* :trash/call-lib
	   *6502-x-index-expression* :trash/call-lib))
      ;; CALL target IN BANK bank. — bank-switched far call
      (bank
       (let ((bank-sym (sym-string bank)))
         (assert (not jmp-p))
         (format out "~%~10T.FarJSR ~a, ~a~%" bank-sym item-sym)
         (setf *6502-accumulator-expression* :trash/far-jsr
	     *6502-x-index-expression* :trash/far-jsr)))
      ;; CALL target. — local near call (unknown label in current bank)
      (t
       (format out "~%~10T~a ~a~%" (if jmp-p "jmp" "jsr") item-sym)
       (setf *6502-accumulator-expression* :trash/call-near
	   *6502-x-index-expression* :trash/call-near)))
    (when jmp-p
      (format out "~%~10T;; tail call~%"))
    (when returning
      (format out "~%~10Tsta ~a" (emit-6502-value returning))
      (setf *6502-accumulator-expression* returning))))

;;; IF / conditional compilation

(defvar *6502-label-counter* 0)

(defun new-6502-label (prefix)
  "Generate unique label with meaningful name (L prefix, letter-start for 64tass)."
  (format nil "EIGHTBOL_~a_~36r" prefix (incf *6502-label-counter*)))

(defun compile-6502-if (out statement cpu)
  (flet ((ensure-statement-list (branch)
	 (let ((b (remove nil (ensure-list branch))))
	   (cond ((null b) nil)
	         ((and (listp (first b)) (keywordp (first (first b)))) b)
	         (t (list b))))))
    (let ((condition (safe-getf (rest statement) :condition))
	(then-statements (ensure-statement-list
		        (safe-getf (rest statement) :then)))
	(else-statements (ensure-statement-list
		        (safe-getf (rest statement) :else)))
	(label-else (new-6502-label "IfElse"))
	(label-end (new-6502-label "IfEnd")))
      (emit-6502-condition out condition *class-id* label-else)
      (dolist (s (ensure-list then-statements))
        (compile-statement cpu (first s) (rest s)))
      (when (and else-statements (not (null else-statements)))
        (format out "~%~10T~a ~a~%" (6502-branch-always-mnemonic) label-end))
      (format out "~%~a:" label-else)
      (setf *6502-accumulator-expression* :trash/label-if-else
	  *6502-x-index-expression* :trash/label-if-else)
      (when (and else-statements (not (null else-statements)))
        (dolist (s (ensure-list else-statements))
	(compile-statement cpu (first s) (rest s)))
        (format out "~%~a:" label-end)
        (setf *6502-accumulator-expression* :trash/if-end
	    *6502-x-index-expression* :trash/if-end)))))

(defun normalize-relation-condition (condition)
  "If CONDITION is (lhs op rhs) with op in the middle, return (op lhs rhs).
If CONDITION is (lhs IS LESS THAN rhs) or (lhs LESS THAN rhs) etc. (5 elements),
return (op lhs rhs). Otherwise return CONDITION unchanged."
  (when (listp condition)
    (cond
      ;; 6-element infix negated equality: (lhs IS NOT EQUAL TO rhs)
      ((= (length condition) 6)
       (destructuring-bind (a b c d e f) condition
         (flet ((token (x str) (string-equal (princ-to-string x) str)))
	 (when (and (token b "IS")
		  (token c "NOT")
		  (or (token d "=") (token d "EQUAL"))
		  (token e "TO"))
	   (return-from normalize-relation-condition
	     (list :not (list '= a f)))))))
      ;; 4-element infix negated equality: (lhs NOT = rhs) / (lhs NOT EQUAL rhs)
      ((= (length condition) 4)
       (destructuring-bind (a b c d) condition
         (flet ((token (x str) (string-equal (princ-to-string x) str)))
	 (when (and (token b "NOT")
		  (or (token c "=") (token c "EQUAL")))
	   (return-from normalize-relation-condition
	     (list :not (list := a d)))))))
      ;; 5-element: (expression is less than expression) or (expression less than expression), etc.
      ((or (member (length condition) '(4 5)))
       (destructuring-bind (a b c d e) condition
         (flet ((token (x str) (string-equal (princ-to-string x) str)))
	 (let ((op (cond ((and (token b "IS") (token c "LESS") (token d "THAN")) '<)
		       ((and (token b "LESS") (token c "THAN")) '<)
		       ((and (token b "IS") (token c "GREATER") (token d "THAN")) '>)
		       ((and (token b "GREATER") (token c "THAN")) '>)
		       (t nil))))
	   (when op (return-from normalize-relation-condition (list op a e)))))))
      ;; 3-element: (lhs op rhs) -> (op lhs rhs)
      ((= (length condition) 3)
       (let ((a (first condition))
	   (b (second condition))
	   (c (third condition)))
         (when (member (princ-to-string a) '(/= ≠ <>) :test #'string-equal)
	 (return-from normalize-relation-condition (list 'not (list '= b c))))
         (when (member (princ-to-string b) '(/= ≠ <>) :test #'string-equal)
	 (return-from normalize-relation-condition (list 'not (list '= a c))))
         (when (member (princ-to-string b) '(= equal < less > greater >= ≤ ≥ <=)
		   :test #'string-equal)
	 (return-from normalize-relation-condition (list b a c)))))))
  condition)

(defun emit-6502-false-when-not-unsigned-greater-than-zero (out lhs class-id branch-label)
  "Emit code that jumps to BRANCH-LABEL when unsigned LHS is not greater than zero.

For rhs zero, unsigned > 0 is equivalent to “any byte non-zero” for multi-byte values.
Uses lda zero flag for width 1 (no redundant cmp #0). Avoids blt after
cmp #0 (unsigned nothing is below zero)."
  (let ((w (max 1 (operand-width lhs))))
    (cond
      ((= w 1)
       (emit-6502-load-expression out lhs class-id)
       (format out "~%~10Tbeq ~a~%" branch-label))
      (t
       (let ((label-then (new-6502-label "IfGT0")))
         (dotimes (i (1- w))
	 (emit-6502-load-byte-n out lhs class-id i w)
	 (format out "~%~10Tbne ~a~%" label-then))
         (emit-6502-load-byte-n out lhs class-id (1- w) w)
         (format out "~%~10Tbeq ~a~%" branch-label)
         (format out "~%~a:" label-then)
         (setf *6502-accumulator-expression* :trash/unsigned
	     *6502-x-index-expression* :trash/unsigned))))))

(defun emit-6502-branch-if-expression-not-all-zero (out expression class-id branch-label)
  "Jump to BRANCH-LABEL if unsigned EXPRESSION is not all-zero bytes.

Used for IF (IS ZERO X), (= X 0), and (= 0 X): the false branch
runs when any byte is non-zero. W is operand-width of EXPRESSION; W=1 uses
emit-6502-load-expression + bne.

@table @asis
@item EXPRESSION
Slot, identifier, or other loadable expression.
@item CLASS-ID
Current class for slot symbols.
@item BRANCH-LABEL
Label when value is not all zeros (condition false for IS-ZERO).
@end table"
  (let ((w (max 1 (expression-operand-width expression))))
    (if (= w 1)
        (progn
	(emit-6502-load-expression out expression class-id)
	(format out "~%~10Tbne ~a~%" branch-label))
        (dotimes (i w)
	(emit-6502-load-byte-n out expression class-id i w)
	(format out "~%~10Tbne ~a~%" branch-label)))))

(defun emit-6502-branch-if-expression-all-zero (out expression class-id branch-label)
  "Jump to BRANCH-LABEL if unsigned EXPRESSION is all-zero bytes (W-wide).

Used for IS NOT ZERO false path and (NOT (= X 0)) when X is zero:
condition is false when every byte is zero. W=1 uses emit-6502-load-expression + beq.

@table @asis
@item EXPRESSION
Slot or identifier.
@item CLASS-ID
Current class for slot symbols.
@item BRANCH-LABEL
Label when value is all zeros (IS-NOT-ZERO is false).
@end table"
  (let ((w (max 1 (expression-operand-width expression))))
    (if (= w 1)
        (progn
	(emit-6502-load-expression out expression class-id)
	(format out "~%~10Tbeq ~a~%" branch-label))
        (let ((label-some (new-6502-label "SomeNz")))
	(dotimes (i w)
	  (emit-6502-load-byte-n out expression class-id i w)
	  (format out "~%~10Tbne ~a~%" label-some))
	(format out "~%~10T~a ~a~%" (6502-branch-always-mnemonic) branch-label)
	(format out "~%~a:" label-some)
	(setf *6502-accumulator-expression* :trash/if0
                *6502-x-index-expression* :trash/if0)))))


