;; src/ast-optimize.lisp — AST-level optimization: constant folding, dead code elimination, CSE, and tail-call detection
;;
;; Performs:

;; 0.  DIVIDE/MULTIPLY by  constant power-of-two  → :compute  with shift
;;   (all backends).

;;   1.      Constant     folding:      pure     literal      arithmetic
;;   in :compute/:set/:move :from.

;;   1b. Algebraic simplification on folded subexpressions: e.g. @code{x
;;      +  (-k) →  x  - k},  @code{x  - (-k)  → x  +  k}, @code{±0}  and
;;      @code{*1} identities, @code{*0} and @code{/1} where operands are
;;      integer constants.

;;   2. Unreachable code elimination:  removes statements after terminal
;;      control                                                     flow
;;      (:goback, :exit-method, :exit-program, :exit, :stop-run).

;;   3.  Dead  store  elimination:  removes  writes  overwritten  before
;;   being read.

;;   4. Common subexpression elimination: identifies and factors out
;;      identical subexpressions into temporary variables to avoid
;;      recomputation.

;;   5.  Tail-call detection:  annotates  INVOKE/CALL  in tail  position
;;      with   :tail-call-p.   (INVOKE   with   :returning   cannot   be
;;      tail-called; CALL has no return value.)
;;
;; Analysis  is   conservative:  PERFORM  and  INVOKE   are  treated  as
;; potentially reading any location;  subscripted accesses with variable
;; indices may alias.
(in-package :eightbol)

;;; Location extraction (for data-flow analysis)

(defun location-key (expression)
  "Return a comparable key for a storage location, or NIL if not a storable location.
   Locations: string (identifier), (:of slot obj), (:subscript base index).
   For (:subscript base idx), if idx is non-constant we return (subscript base) to
   indicate potential alias with any element of base."
  (cond
    ((stringp expression) (list :id expression))
    ((and (listp expression) (eq (first expression) :of))
     (list :of (second expression) (third expression)))
    ((and (listp expression) (eq (first expression) :subscript))
     (if (numberp (third expression))
         (list :subscript (second expression) (third expression))
         (list :subscript (second expression) :variable)))
    (t nil)))

(defun locations-may-alias-p (loc1 loc2)
  "Conservative: true if loc1 and loc2 could refer to the same storage."
  (cond
    ((or (null loc1) (null loc2)) nil)
    ((equal loc1 loc2) t)
    ((and (eq (first loc1) :subscript) (eq (first loc2) :subscript)
          (equal (second loc1) (second loc2)))
     ;; Same base: alias if either index is :variable, or indices match
     (or (eq (third loc1) :variable) (eq (third loc2) :variable)
         (equal (third loc1) (third loc2))))
    (t nil)))

(defun statement-writes-to (statement)
  "Return list of location keys that STATEMENT writes to. NIL if none."
  (when (listp statement)
    (let ((op (first statement)))
      (case op
        ((:move)
         (let ((to (safe-getf (rest statement) :to)))
           (when to (list (location-key to)))))
        ((:+)
         (append (when (safe-getf (rest statement) :to)
                   (list (location-key (safe-getf (rest statement) :to))))
                 (when (safe-getf (rest statement) :giving)
                   (list (location-key (safe-getf (rest statement) :giving))))))
        ((:-)
         (append (when (safe-getf (rest statement) :from)
                   (list (location-key (safe-getf (rest statement) :from))))
                 (when (safe-getf (rest statement) :giving)
                   (list (location-key (safe-getf (rest statement) :giving))))))
        ((:compute)
         (let ((tgt (safe-getf (rest statement) :target)))
           (when tgt (list (location-key tgt)))))
        ((:set)
         (append (when (safe-getf (rest statement) :target)
                   (list (location-key (safe-getf (rest statement) :target))))
                 (when (safe-getf (rest statement) :up-by)
                   (list (location-key (safe-getf (rest statement) :up-by))))
                 (when (safe-getf (rest statement) :down-by)
                   (list (location-key (safe-getf (rest statement) :down-by))))
                 (when (safe-getf (rest statement) :to-self)
                   (list (location-key (safe-getf (rest statement) :to-self))))))
        ((:invoke)
         (let ((ret (safe-getf (rest statement) :returning)))
           (when ret (list (location-key ret)))))
        ((:string-blt)
         (let ((dest (safe-getf (rest statement) :dest)))
           (when dest
             (if (and (listp dest) (eq (first dest) :refmod))
                 (list (location-key (safe-getf (rest dest) :base)))
                 (list (location-key dest))))))
        ((:÷)
         (let ((giving (safe-getf (rest statement) :giving))
               (denominator (safe-getf (rest statement) :denominator)))
           (cond
             (giving (list (location-key giving)))
             (denominator (list (location-key denominator)))
             (t nil))))
        ((:×)
         (let ((giving (safe-getf (rest statement) :giving))
               (by (safe-getf (rest statement) :by)))
           (cond
             (giving (list (location-key giving)))
             (by (list (location-key by)))
             (t nil))))
        (t nil)))))

(defun expression-locations (expression)
  "Return list of location keys read from EXPRESSION."
  (cond
    ((null expression) nil)
    ((stringp expression) (list (list :id expression)))
    ((numberp expression) nil)
    ((and (listp expression) (eq (first expression) :of))
     (list (location-key expression)))
    ((and (listp expression) (eq (first expression) :subscript))
     (list (location-key expression)))
    ((listp expression)
     (delete-duplicates
      (mapcan #'expression-locations (rest expression))
      :test #'equal))
    (t nil)))

(defun statement-reads-from (statement)
  "Return list of location keys that STATEMENT reads from."
  (when (listp statement)
    (let ((op (first statement)))
      (case op
        ((:move)
         (expression-locations (safe-getf (rest statement) :from)))
        ((:+)
         (append (expression-locations (safe-getf (rest statement) :from))
                 (expression-locations (safe-getf (rest statement) :to))))
        ((:-)
         (append (expression-locations (safe-getf (rest statement) :from))
                 (expression-locations (safe-getf (rest statement) :subtrahend))))
        ((:compute)
         (expression-locations (safe-getf (rest statement) :expression)))
        ((:set)
         (append (expression-locations (safe-getf (rest statement) :value))
                 (expression-locations (safe-getf (rest statement) :by))
                 (expression-locations (safe-getf (rest statement) :address-of))))
        ((:invoke)
         (expression-locations (safe-getf (rest statement) :object)))
        ((:if)
         (append (expression-locations (safe-getf (rest statement) :condition))
                 (statement-list-reads (safe-getf (rest statement) :then))
                 (statement-list-reads (safe-getf (rest statement) :else))))
        ((:perform)
         (append (expression-locations (safe-getf (rest statement) :times))
                 (expression-locations (safe-getf (rest statement) :until))
                 (expression-locations (safe-getf (rest statement) :from))
                 (expression-locations (safe-getf (rest statement) :by))))
        ((:string-blt)
         (append (expression-locations (safe-getf (rest statement) :source))
                 (expression-locations (safe-getf (rest statement) :dest))
                 (expression-locations (safe-getf (rest statement) :length))))
        ((:÷)
         (append (expression-locations (safe-getf (rest statement) :numerator))
                 (expression-locations (safe-getf (rest statement) :denominator))
                 (expression-locations (safe-getf (rest statement) :by))))
        ((:×)
         (append (expression-locations (safe-getf (rest statement) :multiplier))
                 (expression-locations (safe-getf (rest statement) :by))))
        (t nil)))))

(defun statement-list-reads (statements)
  (mapcan (lambda (s) (statement-reads-from s)) (ensure-list statements)))

;;; Terminal / control-flow

(defun terminal-statement-p (statement)
  "True if STATEMENT unconditionally exits the current flow (no successor).
Includes @code{:call} only when @code{:tail-call-p} was set by
@code{%maybe-annotate-tail-call-before-return-statement} (@code{CALL … GOBACK}), so @code{GOBACK}
after a tail @code{jmp} is removed by a second unreachable pass.
@code{INVOKE} is never terminal here: it expands to @code{.CallMethod} and returns, so a
following @code{GOBACK} may remain (or a trailing @code{rts} is emitted)."
  (and (listp statement)
       (or (member (first statement) '(:goback :exit-method :exit-program :exit :stop-run))
           (and (eq (first statement) :call)
                (safe-getf (rest statement) :tail-call-p)
                (null (safe-getf (rest statement) :service))
                (null (safe-getf (rest statement) :bank))))))

(defun control-flow-statement-p (statement)
  "True if STATEMENT branches or calls (perform, invoke, goto, call)."
  (and (listp statement)
       (member (first statement) '(:if :perform :goto :call :invoke))))

;;; Literal power-of-two (for DIVIDE/MULTIPLY → shift in AST)

(defun literal-integer-for-divmul-p (expression)
  "True if EXPRESSION is an integer literal (:literal n or integer) for shift lowering."
  (integerp expression))

(defun literal-integer-value (expression)
  "Integer value of EXPRESSION when literal-integer-for-divmul-p."
  (if (integerp expression) expression nil))

(defun power-of-two-shift-count (n)
  (when (and (integerp n) (plusp n))
    (round (log n 2))))

;;; Preserve COBOL source location through rewrites (see parser @code{statement-with-source-location})

(defun %statement-source-location-suffix (statement)
  "Return plist tail fragment @code{:source-file}, @code{:source-line}, @code{:source-sequence}
from STATEMENT, or NIL."
  (when (and (listp statement) (rest statement))
    (let ((p (rest statement)))
      (when (and p (evenp (length p)))
        (append (when (safe-getf p :source-file)
                  (list :source-file (safe-getf p :source-file)))
                (when (safe-getf p :source-line)
                  (list :source-line (safe-getf p :source-line)))
                (when (safe-getf p :source-sequence)
                  (list :source-sequence (safe-getf p :source-sequence))))))))

(defun %append-statement-source-location (head statement)
  "Append location keys from STATEMENT onto new statement list HEAD (HEAD is @code{(type ... keys)})."
  (append head (%statement-source-location-suffix statement)))

;;; DIVIDE / MULTIPLY → :move with :ash (shared by all backends)
;;; (:ash v n) with n < 0 shifts right, n > 0 shifts left, per canonical form.

(defun rewrite-divide-multiply-statement (statement)
  "If STATEMENT is DIVIDE/MULTIPLY with constant power-of-two, return equivalent :move.
DIVIDE by 2^s becomes a right shift (:ash numerator (- s)); MULTIPLY by 2^s a left
shift (:ash operand s). Otherwise return STATEMENT unchanged."
  (unless (listp statement)
    (return-from rewrite-divide-multiply-statement statement))
  (case (first statement)
     (:÷
      (let ((numerator (safe-getf (rest statement) :numerator))
            (denominator (safe-getf (rest statement) :denominator))
            (giving (safe-getf (rest statement) :giving)))
        (unless (power-of-two-p denominator)
          (return-from rewrite-divide-multiply-statement statement))
        (let ((sc (power-of-two-shift-count (literal-integer-value denominator))))
          (unless sc
            (return-from rewrite-divide-multiply-statement statement))
          (cond
            ((and numerator giving)
             (%append-statement-source-location
              (list :move :from (list :ash numerator (- sc)) :to giving)
              statement))
            ((and numerator (null giving))
             (%append-statement-source-location
              (list :move :from (list :ash numerator (- sc)) :to numerator)
              statement))
            (t statement)))))
    (:×
     (let ((multiplier (safe-getf (rest statement) :multiplier))
           (by (safe-getf (rest statement) :by))
           (giving (safe-getf (rest statement) :giving)))
       ;; The power-of-two operand determines the shift count; the other operand
       ;; is shifted. Result lands in GIVING, or back in BY when GIVING is absent.
       (let ((pow-operand (cond ((power-of-two-p multiplier) multiplier)
                                ((power-of-two-p by) by)
                                (t nil))))
         (unless pow-operand
           (return-from rewrite-divide-multiply-statement statement))
         (let ((sc (power-of-two-shift-count (literal-integer-value pow-operand))))
           (unless sc
             (return-from rewrite-divide-multiply-statement statement))
           (let ((shifted (list :ash (if (eq pow-operand multiplier) by multiplier) sc)))
             (cond
               (giving
                (%append-statement-source-location
                 (list :move :from shifted :to giving)
                 statement))
               (by
                (%append-statement-source-location
                 (list :move :from shifted :to by)
                 statement))
               (t statement)))))))
    (t statement)))

(defun rewrite-divide-multiply-in-list (statements)
  (mapcar (lambda (s)
            (cond
              ((and (listp s) (eq (first s) :if))
               (%append-statement-source-location
                (list :if
                      :condition (safe-getf (rest s) :condition)
                      :then (rewrite-divide-multiply-in-list (safe-getf (rest s) :then))
                      :else (rewrite-divide-multiply-in-list (safe-getf (rest s) :else)))
                s))
              (t (rewrite-divide-multiply-statement s))))
          (ensure-list statements)))

;;; Constant folding (literals only — no copybook symbols)

(defun %algebraic-simplify-add (a b)
  "After folding subexpressions A and B, rewrite @code{A + B} for integer constants.
@code{A + (-k)} → @code{A - k}; @code{(-k) + B} → @code{B - k}; @code{+0} identities.
Returns an integer or a @code{:+}/@code{:-} list."
  (cond
    ((and (numberp a) (numberp b)) (+ a b))
    ((and (integerp b) (minusp b)) (list :- :subtrahend a :from (- b) :giving nil))
    ((and (integerp a) (minusp a)) (list :- :subtrahend b :from (- a) :giving nil))
    ((and (integerp b) (zerop b)) a)
    ((and (integerp a) (zerop a)) b)
    ((and (expression-constant-p a) (expression-constant-p b))
     (list :literal (format nil "(~a + ~a)"
                            (expression-constant-value a)
                            (expression-constant-value b))))
    (t (list :+ :from a :to b :giving nil))))

(defun %algebraic-simplify-subtract (a b)
  "Rewrite @code{A - B}: @code{A - (-k)} → @code{A + k}; @code{A - 0} → @code{A}."
  (cond
    ((and (numberp a) (numberp b)) (- a b))
    ((and (integerp b) (minusp b)) (list :+ a (- b)))
    ((and (integerp b) (zerop b)) a)
    ((and (expression-constant-p a) (expression-constant-p b))
     (list :literal (format nil "(~a - ~a)"
                            (expression-constant-value a)
                            (expression-constant-value b))))
    (t (list :- :subtrahend b :from a :giving nil))))

(defun %algebraic-simplify-multiply (a b)
  "Integer identities: @code{*0}, @code{*1}; else @code{:×}."
  (cond
    ((and (numberp a) (numberp b)) (* a b))
    ((or (and (integerp a) (zerop a)) (and (integerp b) (zerop b))) 0)
    ((and (integerp a) (= a 1)) b)
    ((and (integerp b) (= b 1)) a)
    ((and (integerp a) (power-of-two-p a))
     (list :ash b (log a 2)))
    ((and (integerp b) (power-of-two-p b))
     (list :ash a (log b 2)))
    ((and (expression-constant-p a) (expression-constant-p b))
     (list :literal (format nil "(~a * ~a)"
                            (expression-constant-value a)
                            (expression-constant-value b))))
    (t (list :× :multiplier a :by b :giving nil))))

(defun %algebraic-simplify-divide (a b)
  "Integer @code{/1} and @code{0/n}; else @code{:÷}."
  (cond
    ((and (numberp a) (numberp b)) (/ a b))
    ((and (integerp b) (= b 1)) a)
    ((and (integerp a) (zerop a) (integerp b) (not (zerop b))) 0)
    ((and (integerp b) (power-of-two-p b))
     (list :ash a (log b 2)))
    ((and (expression-constant-p a) (expression-constant-p b))
     (list :literal (format nil "(~a / ~a)"
                            (expression-constant-value a)
                            (expression-constant-value b))))
    (t (list :÷ :numerator a :dividend b :giving nil))))

(defun fold-literal-expression (expression)
  "Fold EXPRESSION when all operands are integer or @code{:literal}; apply algebraic
simplification (sign flips, 0/1 identities) so backends see fewer operations.

@table @asis
@item EXPRESSION
Arithmetic AST node, identifier, or integer; copybook symbols are not folded.
@end table

@subsection Outputs
Integer, simplified list, or EXPRESSION unchanged for non-arithmetic leaves."
  (block nil
    (unless (listp expression)
      (return-from fold-literal-expression
        (cond
          ((numberp expression) expression)
          ((stringp expression) expression)
          ((eql :zero expression) 0)
          ((eql :null expression) :null)
          ((eql :self expression) "Self")
          (t (error "unknown expression ~s" expression)))))
    
    ;; Normalize integer (:literal n) to N so binary ops see negative constants.
    (when (and (eq (first expression) :literal) (integerp (second expression)))
      (return (second expression)))
    
    (when (and (equal "(" (first expression))
               (equal ")" (lastcar expression)))
      (when (= 3 (length expression))
        (return (fold-literal-expression (second expression))))
      (return-from fold-literal-expression
        (mapcar #'fold-literal-expression (subseq expression 1 (1- (length expression))))))
    
    (case (first expression)
      
      (:×-expr
       (let ((a (fold-literal-expression (second expression)))
             (b (fold-literal-expression (third expression))))
         (cond ((and (integerp a) (integerp b)) (* a b))
               ((or (= a 0) (= b 0)) 0)
               ((= a 1) b)
               ((= b 1) a)
               (t (list :×-expr a b)))))
      
      (:÷-expr
       (let ((a (fold-literal-expression (second expression)))
             (b (fold-literal-expression (third expression))))
         (cond ((and (integerp a) (integerp b)) (/ a b))
               ((= b 1) a)
               (t (list :÷-expr a b)))))
      
      (:+expr
       (let ((a (fold-literal-expression (second expression)))
             (b (fold-literal-expression (third expression))))
         (cond ((and (integerp a) (integerp b)) (+ a b))
               ((and (numberp b) (minusp b))
                (list :-expr a (- b)))
               (t (list :+expr a b)))))
      
      (:-expr
       (let ((a (fold-literal-expression (second expression)))
             (b (fold-literal-expression (third expression))))
         (cond ((and (integerp a) (integerp b)) (- a b))
               ((and (numberp b) (minusp b))
                (list :+expr a (- b)))
               (t (list :-expr a b)))))
      
      (:+
       (let ((a (fold-literal-expression (getf (rest expression) :from)))
             (b (fold-literal-expression (getf (rest expression) :to))))
         (cond ((and (integerp a) (integerp b)) (+ a b))
               (t (let ((s (%algebraic-simplify-add a b)))
                    (if (and (listp s) (eq (first s) :-))
                        (fold-literal-expression s)
                        s))))))
      (:-
       (let ((a (fold-literal-expression (getf (rest expression) :from)))
             (b (fold-literal-expression (getf (rest expression) :subtrahend))))
         (cond ((and (integerp a) (integerp b)) (- a b))
               (t (let ((s (%algebraic-simplify-subtract a b)))
                    (if (and (listp s) (eq (first s) :+))
                        (fold-literal-expression s)
                        s))))))
      (:×
       (let ((a (fold-literal-expression (getf (rest expression) :multiplier)))
             (b (fold-literal-expression (getf (rest expression) :by))))
         (%algebraic-simplify-multiply a b)))
      
      (:÷
       (let ((a (fold-literal-expression (getf (rest expression) :numerator)))
             (b (fold-literal-expression (getf (rest expression) :denominator))))
         (%algebraic-simplify-divide a b)))
      
      (:ash
       (let ((a (fold-literal-expression (second expression)))
             (n (third expression)))
         (if (and (integerp a) (integerp n))
             (ash a n)
             (list :ash a n))))
      
       (:ash
        (let ((a (fold-literal-expression (second expression)))
              (n (third expression)))
          (if (and (integerp a) (integerp n))
              (ash a (- n))
              (list :ash a n))))
       
       ;; Canonical arithmetic operators
       (:+
        (let ((a (fold-literal-expression (second expression)))
              (b (fold-literal-expression (third expression))))
          (if (and (integerp a) (integerp b))
              (+ a b)
              (list :+ a b))))
       (:-
        (let ((a (fold-literal-expression (second expression)))
              (b (fold-literal-expression (third expression))))
          (if (and (integerp a) (integerp b))
              (- a b)
              (list :- a b))))
       (:×
        (let ((a (fold-literal-expression (second expression)))
              (b (fold-literal-expression (third expression))))
          (if (and (integerp a) (integerp b))
              (* a b)
              (list :× a b))))
       (:÷
        (let ((a (fold-literal-expression (second expression)))
              (b (fold-literal-expression (third expression))))
          (if (and (integerp a) (integerp b) (not (zerop b)))
              (truncate a b)
              (list :÷ a b))))
       
       ;; Bitwise operators
       (:∧
        (let ((a (fold-literal-expression (second expression)))
              (b (fold-literal-expression (third expression))))
          (if (and (integerp a) (integerp b))
              (logand a b)
              (list :∧ a b))))
       (:∨
        (let ((a (fold-literal-expression (second expression)))
              (b (fold-literal-expression (third expression))))
          (if (and (integerp a) (integerp b))
              (logior a b)
              (list :∨ a b))))
       (:⊻
        (let ((a (fold-literal-expression (second expression)))
              (b (fold-literal-expression (third expression))))
          (if (and (integerp a) (integerp b))
              (logxor a b)
              (list :⊻ a b))))
       (:¬
        (let ((a (fold-literal-expression (second expression))))
          (if (integerp a)
              (lognot a)
              (list :¬ a))))
       
       ;; Shift operator
       (:ash
        (let ((a (fold-literal-expression (second expression)))
              (n (fold-literal-expression (third expression))))
          (if (and (integerp a) (integerp n))
              (ash a n)
              (list :ash a n))))
       
       (otherwise expression))))

(defun fold-constants-in-statement (statement)
  "Return STATEMENT with literal sub-expressions folded where safe."
  (unless (listp statement)
    (return-from fold-constants-in-statement statement))
  (when (and (stringp (first statement))
             (string= "(" (first statement))
             (stringp (lastcar statement))
             (string= ")" (lastcar statement)))
    (return-from fold-constants-in-statement
      (fold-constants-in-list (subseq statement 1 (1- (length statement))))))
  (case (first statement)
    
    (:÷
     (let ((numerator (safe-getf (rest statement) :numerator))
           (dividend (safe-getf (rest statement) :dividend)))
       (let* ((n (and numerator dividend
                      (expression-constant-p numerator) (expression-constant-p dividend)))
              (num (when n (expression-constant-value numerator)))
              (den (when n (expression-constant-value dividend))))
         (if (and num den (not (zerop den)))
             (multiple-value-bind (q r) (truncate num den)
               (declare (ignore r))
               (list :literal q))
             statement))))
    
    (:×
     (let ((multiplier (safe-getf (rest statement) :multiplier))
           (by (safe-getf (rest statement) :by)))
       (if (and multiplier by
                (expression-constant-p multiplier) (expression-constant-p by))
           (list :literal (* (expression-constant-value multiplier)
                             (expression-constant-value by)))
           statement)))
    
    (:+
     (let ((from (safe-getf (rest statement) :from))
           (to (safe-getf (rest statement) :to)))
       (if (and from to
                (expression-constant-p from) (expression-constant-p to))
           (list :literal (+ (expression-constant-value from)
                             (expression-constant-value to)))
           statement)))
    
    (:-
     (let ((from (safe-getf (rest statement) :from))
           (subtrahend (safe-getf (rest statement) :subtrahend)))
       (if (and from subtrahend
                (expression-constant-p from) (expression-constant-p subtrahend))
           (list :literal (- (expression-constant-value from)
                             (expression-constant-value subtrahend)))
           statement)))

    (:ash
     (if (and (expression-constant-p (second statement)) (expression-constant-p (third statement)))
         (list :literal (ash (expression-constant-value (second statement))
                             (expression-constant-value (third statement))))
         statement))
    
    (:∨
     (if (every #'expression-constant-p (rest statement))
         (list :literal (format nil "(~{~a~^ | ~})"
                                (mapcar #'expression-constant-value (rest statement))))
         statement))
    
    (:∧
     (if (every #'expression-constant-p (rest statement))
         (list :literal (format nil "(~{~a~^ & ~})"
                                (mapcar #'expression-constant-value (rest statement))))
         statement))
    
    (:⊻
     (if (every #'expression-constant-p (rest statement))
         (list :literal (format nil "(~{~a~^ ^ ~})"
                                (mapcar #'expression-constant-value (rest statement))))
         statement))
    
    (:if
     (%append-statement-source-location
      (list :if
            :condition (fold-literal-expression (safe-getf (rest statement) :condition))
            :then (fold-constants-in-list (safe-getf (rest statement) :then))
            :else (fold-constants-in-list (safe-getf (rest statement) :else)))
      statement))

    (:compute
     (let ((e (fold-literal-expression (safe-getf (rest statement) :expression)))
           (tgt (safe-getf (rest statement) :target)))
       (if (integerp e)
           (%append-statement-source-location (list :move :to tgt :from e) statement)
           (%append-statement-source-location (list :compute :target tgt :expression e) statement))))
    
    (:set
     (let ((pl (copy-list (rest statement))))
       (when (getf pl :value)
         (setf (getf pl :value) (fold-literal-expression (getf pl :value))))
       ;; copy-list preserves :source-line and other keys from STATEMENT
       (cons :set pl)))
    
    (:move
     (let ((f (fold-literal-expression (safe-getf (rest statement) :from)))
           (to (safe-getf (rest statement) :to)))
       (%append-statement-source-location (list :move :to to :from f) statement)))

    (:comment
      (let ((expression (second statement)))
        (cond
          ((and (listp expression) (eql :comment (first expression)))
           (list :comment (format nil "~{~a~^~%~}" (flatten (rest expression)))))
          ((and (listp expression) (= 1 (length expression)))
           (list :comment (first expression)))
          (t statement))))
    
    (t statement)))

(defun fold-constants-in-list (statements)
  (mapcar (lambda (s) (fold-constants-in-statement s)) (ensure-list statements)))

;;; Tail call detection

(defun %maybe-annotate-tail-call-before-return-statement (statements)
  "If STATEMENTS is @code{… INVOKE/CALL GOBACK} (or EXIT* / STOP RUN), set @code{:tail-call-p t}
on the call so @code{eliminate-unreachable-in-list} drops the redundant return.
Otherwise return STATEMENTS unchanged."
  (let ((lst (ensure-list statements)))
    (if (>= (length lst) 2)
        (let* ((l2 (last lst 2))
               (pen (first l2))
               (fin (second l2)))
          (if (and (listp pen) (listp fin)
                   (member (first pen) '(:invoke :call))
                   (member (first fin) '(:goback :exit-method :exit-program :exit :stop-run))
                   (or (and (eq (first pen) :invoke)
                            (null (safe-getf (rest pen) :returning)))
                       (and (eq (first pen) :call)
                            (null (safe-getf (rest pen) :service))
                            (null (safe-getf (rest pen) :bank)))))
              (append (butlast lst 2)
                      (list (list* (first pen) (append (rest pen) (list :tail-call-p t)))
                            fin))
              lst))
        lst)))

(defun annotate-tail-calls-in-list (statements &key (allow-singleton-tail t))
  "Mark @code{INVOKE} as @code{:tail-call-p t} when it is the last statement (or sole
   statement) and has no @code{:returning} — 6502 backend may emit @code{jmp} for tail dispatch.
   @code{CALL} is not auto-marked here: procedure calls must emit @code{jsr} unless a true
   tail position is established by @code{%maybe-annotate-tail-call-before-return-statement}
   (@code{CALL … GOBACK}), which sets @code{:tail-call-p} so @code{jmp} is allowed.
   When @code{ALLOW-SINGLETON-TAIL} is true (default), a sole @code{INVOKE} in the list is
   annotated as tail. When false, singletons are not tail-marked (used when recursing on a
   prefix so a non-final @code{INVOKE} does not emit @code{jmp}).
   @code{(INVOKE … GOBACK)} is normalized first by @code{%maybe-annotate-tail-call-before-return-statement}
   so the invoke is tail-marked and the following return is eliminated."
  (let ((lst (%maybe-annotate-tail-call-before-return-statement (ensure-list statements))))
    (if (null lst)
        lst
        (if (null (cdr lst))
            (let ((only (first lst)))
              (if (and allow-singleton-tail
                       (listp only)
                       (eq (first only) :invoke)
                       (null (safe-getf (rest only) :returning)))
                  (list (list* (first only) (append (rest only) (list :tail-call-p t))))
                  lst))
            (let ((last (first (last lst))))
              (cond
                ((and (listp last) (eq (first last) :if))
                 (append (annotate-tail-calls-in-list (butlast lst) :allow-singleton-tail nil)
                         (list (list :if
                                     :condition (safe-getf (rest last) :condition)
                                     :then (annotate-tail-calls-in-list (safe-getf (rest last) :then))
                                     :else (annotate-tail-calls-in-list (safe-getf (rest last) :else))))))
                ((and (listp last)
                      (eq (first last) :invoke)
                      (null (safe-getf (rest last) :returning)))
                 (append (annotate-tail-calls-in-list (butlast lst) :allow-singleton-tail nil)
                         (list (list* (first last) (append (rest last) (list :tail-call-p t))))))
                (t
                 (append (annotate-tail-calls-in-list (butlast lst) :allow-singleton-tail nil)
                         (list last)))))))))

;;; Unreachable code elimination

(defun eliminate-unreachable-in-list (statements)
  "Remove statements after the first terminal in STATEMENTS.
   Recursively process :if branches."
  (let ((result '())
        (done nil))
    (cl:dolist (s (ensure-list statements))
      (when done (return))
      (cond
        ((terminal-statement-p s)
         (push s result)
         (setf done t))
        ((and (listp s) (eq (first s) :if))
         (push (%append-statement-source-location
                (list :if
                      :condition (safe-getf (rest s) :condition)
                      :then (eliminate-unreachable-in-list (safe-getf (rest s) :then))
                      :else (eliminate-unreachable-in-list (safe-getf (rest s) :else)))
                s)
               result))
        (t (push s result))))
    (nreverse result)))

(defun eliminate-unreachable-in-method-body (statements)
  "Like @code{eliminate-unreachable-in-list} but respects COBOL @code{:paragraph} boundaries.
A top-level @code{GOBACK} must not drop following paragraphs — they are @code{PERFORM} entry points."
  (let ((lst (ensure-list statements))
        (acc '()))
    (flet ((pop-run ()
             (loop until (or (null lst)
                             (and (listp (first lst)) (eq (first (first lst)) :paragraph)))
                   collect (pop lst))))
      ;; Preamble before first paragraph
      (setf acc (nconc acc (eliminate-unreachable-in-list (pop-run))))
      ;; Each :paragraph plus its run until the next paragraph
      (loop while lst
            do (let ((p (pop lst)))
                 (unless (and (listp p) (eq (first p) :paragraph))
                   (error "EIGHTBOL optimize: expected :paragraph, got ~s" p))
                 (setf acc (nconc acc (list p)))
                 (setf acc (nconc acc (eliminate-unreachable-in-list (pop-run))))))
      acc)))

;;; Dead store elimination

(defun dead-store-elim-in-flat-block (statements)
  "Within a flat list (no nested :if/:perform), remove stores that are
   overwritten before being read. Returns new list."
  (let ((kept '())
        (lst (ensure-list statements))
        (n (length (ensure-list statements))))
    (loop for i from 0 below n
          for statement = (nth i lst)
          for writes = (statement-writes-to statement)
          do
             (when writes
               ;; Check if any write is dead: overwritten before read.
               ;; For each write loc, scan forward: read first → live; write first → dead.
               (let ((live-writes (remove-if
                                   (lambda (wloc)
                                     (loop for j from (1+ i) below n
                                           for s = (nth j lst)
                                           for r2 = (statement-reads-from s)
                                           for w2 = (statement-writes-to s)
                                           do
                                              (when (some (lambda (r) (locations-may-alias-p wloc r)) r2)
                                                (return nil))  ; read first → live
                                              (when (some (lambda (w) (locations-may-alias-p wloc w)) w2)
                                                (return t))    ; overwritten before read → dead
                                              ;; Never read in-method: still live — slot writes are
                                              ;; observable (other methods, hardware); not pure DCE.
                                           finally (return nil)))
                                   writes)))
                 (when (null live-writes)
                   ;; All writes dead: skip this statement entirely
                   (return-from dead-store-elim-in-flat-block
                     (append (nreverse kept)
                             (dead-store-elim-in-flat-block (subseq lst (1+ i))))))))
             (push statement kept))
    (nreverse kept)))

(defun eliminate-dead-stores-in-list (statements)
  "Remove dead stores. Recursively processes :if branches.
   Splits basic blocks at :if, :perform, :goto, :call, :invoke."
  (let ((block '())
        (result '()))
    (flet ((flush-block ()
             (when block
               (setf result (nconc result (dead-store-elim-in-flat-block (nreverse block))))
               (setf block '()))))
      (mapc (lambda (s)
              (cond
                ((and (listp s) (eq (first s) :if))
                 (flush-block)
                 (setf result (nconc result
                                     (list (%append-statement-source-location
                                            (list :if
                                                  :condition (safe-getf (rest s) :condition)
                                                  :then (eliminate-dead-stores-in-list (safe-getf (rest s) :then))
                                                  :else (eliminate-dead-stores-in-list (safe-getf (rest s) :else)))
                                            s)))))
                ((or (control-flow-statement-p s) (terminal-statement-p s))
                 (push s block)
                 (flush-block))
                (t (push s block))))
            (ensure-list statements))
      (flush-block))
    result))


;;; Common Subexpression Elimination (CSE - Stub)
;;;
;;; This is a placeholder CSE implementation. Full CSE optimization will be added 
;;; in a future pass to identify identical subexpressions appearing 2+ times
;;; and factor them into temporaries to avoid recomputation.

(defun %cse-process-statements (statements)
  "Apply common subexpression elimination to statement list.
   Currently a stub that returns statements unchanged - full CSE to be implemented."
  statements)

;;; Strength Reduction Optimizer
;;;
;;; Transforms expensive operations to cheaper ones:
;;; - Multiplication by power of 2 → left shift
;;; - Division by power of 2 → right shift (unsigned only)
;;; - Modulo by power of 2 → bitwise AND
;;; - Multiplication by 0 or 1 → identity
;;; - Addition/subtraction by 0 → identity
;;; - Bitwise AND/OR with constants → identity/simplification

(defun is-power-of-two-p (n)
  "True if N is a positive power of 2 (1, 2, 4, 8, 16, ...)."
  (and (integerp n) (plusp n) (zerop (logand n (- n 1)))))

(defun shift-count-for-power-of-two (n)
  "Return the shift count for power-of-two N. E.g., 2→1, 4→2, 8→3."
  (when (is-power-of-two-p n)
    (1- (integer-length n))))

(defun strength-reduce-expression (expression)
  "Apply strength reduction to EXPRESSION.
   Returns transformed expression or EXPRESSION if no reduction applies."
  (unless (listp expression)
    (return-from strength-reduce-expression expression))
  
  (case (first expression)
    
    ;; Multiplication: (:×-expr X Y)
    (:×-expr
     (let ((a (strength-reduce-expression (second expression)))
           (b (strength-reduce-expression (third expression))))
       ;; Multiply by 0 → 0
       (cond ((or (eql a 0) (eql b 0)) 0)
             ;; Multiply by 1 → operand
             ((eql a 1) b)
             ((eql b 1) a)
             ;; Multiply by power of 2 → shift left
             ((is-power-of-two-p b)
              (let ((shift (shift-count-for-power-of-two b)))
                (list :ash a shift)))
             ((is-power-of-two-p a)
              (let ((shift (shift-count-for-power-of-two a)))
                (list :ash b shift)))
             (t (list :×-expr a b)))))
    
    ;; Division: (:÷-expr X Y)
    (:÷-expr
     (let ((a (strength-reduce-expression (second expression)))
           (b (strength-reduce-expression (third expression))))
       ;; Divide by 1 → operand
       (cond ((eql b 1) a)
             ;; Divide by power of 2 (unsigned) → shift right
             ((is-power-of-two-p b)
              (let ((shift (shift-count-for-power-of-two b)))
                (list :ash a shift)))
             (t (list :÷-expr a b)))))
    
    ;; Addition: (:+expr X Y)
    (:+expr
     (let ((a (strength-reduce-expression (second expression)))
           (b (strength-reduce-expression (third expression))))
       ;; Add 0 → operand
       (cond ((eql b 0) a)
             ((eql a 0) b)
             ;; Convert +(-k) to -(k)
             ((and (numberp b) (minusp b))
              (list :-expr a (- b)))
             (t (list :+expr a b)))))
    
    ;; Subtraction: (:-expr X Y)
    (:-expr
     (let ((a (strength-reduce-expression (second expression)))
           (b (strength-reduce-expression (third expression))))
       ;; Subtract 0 → operand
       (cond ((eql b 0) a)
             ;; Subtract -k → add k
             ((and (numberp b) (minusp b))
              (list :+expr a (- b)))
             (t (list :-expr a b)))))
    
    ;; Modulo: (:modulo-expr X Y)
    (:modulo-expr
     (let ((a (strength-reduce-expression (second expression)))
           (b (strength-reduce-expression (third expression))))
       ;; Modulo by power of 2 → bitwise AND with (power-1)
       (cond ((is-power-of-two-p b)
              (let ((mask (- b 1)))
                (list :∧ a mask)))
             (t (list :modulo-expr a b)))))
    
    ;; Bitwise AND: (:∧ X Y)
    (:∧
     (let ((a (strength-reduce-expression (second expression)))
           (b (strength-reduce-expression (third expression))))
       ;; AND with 0 → 0
       (cond ((or (eql a 0) (eql b 0)) 0)
             ;; AND with -1 (all bits set) → operand (if we know width)
             (t (list :∧ a b)))))
    
    ;; Bitwise OR: (:∨ X Y)
    (:∨
     (let ((a (strength-reduce-expression (second expression)))
           (b (strength-reduce-expression (third expression))))
       ;; OR with 0 → operand
       (cond ((eql b 0) a)
             ((eql a 0) b)
             ;; OR with -1 (all bits set) → -1 (if we know width)
             (t (list :∨ a b)))))
    
    ;; Bitwise XOR: (:⊻ X Y)
    (:⊻
     (let ((a (strength-reduce-expression (second expression)))
           (b (strength-reduce-expression (third expression))))
       ;; XOR with 0 → operand
       (cond ((eql b 0) a)
             ((eql a 0) b)
             ;; XOR with self → 0 (but requires knowing if same variable)
             (t (list :⊻ a b)))))
    
    ;; Bitwise NOT: (:¬ X)
    (:¬
     (let ((a (strength-reduce-expression (second expression))))
       (list :¬ a)))
    
    ;; Shift left: (:ash X N)
    (:ash
     (let ((a (strength-reduce-expression (second expression)))
           (n (third expression)))
       (cond ((eql n 0) a)
             (t (list :ash a n)))))
    
    ;; Shift right: (:ash X N)
    (:ash
     (let ((a (strength-reduce-expression (second expression)))
           (n (third expression)))
       (cond ((eql n 0) a)
             (t (list :ash a n)))))
    
    ;; For other expressions, recurse on sublists
    (otherwise
     (mapcar #'strength-reduce-expression expression))))

(defun strength-reduce-in-statement (statement)
  "Apply strength reduction to all expressions in STATEMENT."
  (unless (listp statement)
    (return-from strength-reduce-in-statement statement))
  
  (case (first statement)
    
    ;; :compute :target id :expression expr
    (:compute
     (list :compute
           :target (safe-getf (rest statement) :target)
           :expression (strength-reduce-expression (safe-getf (rest statement) :expression))))
    
    ;; :move :from expr :to id
    (:move
     (list :move
           :from (strength-reduce-expression (safe-getf (rest statement) :from))
           :to (safe-getf (rest statement) :to)))
    
    ;; :+ :from expr :to (expr|id) [:giving id]
    (:+
     (let ((from (safe-getf (rest statement) :from))
           (to (safe-getf (rest statement) :to))
           (giving (safe-getf (rest statement) :giving)))
       (list* :+
              :from (strength-reduce-expression from)
              :to (if (stringp to) to (strength-reduce-expression to))
              (when giving (list :giving giving)))))
    
    ;; :- :minuend expr :subtrahend expr [:giving id]
    (:-
     (let ((minuend (safe-getf (rest statement) :minuend))
           (subtrahend (safe-getf (rest statement) :subtrahend))
           (giving (safe-getf (rest statement) :giving)))
       (list* :-
              :minuend (strength-reduce-expression minuend)
              :subtrahend (strength-reduce-expression subtrahend)
              (when giving (list :giving giving)))))
    
    ;; :× :multiplier expr :by expr [:giving id]
    (:×
     (let ((multiplier (safe-getf (rest statement) :multiplier))
           (by (safe-getf (rest statement) :by))
           (giving (safe-getf (rest statement) :giving)))
       (list* :×
              :multiplier (strength-reduce-expression multiplier)
              :by (strength-reduce-expression by)
              (when giving (list :giving giving)))))
    
    ;; :÷ :numerator expr :denominator expr [:giving id]
    (:÷
     (let ((numerator (safe-getf (rest statement) :numerator))
           (denominator (safe-getf (rest statement) :denominator))
           (giving (safe-getf (rest statement) :giving)))
       (list* :÷
              :numerator (strength-reduce-expression numerator)
              :denominator (strength-reduce-expression denominator)
              (when giving (list :giving giving)))))
    
    ;; :if :condition cond :then stmts :else stmts
    (:if
     (list :if
           :condition (strength-reduce-expression (safe-getf (rest statement) :condition))
           :then (strength-reduce-in-list (safe-getf (rest statement) :then))
           :else (strength-reduce-in-list (safe-getf (rest statement) :else))))
    
    ;; :invoke :object expr :method name [:returning id] [:using expr]
    (:invoke
     (list* :invoke
            :object (strength-reduce-expression (safe-getf (rest statement) :object))
            :method (safe-getf (rest statement) :method)
            (append (when (safe-getf (rest statement) :returning)
                      (list :returning (safe-getf (rest statement) :returning)))
                    (when (safe-getf (rest statement) :using)
                      (list :using (strength-reduce-expression (safe-getf (rest statement) :using)))))))
    
    ;; Default: return as-is
    (otherwise statement)))

(defun strength-reduce-in-list (statements)
  "Apply strength reduction to all statements in a list."
  (mapcar #'strength-reduce-in-statement (ensure-list statements)))

;;; Top-level entry point

(defun optimize-ast (ast)
  "Apply routine AST optimizations (power-of-two div/mul → shift, literal fold with
algebraic simplification, unreachable elimination, dead-store elimination, tail-call
annotation, then a second unreachable pass so redundant @code{GOBACK} after tail
@code{INVOKE}/@code{CALL} is dropped). Returns a new AST (structure is copied).

When AST is a list of program nodes (parser output: @code{((:program ...) ...)}),
map across each node. When a single @code{(:program ...)} plist, optimize directly."
  (when (and (listp ast)
             (listp (first ast))
             (every (lambda (section)
                      (and (listp section) (eq (first section) :program)))
                    ast))
    (return-from optimize-ast
      (remove-if #'null (mapcar #'optimize-ast ast))))
  (unless (and (listp ast) (eq (first ast) :program))
    (return-from optimize-ast ast))
  (let ((methods (safe-getf (rest ast) :methods))
        (class-id (safe-getf (rest ast) :class-id))
        (program-id (safe-getf (rest ast) :program-id))
        (statements (safe-getf (rest ast) :statements))
        (data (safe-getf (rest ast) :data))
        (identification (safe-getf (rest ast) :identification))
        (environment (safe-getf (rest ast) :environment)))
    (list :program
          :class-id class-id
          :program-id program-id
          :identification identification
          :environment environment
          :data data
          :statements statements
          :methods
          (mapcar (lambda (m)
                    (if (and (listp m) (eq (first m) :method))
                        (list :method
                              :method-id (safe-getf (rest m) :method-id)
                              :statements
                              (eliminate-unreachable-in-method-body
                               (annotate-tail-calls-in-list
                                (eliminate-dead-stores-in-list
                                 (%cse-process-statements
                                  (eliminate-unreachable-in-method-body
                                   (fold-constants-in-list
                                    (rewrite-divide-multiply-in-list
                                    (safe-getf (rest m) :statements)))))))))
                        m))
                  (ensure-list methods)))))
