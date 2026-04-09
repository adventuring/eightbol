;; src/ast-optimize.lisp — AST-level dead code elimination and tail-call detection
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

;;   4.  Tail-call detection:  annotates  INVOKE/CALL  in tail  position
;;      with   :tail-call-p.   (INVOKE   with   :returning   cannot   be
;;      tail-called; CALL has no return value.)
;;
;; Analysis  is   conservative:  PERFORM  and  INVOKE   are  treated  as
;; potentially reading any location;  subscripted accesses with variable
;; indices may alias.
(in-package :eightbol)

;;; Location extraction (for data-flow analysis)

(defun location-key (expr)
  "Return a comparable key for a storage location, or NIL if not a storable location.
   Locations: string (identifier), (:of slot obj), (:subscript base index).
   For (:subscript base idx), if idx is non-constant we return (subscript base) to
   indicate potential alias with any element of base."
  (cond
    ((stringp expr) (list :id expr))
    ((and (listp expr) (eq (first expr) :of))
     (list :of (second expr) (third expr)))
    ((and (listp expr) (eq (first expr) :subscript))
     (if (numberp (third expr))
         (list :subscript (second expr) (third expr))
         (list :subscript (second expr) :variable)))
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

(defun stmt-writes-to (stmt)
  "Return list of location keys that STMT writes to. NIL if none."
  (when (listp stmt)
    (let ((op (first stmt)))
      (case op
        ((:move)
         (let ((to (safe-getf (rest stmt) :to)))
           (when to (list (location-key to)))))
        ((:add)
         (append (when (safe-getf (rest stmt) :to)
                   (list (location-key (safe-getf (rest stmt) :to))))
                 (when (safe-getf (rest stmt) :giving)
                   (list (location-key (safe-getf (rest stmt) :giving))))))
        ((:subtract)
         (append (when (safe-getf (rest stmt) :from-target)
                   (list (location-key (safe-getf (rest stmt) :from-target))))
                 (when (safe-getf (rest stmt) :giving)
                   (list (location-key (safe-getf (rest stmt) :giving))))))
        ((:compute)
         (let ((tgt (safe-getf (rest stmt) :target)))
           (when tgt (list (location-key tgt)))))
        ((:set)
         (append (when (safe-getf (rest stmt) :target)
                   (list (location-key (safe-getf (rest stmt) :target))))
                 (when (safe-getf (rest stmt) :up-by)
                   (list (location-key (safe-getf (rest stmt) :up-by))))
                 (when (safe-getf (rest stmt) :down-by)
                   (list (location-key (safe-getf (rest stmt) :down-by))))
                 (when (safe-getf (rest stmt) :to-self)
                   (list (location-key (safe-getf (rest stmt) :to-self))))))
        ((:invoke)
         (let ((ret (safe-getf (rest stmt) :returning)))
           (when ret (list (location-key ret)))))
        ((:string-blt)
         (let ((dest (safe-getf (rest stmt) :dest)))
           (when dest
             (if (and (listp dest) (eq (first dest) :refmod))
                 (list (location-key (safe-getf (rest dest) :base)))
                 (list (location-key dest))))))
        ((:divide)
         (let ((giving (safe-getf (rest stmt) :giving))
               (into (safe-getf (rest stmt) :into)))
           (cond
             (giving (list (location-key giving)))
             (into (list (location-key into)))
             (t nil))))
        ((:multiply)
         (let ((giving (safe-getf (rest stmt) :giving))
               (by (safe-getf (rest stmt) :by)))
           (cond
             (giving (list (location-key giving)))
             (by (list (location-key by)))
             (t nil))))
        (t nil)))))

(defun expr-locations (expr)
  "Return list of location keys read from EXPR."
  (cond
    ((null expr) nil)
    ((stringp expr) (list (list :id expr)))
    ((numberp expr) nil)
    ((and (listp expr) (eq (first expr) :of))
     (list (location-key expr)))
    ((and (listp expr) (eq (first expr) :subscript))
     (list (location-key expr)))
    ((listp expr)
     (delete-duplicates
      (mapcan #'expr-locations (rest expr))
      :test #'equal))
    (t nil)))

(defun stmt-reads-from (stmt)
  "Return list of location keys that STMT reads from."
  (when (listp stmt)
    (let ((op (first stmt)))
      (case op
        ((:move)
         (expr-locations (safe-getf (rest stmt) :from)))
        ((:add)
         (append (expr-locations (safe-getf (rest stmt) :from))
                 (expr-locations (safe-getf (rest stmt) :to))))
        ((:subtract)
         (append (expr-locations (safe-getf (rest stmt) :from))
                 (expr-locations (safe-getf (rest stmt) :from-target))))
        ((:compute)
         (expr-locations (safe-getf (rest stmt) :expression)))
        ((:set)
         (append (expr-locations (safe-getf (rest stmt) :value))
                 (expr-locations (safe-getf (rest stmt) :by))
                 (expr-locations (safe-getf (rest stmt) :address-of))))
        ((:invoke)
         (expr-locations (safe-getf (rest stmt) :object)))
        ((:if)
         (append (expr-locations (safe-getf (rest stmt) :condition))
                 (stmt-list-reads (safe-getf (rest stmt) :then))
                 (stmt-list-reads (safe-getf (rest stmt) :else))))
        ((:perform)
         (append (expr-locations (safe-getf (rest stmt) :times))
                 (expr-locations (safe-getf (rest stmt) :until))
                 (expr-locations (safe-getf (rest stmt) :from))
                 (expr-locations (safe-getf (rest stmt) :by))))
        ((:string-blt)
         (append (expr-locations (safe-getf (rest stmt) :source))
                 (expr-locations (safe-getf (rest stmt) :dest))
                 (expr-locations (safe-getf (rest stmt) :length))))
        ((:divide)
         (append (expr-locations (safe-getf (rest stmt) :divisor))
                 (expr-locations (safe-getf (rest stmt) :into))
                 (expr-locations (safe-getf (rest stmt) :by))))
        ((:multiply)
         (append (expr-locations (safe-getf (rest stmt) :multiplier))
                 (expr-locations (safe-getf (rest stmt) :by))))
        (t nil)))))

(defun stmt-list-reads (stmts)
  (mapcan (lambda (s) (stmt-reads-from s)) (ensure-list stmts)))

;;; Terminal / control-flow

(defun terminal-statement-p (stmt)
  "True if STMT unconditionally exits the current flow (no successor).
Includes @code{:call} only when @code{:tail-call-p} was set by
@code{%maybe-annotate-tail-call-before-return-stmt} (@code{CALL … GOBACK}), so @code{GOBACK}
after a tail @code{jmp} is removed by a second unreachable pass.
@code{INVOKE} is never terminal here: it expands to @code{.CallMethod} and returns, so a
following @code{GOBACK} may remain (or a trailing @code{rts} is emitted)."
  (and (listp stmt)
       (or (member (first stmt) '(:goback :exit-method :exit-program :exit :stop-run))
           (and (eq (first stmt) :call)
                (safe-getf (rest stmt) :tail-call-p)
                (null (safe-getf (rest stmt) :service))
                (null (safe-getf (rest stmt) :bank))))))

(defun control-flow-statement-p (stmt)
  "True if STMT branches or calls (perform, invoke, goto, call)."
  (and (listp stmt)
       (member (first stmt) '(:if :perform :goto :call :invoke))))

;;; Literal power-of-two (for DIVIDE/MULTIPLY → shift in AST)

(defun literal-integer-for-divmul-p (expr)
  "True if EXPR is an integer literal (:literal n or integer) for shift lowering."
  (or (integerp expr)
      (and (listp expr) (eq (first expr) :literal) (integerp (second expr)))))

(defun literal-integer-value (expr)
  "Integer value of EXPR when literal-integer-for-divmul-p."
  (if (integerp expr) expr (second expr)))

(defun power-of-two-shift-count (n)
  "Return k where n = 2^k, or NIL if N is not a positive power of two."
  (when (and (integerp n) (plusp n) (= n (ash 1 (1- (integer-length n)))))
    (1- (integer-length n))))

;;; Preserve COBOL source location through rewrites (see parser @code{stmt-with-source-location})

(defun %stmt-source-location-suffix (stmt)
  "Return plist tail fragment @code{:source-file}, @code{:source-line}, @code{:source-sequence}
from STMT, or NIL."
  (when (and (listp stmt) (rest stmt))
    (let ((p (rest stmt)))
      (when (and p (evenp (length p)))
        (append (when (safe-getf p :source-file)
                  (list :source-file (safe-getf p :source-file)))
                (when (safe-getf p :source-line)
                  (list :source-line (safe-getf p :source-line)))
                (when (safe-getf p :source-sequence)
                  (list :source-sequence (safe-getf p :source-sequence))))))))

(defun %append-stmt-source-location (head stmt)
  "Append location keys from STMT onto new statement list HEAD (HEAD is @code{(type ... keys)})."
  (append head (%stmt-source-location-suffix stmt)))

;;; DIVIDE / MULTIPLY → :compute with shift (shared by all backends)

(defun rewrite-divide-multiply-statement (stmt)
  "If STMT is DIVIDE/MULTIPLY with constant power-of-two, return equivalent :compute.
Otherwise return STMT unchanged."
  (unless (listp stmt)
    (return-from rewrite-divide-multiply-statement stmt))
  (case (first stmt)
    (:divide
     (let ((divisor (safe-getf (rest stmt) :divisor))
           (into (safe-getf (rest stmt) :into))
           (giving (safe-getf (rest stmt) :giving))
           (by (safe-getf (rest stmt) :by)))
       (unless (literal-integer-for-divmul-p divisor)
         (return-from rewrite-divide-multiply-statement stmt))
       (let ((sc (power-of-two-shift-count (literal-integer-value divisor))))
         (unless sc
           (return-from rewrite-divide-multiply-statement stmt))
         (cond
           ((and into giving)
            (%append-stmt-source-location
             (list :compute :target giving
                   :expression (list :shift-right into sc))
             stmt))
           ((and into (null giving) (null by))
            (%append-stmt-source-location
             (list :compute :target into
                   :expression (list :shift-right into sc))
             stmt))
           ((and by giving)
            (%append-stmt-source-location
             (list :compute :target giving
                   :expression (list :shift-right by sc))
             stmt))
           (t stmt)))))
    (:multiply
     (let ((mult (safe-getf (rest stmt) :multiplier))
           (by (safe-getf (rest stmt) :by))
           (giving (safe-getf (rest stmt) :giving)))
       (unless (literal-integer-for-divmul-p mult)
         (return-from rewrite-divide-multiply-statement stmt))
       (let ((sc (power-of-two-shift-count (literal-integer-value mult))))
         (unless sc
           (return-from rewrite-divide-multiply-statement stmt))
         (cond
           ((and by giving)
            (%append-stmt-source-location
             (list :compute :target giving
                   :expression (list :shift-left by sc))
             stmt))
           ((and by (null giving))
            (%append-stmt-source-location
             (list :compute :target by
                   :expression (list :shift-left by sc))
             stmt))
           (t stmt)))))
    (t stmt)))

(defun rewrite-divide-multiply-in-list (stmts)
  (mapcar (lambda (s)
            (cond
              ((and (listp s) (eq (first s) :if))
               (%append-stmt-source-location
                (list :if
                      :condition (safe-getf (rest s) :condition)
                      :then (rewrite-divide-multiply-in-list (safe-getf (rest s) :then))
                      :else (rewrite-divide-multiply-in-list (safe-getf (rest s) :else)))
                s))
              (t (rewrite-divide-multiply-statement s))))
          (ensure-list stmts)))

;;; Constant folding (literals only — no copybook symbols)

(defun %algebraic-simplify-add (a b)
  "After folding subexpressions A and B, rewrite @code{A + B} for integer constants.
@code{A + (-k)} → @code{A - k}; @code{(-k) + B} → @code{B - k}; @code{+0} identities.
Returns an integer or a @code{:add-expr}/@code{:subtract-expr} list."
  (cond
    ((and (integerp b) (minusp b)) (list :subtract-expr a (- b)))
    ((and (integerp a) (minusp a)) (list :subtract-expr b (- a)))
    ((and (integerp b) (zerop b)) a)
    ((and (integerp a) (zerop a)) b)
    (t (list :add-expr a b))))

(defun %algebraic-simplify-subtract (a b)
  "Rewrite @code{A - B}: @code{A - (-k)} → @code{A + k}; @code{A - 0} → @code{A}."
  (cond
    ((and (integerp b) (minusp b)) (list :add-expr a (- b)))
    ((and (integerp b) (zerop b)) a)
    (t (list :subtract-expr a b))))

(defun %algebraic-simplify-multiply (a b)
  "Integer identities: @code{*0}, @code{*1}; else @code{:multiply-expr}."
  (cond
    ((or (and (integerp a) (zerop a)) (and (integerp b) (zerop b))) 0)
    ((and (integerp a) (= a 1)) b)
    ((and (integerp b) (= b 1)) a)
    (t (list :multiply-expr a b))))

(defun %algebraic-simplify-divide (a b)
  "Integer @code{/1} and @code{0/n}; else @code{:divide-expr}."
  (cond
    ((and (integerp b) (= b 1)) a)
    ((and (integerp a) (zerop a) (integerp b) (not (zerop b))) 0)
    (t (list :divide-expr a b))))

(defun fold-literal-expression (expr)
  "Fold EXPR when all operands are integer or @code{:literal}; apply algebraic
simplification (sign flips, 0/1 identities) so backends see fewer operations.

@table @asis
@item EXPR
Arithmetic AST node, identifier, or integer; copybook symbols are not folded.
@end table

@subsection Outputs
Integer, simplified list, or EXPR unchanged for non-arithmetic leaves."
  (unless (listp expr)
    (return-from fold-literal-expression expr))
  ;; Normalize integer (:literal n) to N so binary ops see negative constants.
  (when (and (eq (first expr) :literal) (integerp (second expr)))
    (return-from fold-literal-expression (second expr)))
  (case (first expr)
    (:add-expr
     (let ((a (fold-literal-expression (second expr)))
           (b (fold-literal-expression (third expr))))
       (cond ((and (integerp a) (integerp b)) (+ a b))
             (t (let ((s (%algebraic-simplify-add a b)))
                  (if (and (listp s) (eq (first s) :subtract-expr))
                      (fold-literal-expression s)
                      s))))))
    (:subtract-expr
     (let ((a (fold-literal-expression (second expr)))
           (b (fold-literal-expression (third expr))))
       (cond ((and (integerp a) (integerp b)) (- a b))
             (t (let ((s (%algebraic-simplify-subtract a b)))
                  (if (and (listp s) (eq (first s) :add-expr))
                      (fold-literal-expression s)
                      s))))))
    (:multiply-expr
     (let ((a (fold-literal-expression (second expr)))
           (b (fold-literal-expression (third expr))))
       (cond ((and (integerp a) (integerp b)) (* a b))
             (t (%algebraic-simplify-multiply a b)))))
    (:divide-expr
     (let ((a (fold-literal-expression (second expr)))
           (b (fold-literal-expression (third expr))))
       (cond ((and (integerp a) (integerp b) (not (zerop b))) (floor a b))
             (t (%algebraic-simplify-divide a b)))))
    (:shift-left
     (let ((a (fold-literal-expression (second expr)))
           (n (third expr)))
       (if (and (integerp a) (integerp n))
           (ash a n)
           (list :shift-left a n))))
    (:shift-right
     (let ((a (fold-literal-expression (second expr)))
           (n (third expr)))
       (if (and (integerp a) (integerp n))
           (ash a (- n))
           (list :shift-right a n))))
    (otherwise expr)))

(defun fold-constants-in-statement (stmt)
  "Return STMT with literal sub-expressions folded where safe."
  (unless (listp stmt)
    (return-from fold-constants-in-statement stmt))
  (case (first stmt)
    (:if
     (%append-stmt-source-location
      (list :if
            :condition (fold-literal-expression (safe-getf (rest stmt) :condition))
            :then (fold-constants-in-list (safe-getf (rest stmt) :then))
            :else (fold-constants-in-list (safe-getf (rest stmt) :else)))
      stmt))
    (:compute
     (let ((e (fold-literal-expression (safe-getf (rest stmt) :expression)))
           (tgt (safe-getf (rest stmt) :target)))
       (if (integerp e)
           (%append-stmt-source-location (list :move :to tgt :from e) stmt)
           (%append-stmt-source-location (list :compute :target tgt :expression e) stmt))))
    (:set
     (let ((pl (copy-list (rest stmt))))
       (when (getf pl :value)
         (setf (getf pl :value) (fold-literal-expression (getf pl :value))))
       ;; copy-list preserves :source-line and other keys from STMT
       (cons :set pl)))
    (:move
     (let ((f (fold-literal-expression (safe-getf (rest stmt) :from)))
           (to (safe-getf (rest stmt) :to)))
       (%append-stmt-source-location (list :move :to to :from f) stmt)))
    (t stmt)))

(defun fold-constants-in-list (stmts)
  (mapcar (lambda (s) (fold-constants-in-statement s)) (ensure-list stmts)))

;;; Tail call detection

(defun %maybe-annotate-tail-call-before-return-stmt (stmts)
  "If STMTS is @code{… INVOKE/CALL GOBACK} (or EXIT* / STOP RUN), set @code{:tail-call-p t}
on the call so @code{eliminate-unreachable-in-list} drops the redundant return.
Otherwise return STMTS unchanged."
  (let ((lst (ensure-list stmts)))
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

(defun annotate-tail-calls-in-list (stmts &key (allow-singleton-tail t))
  "Mark @code{INVOKE} as @code{:tail-call-p t} when it is the last statement (or sole
   statement) and has no @code{:returning} — 6502 backend may emit @code{jmp} for tail dispatch.
   @code{CALL} is not auto-marked here: procedure calls must emit @code{jsr} unless a true
   tail position is established by @code{%maybe-annotate-tail-call-before-return-stmt}
   (@code{CALL … GOBACK}), which sets @code{:tail-call-p} so @code{jmp} is allowed.
   When @code{ALLOW-SINGLETON-TAIL} is true (default), a sole @code{INVOKE} in the list is
   annotated as tail. When false, singletons are not tail-marked (used when recursing on a
   prefix so a non-final @code{INVOKE} does not emit @code{jmp}).
   @code{(INVOKE … GOBACK)} is normalized first by @code{%maybe-annotate-tail-call-before-return-stmt}
   so the invoke is tail-marked and the following return is eliminated."
  (let ((lst (%maybe-annotate-tail-call-before-return-stmt (ensure-list stmts))))
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

(defun eliminate-unreachable-in-list (stmts)
  "Remove statements after the first terminal in STMTS.
   Recursively process :if branches."
  (let ((result '())
        (done nil))
    (cl:dolist (s (ensure-list stmts))
      (when done (return))
      (cond
        ((terminal-statement-p s)
         (push s result)
         (setf done t))
        ((and (listp s) (eq (first s) :if))
         (push (%append-stmt-source-location
                (list :if
                      :condition (safe-getf (rest s) :condition)
                      :then (eliminate-unreachable-in-list (safe-getf (rest s) :then))
                      :else (eliminate-unreachable-in-list (safe-getf (rest s) :else)))
                s)
               result))
        (t (push s result))))
    (nreverse result)))

(defun eliminate-unreachable-in-method-body (stmts)
  "Like @code{eliminate-unreachable-in-list} but respects COBOL @code{:paragraph} boundaries.
A top-level @code{GOBACK} must not drop following paragraphs — they are @code{PERFORM} entry points."
  (let ((lst (ensure-list stmts))
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

(defun dead-store-elim-in-flat-block (stmts)
  "Within a flat list (no nested :if/:perform), remove stores that are
   overwritten before being read. Returns new list."
  (let ((kept '())
        (lst (ensure-list stmts))
        (n (length (ensure-list stmts))))
    (loop for i from 0 below n
          for stmt = (nth i lst)
          for writes = (stmt-writes-to stmt)
          do
          (when writes
            ;; Check if any write is dead: overwritten before read.
            ;; For each write loc, scan forward: read first → live; write first → dead.
            (let ((live-writes (remove-if
                               (lambda (wloc)
                                 (loop for j from (1+ i) below n
                                       for s = (nth j lst)
                                       for r2 = (stmt-reads-from s)
                                       for w2 = (stmt-writes-to s)
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
          (push stmt kept))
    (nreverse kept)))

(defun eliminate-dead-stores-in-list (stmts)
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
                                   (list (%append-stmt-source-location
                                          (list :if
                                                :condition (safe-getf (rest s) :condition)
                                                :then (eliminate-dead-stores-in-list (safe-getf (rest s) :then))
                                                :else (eliminate-dead-stores-in-list (safe-getf (rest s) :else)))
                                          s)))))
                ((or (control-flow-statement-p s) (terminal-statement-p s))
                 (push s block)
                 (flush-block))
                (t (push s block))))
            (ensure-list stmts))
      (flush-block))
    result))

;;; Top-level entry point

(defun optimize-ast (ast)
  "Apply routine AST optimizations (power-of-two div/mul → shift, literal fold with
algebraic simplification, unreachable elimination, dead-store elimination, tail-call
annotation, then a second unreachable pass so redundant @code{GOBACK} after tail
@code{INVOKE}/@code{CALL} is dropped). Returns a new AST (structure is copied)."
  (unless (and (listp ast) (eq (first ast) :program))
    (return-from optimize-ast ast))
  (let ((methods (safe-getf (rest ast) :methods))
        (class-id (safe-getf (rest ast) :class-id))
        (data (safe-getf (rest ast) :data))
        (identification (safe-getf (rest ast) :identification))
        (environment (safe-getf (rest ast) :environment)))
    (list :program
          :class-id class-id
          :identification identification
          :environment environment
          :data data
          :methods
          (mapcar (lambda (m)
                    (if (and (listp m) (eq (first m) :method))
                        (list :method
                              :method-id (safe-getf (rest m) :method-id)
                              :statements
                              (eliminate-unreachable-in-method-body
                               (annotate-tail-calls-in-list
                                (eliminate-dead-stores-in-list
                                 (eliminate-unreachable-in-method-body
                                  (fold-constants-in-list
                                   (rewrite-divide-multiply-in-list
                                    (safe-getf (rest m) :statements))))))))
                        m))
                  (ensure-list methods)))))
