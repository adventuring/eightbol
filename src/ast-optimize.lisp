;; src/ast-optimize.lisp — AST-level dead code elimination and tail-call detection
;;
;; Performs:
;;   1. Unreachable code elimination: removes statements after terminal
;;      control flow (:goback, :exit-method, :exit-program, :exit, :stop-run).
;;   2. Dead store elimination: removes writes overwritten before being read.
;;   3. Tail-call detection: annotates INVOKE/CALL in tail position with :tail-call-p.
;;      (INVOKE with :returning cannot be tail-called; CALL has no return value.)
;;
;; Analysis is conservative: PERFORM and INVOKE are treated as potentially
;; reading any location; subscripted accesses with variable indices may alias.
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
         (let ((to (getf (rest stmt) :to)))
           (when to (list (location-key to)))))
        ((:add)
         (append (when (getf (rest stmt) :to)
                   (list (location-key (getf (rest stmt) :to))))
                 (when (getf (rest stmt) :giving)
                   (list (location-key (getf (rest stmt) :giving))))))
        ((:subtract)
         (append (when (getf (rest stmt) :from-target)
                   (list (location-key (getf (rest stmt) :from-target))))
                 (when (getf (rest stmt) :giving)
                   (list (location-key (getf (rest stmt) :giving))))))
        ((:compute :set)
         (let ((tgt (getf (rest stmt) :target)))
           (when tgt (list (location-key tgt)))))
        ((:invoke)
         (let ((ret (getf (rest stmt) :returning)))
           (when ret (list (location-key ret)))))
        ((:string-blt)
         (let ((dest (getf (rest stmt) :dest)))
           (when dest
             (if (and (listp dest) (eq (first dest) :refmod))
                 (list (location-key (getf (rest dest) :base)))
                 (list (location-key dest))))))
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
         (expr-locations (getf (rest stmt) :from)))
        ((:add)
         (append (expr-locations (getf (rest stmt) :from))
                 (expr-locations (getf (rest stmt) :to))))
        ((:subtract)
         (append (expr-locations (getf (rest stmt) :from))
                 (expr-locations (getf (rest stmt) :from-target))))
        ((:compute)
         (expr-locations (getf (rest stmt) :expression)))
        ((:set)
         (expr-locations (getf (rest stmt) :value)))
        ((:invoke)
         (expr-locations (getf (rest stmt) :object)))
        ((:if)
         (append (expr-locations (getf (rest stmt) :condition))
                 (stmt-list-reads (getf (rest stmt) :then))
                 (stmt-list-reads (getf (rest stmt) :else))))
        ((:perform)
         (append (expr-locations (getf (rest stmt) :times))
                 (expr-locations (getf (rest stmt) :until))
                 (expr-locations (getf (rest stmt) :from))
                 (expr-locations (getf (rest stmt) :by))))
        ((:string-blt)
         (append (expr-locations (getf (rest stmt) :source))
                 (expr-locations (getf (rest stmt) :dest))
                 (expr-locations (getf (rest stmt) :length))))
        (t nil)))))

(defun stmt-list-reads (stmts)
  (mapcan (lambda (s) (stmt-reads-from s)) (ensure-list stmts)))

;;; Terminal / control-flow

(defun terminal-statement-p (stmt)
  "True if STMT unconditionally exits the current flow (no successor)."
  (and (listp stmt)
       (member (first stmt) '(:goback :exit-method :exit-program :exit :stop-run))))

(defun control-flow-statement-p (stmt)
  "True if STMT branches or calls (perform, invoke, goto, call)."
  (and (listp stmt)
       (member (first stmt) '(:if :perform :goto :call :invoke))))

;;; Tail call detection

(defun annotate-tail-calls-in-list (stmts)
  "Mark INVOKE/CALL as :tail-call-p t when they are the last statement before return.
   INVOKE with :returning cannot be tail-called (result needed)."
  (let ((lst (ensure-list stmts)))
    (if (null lst)
        '()
        (let ((last (first (last lst))))
          (cond
            ((and (listp last) (eq (first last) :if))
             (append (annotate-tail-calls-in-list (butlast lst))
                     (list (list :if
                                 :condition (getf (rest last) :condition)
                                 :then (annotate-tail-calls-in-list (getf (rest last) :then))
                                 :else (annotate-tail-calls-in-list (getf (rest last) :else))))))
            ((and (listp last)
                  (member (first last) '(:invoke :call))
                  ;; INVOKE with :returning needs the result — not a tail call
                  (or (eq (first last) :call)
                      (null (getf (rest last) :returning))))
             (append (annotate-tail-calls-in-list (butlast lst))
                     (list (list* (first last) (append (rest last) (list :tail-call-p t))))))
            (t
             (append (annotate-tail-calls-in-list (butlast lst)) (list last))))))))

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
         (push (list :if
                     :condition (getf (rest s) :condition)
                     :then (eliminate-unreachable-in-list (getf (rest s) :then))
                     :else (eliminate-unreachable-in-list (getf (rest s) :else)))
               result))
        (t (push s result))))
    (nreverse result)))

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
                                       finally (return t)))  ; never read → dead
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
                                   (list (list :if
                                              :condition (getf (rest s) :condition)
                                              :then (eliminate-dead-stores-in-list (getf (rest s) :then))
                                              :else (eliminate-dead-stores-in-list (getf (rest s) :else)))))))
                ((or (control-flow-statement-p s) (terminal-statement-p s))
                 (push s block)
                 (flush-block))
                (t (push s block))))
            (ensure-list stmts))
      (flush-block))
    result))

;;; Top-level entry point

(defun optimize-ast (ast)
  "Apply dead code elimination to AST. Returns a new AST (structure is copied)."
  (unless (and (listp ast) (eq (first ast) :program))
    (return-from optimize-ast ast))
  (let ((methods (getf (rest ast) :methods))
        (class-id (getf (rest ast) :class-id))
        (data (getf (rest ast) :data))
        (identification (getf (rest ast) :identification))
        (environment (getf (rest ast) :environment)))
    (list :program
          :class-id class-id
          :identification identification
          :environment environment
          :data data
          :methods
          (mapcar (lambda (m)
                    (if (and (listp m) (eq (first m) :method))
                        (list :method
                              :method-id (getf (rest m) :method-id)
                              :statements
                              (annotate-tail-calls-in-list
                               (eliminate-dead-stores-in-list
                                (eliminate-unreachable-in-list
                                 (getf (rest m) :statements)))))
                        m))
                  (ensure-list methods)))))
