;; src/ast-validate.lisp — structural validation after parse / optimize-ast
;;
;; 1. OBJECT REFERENCE classes in :data must appear in DEFINED-CLASS-IDS when
;;    caller enables class validation (compile-eightbol-class :defined-class-ids).
;; 2. Each non-blank method must end in a statement that completes the routine:
;;    GOBACK, EXIT*, STOP RUN, unconditional GO TO, tail INVOKE/CALL, or IF with
;;    ELSE where every branch’s statement list completes.
(in-package :eightbol)

(defun %data-item-object-ref-class (item)
  "If ITEM is a data description list with OBJECT REFERENCE Class, return CLASS."
  (when (and (listp item) (numberp (first item)) (stringp (second item)))
    (dolist (x (cddr item))
      (when (and (listp x) (eq (first x) :usage) (eq (second x) :object-ref))
        (return-from %data-item-object-ref-class (getf (cddr x) :class))))))

(defun collect-object-reference-classes-from-data (data)
  "Walk DATA division tree; return sorted unique strings for OBJECT REFERENCE classes."
  (let ((acc nil))
    (labels ((walk (node)
               (cond
                 ((atom node) nil)
                 ((and (listp node) (numberp (first node)) (stringp (second node)))
                  (let ((c (%data-item-object-ref-class node)))
                    (when (and c (stringp c))
                      (pushnew c acc :test #'string-equal))))
                 (t (dolist (c node) (walk c))))))
      (walk data)
      (sort acc #'string-lessp))))

(defun collect-object-reference-classes-from-ast (ast)
  "Return classes referenced in WORKING-STORAGE from AST (:program node)."
  (unless (and (listp ast) (eq (first ast) :program))
    (return-from collect-object-reference-classes-from-ast nil))
  (let ((data (ast-data ast)))
    (when (and (listp data) (>= (length data) 3))
      (collect-object-reference-classes-from-data (subseq data 3)))))

(defun validate-object-reference-classes (ast defined-class-ids)
  "Signal UNDEFINED-CLASS-REFERENCE if any OBJECT REFERENCE in AST :data is not listed in DEFINED-CLASS-IDS (strings compared with STRING-EQUAL)."
  (when (and defined-class-ids (listp ast))
    (let ((need (collect-object-reference-classes-from-ast ast))
          (known (mapcar (lambda (x) (if (stringp x) x (princ-to-string x)))
                         defined-class-ids)))
      (dolist (c need)
        (unless (member c known :test #'string-equal)
          (error 'undefined-class-reference
                 :message (format nil "Class ~s not defined" c)
                 :class-name c
                 :defined-set known))))))

(defun stmt-completes-method-p (stmt)
  "True if STMT ends the method without fall-through (return, jump, tail call, etc.)."
  (when (and (listp stmt) (first stmt))
    (case (first stmt)
      ((:goback :exit-method :exit-program :exit :stop-run) t)
      (:goto t)
      ((:invoke)
       (and (getf (rest stmt) :tail-call-p)
            (null (getf (rest stmt) :returning))))
      ((:call)
       (getf (rest stmt) :tail-call-p))
      ((:if)
       (let ((then-st (safe-getf (rest stmt) :then))
             (else-st (safe-getf (rest stmt) :else)))
         (and (statement-list-completes-method-p (ensure-list then-st))
              (if (and else-st (not (null else-st)))
                  (statement-list-completes-method-p (ensure-list else-st))
                  nil))))
      ((:evaluate)
       (evaluate-completes-method-p stmt))
      (t nil))))

(defun evaluate-completes-method-p (stmt)
  "True if EVALUATE has WHEN OTHER and every branch’s imperative list completes."
  (let ((clauses (ensure-list (safe-getf (rest stmt) :when-clauses))))
    (and (find :when-other clauses :key (lambda (x) (when (listp x) (first x))))
         (every (lambda (cl)
                  (case (first cl)
                    (:when-other
                     (statement-list-completes-method-p (ensure-list (second cl))))
                    (:when
                     (statement-list-completes-method-p
                      (ensure-list (third cl))))
                    (t nil)))
                clauses))))

(defun statement-list-completes-method-p (stmts)
  "True if non-empty STMTS ends with a completing statement."
  (let ((lst (ensure-list stmts)))
    (and lst
         (stmt-completes-method-p (car (last lst))))))

(defun method-blank-ast-p (method-node)
  "True if METHOD node has no statements (blank method / shared stub)."
  (null (ensure-list (ast-method-statements method-node))))

(defun validate-method-terminations (ast)
  "Signal ROUTINE-NOT-TERMINATED for any non-blank method that can fall through past its last statement."
  (unless (and (listp ast) (eq (first ast) :program))
    (return-from validate-method-terminations nil))
  (dolist (m (ensure-list (ast-methods ast)))
    (when (and (listp m) (eq (first m) :method))
      (unless (method-blank-ast-p m)
        (unless (statement-list-completes-method-p (ast-method-statements m))
          (error 'routine-not-terminated
                 :message (format nil "Method ~s (class ~s) lacks terminating GOBACK/EXIT/STOP/jump/tail-call"
                                  (ast-method-name m)
                                  (ast-class-id ast))
                 :method-id (ast-method-name m)))))))

(defun validate-eightbol-program (ast &key defined-class-ids (validate-termination t))
  "Run validations on AST (typically after OPTIMIZE-AST). When DEFINED-CLASS-IDS is non-NIL, check OBJECT REFERENCE classes in :data. When VALIDATE-TERMINATION, check each method ends properly."
  (when defined-class-ids
    (validate-object-reference-classes ast defined-class-ids))
  (when validate-termination
    (validate-method-terminations ast))
  ast)
