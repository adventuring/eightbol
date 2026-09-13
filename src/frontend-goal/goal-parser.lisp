;; src/frontend-goal/goal-parser.lisp — Parsing for GOAL → canonical EIGHTBOL AST
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; AST node constructors with kebab-case identifier normalization

(defun goal-atom-node (value type)
  "Create an atom/number literal (value is self-evaluating in GOAL)."
  value)

(defun goal-ident-node (name)
  "Create a GOAL identifier reference with kebab-case normalization."
  (goal-normalize-identifier name))

(defun goal-list-node (elements)
  "Express list as canonical form."
  elements)

(defun goal-defun-node (name params body)
  "Create a GOAL function definition → canonical :method node."
  (make-method-node (goal-normalize-identifier name)
                    :statements (mapcan (lambda (form) (goal-form-to-statements form)) body)))

(defun goal-defmethod-node (class method-name params body)
  "Create a GOAL method definition → canonical :method node."
  (make-method-node (format nil "~a-~a"
                           (goal-normalize-identifier class)
                           (goal-normalize-identifier method-name))
                    :statements (mapcan (lambda (form) (goal-form-to-statements form)) body)))

(defun goal-let-node (bindings body)
  "Convert let to SET statements followed by body."
  (loop for (var val) in bindings
        collect (make-set-node (goal-normalize-identifier var) val)
        finally (return (mapcan (lambda (form) (goal-form-to-statements form)) body))))

(defun goal-if-node (test then else)
  "Create GOAL if-expression → canonical :if node."
  (make-if-node (goal-form-to-expr test)
                (mapcan (lambda (form) (goal-form-to-statements form)) (ensure-list then))
                (mapcan (lambda (form) (goal-form-to-statements form)) (ensure-list else))))

(defun goal-quote-node (expr)
  "Quote node — kept as-is for Lisp compatibility."
  (list :quote expr))

(defun goal-parse-number (token)
  "Parse a number token and return the numeric value."
  (parse-goal-number (cdr token)))

(defun goal-form-to-statements (form)
  "Convert a GOAL form to a list of canonical AST statement nodes."
  (cond
    ((null form) '())
    ((numberp form) '())
    ((stringp form) '())
    ((symbolp form) '())
    ((listp form)
     (let ((op (car form)))
       (case op
         ((:defun DEFFUN)
          (list (goal-defun-node (cadr form) (caddr form) (cdddr form))))
         ((:defmethod DEFMETHOD)
          (list (goal-defmethod-node (cadr form) (caddr form) (cadddr form) (cddddr form))))
         ((:let LET)
          (let ((bindings (cadr form))
                (body (cddr form)))
            (goal-let-node bindings body)))
         ((:let* LETSTAR)
          (goal-let-node (cadr form) (cddr form)))
         ((:if IF)
          (list (goal-if-node (cadr form) (caddr form) (cadddr form))))
         ((:when WHEN)
          (list (goal-if-node (cadr form) (cddr form) '())))
         ((:unless UNLESS)
          (list (goal-if-node (list :not (cadr form)) (cddr form) '())))
         ((:set SET!)
          (list (make-set-node (goal-normalize-identifier (cadr form))
                               (goal-form-to-expr (caddr form)))))
         ((:setq SETQ)
          (list (make-set-node (goal-normalize-identifier (cadr form))
                               (goal-form-to-expr (caddr form)))))
         ((:return RETURN)
          (if (cadr form)
              (list (list :goback :value (goal-form-to-expr (cadr form))))
              (list (list :goback))))
         ((:progn PROGN)
          (mapcan #'goal-form-to-statements (cdr form)))
         ((:quote QUOTE)
          '())
         (t
          (list (make-call-node (goal-normalize-identifier op)
                                :args (mapcar #'goal-form-to-expr (cdr form))))))))
    (t (list form))))

(defun goal-form-to-expr (form)
  "Convert a GOAL form to a canonical expression value."
  (cond
    ((null form) '(:null))
    ((numberp form) form)
    ((stringp form) form)
    ((symbolp form)
     (if (member form '(t T :true TRUE))
         1
         (goal-normalize-identifier form)))
    ((listp form)
     (let ((op (car form)))
       (case op
         ((:quote QUOTE) (cadr form))
         ((:if IF) (goal-form-to-expr (cadr form)))
         ((:when WHEN) (goal-form-to-expr (cadr form)))
         ((:unless UNLESS) (list :not (goal-form-to-expr (cadr form))))
         ((:+ + ADD)
          (list :+ (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
         ((:- - SUBTRACT)
          (list :- (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
         ((:× * MULTIPLY)
          (list :× (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
         ((:÷ / DIVIDE)
          (list :÷ (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
         ((:mod MOD)
          (list :mod (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
         ((:eq = EQUAL)
          (list := (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
         ((:ne != NOT-EQUAL)
          (list :≠ (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
         ((:lt < LESS-THAN)
          (list :< (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
         ((:gt > GREATER-THAN)
          (list :> (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
         ((:le <= LESS-EQUAL)
          (list :≤ (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
         ((:ge >= GREATER-EQUAL)
          (list :≥ (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
         ((:and AND)
          (list :and (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
         ((:or OR)
          (list :or (goal-form-to-expr (cadr form)) (goal-form-to-expr (caddr form))))
         ((:not NOT)
          (list :not (goal-form-to-expr (cadr form))))
         (t
          (list :call (goal-normalize-identifier op)
                :args (mapcar #'goal-form-to-expr (cdr form)))))))
    (t form)))

(defun goal-parse-toplevel (tokens)
  "Parse top-level GOAL forms into canonical EIGHTBOL AST.
Returns a list of AST nodes."
  (let ((forms '())
        (remaining tokens))
    (loop while remaining
          do (multiple-value-bind (form rest) (goal-parse-form remaining)
               (push form forms)
               (setf remaining rest)))
    (nreverse forms)))

(defun goal-parse-form (tokens)
  "Parse a single GOAL form from token stream.
Returns (values form-node remaining-tokens)."
  (unless tokens
    (error "Unexpected end of input"))
  (let ((token (first tokens))
        (rest (rest tokens)))
    (case (car token)
      (:lparen
       (goal-parse-sexp rest))
      (:number
       (values (goal-parse-number token) rest))
      (:dword
       (values (list :dword (cdr token)) rest))
      (:string
       (values (cdr token) rest))
      (:ident
       (values (goal-ident-node (cdr token)) rest))
      (:nil
       (values '(:null) rest))
      (:true
       (values 1 rest))
      (:false
       (values 0 rest))
      (t
       (error "Unexpected token: ~s" token)))))

(defun goal-parse-sexp (tokens)
  "Parse an S-expression (list) starting after lparen.
Returns (values list-node remaining-tokens)."
  (unless tokens
    (error "Unexpected end of input in S-expression"))
  (if (eq (caar tokens) :rparen)
      (values '() (rest tokens))
      (let ((elements '())
            (remaining tokens))
        (loop
          (multiple-value-bind (elem rest-tokens) (goal-parse-form remaining)
            (push elem elements)
            (setf remaining rest-tokens)
            (unless remaining
              (error "Unexpected end of input in S-expression"))
            (if (eq (caar remaining) :rparen)
                (return))))
        (values (nreverse elements) (rest remaining)))))

(defun goal-parse-source (source)
  "Parse complete GOAL SOURCE string into canonical EIGHTBOL AST.
Returns (:program ...) with :method nodes for defun/defmethod."
  (let* ((tokens (goal-lex-source source))
         (forms (goal-parse-toplevel tokens))
         (methods '())
         (toplevel-stmts '()))
    (dolist (form forms)
      (cond
        ((and (listp form)
              (member (car form) '(:defun DEFFUN)))
         (push (goal-defun-node (cadr form) (caddr form) (cdddr form)) methods))
        ((and (listp form)
              (member (car form) '(:defmethod DEFMETHOD)))
         (push (goal-defmethod-node (cadr form) (caddr form) (cadddr form) (cddddr form)) methods))
        (t
         (setf toplevel-stmts (append toplevel-stmts (goal-form-to-statements form))))))
    (make-program-node "GoalProgram"
                       :methods (nconc (when toplevel-stmts
                                        (list (make-method-node "TopLevel"
                                                                :statements toplevel-stmts)))
                                      (nreverse methods)))))
