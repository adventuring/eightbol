;; src/frontend-goal/goal-parser.lisp — Parsing for GOAL
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; AST node constructors with kebab-case identifier normalization
(defun goal-atom-node (value type)
  "Create a GOAL atom AST node."
  (list :atom :value value :type type))

(defun goal-ident-node (name)
  "Create a GOAL identifier node with kebab-case normalization."
  (let ((normalized (goal-normalize-identifier name)))
    (list :ident :name normalized)))

(defun goal-list-node (elements)
  "Create a GOAL list/S-expression node."
  (list :list :elements elements))

(defun goal-defun-node (name params body)
  "Create a GOAL function definition node."
  (list :defun
        :name (goal-normalize-identifier name)
        :params (mapcar #'goal-normalize-identifier params)
        :body body))

(defun goal-defmethod-node (class method-name params body)
  "Create a GOAL method definition node."
  (list :defmethod
        :class (goal-normalize-identifier class)
        :name (goal-normalize-identifier method-name)
        :params (mapcar #'goal-normalize-identifier params)
        :body body))

(defun goal-let-node (bindings body)
  "Create a GOAL let-binding node."
  (let ((normalized-bindings
          (mapcar (lambda (b)
                    (list (goal-normalize-identifier (car b)) (cadr b)))
                  bindings)))
    (list :let :bindings normalized-bindings :body body)))

(defun goal-if-node (test then else)
  "Create a GOAL if-expression node."
  (list :if :test test :then then :else else))

(defun goal-quote-node (expr)
  "Create a GOAL quote node."
  (list :quote :expr expr))

(defun goal-parse-number (token)
  "Parse a number token and return (:number value) node."
  (let ((parsed (parse-goal-number (cdr token))))
    (if (stringp parsed)
        (list :dword :value parsed)
        (list :number :value parsed))))

(defun goal-parse-expr (tokens)
  "Parse a GOAL expression from token stream.
Returns (values expr-node remaining-tokens)"
  (unless tokens
    (error "Unexpected end of input"))
  (let ((token (first tokens))
        (rest (rest tokens)))
    (case (car token)
      (:number
       (values (goal-parse-number token) rest))
      (:dword
       (values (list :dword :value (cdr token)) rest))
      (:string
       (values (list :string :value (cdr token)) rest))
      (:ident
       (values (goal-ident-node (cdr token)) rest))
      (:nil
       (values '(:nil) rest))
      (:true
       (values (list :boolean :value t) rest))
      (:false
       (values (list :boolean :value nil) rest))
      (:lparen
       (goal-parse-sexp rest))
      (:quote
       (multiple-value-bind (expr remaining) (goal-parse-expr rest)
         (values (goal-quote-node expr) remaining)))
      (t
       (error "Unexpected token: ~s" token)))))

(defun goal-parse-sexp (tokens)
  "Parse an S-expression (list) starting after lparen.
Returns (values list-node remaining-tokens)"
  (unless tokens
    (error "Unexpected end of input in S-expression"))
  (if (eq (caar tokens) :rparen)
      (values (goal-list-node '()) (rest tokens))
      (let ((elements '())
            (remaining tokens))
        (loop
          (multiple-value-bind (elem rest-tokens) (goal-parse-expr remaining)
            (push elem elements)
            (setf remaining rest-tokens)
            (unless remaining
              (error "Unexpected end of input in S-expression"))
            (if (eq (caar remaining) :rparen)
                (return))))
        (values (goal-list-node (nreverse elements)) (rest remaining)))))

(defun goal-parse-toplevel (tokens)
  "Parse top-level GOAL forms.
Returns list of AST nodes."
  (let ((forms '())
        (remaining tokens))
    (loop while remaining
          do (multiple-value-bind (form rest) (goal-parse-expr remaining)
               (push form forms)
               (setf remaining rest)))
    (nreverse forms)))

(defun goal-parse-source (source)
  "Parse complete GOAL SOURCE string into AST.
Returns list of AST nodes."
  (let ((tokens (goal-lex-source source)))
    (goal-parse-toplevel tokens)))
