;; src/grammar-build.lisp — AST constructors for BASIC programs
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; AST node constructors matching src/ast.lisp shapes
;;; NOTE: make-program-node / make-method-node / make-copy-node live in
;;; src/ast.lisp (canonical). Keep this file free of duplicates.

(defun make-procedure-node (name &key statements)
  "Build a :procedure AST node (global procedure)."
  (list :procedure
        :name       name
        :statements (or statements '())))

(defun make-move-node (from to)
  "Build a :move AST node."
  (list :move :from from :to to))

(defun make-invoke-node (object method &key args returning)
  "Build an :invoke AST node."
  (list* :invoke :object object :method method
         (append (when args `(:args ,args))
                 (when returning `(:returning ,returning)))))

(defun make-call-node (target &key args bank library returning)
  "Build a :call AST node."
  (list* :call :target target
         (append (when args `(:args ,args))
                 (when bank `(:bank ,bank))
                 (when library `(:library ,library))
                 (when returning `(:returning ,returning)))))

(defun make-if-node (condition then-stmts &optional else-stmts)
  "Build an :if AST node."
  (list :if :condition condition :then then-stmts :else (or else-stmts '())))

(defun make-goback-node ()
  "Build a :goback AST node."
  (list :goback))

(defun make-exit-method-node ()
  "Build an :exit-method AST node."
  (list :exit-method))

(defun make-stop-run-node (&optional code)
  "Build a :stop-run AST node. CODE is a string literal per BASIC STOP \"CODE\"."
  (if code
      (list :stop-run :code code)
      (list :stop-run)))

(defun make-log-fault-node (code)
  "Build a :log-fault AST node. CODE is a string literal per BASIC LOG FAULT \"CODE\"."
  (list :log-fault :code code))

(defun make-perform-node (procedure &key times until varying from by then body)
  "Build a :perform AST node. BODY (or THEN) enables an inline loop body
(PERFORM ... WITH inline body); backends require TIMES, UNTIL, or VARYING
alongside."
  (let ((inline-body (or body then)))
    (append (list :perform :procedure procedure)
            (when times `(:times ,times))
            (when until `(:until ,until))
            (when varying `(:varying ,varying :from ,from :by ,by))
            (when inline-body `(:body ,inline-body)))))

(defun make-set-node (target value)
  "Build a :set AST node."
  (list :set :target target :value value))

(defun make-assembly-entry-node (label)
  "Build an :assembly-entry AST node."
  (list :assembly-entry :label label))

(defun make-string-blt-node (source dest &optional length)
  "Build a :string-blt AST node."
  (list* :string-blt :source source :dest dest
         (when length `(:length ,length))))

(defun make-identifier (name)
  "Build an identifier reference."
  (string name))

(defun make-qualified-identifier (slot object &optional class)
  "Build a qualified identifier (:of slot obj [class])."
  (if class
      (list :of slot object class)
      (list :of slot object)))

(defun make-subscript-node (name index)
  "Build a subscripted identifier (:subscript name index)."
  (list :subscript name index))

(defun make-refmod-node (base start length)
  "Build a reference modification node."
  (list :refmod :base base :start start :length length))

(defun make-add-node (from to &optional giving)
  "Build an :add AST node."
  (if giving
      (list :add :from from :to to :giving giving)
      (list :add :from from :to to)))

(defun make-subtract-node (subtrahend from &optional giving)
  "Build a :subtract AST node."
  (if giving
      (list :subtract :subtrahend subtrahend :from from :giving giving)
      (list :subtract :subtrahend subtrahend :from from)))

(defun make-multiply-node (by multiplier &optional giving)
  "Build a :multiply AST node."
  (if giving
      (list :multiply :by by :multiplier multiplier :giving giving)
      (list :multiply :by by :multiplier multiplier)))

(defun make-divide-node (numerator denominator &optional giving)
  "Build a :divide AST node."
  (if giving
      (list :divide :numerator numerator :denominator denominator :giving giving)
      (list :divide :numerator numerator :denominator denominator)))

(defun make-compute-node (target expression)
  "Build a :compute AST node."
  (list :compute :target target :expression expression))

(defun make-expression-add (e1 e2)
  (list :add :from e1 :to e2 :giving nil))

(defun make-expression-subtract (e1 e2)
  (list :subtract :subtrahend e2 :from e1 :giving nil))

(defun make-expression-multiply (e1 e2)
  (list :multiply :by e1 :multiplier e2 :giving nil))

(defun make-expression-divide (e1 e2)
  (list :divide :numerator e1 :denominator e2 :giving nil))

(defun make-expression-shift-left (expr n)
  (list :shift-left expr n))

(defun make-expression-shift-right (expr n)
  (list :shift-right expr n))

(defun make-expression-bit-and (e1 e2)
  (list :bit-and e1 e2))

(defun make-expression-bit-or (e1 e2)
  (list :bit-or e1 e2))

(defun make-expression-bit-xor (e1 e2)
  (list :bit-xor e1 e2))

(defun make-expression-bit-not (expr)
  (list :bit-not expr))

(defun make-conditional-eq (e1 e2)
  (list '= e1 e2))

(defun make-conditional-ne (e1 e2)
  (list '/= e1 e2))

(defun make-conditional-lt (e1 e2)
  (list '< e1 e2))

(defun make-conditional-le (e1 e2)
  (list '<= e1 e2))

(defun make-conditional-gt (e1 e2)
  (list '> e1 e2))

(defun make-conditional-ge (e1 e2)
  (list '>= e1 e2))

(defun make-conditional-node (op e1 e2)
  "Build a conditional node (OP E1 E2) with a Lisp-style comparison operator."
  (list op e1 e2))

(defun make-conditional-and (c1 c2)
  (list :and c1 c2))

(defun make-conditional-or (c1 c2)
  (list :or c1 c2))

(defun make-conditional-not (cond)
  (list :not cond))

(defun make-conditional-is-null (expr)
  (list :null expr))

(defun make-conditional-is-not-null (expr)
  (list :not-null expr))

(defun make-conditional-is-zero (expr)
  (list '= expr 0))

(defun make-conditional-is-not-zero (expr)
  (list '/= expr 0))

(defun make-literal-number (n)
  n)

(defun make-literal (value &key (type :number))
  "Build a literal AST node. Numbers and strings are self-representing."
  (declare (ignorable type))
  value)

(defun make-literal-string (s)
  s)

(defun make-self ()
  :self)

(defun make-null ()
  :null)

(defun make-error-node (code)
  "Build an error AST node for invalid syntax."
  (list :error :code code))

;;; FORTRAN-specific constructors (explicit typing, no implicit numeric)

(defun make-fortran-move-node (from to &optional target-type)
  "Build a :fortran-move AST node with explicit target-type."
  (list :fortran-move :from from :to to :target-type target-type))

(defun make-fortran-compute-node (target expression &optional result-type)
  "Build a :fortran-compute AST node with explicit result-type."
  (list :fortran-compute :target target :expression expression :result-type result-type))

(defun make-fortran-do-node (var from to &key by stmts)
  "Build a :fortran-do AST node for DO loops."
  (list :fortran-do :var var :from from :to to :by by :stmts stmts))

(defun make-fortran-if-node (condition then else)
  "Build a :fortran-if AST node."
  (list :fortran-if :condition condition :then then :else else))

(defun make-fortran-arithmetic-node (op left right &optional result-type)
  "Build a :fortran-arithmetic AST node with type info."
  (list :fortran-arithmetic :op op :left left :right right :result-type result-type))

;;; Dialogue, Print, and Input AST nodes (Prolog-like structure)
;;; make-dialogue-node lives in src/ast.lisp (canonical, keyword :speaker/:text form).

(defun make-print-node (expressions)
  "Build a :print AST node.
EXPRESSIONS is a list of expressions to output."
  (list :print :expressions (ensure-list expressions)))

(defun make-input-node (variables &key prompt)
  "Build an :input AST node with optional prompt.
VARIABLES is a list of identifiers to read; PROMPT is optional greeting text."
  (list* :input :variables (ensure-list variables)
         (when prompt `(:prompt ,prompt))))

(defun make-prolog-goal (functor &rest args)
  "Build a Prolog-like goal structure (functor args…).
FUNCTOR is the goal predicate name; ARGS are arguments."
  (list* (intern functor) args))
