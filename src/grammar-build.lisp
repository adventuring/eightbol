;; src/grammar-build.lisp — AST constructors for BASIC programs
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; AST node constructors matching src/ast.lisp shapes
;;; NOTE: make-program-node / make-method-node / make-copy-node live in
;;; src/ast.lisp (canonical). Keep this file free of duplicates.

;;; ============================================================================
;;; PRAGMATIC DECLARATIONS — Optimize hints and temporary variable allocation
;;; ============================================================================

(defun parse-declare-annotation (text)
  "Parse (declare ...) annotation from comment text.
   
   Searches TEXT for (declare ... ) S-expression and extracts declaration forms.
   
   Returns list of declaration forms:
   - (optimize (speed N) (space N) (safety N)) where N ∈ [0,3]
   - (temp Var1 Var2 Var3)
   
   Returns NIL if no valid declaration found.
   
   Examples:
   - (parse-declare-annotation \"(declare (optimize (speed 3) (space 1)))\")
     → ((optimize (speed 3) (space 1)))
   - (parse-declare-annotation \"(declare (temp TempX TempY) (optimize (speed 2)))\")
     → ((temp TempX TempY) (optimize (speed 2)))
   - (parse-declare-annotation \"no declarations here\")
     → NIL"
  (when (and (stringp text) (not (zerop (length text))))
    (let ((start (search "(declare" text)))
      (when start
        ;; Find matching close paren for declare form
        (let ((paren-depth 1)
              (pos (+ start 8))
              (found-close nil))
          (loop while (and (< pos (length text)) (> paren-depth 0)) do
            (cond
              ((char= (char text pos) #\()
               (incf paren-depth))
              ((char= (char text pos) #\))
               (decf paren-depth)
               (when (zerop paren-depth)
                 (setf found-close pos))))
            (incf pos))
          
          (when found-close
            ;; Extract and parse the declare form
            (let ((declare-text (subseq text start (1+ found-close))))
              (ignore-errors
                (let ((form (read-from-string declare-text)))
                  (when (and (listp form) (eq (first form) 'declare))
                    (rest form)))))))))))

(defun validate-declare-form (form)
  "Validate a single declaration form.
    
    Valid forms:
    - (optimize (speed N) (space N) (safety N)) where N ∈ [0,3]
    - (temp Var1 Var2 ...)
    
    Returns T if valid, signals error otherwise."
  (cond
    ((and (listp form) (string-equal (first form) "optimize"))
     ;; (optimize (speed N) (space N) (safety N))
     (dolist (hint (rest form))
       (unless (and (listp hint)
                    (member (first hint) '(speed space safety) :test #'string-equal)
                    (numberp (second hint))
                    (>= (second hint) 0)
                    (<= (second hint) 3))
         (error 'compiler-error
                :message (format nil "Invalid optimize hint: ~s (must be (speed|space|safety N) where N ∈ [0,3])" hint))))
     t)
    ((and (listp form) (string-equal (first form) "temp"))
     ;; (temp Var1 Var2 ...)
     (when (< (length form) 2)
       (error 'compiler-error
              :message "Invalid temp declaration: must have at least one variable"))
     t)
    (t
     (error 'compiler-error
            :message (format nil "Unknown declaration form: ~s (valid: optimize, temp)" form)))))

;;; FRONTEND HELPER: Per-frontend state for capturing declarations
;;; Frontends should use this mechanism to preserve comments for declaration extraction.

(defvar *last-line-comment* nil
  "Most recent line comment text (set by lexer, cleared after use by parser).
   Used by frontends to capture declarations before procedures/methods/programs.
   
   Example usage in lexer:
     (setf *last-line-comment* comment-text)
   
   Example usage in parser:
     (let ((decls (when *last-line-comment* (parse-declare-annotation *last-line-comment*))))
       (setf *last-line-comment* nil)
       (make-procedure-node name :declare decls ...))")

(defun extract-and-clear-declaration ()
  "Extract pending declaration from *last-line-comment* and clear it.
   
   Returns list of declaration forms or NIL if none.
   Automatically clears the comment variable after extraction.
   
   Frontends should call this just before creating procedure/method/program nodes.
   
   Example:
     (let ((decls (extract-and-clear-declaration)))
       (make-procedure-node \"MyProc\" :declare decls :statements stmts))"
  (when *last-line-comment*
    (let ((decls (parse-declare-annotation *last-line-comment*)))
      (setf *last-line-comment* nil)
      decls)))

(defun make-procedure-with-declarations (name &key statements preceding-comment)
  "Convenience wrapper: parse declarations from comment and create procedure node.
   
   PRECEDING-COMMENT: raw comment text before this procedure
   
   Automatically extracts declarations and validates them.
   
   Example:
     (make-procedure-with-declarations \"Helper\"
       :statements (list ...)
       :preceding-comment \"(declare (optimize (speed 3)))\")
   
   Returns:
     (:procedure :name \"Helper\" :statements (...) :declare ((optimize (speed 3))))"
  (let ((decls (when preceding-comment (parse-declare-annotation preceding-comment))))
    (make-procedure-node name :statements statements :declare decls)))

(defun make-method-with-declarations (method-id &key statements preceding-comment)
  "Convenience wrapper: parse declarations from comment and create method node.
   
   PRECEDING-COMMENT: raw comment text before this method
   
   Automatically extracts declarations and validates them.
   
   Example:
     (make-method-with-declarations \"Update\"
       :statements (list ...)
       :preceding-comment \"(declare (optimize (speed 3) (space 1)))\")
   
   Returns:
     (:method :method-id \"Update\" :statements (...) :declare ((optimize (speed 3) (space 1))))"
  (let ((decls (when preceding-comment (parse-declare-annotation preceding-comment))))
    (make-method-node method-id :statements statements :declare decls)))

(defun make-program-with-declarations (class-id &key data methods preceding-comment identification environment)
  "Convenience wrapper: parse declarations from comment and create program node.
   
   PRECEDING-COMMENT: raw comment text before this program
   
   Automatically extracts declarations and validates them.
   
   Example:
     (make-program-with-declarations \"MyApp\"
       :methods (...)
       :data (...)
       :preceding-comment \"(declare (optimize (speed 3) (space 2) (safety 3)))\")
   
   Returns:
     (:program :class-id \"MyApp\" :methods (...) :data (...) :declare ((optimize ...)))"
  (let ((decls (when preceding-comment (parse-declare-annotation preceding-comment))))
    (make-program-node class-id 
                       :data data 
                       :methods methods 
                       :identification identification
                       :environment environment
                       :declare decls)))

(defun make-procedure-node (name &key statements declare)
  "Build a :procedure AST node (global procedure).
   
   DECLARE is optional list of declaration forms:
   - (optimize (speed N) (space N) (safety N))
   - (temp Var1 Var2 Var3)
   
   Example:
   (make-procedure-node \"Helper\"
     :statements (...)
     :declare ((optimize (speed 3) (space 1)) (temp TempX)))"
  (when declare
    (dolist (form declare)
      (validate-declare-form form)))
  (list* :procedure
         :name       name
         :statements (or statements '())
         (when declare `(:declare ,declare))))

(defun make-move-node (from to)
  "Build a :move AST node."
  (list :move :from from :to to))

(defun make-invoke-node (object method &key args returning)
  "Build an :invoke AST node. Note: :invoke CANNOT use accumulator for return value."
  (list* :invoke :object object :method method
         (append (when args `(:args ,args))
                 (when returning `(:returning ,returning)))))

(defun make-call-node (target &key args bank library returning type)
  "Build a :call AST node with calling convention type.
   
   TYPE determines accumulator return capability:
   - :subroutine — local subroutine, can return 1 byte in accumulator
   - :library — library function, can return 1 byte in accumulator
   - :far-service — far call to service bank, CANNOT use accumulator for return
   
   If not specified, TYPE defaults to :subroutine for local calls and :far-service for bank calls."
  (let ((inferred-type (or type
                          (if bank :far-service :subroutine))))
    (list* :call :target target :type inferred-type
           (append (when args `(:args ,args))
                   (when bank `(:bank ,bank))
                   (when library `(:library ,library))
                   (when returning `(:returning ,returning))))))

(defun make-if-node (condition then-stmts &optional else-stmts)
  "Build an :if AST node."
  (list :if :condition condition :then then-stmts :else (or else-stmts '())))

(defun make-goback-node ()
  "Build a :goback AST node."
  (list :goback))

(defun make-exit-method-node ()
  "Build a return node. :exit-method is desugared to the canonical :goback."
  (list :goback))

(defun make-stop-run-node (&optional code)
  "Build a return node. :stop-run is desugared to canonical :goback;
CODE is a string literal per BASIC STOP \"CODE\"."
  (if code
      (list :goback :code code)
      (list :goback)))

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
  "Build an assignment node. :set is desugared to canonical (:move :from value :to target)."
  (list :move :from value :to target))

(defun make-assembly-entry-node (label)
  "Build an :assembly-entry AST node."
  (list :assembly-entry :label label))

(defun make-declare-node (declare-type &key variables)
  "Build a :declare AST node for temporary variable declarations.
   
   DECLARE-TYPE: :temp — declares additional temporary variables
   VARIABLES: list of variable names available for complex expressions
   
   Example: (:declare :type :temp :variables (TempVar1 TempVar2))"
  (list* :declare :type declare-type
         (when variables `(:variables ,variables))))

(defun make-string-blt-node (source dest &optional length)
  "Build a :string-blt AST node."
  (list* :string-blt :source source :dest dest
         (when length `(:length ,length))))

(defun make-identifier (name)
  "Build an identifier reference."
  (list :identifier (string name)))

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
  "Build an :+ AST node."
  (if giving
      (list :+ :from from :to to :giving giving)
      (list :+ :from from :to to)))

(defun make-subtract-node (subtrahend from &optional giving)
  "Build an :- AST node."
  (if giving
      (list :- :subtrahend subtrahend :from from :giving giving)
      (list :- :subtrahend subtrahend :from from)))

(defun make-multiply-node (by multiplier &optional giving)
  "Build a :× AST node."
  (if giving
      (list :× :by by :multiplier multiplier :giving giving)
      (list :× :by by :multiplier multiplier)))

(defun make-divide-node (numerator denominator &optional giving)
  "Build a :÷ AST node."
  (if giving
      (list :÷ :numerator numerator :denominator denominator :giving giving)
      (list :÷ :numerator numerator :denominator denominator)))

(defun make-compute-node (target expression)
  "COMPUTE TARGET = EXPRESSION — desugared to canonical (:move :from expr :to target)."
  (list :move :from expression :to target))

(defun make-expression-add (e1 e2)
  (list :+ :from e1 :to e2 :giving nil))

(defun make-expression-subtract (e1 e2)
  (list :- :subtrahend e2 :from e1 :giving nil))

(defun make-expression-multiply (e1 e2)
  (list :× :by e1 :multiplier e2 :giving nil))

(defun make-expression-divide (e1 e2)
  (list :÷ :numerator e1 :denominator e2 :giving nil))

(defun make-expression-shift-left (expr n)
  "Arithmetic shift LEFT by N: (list :ash expr n) for n >= 0."
  (list :ash expr n))

(defun make-expression-shift-right (expr n)
  "Arithmetic shift RIGHT by N: canonical :ash uses a signed amount,
so right shift by N becomes (list :ash expr (- n))."
  (list :ash expr (- n)))

(defun make-expression-bit-and (e1 e2)
  (list :∧ e1 e2))

(defun make-expression-bit-or (e1 e2)
  (list :∨ e1 e2))

(defun make-expression-bit-xor (e1 e2)
  (list :⊻ e1 e2))

(defun make-expression-bit-not (expr)
  (list :¬ expr))

(defun make-conditional-eq (e1 e2)
  (list := e1 e2))

(defun make-conditional-ne (e1 e2)
  (list :≠ e1 e2))

(defun make-conditional-lt (e1 e2)
  (list :< e1 e2))

(defun make-conditional-le (e1 e2)
  (list :≤ e1 e2))

(defun make-conditional-gt (e1 e2)
  (list :> e1 e2))

(defun make-conditional-ge (e1 e2)
  (list :≥ e1 e2))

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
  (list := expr 0))

(defun make-conditional-is-not-zero (expr)
  (list :≠ expr 0))

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

;;; ============================================================================
;;; VARIABLE ERASURE FRAMEWORK — Resolve all variables to globals/slots/temps
;;; ============================================================================

;;; Reserved temporary variables (hardware/ABI defined)
(defparameter +math-temp+ "MathTemp"
  "1-byte reserved temporary for arithmetic operations (e.g. intermediate byte results).")

(defparameter +multiply-temp+ "MultiplyTemp"
  "16-bit (word) reserved temporary for multiplication/wide arithmetic.")

(defun make-global-reference (var-name)
  "Build a (:global NAME) reference."
  (list :global (string var-name)))

(defun make-slot-reference (var-name)
  "Build a (:slot NAME) reference for instance slot."
  (list :slot (string var-name)))

(defun make-temp-reference (temp-name)
  "Build a reference to a reserved temporary (MathTemp or MultiplyTemp)."
  (list :global (string temp-name)))

(defun resolve-variable (var-name copybook-slot-table &key object)
  "Resolve VAR-NAME to copybook entry or reserved temporary.
   
   COPYBOOK-SLOT-TABLE is the slot table hash from load-copybook-tables.
   OBJECT is the enclosing object name (for slot lookups).
   
   Returns: (:global NAME), (:slot NAME), or signals error if undefined.
   
   Lookup order:
   1. If OBJECT and VAR-NAME in object's slots → (:slot NAME)
   2. If VAR-NAME in globals → (:global NAME)
   3. If VAR-NAME is reserved temp (MathTemp/MultiplyTemp) → (:global NAME)
   4. Otherwise: error 'undefined-variable"
  (cond
    ;; Check if it's a reserved temporary
    ((member var-name (list +math-temp+ +multiply-temp+) :test #'string-equal)
     (make-global-reference var-name))
    
    ;; Check in slot table (globals are also in slot table)
    ((and copybook-slot-table (gethash (eightbol::cobol-slot-table-name-key var-name) copybook-slot-table))
     (let ((origin (gethash (eightbol::cobol-slot-table-name-key var-name) copybook-slot-table)))
       (if (and object (string-equal origin object))
           (make-slot-reference var-name)
           (make-global-reference var-name))))
    
    ;; Not found
    (t (error 'eightbol::compiler-error
              :message (format nil "Undefined variable: ~a" var-name)))))

(defun resolve-expression (expr copybook-slot-table &key object)
  "Recursively resolve all variables in EXPR.
   
   Replaces bare variable symbols with (:global NAME) or (:slot NAME).
   Returns updated expression with all variables qualified.
   
   EXPR can be:
   - Number (literal) → unchanged
   - String (literal) → unchanged
   - Symbol → resolve to (:global SYM) or (:slot SYM)
   - (:of slot obj) → recurse, resolve slot and obj
   - (:subscript base idx) → recurse on both
   - (:refmod :base b :start s :length l) → recurse on all
   - List → recurse on all elements"
  (cond
    ((null expr) expr)
    ((numberp expr) expr)
    ((stringp expr) expr)
    ((keywordp expr) expr)
    ((symbolp expr)
     ;; Bare symbol → resolve to copybook entry
     (resolve-variable (symbol-name expr) copybook-slot-table :object object))
    ((and (listp expr) (eq (first expr) :of))
     ;; (:of slot obj) → resolve slot and obj
     (list :of
           (resolve-variable (second expr) copybook-slot-table :object object)
           (resolve-expression (third expr) copybook-slot-table :object object)
           (when (fourth expr) (fourth expr))))
    ((and (listp expr) (eq (first expr) :subscript))
     ;; (:subscript base idx) → resolve both
     (list :subscript
           (resolve-expression (second expr) copybook-slot-table :object object)
           (resolve-expression (third expr) copybook-slot-table :object object)))
    ((and (listp expr) (eq (first expr) :refmod))
     ;; (:refmod :base b :start s :length l)
     (list :refmod
           :base (resolve-expression (eightbol::safe-getf (rest expr) :base) copybook-slot-table :object object)
           :start (resolve-expression (eightbol::safe-getf (rest expr) :start) copybook-slot-table :object object)
           :length (resolve-expression (eightbol::safe-getf (rest expr) :length) copybook-slot-table :object object)))
    ((and (listp expr) (eq (first expr) :global))
     ;; Already qualified
     expr)
    ((and (listp expr) (eq (first expr) :slot))
     ;; Already qualified
     expr)
    ((listp expr)
     ;; Generic list → recurse on elements (preserving structure)
     (mapcar (lambda (e) (resolve-expression e copybook-slot-table :object object)) expr))
    (t expr)))

(defun allocate-temp-for-intermediate (expression-type bit-width)
  "Allocate appropriate reserved temporary for intermediate value.
   
   EXPRESSION-TYPE: :arithmetic, :×, :÷, etc.
   BIT-WIDTH: required bit width (1 for byte, 16 for word, etc.)
   
   Returns: temporary variable name (MathTemp or MultiplyTemp)
   
   Strategy:
   - Byte operations (≤8 bits) → MathTemp
   - Word operations (9-16 bits) → MultiplyTemp
   - Larger → error (not supported yet)"
  (cond
    ((and (numberp bit-width) (<= bit-width 8))
     +math-temp+)
    ((and (numberp bit-width) (<= bit-width 16))
     +multiply-temp+)
    (t (error 'compiler-error
              :message (format nil "Bit width ~a exceeds reserved temporary capacity" bit-width)))))

(defun erase-locals (ast copybook-slot-table &key object)
  "Remove all local variables from AST, replacing with globals/slots/temps.
   
   Recursively walks AST:
   - Resolves all variable references
   - Detects/allocates reserved temps for intermediate values
   - Errors on undefined variables
   - Returns updated AST with no bare variable symbols
   
   AST can be:
   - :program node
   - :method node
   - Statement list
   - Single statement
   - Expression
   
   COPYBOOK-SLOT-TABLE: slot table hash from load-copybook-tables
   OBJECT: current object context (for slot resolution)"
  (cond
    ((null ast) ast)
    ((numberp ast) ast)
    ((stringp ast) ast)
    ((keywordp ast) ast)
    ((symbolp ast)
     ;; Bare symbol → error (all variables must be resolved before entering erase-locals)
     (error 'eightbol::compiler-error
            :message (format nil "Unresolved variable in AST: ~a" ast)))
    ((and (listp ast) (eq (first ast) :program))
     ;; (:program :class-id … :data … :methods …)
     (list :program
           :class-id (eightbol::safe-getf (rest ast) :class-id)
           :identification (eightbol::safe-getf (rest ast) :identification)
           :environment (eightbol::safe-getf (rest ast) :environment)
           :data (erase-locals (eightbol::ast-data ast) copybook-slot-table :object object)
           :methods (mapcar (lambda (m) (erase-locals m copybook-slot-table :object object))
                            (eightbol::ast-methods ast))))
    ((and (listp ast) (eq (first ast) :method))
     ;; (:method :method-id … :statements …)
     (list :method
           :method-id (eightbol::safe-getf (rest ast) :method-id)
           :statements (mapcar (lambda (s) (erase-locals s copybook-slot-table :object object))
                               (eightbol::ast-method-statements ast))))
    ((and (listp ast) (eq (first ast) :move))
     ;; (:move :from expr :to id)
     (list :move
           :from (resolve-expression (eightbol::safe-getf (rest ast) :from) copybook-slot-table :object object)
           :to (resolve-expression (eightbol::safe-getf (rest ast) :to) copybook-slot-table :object object)))
    ((and (listp ast) (eq (first ast) :set))
     ;; (:set :target id :value expr)
     (list :set
           :target (resolve-expression (eightbol::safe-getf (rest ast) :target) copybook-slot-table :object object)
           :value (resolve-expression (eightbol::safe-getf (rest ast) :value) copybook-slot-table :object object)))
    ((and (listp ast) (eq (first ast) :compute))
     ;; (:compute :target id :expression expr)
     (list :compute
           :target (resolve-expression (eightbol::safe-getf (rest ast) :target) copybook-slot-table :object object)
           :expression (resolve-expression (eightbol::safe-getf (rest ast) :expression) copybook-slot-table :object object)))
    ((and (listp ast) (eq (first ast) :if))
     ;; (:if :condition cond :then stmts :else stmts)
     (list :if
           :condition (resolve-expression (eightbol::safe-getf (rest ast) :condition) copybook-slot-table :object object)
           :then (mapcar (lambda (s) (erase-locals s copybook-slot-table :object object))
                         (eightbol::ensure-list (eightbol::safe-getf (rest ast) :then)))
           :else (mapcar (lambda (s) (erase-locals s copybook-slot-table :object object))
                         (eightbol::ensure-list (eightbol::safe-getf (rest ast) :else)))))
    ((and (listp ast) (eq (first ast) :+))
     ;; (:+ :from expr :to id [:giving id])
     (list* :+
            :from (resolve-expression (eightbol::safe-getf (rest ast) :from) copybook-slot-table :object object)
            :to (resolve-expression (eightbol::safe-getf (rest ast) :to) copybook-slot-table :object object)
            (when (eightbol::safe-getf (rest ast) :giving)
              (list :giving (resolve-expression (eightbol::safe-getf (rest ast) :giving) copybook-slot-table :object object)))))
    ((and (listp ast) (eq (first ast) :-))
     ;; (:- :subtrahend expr :from expr [:giving id])
     (list* :-
            :subtrahend (resolve-expression (eightbol::safe-getf (rest ast) :subtrahend) copybook-slot-table :object object)
            :from (resolve-expression (eightbol::safe-getf (rest ast) :from) copybook-slot-table :object object)
            (when (eightbol::safe-getf (rest ast) :giving)
              (list :giving (resolve-expression (eightbol::safe-getf (rest ast) :giving) copybook-slot-table :object object)))))
    ((and (listp ast) (eq (first ast) :invoke))
     ;; (:invoke :object obj :method "Name" [:args args] [:returning id])
     (list* :invoke
            :object (resolve-expression (eightbol::safe-getf (rest ast) :object) copybook-slot-table :object object)
            :method (eightbol::safe-getf (rest ast) :method)
            (when (eightbol::safe-getf (rest ast) :args)
              (list :args (resolve-expression (eightbol::safe-getf (rest ast) :args) copybook-slot-table :object object)))
            (when (eightbol::safe-getf (rest ast) :returning)
              (list :returning (resolve-expression (eightbol::safe-getf (rest ast) :returning) copybook-slot-table :object object)))))
    ((and (listp ast) (eq (first ast) :string-blt))
     ;; (:string-blt :source src :dest dst [:length len])
     (list* :string-blt
            :source (resolve-expression (eightbol::safe-getf (rest ast) :source) copybook-slot-table :object object)
            :dest (resolve-expression (eightbol::safe-getf (rest ast) :dest) copybook-slot-table :object object)
            (when (eightbol::safe-getf (rest ast) :length)
              (list :length (resolve-expression (eightbol::safe-getf (rest ast) :length) copybook-slot-table :object object)))))
    ((and (listp ast) (eq (first ast) :perform))
     ;; (:perform :procedure name [:times expr] [:until cond] [:varying var :from init :by step] [:body stmts])
     (list* :perform
            :procedure (eightbol::safe-getf (rest ast) :procedure)
            (when (eightbol::safe-getf (rest ast) :times)
              (list :times (resolve-expression (eightbol::safe-getf (rest ast) :times) copybook-slot-table :object object)))
            (when (eightbol::safe-getf (rest ast) :until)
              (list :until (resolve-expression (eightbol::safe-getf (rest ast) :until) copybook-slot-table :object object)))
            (when (eightbol::safe-getf (rest ast) :varying)
              (list :varying (resolve-expression (eightbol::safe-getf (rest ast) :varying) copybook-slot-table :object object)
                    :from (resolve-expression (eightbol::safe-getf (rest ast) :from) copybook-slot-table :object object)
                    :by (resolve-expression (eightbol::safe-getf (rest ast) :by) copybook-slot-table :object object)))
            (when (eightbol::safe-getf (rest ast) :body)
              (list :body (mapcar (lambda (s) (erase-locals s copybook-slot-table :object object))
                                  (eightbol::ensure-list (eightbol::safe-getf (rest ast) :body)))))))
    ((listp ast)
     ;; Generic list → recurse on all elements
     (mapcar (lambda (e) (erase-locals e copybook-slot-table :object object)) ast))
    (t ast)))
