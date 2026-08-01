;; src/objective-parser.lisp — YACC grammar for Objective-C → EIGHTBOL AST
;; Following the pattern of pascal-parser.lisp, basic-parser.lisp
(in-package :eightbol)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun objective-token-list ()
    (mapcar (compose #'intern #'string)
            '(|(| |)| |:| |,| + - * / = <> < <= > >=
              @interface @implementation @end @property @selector
              @protocol @public @private @protected @synthesize
              @autoreleasepool @try @catch @finally @throw
              [ ] |.| -> self super nil yes no true false
              if else for while do return break continue switch case default
              number string ident))))

(defun objective-parse-program (statements)
  "Top-level program node from list of method nodes and class definitions."
  (let ((class-nodes '())
        (method-nodes '())
        (data-nodes '()))
    (dolist (item statements)
      (cond
        ((and (listp item) (eq (first item) :class-definition))
         (push item class-nodes))
        ((and (listp item) (eq (first item) :method))
         (push item method-nodes))
        ((and (listp item) (eq (first item) :dd))
         (push item data-nodes))))
    (if class-nodes
        (make-program-node (or (getf (append (list :class-id nil) (first class-nodes)) :class-id)
                                "ObjectiveClass")
                           :methods (nreverse method-nodes)
                           :data (nreverse data-nodes))
        (make-program-node "ObjectiveProgram"
                           :methods (nreverse method-nodes)))))

(defun objective-parse-message-send (receiver selector args)
  "Build invoke node for Objective-C message send."
  (let ((method-name (if (stringp selector) selector
                        (apply #'concatenate 'string (mapcar (lambda (s) (if (listp s) (first s) s))
                                                            (ensure-list selector))))))
    (if (null args)
        (list :invoke :object receiver :method method-name)
        (list :invoke :object receiver :method method-name :arguments args))))

(defun objective-parse-keyword-args (args)
  "Parse keyword arguments alternating: SELECTOR-COLON argument ..."
  (when args
    (let ((selector-parts '())
          (arg-values '())
          (args-remaining (ensure-list args)))
      (loop while args-remaining
            do (destructuring-bind (sel-part arg &optional rest) args-remaining
                 (push (format nil "~a:" (if (listp sel-part) (first sel-part) sel-part)) selector-parts)
                 (push arg arg-values)
                 (setf args-remaining rest)))
      (values (apply #'concatenate 'string (nreverse selector-parts))
              (nreverse arg-values)))))

(defun objective-parse-property-get (object property)
  "Property access as message send to getter."
  (list :invoke :object object :method property))

(defun objective-parse-property-set (object property value)
  "Property assignment as move (setter semantics)."
  (let ((slot-name (format nil "~aOf~a" property (if (eq object :self) "Self" object))))
    (make-move-node value slot-name)))

(defun objective-parse-assignment (lhs rhs)
  "Assignment statement - handles both variables and property access."
  (if (listp lhs)
      (cond
        ((eq (first lhs) :property-access)
         (destructuring-bind (_ obj prop) lhs
           (declare (ignore _))
           (objective-parse-property-set obj prop rhs)))
        (t (make-move-node rhs lhs)))
      (make-move-node rhs lhs)))

(defun objective-parse-if (condition then-stmts else-stmts)
  "Build IF statement node."
  (make-if-node condition then-stmts (when else-stmts (ensure-list else-stmts))))

(defun objective-parse-while (condition stmts)
  "Build WHILE loop as PERFORM UNTIL NOT condition."
  (make-perform-node "WHILE" :until (list :not condition) :stmts (ensure-list stmts)))

(defun objective-parse-for (var from to step stmts)
  "Build FOR loop as PERFORM VARYING."
  (let ((by (or step 1))
        (until (list '> var to)))
    (make-perform-node (format nil "FOR-~a" var)
                       :varying var
                       :from from
                       :by by
                       :until until
                       :stmts (ensure-list stmts))))

(defun objective-parse-method-def (modifier name parameters return-type stmts)
  "Build method node from definition."
  (declare (ignore modifier parameters return-type))
  (make-method-node name :statements (ensure-list stmts)))

(defun objective-parse-class-def (name superclass data-methods)
  "Build class definition for @interface. Returns :class-definition node."
  (let ((methods '())
        (data '()))
    (dolist (item data-methods)
      (when (and (listp item) (eq (first item) :method))
        (push item methods))
      (when (and (listp item) (eq (first item) :dd))
        (push item data)))
    (list :class-definition :name name :superclass superclass :methods (nreverse methods) :data (nreverse data))))

(defun objective-build-selector (parts)
  "Build selector string from keyword parts."
  (if (stringp parts)
      parts
      (apply #'concatenate 'string (mapcar (lambda (p) (if (listp p) (first p) p))
                                          (ensure-list parts)))))

(defun objective-parse-selector-part (selectors)
  "Concatenate selector parts into method name."
  (if (stringp selectors)
      selectors
      (apply #'concatenate 'string (ensure-list selectors))))

(defun objective-literal-number (n)
  "Convert number token to literal."
  (if (stringp n) (parse-integer n) n))

(defun objective-literal-string (s)
  "Convert string token to literal."
  (if (stringp s) s (princ-to-string s)))

(defun objective-expression-add (left right)
  "Build addition expression."
  (list :add :from left :to right :giving nil))

(defun objective-expression-sub (left right)
  "Build subtraction expression."
  (list :subtract :subtrahend right :from left :giving nil))

(defun objective-expression-mul (left right)
  "Build multiply expression."
  (list :multiply :by left :multiplier right :giving nil))

(defun objective-expression-div (left right)
  "Build divide expression."
  (list :divide :numerator left :denominator right :giving nil))

(defun objective-conditional-eq (left right)
  "Build equality condition."
  (list '= left right))

(defun objective-conditional-ne (left right)
  "Build inequality condition."
  (list '/= left right))

(defun objective-conditional-lt (left right)
  "Build less-than condition."
  (list '< left right))

(defun objective-conditional-le (left right)
  "Build less-equal condition."
  (list '<= left right))

(defun objective-conditional-gt (left right)
  "Build greater-than condition."
  (list '> left right))

(defun objective-conditional-ge (left right)
  "Build greater-equal condition."
  (list '>= left right))

(defun objective-conditional-and (left right)
  "Build AND condition."
  (list :and left right))

(defun objective-conditional-or (left right)
  "Build OR condition."
  (list :or left right))

(defun objective-conditional-not (expr)
  "Build NOT condition."
  (list :not expr))

(eval
 `(yacc:define-parser *objective-parser*
    (:start-symbol objective-program)
    (:terminals (,@(objective-token-list) number string ident))
    (:precedence ((:left or)
                  (:left and)
                  (:left not)
                  (:left = != < <= > >=)
                  (:left + -)
                  (:left * /))
                 (:right =))
    (:muffle-conflicts :all)

    ;; Top level
    (objective-program
     (global-top-level
      (lambda (forms) (objective-parse-program (ensure-list forms)))))

    (global-top-level
     (top-level-form
      (lambda (f) (list f)))
     (global-top-level top-level-form
                       (lambda (forms f) (append forms (list f)))))

    (top-level-form
     (class-definition)
     (method-definition)
     (statement)
     (data-declaration))
    
    (class-definition
     (@interface class-name superclass? ivar-declaration*? @end
                 (lambda (_ name super ivars ___)
                   (declare (ignore _ ___))
                   (objective-parse-class-def name super ivars))))

    (superclass
     (|:| class-name (lambda (_ name) name)))

    (ivar-declaration
     (type-spec ident
                (lambda (type name)
                  (declare (ignore type))
                  (make-fortran-declare name :type :object))))

    (type-spec
     (ident (lambda (name) name))
     (:void (lambda () :void))
     (:id (lambda () :id)))

    (method-definition
     (method-sign compound-statement
                  (lambda (sign body)
                    (objective-parse-method-def (first sign) (second sign) (third sign) (fourth sign) body))))

    (method-sign
     (+ method-selector (lambda (_ sel) (cons :class sel)))
     (- method-selector (lambda (_ sel) (cons :instance sel))))

    (method-selector
     (selector-part+
      (lambda (parts)
        (objective-parse-selector-part parts))))

    (selector-part
     (ident colon expression
            (lambda (name _ args)
              (cons (cons name ":") args))))

    (compound-statement
     ({ statement-list }
        (lambda (_ stmts __) (declare (ignore _ __)) stmts)))

    (statement-list
     (statement
      (lambda (s) (list s)))
     (statement-list statement
                     (lambda (stmts s) (append stmts (list s)))))

    (statement
     (message-send)
     (assignment-stmt)
     (if-stmt)
     (while-stmt)
     (for-stmt)
     (return-stmt)
     (expression-stmt))

    (message-send
     (lbracket expression selector-part* argument-list? rbracket
               (lambda (_ receiver selector args __)
                 (declare (ignore _ __))
                 (objective-parse-message-send receiver 
                                               (objective-parse-selector-part selector)
                                               (when args args))))

     (lbracket expression selector-part+ argument-list? rbracket
               (lambda (_ receiver selectors args __)
                 (declare (ignore _ __))
                 (objective-parse-message-send receiver
                                               (objective-parse-selector-part selectors)
                                               (when args args)))))

    (argument-list
     (lpar expression-list rpar
           (lambda (_ args __) args)))

    (expression-list
     (expression
      (lambda (e) (list e)))
     (expression-list comma expression
                      (lambda (args _ e) (append args (list e)))))

    (assignment-stmt
     (lvalue assign expression
             (lambda (lhs _ rhs)
               (objective-parse-assignment lhs rhs))))

    (lvalue
     (ident (lambda (name) name))
     (property-access (lambda (p) p)))

    (property-access
     (expr dot ident
           (lambda (obj _ prop)
             (list :property-access obj prop))))

    (expr
     (expression))

    (if-stmt
     (if lpar expression rpar statement
         (lambda (_ _ cond __ then)
           (declare (ignore _ __))
           (objective-parse-if cond then nil)))
     (if lpar expression rpar statement else-part
         (lambda (_ _ cond __ then _ else)
           (declare (ignore _ __ _))
           (objective-parse-if cond then else))))

    (else-part
     (else statement
           (lambda (_ s) s)))

    (while-stmt
     (while lpar expression rpar statement
       (lambda (_ _ cond __ stmts)
         (declare (ignore _ __))
         (objective-parse-while cond stmts))))

    (for-stmt
     (for lpar for-init for-cond for-step rpar statement
       (lambda (_ __ init _ cond _ step __ stmts)
         (declare (ignore _ __ _ __))
         (objective-parse-for init cond step stmts))))

    (for-init
     (decl-init))

    (decl-init
     (var-assign))

    (var-assign
     (ident assign expression
            (lambda (var _ val) (list var val))))

    (for-cond
     (expression))

    (for-step
     (expression))

    (return-stmt
     (return expression?
             (lambda (_ val)
               (if val
                   (list :set :target val :value :nil)
                   (list :goback)))))

    (expression-stmt
     (expression
      (lambda (e) e)))

    (expression
     (literal)
     (ident (lambda (n) n))
     (self (lambda () :self))
     (super (lambda () :super))
     (nil (lambda () :null))
     (yes (lambda () t))
     (no (lambda () nil))
     (true (lambda () t))
     (false (lambda () nil))
     (lpar expression rpar (lambda (_ e _) e))
     (expression binop expression
                 (lambda (left op right)
                   (ecase op
                     (:plus (objective-expression-add left right))
                     (:minus (objective-expression-sub left right))
                     (:times (objective-expression-mul left right))
                     (:divide (objective-expression-div left right))
                     (:equals (objective-conditional-eq left right))
                     (:ne (objective-conditional-ne left right))
                     (:lt (objective-conditional-lt left right))
                     (:le (objective-conditional-le left right))
                     (:gt (objective-conditional-gt left right))
                     (:ge (objective-conditional-ge left right))
                     (:and (objective-conditional-and left right))
                     (:or (objective-conditional-or left right))))))

    (literal
     (number (lambda (n) (objective-literal-number n)))
     (string (lambda (s) (objective-literal-string s))))
    
    (binop
     (+ (constantly :plus))
     (- (constantly :minus))
     (* (constantly :times))
     (/ (constantly :divide))
     (== (constantly :equals))
     (!= (constantly :ne))
     (< (constantly :lt))
     (<= (constantly :le))
     (> (constantly :gt))
     (>= (constantly :ge))
     (&& (constantly :and))
     (|| (constantly :or))
     (! (constantly :not)))))

(defun objective-lex-stream (tokens)
  "Convert token list to YACC lexer thunk."
  (lambda ()
    (when tokens
      (let ((tok (pop tokens)))
        (values (first tok) (second tok))))))

(defun parse-objective (source)
  "Parse Objective-C source string and return eightbol AST."
  (let ((tokens (objective-lex-source source)))
    (yacc:parse-with-lexer
     (objective-lex-stream tokens)
     *objective-parser*)))

(defun objective-make-parser ()
  "Return the objective YACC parser function."
  *objective-parser*)

(provide 'objective-parser)
