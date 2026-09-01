;;; src/frontend-lingo/lingo-parser.lisp — YACC parser for Lingo source
;;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defun lingo-parse-program (statements)
  "Top-level program node from list of statement nodes."
  (make-program-node
   "LingoProgram"
   :methods (remove nil statements)))

(defun lingo-make-ident (name)
  "Normalize identifier to Header-Case."
  (if (stringp name)
      (lingo-normalize-identifier name)
      name))

;;; Token list for Lingo YACC parser
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun lingo-token-list ()
    (mapcar (compose #'intern #'string)
            '(|(| |)| |{| |}| |[| |]| |.| |,| |:| |::=|
              + - * / = == <> != < > <= >=
              on end if then else endif repeat with for in to step up down where
              while until do loop exit next put get input print set call goto go
              frame properties property tell endtell method procedure class return
              play stop halt library service bank of the and or not true false null
              self assembly entry copy as data
              number string ident))))

;; Lingo YACC grammar with AST node construction
(eval
 `(yacc:define-parser *lingo-parser*
    (:muffle-conflicts :some)
    (:terminals (,@(lingo-token-list)))
    (:start-symbol program)

    ;;  Program Root 
    (program
     ((constantly nil))
     (definition-list (lambda (defs) defs)))

    ;;  Definition List 
    (definition-list
     (definition (lambda (d) (list d)))
     (definition-list definition
                      (lambda (prev curr)
                        (append prev (list curr)))))

    ;;  Definitions 
    (definition
     (method-def)
     (procedure-def)
     (class-def))

    ;;  Method Definition 
    (method-def
     (method ident |(| param-list |)| statement-list end
             (lambda (name params body)
               (make-method-node (lingo-make-ident name) :statements body))))

    ;;  Procedure Definition 
    (procedure-def
     (procedure ident |(| param-list |)| statement-list end
                (lambda (name params body)
                  (lingo-make-procedure-node (lingo-make-ident name) :statements body))))

    ;;  Class Definition 
    (class-def
     (class ident statement-list end
            (lambda (name body)
              (list :class :name (lingo-make-ident name) :body body))))

    ;;  Parameter List 
    (param-list
     ((constantly nil))
     (param-item-list (lambda (p) p)))

    (param-item-list
     (ident (lambda (i) (list (lingo-make-ident i))))
     (param-item-list |,| ident
                      (lambda (prev i)
                        (append prev (list (lingo-make-ident i))))))

    ;;  Statement List 
    (statement-list
     ((constantly nil))
     (statement (lambda (s) (list s)))
     (statement-list statement
                     (lambda (prefix suffix)
                       (append prefix (list suffix)))))

    ;;  Statements 
    (statement
     (assignment)
     (call-stmt)
     (copy-stmt)
     (loop-stmt)
     (if-stmt)
     (return-stmt)
     (goto-stmt)
     (io-stmt)
     (expression))

    ;;  I/O Statements 
    (io-stmt
     (put expression
          (lambda (val)
            (list :put :value val)))
     (get ident
          (lambda (var)
            (list :get :target (lingo-make-ident var))))
     (input ident
            (lambda (var)
              (list :input :target (lingo-make-ident var))))
     (print expression
            (lambda (val)
              (list :print :value val))))

    ;;  Assignments 
    (assignment
     (set-stmt)
     (move-stmt)
     (property-stmt))

    ;;  Set Statement 
    (set-stmt
     (set ident to expression
          (lambda (target value)
            (make-move-node (lingo-make-ident target) value))))

    ;;  Move Statement 
    (move-stmt
     (on expression to ident
         (lambda (value target)
           (make-move-node value (lingo-make-ident target)))))

    ;;  Property Statement 
    (property-stmt
     (property ident-list
               (lambda (idents)
                 (list :property-decl :identifiers idents))))

    ;;  Call Statement 
    (call-stmt
     (call expression |(| arg-list |)|
           (lambda (target args)
             (make-invoke-node target)))
     (call ident
           (lambda (target)
             (make-invoke-node (lingo-make-ident target)))))

    ;;  Copy Statement 
    (copy-stmt
     (copy string
           (lambda (filename)
             (list :copy :name filename))))

    ;;  Loop Statements 
    (loop-stmt
     (for ident in expression to expression step expression do statement-list end
       (lambda (var start end step body)
         (make-perform-node (lingo-make-ident var)
                            :from start :to end :by step
                            :body body)))
     (while expression do statement-list end
       (lambda (cond body)
         body))
     (repeat statement-list until expression end
       (lambda (body cond)
         body)))
    
    ;;  Conditional Statements 
    (if-stmt
     (if expression then statement-list endif
         (lambda (cond then-stmts)
           (make-if-node cond then-stmts nil)))
     (if expression then statement-list else statement-list endif
         (lambda (cond then-stmts else-stmts)
           (make-if-node cond then-stmts else-stmts))))

    ;;  Return Statement 
    (return-stmt
     (return
       (lambda () (make-goback-node)))
     (return expression
             (lambda (val)
               (make-move-node val "RESULT"))))

    ;;  Goto Statement 
    (goto-stmt
     (goto ident
           (lambda (target)
             (make-goto-node (lingo-make-ident target)))))

    ;;  Expression Parsing 
    (expression
     (or-expr))

    (or-expr
     (and-expr)
     (or-expr or and-expr
              (lambda (l r) (make-conditional-or l r))))

    (and-expr
     (not-expr)
     (and-expr and not-expr
               (lambda (l r) (make-conditional-and l r))))

    (not-expr
     (comparison)
     (not not-expr
          (lambda (e) (make-conditional-not e))))

    (comparison
     (add-expr)
     (comparison = add-expr
                 (lambda (l r) (make-conditional-eq l r)))
     (comparison <> add-expr
                 (lambda (l r) (make-conditional-ne l r)))
     (comparison != add-expr
                 (lambda (l r) (make-conditional-ne l r)))
     (comparison < add-expr
                 (lambda (l r) (make-conditional-lt l r)))
     (comparison > add-expr
                 (lambda (l r) (make-conditional-gt l r)))
     (comparison <= add-expr
                 (lambda (l r) (make-conditional-le l r)))
     (comparison >= add-expr
                 (lambda (l r) (make-conditional-ge l r))))

    (add-expr
     (mult-expr)
     (add-expr + mult-expr
               (lambda (l r) (make-expression-add l r)))
     (add-expr - mult-expr
               (lambda (l r) (make-expression-subtract l r))))

    (mult-expr
     (primary-expr)
     (mult-expr * primary-expr
                (lambda (l r) (make-expression-multiply l r)))
     (mult-expr / primary-expr
                (lambda (l r) (make-expression-divide l r))))

    ;;  Primary Expressions 
    (primary-expr
     (literal)
     (ident)
     (|(| expression |)|
          (lambda (e) e))
     (primary-expr |.| ident
                   (lambda (obj prop)
                     (make-qualified-identifier (lingo-make-ident prop) obj)))
     (primary-expr |:| ident |(| arg-list |)|
                   (lambda (obj method args)
                     (list :method-call :object obj :method (lingo-make-ident method))))
     (ident |(| arg-list |)|
            (lambda (func args)
              (list :function-call :name (lingo-make-ident func)))))

    ;;  Literals 
    (literal
     (number (lambda (n) (string-to-number n)))
     (string (lambda (s) s))
     (true (lambda () :true))
     (false (lambda () :false))
     (null (lambda () :null))
     (self (lambda () :self)))

    ;;  Identifier List 
    (ident-list
     (ident (lambda (i) (list (lingo-make-ident i))))
     (ident-list ident
                 (lambda (prev i)
                   (append prev (list (lingo-make-ident i))))))

    ;;  Argument List
    
    (arg-list
     ((constantly nil))
     (expression (lambda (e) (list e)))
     (arg-list |,| expression
               (lambda (_comma args)
                 args)))))

(defun string-to-number (s)
  "Convert string literal to numeric value, supporting all number formats."
  (when (stringp s)
    (multiple-value-bind (val _base)
        (lingo-parse-number s)
      (or val (parse-integer s :radix 10 :junk-allowed t)))))

;;; Helper functions for standard EIGHTBOL node production

(defun lingo-make-procedure-node (name &key statements)
  "Create a procedure/method AST node."
  (make-method-node name :statements statements))

(defun make-conditional-or (left right)
  "Create a conditional OR node."
  (list :or left right))

(defun make-conditional-and (left right)
  "Create a conditional AND node."
  (list :and left right))

(defun make-conditional-not (expr)
  "Create a conditional NOT node."
  (list :not expr))

(defun make-conditional-eq (left right)
  "Create an equality comparison node."
  (list :eq left right))

(defun make-conditional-ne (left right)
  "Create a not-equal comparison node."
  (list :neq left right))

(defun make-conditional-lt (left right)
  "Create a less-than comparison node."
  (list :lt left right))

(defun make-conditional-gt (left right)
  "Create a greater-than comparison node."
  (list :gt left right))

(defun make-conditional-le (left right)
  "Create a less-than-or-equal comparison node."
  (list :le left right))

(defun make-conditional-ge (left right)
  "Create a greater-than-or-equal comparison node."
  (list :ge left right))

(defun make-expression-add (left right)
  "Create an addition expression node."
  (list :add :from left :to right))

(defun make-expression-subtract (left right)
  "Create a subtraction expression node."
  (list :subtract :from left :from-target right))

(defun make-expression-multiply (left right)
  "Create a multiplication expression node."
  (list :compute :target 'result :expression (list :* left right)))

(defun make-expression-divide (left right)
  "Create a division expression node."
  (list :compute :target 'result :expression (list :/ left right)))

(defun make-goto-node (label)
  "Create a goto (unconditional jump) AST node."
  (list :goto :target label))

(defun make-qualified-identifier (prop obj)
  "Create a qualified identifier (object.property) AST node."
  (list :property :object obj :name prop))

(defun make-perform-node (var &key from to by body)
  "Create a perform/loop AST node."
  (let ((node (list :perform :varying var)))
    (when from (setf node (append node (list :from from))))
    (when to (setf node (append node (list :to to))))
    (when by (setf node (append node (list :by by))))
    (when body (setf node (append node (list :body body))))
    node))

(defun make-move-node (value target)
  "Create a move/assignment AST node."
  (list :move :from value :to target))

(defun make-if-node (cond then-stmts else-stmts)
  "Create an if-conditional AST node."
  (let ((node (list :if :condition cond :then then-stmts)))
    (when else-stmts (setf node (append node (list :else else-stmts))))
    node))

(defun make-invoke-node (target &key args)
  "Create an invocation (method/function call) AST node."
  (declare (ignore args))
  (list :invoke :target target))

(defun make-goback-node ()
  "Create a return/goback AST node."
  (list :goback))
