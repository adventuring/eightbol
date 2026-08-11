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
                  (make-procedure-node (lingo-make-ident name) :statements body))))

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
