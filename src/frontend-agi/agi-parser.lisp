;; src/frontend-agi/agi-parser.lisp — YACC grammar for AGI → EIGHTBOL AST
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Token list for AGI YACC parser
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun agi-token-list ()
    (mapcar (compose #'intern #'string)
            '(|(| |)| |,| |:| + - * / = <> < <= > >=
              if else then gosub goto newroom loadlogics return
              quit assign increment decrement set reset said
              test posn controller havekey and or not
              << >>
              number string ident))))

;;; Parser action functions for AGI
;;; NOTE: These producers emit the CANONICAL AST shapes from src/ast.lisp
;;; explicitly — several convenience constructors (make-move-node,
;;; make-literal-number, …) are redefined by later-loaded frontends and the
;;; shadowed variants do not match the shared backend contract.
(defun agi-parse-program (statements)
  "Top-level program: list of statements → AST."
  (make-program-node "AgiProgram"
                     :methods (list (make-method-node "Main" :statements statements))))

(defun agi-parse-if (cond then-stmts &optional else-stmts)
  "IF cond THEN stmts [ELSE stmts]"
  (list :if :condition cond
        :then then-stmts
        :else (or else-stmts '())))

(defun agi-parse-if-else (cond then-stmts else-stmts)
  "IF cond THEN stmts ELSE stmts"
  (list :if :condition cond
        :then then-stmts
        :else else-stmts))

(defun agi-parse-gosub (routine-name)
  "GOSUB routine-name"
  (list :call :target routine-name))

(defun agi-parse-goto (label)
  "GOTO label"
  (list :goto :target label))

(defun agi-parse-newroom (room-id)
  "NEWROOM room-id — Load and display new map/room"
  (list :move :from room-id :to "Current.Room"))

(defun agi-parse-loadlogics (script-id)
  "LOADLOGICS script-id — Load and execute script/logic"
  (list :call :target script-id))

(defun agi-parse-quit ()
  "QUIT — End game/return to title"
  (list :goback))

(defun agi-parse-return ()
  "RETURN — Return from subroutine"
  (list :goback))

(defun agi-parse-assignment (target value)
  "target = value"
  (list :move :from value :to target))

(defun agi-parse-assign (target value)
  "ASSIGN target value — Set variable to value"
  (list :move :from value :to target))

(defun agi-parse-posn (left top right bottom &optional variable)
  "POSN left top right bottom [variable] — Check bounding box
Set variable to 1 if a condition is met within bounding box, 0 otherwise."
  (let ((bbox-check (list 'posn left top right bottom)))
    (if variable
        (list :move :from bbox-check :to variable)
        bbox-check)))

(defun agi-parse-print (message &optional x y)
  "PRINT \"message\" [at x,y] — Display text on screen"
  (declare (ignore x y))
  (list :print :expressions (list message)))

(defun agi-parse-input (prompt-var &optional max-length)
  "INPUT prompt-var [max-length] — Get user input"
  (declare (ignore max-length))
  (list :input :variables (list prompt-var)))

(defun agi-parse-dialogue (speaker text)
  "DIALOGUE \"speaker\" \"text\" — Display dialogue"
  (list :dialogue :speaker speaker :text text))

(defun agi-parse-increment (var)
  "INCREMENT var — add 1 to variable"
  (list :move :from (make-expression-add var 1) :to var))

(defun agi-parse-decrement (var)
  "DECREMENT var — subtract 1 from variable"
  (list :move :from (make-expression-subtract var 1) :to var))

(defun agi-parse-arithmetic (op left right)
  "Arithmetic operation."
  (case op
    (:add (make-expression-add left right))
    (:subtract (make-expression-subtract left right))
    (:times (make-expression-multiply left right))
    (:divide (make-expression-divide left right))
    (:lshift (make-expression-shift-left left right))
    (:rshift (make-expression-shift-right left right))
    (t (list op left right))))

(defun agi-parse-boolean (op operands)
  "Boolean operation."
  (case op
    (:and (apply #'make-conditional-and operands))
    (:or (apply #'make-conditional-or operands))
    (:not (make-conditional-not (car operands)))
    (t (list op operands))))

(defun agi-parse-comparison (op left right)
  "Comparison operation."
  (case op
    (:= (make-conditional-eq left right))
    (:ne (make-conditional-ne left right))
    (:lt (make-conditional-lt left right))
    (:le (make-conditional-le left right))
    (:gt (make-conditional-gt left right))
    (:ge (make-conditional-ge left right))
    (t (list op left right))))

;;; YACC parser definition
;;; Terminals are KEYWORD symbols — they must match the tokens emitted by
;;; agi-lex-line (see cl-yacc identity-based terminal matching). cl-yacc passes
;;; EVERY RHS semantic value to the action lambda (terminal lexemes included),
;;; so each action must accept one argument per RHS symbol.
(eval-when (:execute :load-toplevel)
  (eval
   `(yacc:define-parser *agi-parser*
      (:start-symbol program)
      (:terminals (:lparen :rparen :comma :colon
                   :plus :minus :times :divide
                   :equal :lt :le :gt :ge :ne
                   :lshift :rshift :dot
                   :if :else :end-if :then
                   :gosub :goto :newroom :loadlogics
                   :return :quit :assign :increment :decrement
                   :set :reset :said :test :posn :controller :havekey
                   :and :or :not :print :input :dialogue
                   :number :string :ident))
      (:precedence ((:left :or)
                    (:left :and)
                    (:left :not)
                    (:left :equal :ne :lt :le :gt :ge)
                    (:left :plus :minus)
                    (:left :times :divide)
                    (:left :lshift :rshift)))

      ;; Program is a list of statements
      (program
       (statement-list
        (lambda (stmts) (agi-parse-program stmts))))

      (statement-list
       (statement
        (lambda (s) (list s)))
       (statement-list statement
                       (lambda (list-stmts stmt) (append list-stmts (list stmt)))))

      ;; Statements
      (statement
       (if-statement (lambda (s) s))
       (gosub-statement (lambda (s) s))
       (goto-statement (lambda (s) s))
       (newroom-statement (lambda (s) s))
       (loadlogics-statement (lambda (s) s))
       (quit-statement (lambda (s) s))
       (return-statement (lambda (s) s))
       (assign-statement (lambda (s) s))
       (assign-value-statement (lambda (s) s))
       (posn-statement (lambda (s) s))
       (print-statement (lambda (s) s))
       (input-statement (lambda (s) s))
       (dialogue-statement (lambda (s) s))
       (increment-statement (lambda (s) s))
       (decrement-statement (lambda (s) s))
       (set-statement (lambda (s) s)))

      (if-statement
       (:if condition :then statement-list :end-if
            (lambda (kw-if cond kw-then then kw-end)
              (declare (ignore kw-if kw-then kw-end))
              (agi-parse-if cond then)))
       (:if condition :then statement-list :else statement-list :end-if
            (lambda (kw-if cond kw-then then kw-else else kw-end)
              (declare (ignore kw-if kw-then kw-else kw-end))
              (agi-parse-if-else cond then else))))

      (gosub-statement
       (:gosub :lparen :ident :rparen
               (lambda (kw-gosub kw-lparen id kw-rparen)
                 (declare (ignore kw-gosub kw-lparen kw-rparen))
                 (agi-parse-gosub id)))
       (:gosub :ident
               (lambda (kw-gosub id)
                 (declare (ignore kw-gosub))
                 (agi-parse-gosub id))))

      (goto-statement
       (:goto :ident
              (lambda (kw-goto id)
                (declare (ignore kw-goto))
                (agi-parse-goto id))))

      (newroom-statement
       (:newroom :number
                 (lambda (kw-newroom room-id)
                   (declare (ignore kw-newroom))
                   (agi-parse-newroom (parse-integer room-id))))
       (:newroom :ident
                 (lambda (kw-newroom room-var)
                   (declare (ignore kw-newroom))
                   (agi-parse-newroom (make-identifier room-var)))))

      (loadlogics-statement
       (:loadlogics :number
                    (lambda (kw-loadlogics script-id)
                      (declare (ignore kw-loadlogics))
                      (agi-parse-loadlogics (parse-integer script-id))))
       (:loadlogics :ident
                    (lambda (kw-loadlogics script-var)
                      (declare (ignore kw-loadlogics))
                      (agi-parse-loadlogics (make-identifier script-var)))))

      (quit-statement
       (:quit
        (lambda (kw-quit)
          (declare (ignore kw-quit))
          (agi-parse-quit))))

      (return-statement
       (:return
         (lambda (kw-return)
           (declare (ignore kw-return))
           (agi-parse-return))))

      (assign-statement
       (:assign :ident expression
                (lambda (kw-assign id expr)
                  (declare (ignore kw-assign))
                  (agi-parse-assign (make-identifier id) expr))))

      (assign-value-statement
       (:ident :equal expression
               (lambda (id kw-equal expr)
                 (declare (ignore kw-equal))
                 (agi-parse-assignment (make-identifier id) expr))))

      (posn-statement
        (:posn :number :number :number :number
               (lambda (kw-posn l top r b)
                 (declare (ignore kw-posn))
                 (agi-parse-posn l top r b)))
        (:posn :number :number :number :number :ident
               (lambda (kw-posn l top r b id)
                 (declare (ignore kw-posn))
                 (agi-parse-posn l top r b (make-identifier id)))))

      (print-statement
       (:print :string
               (lambda (kw-print msg)
                 (declare (ignore kw-print))
                 (agi-parse-print msg)))
       (:print :string :number :number
               (lambda (kw-print msg x y)
                 (declare (ignore kw-print))
                 (agi-parse-print msg x y))))

      (input-statement
       (:input :ident
               (lambda (kw-input var)
                 (declare (ignore kw-input))
                 (agi-parse-input (make-identifier var))))
       (:input :ident :number
               (lambda (kw-input var len)
                 (declare (ignore kw-input))
                 (agi-parse-input (make-identifier var) (parse-integer len)))))

      (dialogue-statement
       (:dialogue :string :string
                  (lambda (kw-dialogue speaker text)
                    (declare (ignore kw-dialogue))
                    (agi-parse-dialogue speaker text))))

      (increment-statement
       (:increment :lparen :ident :rparen
                   (lambda (kw-inc kw-lparen id kw-rparen)
                     (declare (ignore kw-inc kw-lparen kw-rparen))
                     (agi-parse-increment id)))
       (:increment :ident
                   (lambda (kw-inc id)
                     (declare (ignore kw-inc))
                     (agi-parse-increment id))))

      (decrement-statement
       (:decrement :lparen :ident :rparen
                   (lambda (kw-dec kw-lparen id kw-rparen)
                     (declare (ignore kw-dec kw-lparen kw-rparen))
                     (agi-parse-decrement id)))
       (:decrement :ident
                   (lambda (kw-dec id)
                     (declare (ignore kw-dec))
                     (agi-parse-decrement id))))

      (set-statement
       (:set :lparen :ident :comma expression :rparen
             (lambda (kw-set kw-lparen id kw-comma expr kw-rparen)
               (declare (ignore kw-set kw-lparen kw-comma kw-rparen))
               (agi-parse-assign (make-identifier id) expr)))
       (:set :ident
             (lambda (kw-set id)
               (declare (ignore kw-set))
               (agi-parse-assignment (make-identifier id) 1)))
       (:reset :ident
               (lambda (kw-reset id)
                 (declare (ignore kw-reset))
                 (agi-parse-assignment (make-identifier id) 0))))

      ;; Conditions and expressions
      (condition
       (expression
        (lambda (e) e)))

      (expression
       (primary
        (lambda (p) p))
       (expression :plus expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-arithmetic :add l r)))
       (expression :minus expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-arithmetic :subtract l r)))
       (expression :times expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-arithmetic :times l r)))
       (expression :divide expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-arithmetic :divide l r)))
       (expression :lshift expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-arithmetic :lshift l r)))
       (expression :rshift expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-arithmetic :rshift l r)))
       (expression :equal expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-comparison := l r)))
       (expression :ne expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-comparison :ne l r)))
       (expression :lt expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-comparison :lt l r)))
       (expression :le expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-comparison :le l r)))
       (expression :gt expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-comparison :gt l r)))
       (expression :ge expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-comparison :ge l r)))
       (expression :and expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-boolean :and (list l r))))
       (expression :or expression
                   (lambda (l kw-op r)
                     (declare (ignore kw-op))
                     (agi-parse-boolean :or (list l r))))
       (:not expression
             (lambda (kw-not e)
               (declare (ignore kw-not))
               (agi-parse-boolean :not (list e))))
       (:lparen expression :rparen
            (lambda (kw-lparen e kw-rparen)
              (declare (ignore kw-lparen kw-rparen))
              e)))

      (primary
       (:ident
        (lambda (id) id))
       (:number
        (lambda (n) (parse-integer n)))
       (:string
        (lambda (s) s)))
      )))

(defun parse-agi (source)
  "Parse AGI SOURCE string into a :program AST.
   Tokens come from agi-lex-source; terminals match the YACC grammar keywords."
  (agi-parse-from-tokens (agi-lex-source source)))

(defun agi-parse-from-tokens (tokens)
  "Parse token list into AGI AST."
  (yacc:parse-with-lexer
   (let ((token-index 0))
     (lambda ()
       (if (< token-index (length tokens))
           (let ((token (elt tokens token-index)))
             (incf token-index)
             (values (car token) (cdr token)))
           (values nil nil))))
   *agi-parser*))
