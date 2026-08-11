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
(defun agi-parse-program (statements)
  "Top-level program: list of statements → AST."
  (make-program-node 'agi-program
                     :data (list (cons 'main-block statements))))

(defun agi-parse-if (cond then-stmt)
  "IF cond THEN stmt"
  (make-if-node cond (list then-stmt) '()))

(defun agi-parse-if-else (cond then-stmt else-stmt)
  "IF cond THEN stmt ELSE stmt"
  (make-if-node cond (list then-stmt) (list else-stmt)))

(defun agi-parse-gosub (routine-name)
  "GOSUB routine-name"
  (make-call-node routine-name))

(defun agi-parse-goto (label)
  "GOTO label"
  (make-call-node label))

(defun agi-parse-newroom (room-id)
  "NEWROOM room-id — Load and display new map/room"
  (make-move-node room-id (make-identifier "Current.Room")))

(defun agi-parse-loadlogics (script-id)
  "LOADLOGICS script-id — Load and execute script/logic"
  (make-call-node script-id))

(defun agi-parse-quit ()
  "QUIT — End game/return to title"
  (make-goback-node))

(defun agi-parse-return ()
  "RETURN — Return from subroutine"
  (make-goback-node))

(defun agi-parse-assignment (target value)
  "target = value"
  (make-move-node value target))

(defun agi-parse-assign (target value)
  "ASSIGN target value — Set variable to value"
  (make-move-node value target))

(defun agi-parse-posn (left top right bottom &optional variable)
  "POSN left top right bottom [variable] — Check bounding box
Set variable to 1 if a condition is met within bounding box, 0 otherwise."
  (let ((bbox-check (list 'posn left top right bottom)))
    (if variable
        (make-move-node bbox-check variable)
        bbox-check)))

(defun agi-parse-print (message &optional x y)
  "PRINT \"message\" [at x,y] — Display text on screen"
  (list :print-statement :message message :x x :y y))

(defun agi-parse-input (prompt-var &optional max-length)
  "INPUT prompt-var [max-length] — Get user input"
  (list :input-statement :target prompt-var :max-length max-length))

(defun agi-parse-dialogue (speaker text)
  "DIALOGUE \"speaker\" \"text\" — Display dialogue"
  (list :dialogue-statement :speaker speaker :text text))

(defun agi-parse-increment (var)
  "INCREMENT var"
  (list :+ (make-identifier var) (make-literal-number 1)))

(defun agi-parse-decrement (var)
  "DECREMENT var"
  (list :- (make-identifier var) (make-literal-number 1)))

(defun agi-parse-arithmetic (op left right)
  "Arithmetic operation."
  (list op left right))

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
(eval-when (:execute :load-toplevel)
  (eval
   `(yacc:define-parser *agi-parser*
      (:start-symbol program)
      (:terminals (,@(agi-token-list)
                   number string ident))
      (:precedence ((:left or)
                    (:left and)
                    (:left not)
                    (:left = <> <= < >= >)
                    (:left + -)
                    (:left * /)
                    (:left << >>)))

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
       (if-statement)
       (gosub-statement)
       (goto-statement)
       (newroom-statement)
       (loadlogics-statement)
       (quit-statement)
       (return-statement)
       (assign-statement)
       (assign-value-statement)
       (posn-statement)
       (print-statement)
       (input-statement)
       (dialogue-statement)
       (increment-statement)
       (decrement-statement)
       (set-statement))

      (if-statement
       (:IF condition :THEN statement
            (lambda (c s) (agi-parse-if c s)))
       (:IF condition :THEN statement :ELSE statement
            (lambda (c s1 s2) (agi-parse-if-else c s1 s2))))

      (gosub-statement
       (:GOSUB ident
               (lambda (id) (agi-parse-gosub id))))

      (goto-statement
       (:GOTO ident
              (lambda (id) (agi-parse-goto id))))

      (newroom-statement
       (:NEWROOM number
                 (lambda (room-id) (agi-parse-newroom (parse-integer room-id))))
       (:NEWROOM ident
                 (lambda (room-var) (agi-parse-newroom (make-identifier room-var)))))

      (loadlogics-statement
       (:LOADLOGICS number
                    (lambda (script-id) (agi-parse-loadlogics (parse-integer script-id))))
       (:LOADLOGICS ident
                    (lambda (script-var) (agi-parse-loadlogics (make-identifier script-var)))))

      (quit-statement
       (:QUIT
        (lambda () (agi-parse-quit))))

      (return-statement
       (:RETURN
         (lambda () (agi-parse-return))))

      (assign-statement
       (:ASSIGN ident expression
                (lambda (id expr) (agi-parse-assign (make-identifier id) expr))))

      (assign-value-statement
       (ident :EQUAL expression
              (lambda (id expr) (agi-parse-assignment (make-identifier id) expr))))

      (posn-statement
        (:POSN number number number number
               (lambda (l top r b) (agi-parse-posn l top r b)))
        (:POSN number number number number ident
               (lambda (l top r b id) (agi-parse-posn l top r b (make-identifier id)))))

      (print-statement
       (:PRINT string
               (lambda (msg) (agi-parse-print msg)))
       (:PRINT string number number
               (lambda (msg x y) (agi-parse-print msg x y))))

      (input-statement
       (:INPUT ident
               (lambda (var) (agi-parse-input (make-identifier var))))
       (:INPUT ident number
               (lambda (var len) (agi-parse-input (make-identifier var) (parse-integer len)))))

      (dialogue-statement
       (:DIALOGUE string string
                  (lambda (speaker text) (agi-parse-dialogue speaker text))))

      (increment-statement
       (:INCREMENT ident
                   (lambda (id) (agi-parse-increment id))))

      (decrement-statement
       (:DECREMENT ident
                   (lambda (id) (agi-parse-decrement id))))

      (set-statement
       (:SET ident
             (lambda (id) (agi-parse-assignment (make-identifier id) (make-literal-number 1))))
       (:RESET ident
               (lambda (id) (agi-parse-assignment (make-identifier id) (make-literal-number 0)))))

      ;; Conditions and expressions
      (condition
       (expression
        (lambda (e) e)))

      (expression
       (primary
        (lambda (p) p))
       (expression :+ expression
                   (lambda (l r) (agi-parse-arithmetic :add l r)))
       (expression :- expression
                   (lambda (l r) (agi-parse-arithmetic :subtract l r)))
       (expression :* expression
                   (lambda (l r) (agi-parse-arithmetic :times l r)))
       (expression :/ expression
                   (lambda (l r) (agi-parse-arithmetic :divide l r)))
       (expression :<< expression
                   (lambda (l r) (agi-parse-arithmetic :lshift l r)))
       (expression :>> expression
                   (lambda (l r) (agi-parse-arithmetic :rshift l r)))
       (expression := expression
                   (lambda (l r) (agi-parse-comparison := l r)))
       (expression :<> expression
                   (lambda (l r) (agi-parse-comparison :ne l r)))
       (expression :< expression
                   (lambda (l r) (agi-parse-comparison :lt l r)))
       (expression :<= expression
                   (lambda (l r) (agi-parse-comparison :le l r)))
       (expression :> expression
                   (lambda (l r) (agi-parse-comparison :gt l r)))
       (expression :>= expression
                   (lambda (l r) (agi-parse-comparison :ge l r)))
       (expression :and expression
                   (lambda (l r) (agi-parse-boolean :and (list l r))))
       (expression :or expression
                   (lambda (l r) (agi-parse-boolean :or (list l r))))
       (:NOT expression
             (lambda (e) (agi-parse-boolean :not (list e))))
       (|(| expression |)|
            (lambda (e) e)))

      (primary
       (ident
        (lambda (id) (make-identifier id)))
       (number
        (lambda (n) (make-literal-number (parse-integer n))))
       (string
        (lambda (s) (make-literal-string s))))
      )))

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
