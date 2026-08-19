;; src/frontend-scumm/scumm-parser.lisp — YACC grammar for SCUMM → EIGHTBOL AST
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Token list for SCUMM YACC parser
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun scumm-token-list ()
    (mapcar (compose #'intern #'string)
            '(|(| |)| |[| |]| |;| |,|
              + - * / % = == != < > <= >=
              && || !
              if else endif while endwhile for endfor
              break continue return
              define set get put call exit goto label
              assert wait stop start fade restore save restart
              version to end
              dialogue print input
              number hex-number octal-number binary-number dword-number
              string ident))))

;;; Parser action functions for SCUMM

(defun scumm-parse-program (statements)
  "Top-level program node."
  (make-program-node "SCUMM" :data (when statements (list (cons 'main-block statements)))))

(defun scumm-parse-if (cond then-stmt &optional else-stmt)
  "if (cond) then-stmt [else else-stmt]"
  (make-if-node cond (list then-stmt) (if else-stmt (list else-stmt) '())))

(defun scumm-parse-while (cond body)
   "while (cond) body"
   (make-perform-node "WHILE" :until (make-conditional-not cond) :body (ensure-list body)))

(defun scumm-parse-for (var start end body)
   "for var = start to end body"
   (make-perform-node "FOR" :varying var :from start :by 1
                      :until (make-conditional-gt (make-identifier var) end) :body (ensure-list body)))

(defun scumm-parse-set (var expr)
  "set var = expr"
  (make-move-node expr (make-identifier var)))

(defun scumm-parse-define (name body)
  "define name to body end"
  (make-method-node name :statements (list body)))

(defun scumm-parse-call (name args)
   "call name(args)"
   (make-call-node name))

(defun scumm-parse-return (&optional val)
  "return [val]"
  (make-goback-node))

(defun scumm-parse-goto (label)
  "goto label"
  (make-goto-node label))

(defun scumm-parse-print (msg &optional args)
  "print msg [, args...]
   Produces a :print AST node for output statements."
  (make-print-node (cons msg (or args '()))))

(defun scumm-parse-input (prompt var)
  "input prompt var
   Produces an :input AST node for reading user input."
  (make-input-node (make-identifier var) :prompt prompt))

(defun scumm-parse-dialogue (npc msg &optional responses)
  "dialogue npc says msg [with responses]
   Produces a :dialogue AST node for NPC dialogue."
  (make-dialogue-node :speaker npc :text msg
                      :statements (or responses '())))

(defun scumm-parse-number (token-value token-type)
  "Parse number literals based on token type.
   
   Handles:
   - :number (decimal)
   - :hex-number (0x prefix)
   - :octal-number (0o prefix)
   - :binary-number (0b prefix)
   - :dword-number (0d\"WORD\" format)"
  (case token-type
    (:hex-number (parse-hex-literal token-value))
    (:octal-number (parse-octal-literal token-value))
    (:binary-number (parse-binary-literal token-value))
    (:dword-number (parse-dword-literal token-value))
    (t (parse-integer token-value))))

;;; Create the YACC parser
(eval-when (:execute :load-toplevel)
  (eval
   `(yacc:define-parser *scumm-parser*
      (:start-symbol program)
      (:terminals (lparen rparen lbracket rbracket semicolon comma
                   plus minus times divide mod assign
                   equal ne lt gt le ge
                   and or not
                   if else endif while endwhile for endfor
                   break continue return
                   define set get put call exit goto label
                   assert wait stop start fade restore save restart
                   version to end
                   dialogue print input
                   number hex-number octal-number binary-number dword-number
                   string ident))
      
      (program
       (statement
        (lambda (s) (scumm-parse-program (list s))))
       (program statement
        (lambda (prog s) (nconc prog (list s)))))
      
      (statement
       (if-stmt
        (lambda (s) s))
       (while-stmt
        (lambda (s) s))
       (for-stmt
        (lambda (s) s))
       (define-stmt
        (lambda (s) s))
       (assignment
        (lambda (a) a))
       (call-stmt
        (lambda (c) c))
       (return-stmt
        (lambda (r) r))
       (goto-stmt
        (lambda (g) g))
       (print-stmt
        (lambda (p) p))
       (input-stmt
        (lambda (i) i))
       (dialogue-stmt
        (lambda (d) d))
       (expr-stmt
        (lambda (e) e)))
      
      (if-stmt
       (if lparen expression rparen statement
        (lambda (cond stmt) (scumm-parse-if cond stmt)))
       (if lparen expression rparen statement else statement endif
        (lambda (cond s1 s2) (scumm-parse-if cond s1 s2))))
      
      (while-stmt
       (while lparen expression rparen statement endwhile
        (lambda (cond stmt) (scumm-parse-while cond stmt))))
      
      (for-stmt
       (for ident assign expression to expression statement endfor
        (lambda (v s e stmt) (scumm-parse-for v s e stmt))))
      
      (define-stmt
       (define ident to statement end
        (lambda (name stmt) (scumm-parse-define name stmt))))
      
      (assignment
       (set ident assign expression semicolon
        (lambda (v e) (scumm-parse-set v e))))
      
      (call-stmt
       (call ident lparen rparen semicolon
        (lambda (name) (scumm-parse-call name '())))
       (call ident lparen expr-list rparen semicolon
        (lambda (name args) (scumm-parse-call name args))))
      
      (return-stmt
       (return semicolon
        (lambda () (scumm-parse-return)))
       (return expression semicolon
        (lambda (e) (scumm-parse-return e))))
      
      (goto-stmt
       (goto label semicolon
        (lambda (lbl) (scumm-parse-goto lbl))))
      
      (print-stmt
       (print string semicolon
        (lambda (msg) (scumm-parse-print msg)))
       (print string comma expr-list semicolon
        (lambda (msg args) (scumm-parse-print msg args))))
      
      (input-stmt
       (input string ident semicolon
        (lambda (prompt var) (scumm-parse-input prompt var))))
      
      (dialogue-stmt
       (dialogue ident string semicolon
        (lambda (npc msg) (scumm-parse-dialogue npc msg)))
       (dialogue ident string comma expr-list semicolon
        (lambda (npc msg responses) (scumm-parse-dialogue npc msg responses))))
      
      (expr-stmt
       (expression semicolon
        (lambda (e) e)))
      
      (expression
       (ident
        (lambda (id) (make-identifier id)))
       (number-literal
        (lambda (n) n))
       (string
        (lambda (s) s))
       (expression plus expression
        (lambda (l r) (make-expression-add l r)))
       (expression minus expression
        (lambda (l r) (make-expression-subtract l r)))
       (expression times expression
        (lambda (l r) (make-expression-multiply l r)))
       (expression divide expression
        (lambda (l r) (make-expression-divide l r)))
       (expression mod expression
        (lambda (l r) (list :mod l r)))
       (expression equal expression
        (lambda (l r) (make-conditional-eq l r)))
       (expression ne expression
        (lambda (l r) (make-conditional-ne l r)))
       (expression lt expression
        (lambda (l r) (make-conditional-lt l r)))
       (expression gt expression
        (lambda (l r) (make-conditional-gt l r)))
       (expression le expression
        (lambda (l r) (make-conditional-le l r)))
       (expression ge expression
        (lambda (l r) (make-conditional-ge l r)))
       (expression and expression
        (lambda (l r) (make-conditional-and l r)))
       (expression or expression
        (lambda (l r) (make-conditional-or l r)))
       (not expression
        (lambda (e) (make-conditional-not e)))
       (lparen expression rparen
        (lambda (e) e))
       (ident lbracket expression rbracket
        (lambda (id e) (list :subscript id e))))
      
      (number-literal
       (number
        (lambda (n) (parse-integer n)))
       (hex-number
        (lambda (n) (scumm-parse-number n :hex-number)))
       (octal-number
        (lambda (n) (scumm-parse-number n :octal-number)))
       (binary-number
        (lambda (n) (scumm-parse-number n :binary-number)))
       (dword-number
        (lambda (n) (scumm-parse-number n :dword-number))))
      
      (expr-list
       (expression
        (lambda (e) (list e)))
       (expr-list comma expression
        (lambda (el e) (nconc el (list e))))))))

(defun scumm-parser ()
  "Return the SCUMM YACC parser function."
  *scumm-parser*)
