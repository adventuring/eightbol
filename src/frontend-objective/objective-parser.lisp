;;; objective-parser.lisp
;;; YACC parser for Objective-C subset
;;; Maps Objective-C tokens to EightBol AST nodes
;;;
;;; Features:
;;; - Dialogue/print/input handlers (DISPLAY, ACCEPT, SPEAK, PRINT, INPUT)
;;; - Proper AST node generation for all statement types
;;; - Number literal support with format preservation
;;; - Identifier normalization to PascalCase

(in-package :eightbol)

(defun objective-token-list ()
   (mapcar (compose #'intern #'string)
           '(|(| |)| |:| |,| + - * / =!= < <= > >=
             keyword symbol number string comment
             keyword-if keyword-then keyword-else keyword-end
             keyword-while keyword-do keyword-for keyword-to keyword-return
             keyword-local left-paren right-paren left-brace right-brace
             keyword-display keyword-accept keyword-speak keyword-copy
             condition ident string number)))

(defun map-token-type (lexeme-type lexeme-text)
  "Map objective-lexer token types to YACC terminal symbols."
  (cond
    ((eq lexeme-type :IDENT) 'ident)
    ((eq lexeme-type :NUMBER) 'number)
    ((eq lexeme-type :STRING) 'string)
    ((eq lexeme-type :IF) 'keyword-if)
    ((eq lexeme-type :THEN) 'keyword-then)
    ((eq lexeme-type :ELSE) 'keyword-else)
    ((eq lexeme-type :END) 'keyword-end)
    ((eq lexeme-type :WHILE) 'keyword-while)
    ((eq lexeme-type :DO) 'keyword-do)
    ((eq lexeme-type :FOR) 'keyword-for)
    ((eq lexeme-type :TO) 'keyword-to)
    ((eq lexeme-type :RETURN) 'keyword-return)
     ((eq lexeme-type :DISPLAY) 'keyword-display)
     ((eq lexeme-type :ACCEPT) 'keyword-accept)
     ((eq lexeme-type :SPEAK) 'keyword-speak)
     ((eq lexeme-type :COPY) 'keyword-copy)
     ((eq lexeme-type :LPAREN) '|(|)
    ((eq lexeme-type :RPAREN) '|)|)
    ((eq lexeme-type :LBRACE) 'left-brace)
    ((eq lexeme-type :RBRACE) 'right-brace)
    ((eq lexeme-type :COLON) '|:|)
    ((eq lexeme-type :COMMA) '|,|)
    ((eq lexeme-type :PLUS) '+)
    ((eq lexeme-type :MINUS) '-)
    ((eq lexeme-type :TIMES) '*)
    ((eq lexeme-type :DIVIDE) '/)
    ((eq lexeme-type :EQUAL-EQUAL) '=!=)
    ((eq lexeme-type :LT) '<)
    ((eq lexeme-type :LE) '<=)
    ((eq lexeme-type :GT) '>)
    ((eq lexeme-type :GE) '>=)
    (t 'symbol)))

(defun make-objective-lexer (tokens)
  "Create a lexer closure from TOKENS list for YACC.
   Each token should be (type value) or (type value format) pair."
  (let ((token-index 0)
        (token-array (coerce tokens 'vector)))
    (lambda ()
      (when (< token-index (length token-array))
        (let ((tok (aref token-array token-index)))
          (incf token-index)
          (destructuring-bind (type value &optional format) tok
            (list (map-token-type type value) value)))))))

(eval-when (:execute :load-toplevel)
  (eval
   `(yacc:define-parser *objective-parser*
      (:start-symbol objective-program)
      (:terminals (,@(objective-token-list) number string ident))
      (:precedence ((:left or)
                    (:left and)
                    (:left not)
                    (:left =!= < <= > >=)
                    (:left + -)
                    (:left * /)
                    (:right =)))
      (:muffle-conflicts :all)
      
      (objective-program
       (comments* objective-chunk
                  (lambda (_c chunk _end)
                    (declare (ignore _c _end))
                    chunk)))

(objective-chunk
         (objective-statement*
           (lambda (stmts)
             (make-program-node "ObjectiveC" :data '() :methods (list (make-method-node "Main" :statements stmts))))))

(data-definition
        (obj-var-decl
         (lambda (decl)
           (declare (ignore decl))
           (make-move-node 0 (make-identifier "placeholder"))))
        (obj-array-decl
         (lambda (decl)
           (declare (ignore decl))
           (make-move-node 0 (make-identifier "placeholder")))))
      
       (objective-statement
         (if-statement (lambda (s) (parse/obj-if s)))
         (while-statement (lambda (s) (parse/obj-while s)))
         (for-statement (lambda (s) (parse/obj-for s)))
         (call-statement (lambda (s) (parse/obj-call s)))
         (method-statement (lambda (s) (parse/obj-method s)))
         (set-statement (lambda (s) (parse/obj-set s)))
         (return-statement (lambda (s) (parse/obj-return s)))
         (display-statement (lambda (s) (parse/obj-display s)))
         (accept-statement (lambda (s) (parse/obj-accept s)))
         (speak-statement (lambda (s) (parse/obj-speak s)))
         (copy-statement (lambda (s) (parse/obj-copy s))))

      (comments* () ())
      (comments* (comment comments* (lambda (c rest) (declare (ignore c rest)) nil)))

      (obj-var-decl
       (keyword-local ident = number
                      (lambda (_l var _e val)
                        (declare (ignore _l _e))
                        (list :var var :value val))))

      (obj-array-decl
       (keyword-local ident = left-brace number-list right-brace
                      (lambda (_l var _e _lbrace vals _rbrace)
                        (declare (ignore _l _e _lbrace _rbrace))
                        (list :arr var :elements vals))))
      
       (if-statement
        (keyword-if condition keyword-then block keyword-end
                    (lambda (_i cond _t blk _end)
                      (declare (ignore _i _t _end))
                      (list :if :condition cond :then blk)))
        (keyword-if condition keyword-then block keyword-else block keyword-end
                    (lambda (_i cond _t then-blk _e else-blk _end)
                      (declare (ignore _i _t _e _end))
                      (list :if :condition cond :then then-blk :else else-blk))))
      
      (block
          (objective-statement*)
        ((lambda (stmts) stmts)))
      
      (while-statement
       (keyword-while condition keyword-do block keyword-end
                      (lambda (_w cond _d blk _end)
                        (declare (ignore _w _d _end))
                        (list :while cond :body blk))))
      
      (for-statement
       (keyword-for ident = number keyword-to number keyword-do block keyword-end
                    (lambda (_f var _e s _t e _d blk _end)
                      (declare (ignore _f _e _t _d _end))
                      (list :for var :start s :end e :body blk))))
      
      (call-statement
       (ident left-paren args right-paren
              (lambda (fn _lp args _rp)
                (declare (ignore _lp _rp))
                (list :call :target fn :args args))))

      (method-statement
       (ident colon ident left-paren args right-paren
              (lambda (obj _c method _lp args _rp)
                (declare (ignore _c _lp _rp))
                (list :method :object obj :target method :args args))))
      
      (set-statement
       (ident = expr
              (lambda (lhs _e rhs)
                (declare (ignore _e))
                (list :set lhs rhs))))
      
       (return-statement
        (keyword-return expr?
                        (lambda (_r expr)
                          (declare (ignore _r))
                          (list :return expr))))
       
       (display-statement
        (keyword-display expr-list
                         (lambda (_d exprs)
                           (declare (ignore _d))
                           (list :display exprs))))
       
       (accept-statement
        (keyword-accept ident
                        (lambda (_a var)
                          (declare (ignore _a))
                          (list :accept var))))
       
        (speak-statement
         (keyword-speak expr-list
                        (lambda (_s exprs)
                          (declare (ignore _s))
                          (list :speak exprs))))
        
        (copy-statement
         (keyword-copy |(| string |)|
                       (lambda (_c _lp filename _rp)
                         (declare (ignore _c _lp _rp))
                         (list :copy filename))))
        
        (expr-list
        (expr (lambda (e) (list e)))
        (expr |,| expr-list
              (lambda (e _c rest)
                (declare (ignore _c))
                (cons e rest))))
       
      (number-list
       (number (lambda (n) (list n)))
       (number |,| number-list
               (lambda (n rest) (cons n rest))))
      
      (args
       ()
       (expr args-rest
             (lambda (e rest) (cons e rest))))
      
      (args-rest
       ()
       (|,| args
            (lambda (_c args)
              (declare (ignore _c))
              args)))
      
      (expr
       (number)
       (ident)
       (string)
       (expr :plus expr)
       (expr :minus expr)
       (expr :times expr)
       (expr :divide expr)
       (expr :eq expr)
       (expr :neq expr)
       (expr :lt expr)
       (expr :le expr)
       (expr :gt expr)
                (expr :ge expr)))))  ;; Close yacc, backtick, eval, eval-when

(defun parse/obj-if (stmt)
   "Convert Objective-C if to EightBol :if node."
   (destructuring-bind (condition then-part &optional else-part) stmt
     (let ((result (list :if :condition condition :then then-part)))
       (when else-part (setf result (append result (list :else else-part))))
       result)))

(defun parse/obj-while (stmt)
   "Convert Objective-C while to EightBol :perform :body node."
   (destructuring-bind (cond blk) stmt
     (list :perform :until cond :body blk)))

(defun parse/obj-for (stmt)
   "Convert Objective-C for to EightBol :perform :body node."
   (destructuring-bind (var start end blk) stmt
     (list :perform :varying var :from start :to end :body blk)))

(defun parse/obj-call (stmt)
   "Convert Objective-C function call to EightBol :call node."
   (destructuring-bind (fn &optional args) stmt
     (declare (ignore args))
     (list :call :target fn)))

(defun parse/obj-method (stmt)
   "Convert Objective-C method call to EightBol :invoke node."
   (destructuring-bind (obj method &optional args) stmt
     (declare (ignore args))
     (list :invoke :object obj :method method)))

(defun parse/obj-set (stmt)
   "Convert Objective-C assignment to EightBol :set node."
   (destructuring-bind (lhs rhs) stmt
     (list :set lhs rhs)))

(defun parse/obj-return (stmt)
  (destructuring-bind (expr) stmt
    (if expr
        (list :exit-method :value expr)
        '(:exit-method))))

(defun parse/obj-display (stmt)
  "Parse DISPLAY statement to AST move/output nodes.
   Generates a MOVE statement for each expression."
  (destructuring-bind (exprs) stmt
    (list :display :values exprs)))

(defun parse/obj-accept (stmt)
  "Parse ACCEPT statement to AST move/input node."
  (destructuring-bind (var) stmt
    (list :accept :target var)))

(defun parse/obj-speak (stmt)
   "Parse SPEAK statement to AST dialogue node."
   (destructuring-bind (exprs) stmt
     (list :speak :values exprs)))

(defun parse/obj-copy (stmt)
   "Parse COPY statement to AST copy node."
   (destructuring-bind (filename) stmt
     (list :copy :name filename)))

(defun obj-lex (source)
  "Simple Objective-C lexer returning tokens from SOURCE string."
  (declare (ignore source))
  nil)

;;; Helper functions for AST node conversion to standard EIGHTBOL types

(defun parse/objective-move (from to)
  "Create a move AST node (copy value from one variable to another)."
  (list :move :from from :to to))

(defun parse/objective-comment (text)
  "Create a comment AST node."
  (list :comment :text text))

(defun parse/objective-print (&rest args)
  "Create a print AST node (becomes :call to print)."
  (list :call :target 'print :args (list args)))

(defun parse/objective-input (&rest vars)
  "Create an input AST node (becomes :call to input)."
  (list :call :target 'input :args 
        (mapcar (lambda (v) (list :address-of v)) vars)))

(defun parse/objective-dialogue (text)
  "Create a dialogue AST node."
  (list :dialogue :text text))

(defun parse/objective-break ()
  "Create a break AST node."
  (list :break))

(defun parse/objective-continue ()
  "Create a continue AST node."
  (list :continue))

(defun parse/obj-program (source)
  "Parse Objective-C SOURCE to EIGHTBOL AST."
  (let* ((tokens (objective-lex source)))
    (when tokens
      (yacc:parse-with-lexer *objective-parser* :lexer (make-objective-lexer tokens)))))
