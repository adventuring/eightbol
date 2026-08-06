;;; objective-parser.lisp
;;; YACC parser for Objective-C subset
;;; Maps Objective-C tokens to EightBol AST nodes

(in-package :eightbol)

(defun objective-token-list ()
  (mapcar (compose #'intern #'string)
          '(|(| |)| |:| |,| + - * / =!= < <= > >=
            keyword symbol number string comment)))

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
       (data-definition* objective-statement*
                         (lambda (dd stmts)
                           (append dd stmts))))

      (data-definition
       (obj-var-decl
        (lambda (decl)
          (list :dd :level 01 :label (getf decl :var)
                    :attr (list :usage :binary :value (getf decl :value)))))
       (obj-array-decl
        (lambda (decl)
          (list :dd :level 01 :label (getf decl :arr)
                    :attr (list :usage :binary :occurs (list 'length (getf decl :elements)))))))
      
      (objective-statement
       (if-statement (lambda (s) (parse/obj-if s)))
       (while-statement (lambda (s) (parse/obj-while s)))
       (for-statement (lambda (s) (parse/obj-for s)))
       (call-statement (lambda (s) (parse/obj-call s)))
       (method-statement (lambda (s) (parse/obj-method s)))
       (set-statement (lambda (s) (parse/obj-set s)))
       (return-statement (lambda (s) (parse/obj-return s))))

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
       (keyword-if condition keyword-then block keyword-else else-block keyword-end
                   (lambda (_i cond _t blk _e else-block  _end)
                     (declare (ignore _i _t _e _end))
                     (list :if cond :then blk :else else-blocks
                           e))))
      
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
       (expr :ge expr)))))

(defun parse/obj-if (stmt)
  (destructuring-bind (cond blk else) stmt
    (declare (ignore else))
    (list :if :condition cond :then blk)))

(defun parse/obj-while (stmt)
  (destructuring-bind (cond blk) stmt
    (list :perform :until cond :with blk)))

(defun parse/obj-for (stmt)
  (destructuring-bind (var start end blk) stmt
    (declare (ignore var))
    (list :perform :varying :from start :to end :with blk)))

(defun parse/obj-call (stmt)
  (destructuring-bind (fn args) stmt
    (list :call :target fn :args args)))

(defun parse/obj-method (stmt)
  (destructuring-bind (obj method args) stmt
    (list :method :object obj :target method :args args)))

(defun parse/obj-set (stmt)
  (destructuring-bind (lhs rhs) stmt
    (list :set :target lhs :value rhs)))

(defun parse/obj-return (stmt)
  (destructuring-bind (expr) stmt
    (list :exit-method :value expr)))

(defun obj-lex (source)
  "Simple Objective-C lexer returning tokens from SOURCE string."
  (declare (ignore source))
  nil)

(defun parse/obj-program (source)
  "Parse Objective-C SOURCE to EIGHTBOL AST."
  (declare (ignore source))
  nil)
