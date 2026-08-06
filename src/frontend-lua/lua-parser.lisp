;;; lua-parser.lisp
;;; YACC parser for Lua subset
;;; Maps Lua tokens to EightBol AST nodes

(in-package :eightbol)

(defun lua-token-list ()
  (mapcar (compose #'intern #'string)
          '(|(| |)| |:| |,| + - * / = == ~= < <= > >= 
            keyword symbol number string comment)))

(eval-when (:execute :load-toplevel)
  (eval
   `(yacc:define-parser *lua-parser*
      (:start-symbol lua-program)
      (:terminals (,@(lua-token-list) number string ident))
      (:precedence ((:left :or) (:left :and)
                                (:left :eq :neq :lt :le :gt :ge)
                                (:left :plus :minus)
                                (:left :times :divide)
                                (:right :power :not)))
      (:muffle-conflicts :some)

      (lua-program
       (comments* lua-chunk
                  (lambda (_ignored-comments chunk)
                    (declare (ignore _ignored-comments))
                    chunk)))

      (lua-chunk
       (data-definition* lua-statement*
                         (lambda (dd stmts)
                           (append dd stmts))))

      (data-definition
       (lua-var-decl
        (lambda (decl)
          (list :dd :level 01 :label (getf decl :var)
                    :attr (list :usage :binary :value (getf decl :value)))))
       (lua-array-decl
        (lambda (decl)
          (list :dd :level 01 :label (getf decl :arr)
                    :attr (list :usage :binary :occurs (list 'length (getf decl :elements)))))))

      (lua-statement
       (if-statement (lambda (s) (parse/lua-if s)))
       (while-statement (lambda (s) (parse/lua-while s)))
       (for-statement (lambda (s) (parse/lua-for s)))
       (call-statement (lambda (s) (parse/lua-call s)))
       (method-statement (lambda (s) (parse/lua-method s)))
       (set-statement (lambda (s) (parse/lua-set s)))
       (return-statement (lambda (s) (parse/lua-return s))))

      (comments* () ())
      (comments* (comment comments* (lambda (c rest) (declare (ignore c rest)) nil)))

      (lua-var-decl
       (keyword-local ident = number
                      (lambda (_local var _eq val)
                        (declare (ignore _local _eq))
                        (list :var var :value val))))

      (lua-array-decl
       (keyword-local ident = left-brace number-list right-brace
                      (lambda (_local var _eq _lbrace vals _rbrace)
                        (declare (ignore _local _eq _lbrace _rbrace))
                        (list :arr var :elements vals))))

      (if-statement
       (keyword-if condition keyword-then block keyword-else else-block keyword-end
                   (lambda (_if cond _then blk _else else_blk _end)
                     (declare (ignore _if _then _else))
                     (list :if cond :then blk :else else_blk))))

      (block
          (lua-statement*)
        ((lambda (stmts) stmts)))

      (while-statement
       (keyword-while condition keyword-do block keyword-end
                      (lambda (_while cond _do blk _end)
                        (declare (ignore _while _do _end))
                        (list :while cond :body blk))))

      (for-statement
       (keyword-for ident = number keyword-to number keyword-do block keyword-end
                    (lambda (_for _v start _to end _do blk _end)
                      (declare (ignore _for _v _to _do _end))
                      (list :perform :varying :from start :to end :with blk))))

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

(defun parse/lua-if (stmt)
  (destructuring-bind (cond blk else) stmt
    (declare (ignore else))
    (list :if :condition cond :then blk)))

(defun parse/lua-while (stmt)
  (destructuring-bind (cond blk) stmt
    (list :perform :until cond :with blk)))

(defun parse/lua-for (stmt)
  (destructuring-bind (var start end blk) stmt
    (declare (ignore var))
    (list :perform :varying :from start :to end :with blk)))

(defun parse/lua-call (stmt)
  (destructuring-bind (fn args) stmt
    (list :call :target fn :args args)))

(defun parse/lua-method (stmt)
  (destructuring-bind (obj method args) stmt
    (list :call :target method :object obj :args args)))

(defun parse/lua-set (stmt)
  (destructuring-bind (lhs rhs) stmt
    (list :set lhs rhs)))

(defun parse/lua-return (stmt)
  (destructuring-bind (expr) stmt
    (list :exit-method :value expr)))

(defun lua-lex (source)
  "Simple Lua lexer returning tokens from SOURCE string."
  (declare (ignore source))
  nil)

(defun parse/lua-program (source)
  "Parse Lua SOURCE to EIGHTBOL AST."
  (declare (ignore source))
  nil)
