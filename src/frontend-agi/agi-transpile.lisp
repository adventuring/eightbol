;; agi-transpile.lisp
;; Transpile for AGI (Adventure Game Interpreter) language

(in-package #:eightbol)

(defvar *agi-accumulator* nil)

(defun agi-transpile (ast)
  "Transpile AGI AST to internal AST format."
  (labels
      ((transpile-stmt (stmt)
         (cond
           ;; IF statement
           ((eq (car stmt) :if)
            (list :if
                  (transpile-exp (cadr stmt))
                  (transpile-stmts (caddr stmt))
                  (transpile-stmts (cadddr stmt))))
           ;; SET statement
           ((eq (car stmt) :set)
            (list :set
                  (cadr stmt)  ;; variable name
                  (transpile-exp (caddr stmt))))
           ;; CALL-ACC statement
           ((eq (car stmt) :call-acc)
            (list :call-acc
                  :target (cadr stmt)
                  :using (when (caddr stmt) (transpile-exp (caddr stmt)))))
           ;; GOBACK statement
           ((eq (car stmt) :goback)
            (list :goback
                  :using (when (cadr stmt) (transpile-exp (cadr stmt)))))
           ;; SAID statement
           ((eq (car stmt) :said)
            (list :said
                  (transpile-exp (cadr stmt))))
           ;; TEST statement
           ((eq (car stmt) :test)
            (list :test
                  (transpile-exp (cadr stmt))))
           ;; POSN statement
           ((eq (car stmt) :posn)
            (list :posn
                  (transpile-exp (cadr stmt))))
           ;; CONTROLLER statement
           ((eq (car stmt) :controller)
            (list :controller
                  (transpile-exp (cadr stmt))))
           ;; HAVEKEY statement
           ((eq (car stmt) :havekey)
            (list :havekey
                  (transpile-exp (cadr stmt))))
           (t
            (list :error (format nil "Unknown statement type: ~a" (car stmt))))))
       
       (transpile-stmts (stmts)
         (mapcar #'transpile-stmt stmts))
       
       (transpile-exp (exp)
         (cond
           ((atom exp) exp)
           ((eq (car exp) :literal) exp)
           ((eq (car exp) :variable) exp)
           ((eq (car exp) :dotted-variable)
            ;; Convert dotted variable to a list representation
            (cons :dotted-variable (cdr exp)))
           ((eq (car exp) :infix-op)
            (list :infix-op
                  (cadr exp)
                  (transpile-exp (caddr exp))
                  (transpile-exp (cadddr exp))))
           (t exp))))
    
    (transpile-stmts ast)))

(defun compile-agi-from-path (path
                              &key (cpus +supported-cpus+)
                                   output-file
                                   root-directory)
  "Compile AGI source file from PATH via parse-agi → compile-ast-program."
  (let ((source (uiop:read-file-string path)))
    (compile-ast-program (parse-agi source)
                         :cpus cpus
                         :output-file output-file
                         :copybook-paths nil
                         :root-directory (or root-directory (truename #p".")))))

;; Export the function
(defun |compile-agi-from-path| (&rest args)
  (apply #'compile-agi-from-path args))

(export '(compile-agi-from-path |compile-agi-from-path|))
