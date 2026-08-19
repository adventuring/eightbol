;; src/frontend-sci/sci-transpile.lisp — SCI compilation driver
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defun compile-sci-from-path (path
                              &key (cpus +supported-cpus+)
                                   output-file
                                   root-directory)
  "Compile SCI source file from PATH via parse-sci → compile-ast-program."
  (let* ((source (uiop:read-file-string path))
         (tokens (sci-lex-source source))
         (ast (yacc:parse-with-lexer
               (let ((token-index 0))
                 (lambda ()
                   (if (< token-index (length tokens))
                       (let ((token (elt tokens token-index)))
                         (incf token-index)
                         (values (car token) (cdr token)))
                       (values nil nil))))
               *sci-parser*)))
    (compile-ast-program (if (listp ast) ast (list ast))
                         :cpus cpus
                         :output-file output-file
                         :copybook-paths nil
                         :root-directory (or root-directory (truename #p".")))))

;; Export the function
(defun |compile-sci-from-path| (&rest args)
  (apply #'compile-sci-from-path args))

(export '(compile-sci-from-path |compile-sci-from-path|))
