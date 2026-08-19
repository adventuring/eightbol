;; src/frontend-scumm/scumm-transpile.lisp — SCUMM compilation driver
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defun compile-scumm-from-path (path
                                &key (cpus +supported-cpus+)
                                     output-file
                                     root-directory)
  "Compile SCUMM source file from PATH via parse-scumm → compile-ast-program."
  (let* ((source (uiop:read-file-string path))
         (tokens (scumm-lex-source source))
         (ast (yacc:parse-with-lexer
               (let ((token-index 0))
                 (lambda ()
                   (if (< token-index (length tokens))
                       (let ((token (elt tokens token-index)))
                         (incf token-index)
                         (values (car token) (cdr token)))
                       (values nil nil))))
               *scumm-parser*)))
    (compile-ast-program (if (listp ast) ast (list ast))
                         :cpus cpus
                         :output-file output-file
                         :copybook-paths nil
                         :root-directory (or root-directory (truename #p".")))))

;; Export the function
(defun |compile-scumm-from-path| (&rest args)
  (apply #'compile-scumm-from-path args))

(export '(compile-scumm-from-path |compile-scumm-from-path|))
