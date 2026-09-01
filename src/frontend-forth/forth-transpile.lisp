;; src/frontend-forth/forth-transpile.lisp — Forth compilation entry point
;;; Copyright © 2026 Interworldly Adventuring, LLC
;;
;; Transforms Forth parser output (canonical AST nodes) into the standard
;; EIGHTBOL AST consumed by all back-ends. The parser already emits
;; canonical nodes (make-if-node, make-perform-node, make-call-node, etc.)
;; via grammar-build.lisp constructors, so this layer is intentionally thin:
;; it walks the parser's method-level output and normalises a few Forth-isms
;; that don't map 1:1 to canonical form.
(in-package :eightbol)

(defun compile-forth-from-path (input-file &key cpus output-file)
  "Compile Forth source file to assembly for specified CPUs.
INPUT-FILE: Path to .forth source file.
CPUS: List of target CPU keywords (e.g. :6502 :z80 :arm7).
OUTPUT-FILE: Optional output assembly file path."
  (let ((ast (forth-compile-file input-file)))
    (when output-file
      (with-open-file (out output-file :direction :output :if-exists :supersede)
        (write-ast ast out)))
    (when cpus
      (dolist (cpu cpus)
        (compile-to-assembly cpu ast)))
    ast))

(defun compile-forth-string (source-string &key (program-name "Script") cpus)
  "Compile Forth SOURCE-STRING to canonical EIGHTBOL AST.
When CPUS is supplied, also compile to each target."
  (let ((ast (forth-compile-source source-string program-name)))
    (when cpus
      (dolist (cpu cpus)
        (compile-to-assembly cpu ast)))
    ast))
