;; src/frontend-garry/garry-transpile.lisp — Garry compilation entry point
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

(defun compile-garry-from-path (input-file &key cpus output-file)
  "Compile Garry GameMaker source file to assembly for specified CPUs.
INPUT-FILE: Path to .gam source file.
CPUS: List of target CPU keywords (e.g. :6502 :z80 :arm7).
OUTPUT-FILE: Optional output assembly file path."
  (let ((ast (garry-compile-file input-file)))
    (when output-file
      (with-open-file (out output-file :direction :output :if-exists :supersede)
        (write-ast ast out)))
    (when cpus
      (dolist (cpu cpus)
        (compile-to-assembly cpu ast)))
    ast))

(defun compile-garry-string (source-string &key (program-name "Game") cpus)
  "Compile Garry SOURCE-STRING to canonical EIGHTBOL AST.
When CPUS is supplied, also compile to each target."
  (let ((ast (garry-compile-source source-string program-name)))
    (when cpus
      (dolist (cpu cpus)
        (compile-to-assembly cpu ast)))
    ast))