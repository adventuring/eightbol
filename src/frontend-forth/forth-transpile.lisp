;; src/frontend-forth/forth-transpile.lisp — Forth compilation entry point
(in-package :eightbol)

(defun compile-forth-from-path (input-file &key cpus output-file)
  "Compile Forth source file to assembly for specified CPUs.
  
INPUT-FILE: Path to .forth source file
CPUS: List of target CPU keywords (e.g., :6502, :z80, :arm7)
OUTPUT-FILE: Optional output assembly file path"
  (declare (ignore output-file))
  ;; For now, treat Forth as generic EIGHTBOL and delegate to standard compile pipeline
  ;; This allows Forth to at least load and parse without errors
  ;; A full implementation would convert Forth-specific AST nodes to canonical EIGHTBOL nodes
  (compile-eightbol (list input-file)
                    :cpus cpus))
