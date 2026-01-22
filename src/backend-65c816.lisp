;; src/backend-65c816.lisp — 65C816 code generation backend
;;
;; The 65C816 extends the 65C02 with 16-bit accumulator/index, 24-bit addressing,
;; and bank registers. For 8-bit EIGHTBOL code we emit 6502-compatible output;
;; the 65C816 in emulation mode (e=1) executes 6502 opcodes. Native mode would
;; require rep/sep for 8-bit operations.
;; Code generation delegates to the 6502 backend.
(in-package :eightbol)

(defmethod compile-to-assembly (ast (cpu (eql :65c816)) output-stream)
  (compile-6502-family ast output-stream :65c816))
