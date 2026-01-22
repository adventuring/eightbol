;; src/backend-HuC6280.lisp — HuC6280 code generation backend
;;
;; The HuC6280 (Hudson Soft) is used in the PC Engine / TurboGrafx-16. It is
;; based on the 65C02 with additional instructions (TAM, TMA, block transfers,
;; etc.) and an MMU. For standard EIGHTBOL constructs we emit 6502/65C02
;; compatible code; the HuC6280 executes all 6502 instructions.
;; Code generation delegates to the 6502 backend.
(in-package :eightbol)

(defmethod compile-to-assembly (ast (cpu (eql :huc6280)) output-stream)
  (compile-6502-family ast output-stream :huc6280))
