;; src/backend-65c02.lisp — 65C02 code generation backend
;;
;; The 65C02 is a CMOS variant of the 6502 with additional instructions:
;;   stz (store zero), trb (test and reset bits), tsb (test and set bits)
;;   bra (branch always), bbr/bbs (branch on bit), smb/rmb (set/reset memory bit)
;; It lacks undocumented 6502 opcodes (lax, sax, etc.).
;; Code generation delegates to the 6502 backend; 65C02 is a superset for our purposes.
(in-package :eightbol)

(defmethod compile-to-assembly (ast (cpu (eql :65c02)) output-stream)
  (compile-6502-family ast output-stream :65c02))
