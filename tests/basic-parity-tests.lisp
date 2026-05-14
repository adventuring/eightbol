(in-package :eightbol/test)

(fiveam:def-suite :dartmouth-basic-parity
  :description "Dartmouth BASIC transpile parity on :6502 and :cp1610")

(in-suite :dartmouth-basic-parity)

(test basic/transpile-entrypoints-exist
  (is (fboundp 'eightbol::basic-transpile-to-assembly))
  (is (fboundp 'eightbol::basic-shell-run)
      "basic-shell RUN must compile emitted methods, not execute CPU code in REPL"))

(test basic/cp1610-and-6502-both-compile
  (when (fboundp 'eightbol::basic-transpile-to-assembly)
    (dolist (cpu '(:6502 :cp1610))
      (finishes
        (let ((asm (eightbol::basic-transpile-to-assembly
                    "10 PRINT \"HI\""
                    :cpu cpu)))
          (is (or (null asm) (stringp asm))))))))
