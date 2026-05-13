(in-package :eightbol/test)

(fiveam:def-suite :dartmouth-basic-parity
  :description "Dartmouth BASIC transpile parity for :6502 and :cp1610")

(in-suite :dartmouth-basic-parity)

(defun basic-cobol-for-cpu (cpu source)
  (declare (ignore cpu))
  (transpile-basic-to-cobol-string "BasicParity" source))

(test basic/print-both-cpus
  (let ((src "10 PRINT \"HI\"
20 END"))
    (dolist (cpu '(:6502 :cp1610))
      (let ((cobol (basic-cobol-for-cpu cpu src)))
        (is (search "PRINT" cobol) "CPU ~s should transpile PRINT" cpu)
        (is (search "END" cobol) "CPU ~s should transpile END" cpu)))))

(test basic/run-compiles-not-required-to-execute
  "RUN in the BASIC shell compiles; it does not execute machine code in REPL."
  (is (find-package :eightbol/basic-shell)))
