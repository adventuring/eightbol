(in-package :eightbol/test)

(fiveam:def-suite :eightbol-cp1610-6502-parity
  :description "EIGHTBOL statement parity on :6502 and :cp1610 backends")

(in-suite :eightbol-cp1610-6502-parity)

(defmacro with-parity-tables ((slots consts) &body body)
  `(let ((,slots (make-hash-table :test 'equalp))
         (,consts (make-hash-table :test 'equalp)))
     (setf (gethash "Next-Song" ,slots) "Phantasia-Globals")
     ,@body))

(defun parity-asm (cpu stmt class &key (slots nil) (consts nil))
  (compile-method-ast-with-tables
   `(:method :method-id "P" :statements (,stmt))
   class cpu :slot-table slots :const-table consts))

(test parity/move-literal-both-cpus
  (with-parity-tables (slots consts)
    (dolist (cpu '(:6502 :cp1610))
      (let ((asm (parity-asm cpu '(:move :from 0 :to "Next-Song") "Character"
                             :slots slots :consts consts)))
        (is (plusp (length asm)))
        (is (search "Method" asm))))))

(test parity/dup-both-cpus
  (with-parity-tables (slots consts)
    (dolist (cpu '(:6502 :cp1610))
      (let ((asm (parity-asm cpu '(:dup) "Character" :slots slots)))
        (is (plusp (length asm)))))))

(test parity/invoke-both-cpus
  (with-parity-tables (slots consts)
    (dolist (cpu '(:6502 :cp1610))
      (let ((asm (parity-asm cpu
                             '(:invoke :target "Self" :method "Connect")
                             "Character"
                             :slots slots)))
        (is (plusp (length asm)))))))
