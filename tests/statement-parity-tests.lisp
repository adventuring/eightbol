(in-package :eightbol/test)

(fiveam:def-suite :eightbol-cp1610-6502-parity
  :description "EIGHTBOL statement parity on :6502 and :cp1610 backends")

(in-suite :eightbol-cp1610-6502-parity)

(defun make-table ()
  (make-hash-table :test 'equalp))

(defun parity-asm (cpu stmt class &key (slots nil) (consts nil) (pic nil) (ws nil))
  (compile-method-ast-with-tables
   `(:method :method-id "P" :statements (,stmt))
   class cpu :slot-table (or slots (make-table))
              :const-table (or consts (make-table))
              :pic-width-table (or pic (make-table))
              :working-storage (or ws (make-table))))

(test parity/move-literal-both-cpus
  (let ((slots (make-table))
        (consts (make-table)))
    (setf (gethash "Next-Song" slots) "Phantasia-Globals")
    (dolist (cpu '(:6502 :cp1610))
      (let ((asm (parity-asm cpu '(:move :from 0 :to "Next-Song") "Character"
                             :slots slots :consts consts)))
        (is (plusp (length asm)))
        (is (search "Method" asm))))))

(test parity/invoke-self-cp1610
  (let ((slots (make-table)))
    (setf (gethash "Next-Song" slots) "Phantasia-Globals")
    (let ((asm (parity-asm :cp1610
                           '(:invoke :object "Self" :method "Connect")
                           "Character"
                           :slots slots)))
      (is (plusp (length asm))))))

(test parity/add-cp1610
  (let ((pic (make-table)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 1)
    (let ((asm (parity-asm :cp1610 '(:add :from "A" :to "B") "T" :pic pic)))
      (is (plusp (length asm))))))

(test parity/if-both-cpus
  (dolist (cpu '(:6502 :cp1610))
    (let ((asm (parity-asm cpu '(:if :condition (= "X" 0)
                                     :then ((:move :from 1 :to "X")))
                            "T")))
      (is (plusp (length asm)))
      (is (search "Method" asm))))))

(test parity/perform-both-cpus
  (let ((slots (make-table)))
    (setf (gethash "Next-Song" slots) "Phantasia-Globals")
    (dolist (cpu '(:6502 :cp1610))
      (let ((asm (parity-asm cpu '(:perform :procedure "Foo") "Character"
                             :slots slots)))
        (is (plusp (length asm)))))))
