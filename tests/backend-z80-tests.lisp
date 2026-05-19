;; tests/backend-z80-tests.lisp — Z80 backend statement tests
;;
;; Verifies Z80-specific statement handling via compile-method-ast-with-tables

(in-package :eightbol/test)

(fiveam:def-suite :backend-z80
  :description "Z80 backend statement emission")
(in-suite :backend-z80)

(defun z80-asm (stmt &key (class-id "Character") (slots nil) (consts nil) (pic nil))
  (compile-method-ast-with-tables
   `(:method :method-id "M" :statements (,stmt))
   class-id :z80
   :slot-table (or slots (make-hash-table :test 'equalp))
   :const-table (or consts (make-hash-table :test 'equalp))
   :pic-width-table (or pic (make-hash-table :test 'equalp))))

(test z80/move-literal-to-var
  "MOVE literal TO var emits ld sequence."
  (let ((asm (z80-asm '(:move :from 42 :to "X"))))
    (is (search "ld" asm))))

(test z80/perform
  "PERFORM procedure emits call."
  (let ((asm (z80-asm '(:perform :procedure "Foo"))))
    (is (search "call Foo" asm))))

(test z80/if-equal
  "IF condition emits cp and jp."
  (let ((asm (z80-asm '(:if :condition (= "A" 0) :then ((:move :from 1 :to "A"))))))
    (is (search "cp" asm))
    (is (search "jp" asm))))

(test z80/add
  "ADD emits add instruction."
  (let ((pic (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 1)
    (let ((asm (z80-asm '(:add :from "A" :to "B") :pic pic)))
      (is (search "add" asm)))))

(test z80/subtract
  "SUBTRACT emits sub or sbc."
  (let ((pic (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 1)
    (let ((asm (z80-asm '(:subtract :from "A" :from-target "B" :giving "C") :pic pic)))
      (is (search "sub" asm)))))

(test z80/divide-signals-error
  "DIVIDE on Z80 signals backend-error."
  (signals eightbol::backend-error
    (z80-asm '(:divide :from "A" :into "B"))))

(test z80/multiply-signals-error
  "MULTIPLY on Z80 signals backend-error."
  (signals eightbol::backend-error
    (z80-asm '(:multiply :by "A" :on "B"))))

(test z80/invoke-super
  "INVOKE SUPER emits call to parent class method."
  (let ((eightbol::*parent-classes* (let ((h (make-hash-table :test 'equalp)))
                                       (setf (gethash "Character" h) "Actor") h))
        (eightbol::*method-id* "Think")
        (eightbol::*class-id* "Character"))
    (let ((asm (z80-asm '(:invoke-super))))
      (is (search "call MethodActorThink" asm)))))

(test z80/set-address-of
  "SET dest TO ADDRESS OF var emits ld hl, #symbol."
  (let ((pic (make-hash-table :test 'equalp))
        (slot-table (make-hash-table :test 'equalp)))
    (setf (gethash "X" pic) 2)
    (setf (gethash "Y" pic) 2)
    (setf (gethash "X" slot-table) "Character")
    (let ((asm (z80-asm '(:set :target "Y" :address-of "X") :slot-table slot-table :pic pic)))
      (is (search "ld hl, #CharacterX" asm))
      (is (search "ld (#CharacterY)" asm))
      (is (search "ld (#CharacterY+1)" asm)))))
