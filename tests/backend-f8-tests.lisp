;; tests/backend-f8-tests.lisp - F8 backend statement tests
;;
;; Verifies F8-specific statement emission via compile-method-ast-with-tables.

(in-package :eightbol/test)

(fiveam:def-suite :backend-f8
  :description "F8 backend statement emission")
(in-suite :backend-f8)

(defun f8-asm (stmt &key (class-id "Character") (slot-table nil) (consts nil) (pic nil))
  (compile-method-ast-with-tables
   `(:method :method-id "M" :statements (,stmt))
   class-id :f8
   :slot-table (or slot-table (make-hash-table :test 'equalp))
   :const-table (or consts (make-hash-table :test 'equalp))
   :pic-width-table (or pic (make-hash-table :test 'equalp))))

(defun f8-hash (&rest pairs)
  (let ((table (make-hash-table :test 'equalp)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key table) value))
    table))

(test f8/move-literal-to-var
  "MOVE literal TO var emits LI/DCI/ST sequence."
  (let ((asm (f8-asm '(:move :from 42 :to "X"))))
    (is (search "LI 42" asm))
    (is (search "DCI X" asm))
    (is (search "ST" asm))))

(test f8/move-literal-to-self
  "MOVE literal TO instance slot emits word load and class-prefixed store."
  (let ((slot-table (f8-hash "X" "Character"))
        (pic (f8-hash "X" 2)))
    (let ((asm (f8-asm '(:move :from 100 :to "X") :slot-table slot-table :pic pic)))
      (is (search "LI 100" asm))
      (is (search "LR 10, A" asm))
      (is (search "DCI CharacterX" asm))
      (is (search "ST" asm)))))

(test f8/perform
  "PERFORM procedure emits PI (subroutine call)."
  (let ((asm (f8-asm '(:perform :procedure "Foo"))))
    (is (search "PI Foo" asm))))

(test f8/perform-with-body-until
  "PERFORM UNTIL with inline :body emits condition-checked loop."
  (let ((asm (f8-asm '(:perform :until (= "Done" 0) :body ((:move :from 1 :to "X"))))))
    (is (search "DCI Done" asm))
    (is (search "XS 8" asm))
    (is (search "BNZ" asm))
    (is (search "LI 1" asm))
    (is (search "DCI X" asm))
    (is (search "BR" asm))))

(test f8/perform-with-body-times
  "PERFORM TIMES with inline :body emits counter loop around body."
  (let ((asm (f8-asm '(:perform :times 3 :body ((:move :from 1 :to "X"))))))
    (is (search "LI 3" asm))
    (is (search "LR 6, A" asm))
    (is (search "CI 0" asm))
    (is (search "DS 6" asm))
    (is (search "BR" asm))))

(test f8/perform-with-body-requires-bounder
  "PERFORM with inline :body and no TIMES/UNTIL/VARYING signals error."
  (signals error
    (f8-asm '(:perform :body ((:move :from 1 :to "X"))))))

(test f8/call-acc
  "CALL...USING loads :using into A then PI target."
  (let ((asm (f8-asm '(:call-acc :target "Foo" :using 7))))
    (is (search "LI 7" asm))
    (is (search "PI Foo" asm))))

(test f8/if-equal
  "IF equality condition emits DCI/LM/XS and branches on BNZ."
  (let ((asm (f8-asm '(:if :condition (= "A" 0) :then ((:move :from 1 :to "A"))))))
    (is (search "DCI A" asm))
    (is (search "XS 8" asm))
    (is (search "BNZ" asm))))

(test f8/add
  "ADD of 16-bit slots emits AS 12 / LNK / AS 13 chain."
  (let ((pic (f8-hash "A" 2 "B" 2)))
    (let ((asm (f8-asm '(:add :from "A" :to "B") :pic pic)))
      (is (search "AS 12" asm))
      (is (search "LNK" asm))
      (is (search "AS 13" asm)))))

(test f8/subtract
  "SUBTRACT emits two's-complement add (COM/INC/AS 8)."
  (let ((pic (f8-hash "A" 1 "B" 1)))
    (let ((asm (f8-asm '(:subtract :from "A" :from-target "B" :giving "C") :pic pic)))
      (is (search "COM" asm))
      (is (search "INC" asm))
      (is (search "AS 8" asm)))))

(test f8/set-address-of-symbol
  "SET dest TO ADDRESS OF var emits LI/HIGH symbol loading."
  (let ((slot-table (f8-hash "X" "Character" "Y" "Character"))
        (pic (f8-hash "X" 2 "Y" 2)))
    (let ((asm (f8-asm '(:set :target "Y" :address-of "X") :slot-table slot-table :pic pic)))
      (is (search "LI CharacterX" asm))
      (is (search "LI HIGH(CharacterX)" asm))
      (is (search "LR 10, A" asm))
      (is (search "DCI CharacterY" asm)))))

(test f8/set-address-of-literal
  "SET dest TO ADDRESS OF literal signals error."
  (let ((slot-table (f8-hash "Y" "Character")))
    (signals error
      (f8-asm '(:set :target "Y" :address-of 42) :slot-table slot-table))))

(test f8/goback
  "GOBACK emits POP."
  (let ((asm (f8-asm '(:goback))))
    (is (search "POP" asm))))

(test f8/stop-run
  "STOP RUN emits POP."
  (let ((asm (f8-asm '(:stop-run))))
    (is (search "POP" asm))))

(test f8/call
  "CALL emits PI to target."
  (let ((asm (f8-asm '(:call :target "Foo"))))
    (is (search "PI Foo" asm))))

(test f8/invoke-super
  "INVOKE SUPER emits PI to parent class method."
  (let ((eightbol::*parent-classes* (f8-hash "Character" "Actor")))
    (let ((asm (f8-asm '(:invoke-super))))
      (is (search "PI MethodActorM" asm)))))

(test f8/perform-times
  "PERFORM ... TIMES subroutine call emits counter loop with PI."
  (let ((asm (f8-asm '(:perform :times 5 :procedure "Foo"))))
    (is (search "LI 5" asm))
    (is (search "PI Foo" asm))
    (is (search "DS 6" asm))))

(test f8/perform-until
  "PERFORM UNTIL subroutine call emits condition-checked loop with PI."
  (let ((asm (f8-asm '(:perform :until (= "Done" 0) :procedure "Foo"))))
    (is (search "PI Foo" asm))
    (is (search "BR" asm))))

(test f8/divide-signals-error
  "DIVIDE on F8 signals source-error (non-power-of-two divisor)."
  (signals eightbol::source-error
    (f8-asm '(:divide :divisor 3 :into "B"))))

(test f8/multiply-signals-error
  "MULTIPLY on F8 signals source-error (non-power-of-two multiplier)."
  (signals eightbol::source-error
    (f8-asm '(:multiply :multiplier 3 :on "B"))))
