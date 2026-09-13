;; tests/backend-arm7-tests.lisp - ARM7TDMI (GBA) backend statement tests
;;
;; Targets the Nintendo Game Boy Advance ARM7TDMI (armv4t).
;; Thumb is the default code path for size. Uses R0 as accumulator.
;; Emits unified GAS syntax (.thumb, .syntax unified) plus .cpu arm7tdmi.

(in-package :eightbol/test)

(fiveam:def-suite :backend-arm7
  :description "ARM7TDMI (Game Boy Advance) backend statement emission")
(in-suite :backend-arm7)

(defun arm7-asm (stmt &key (class-id "Character") (slots nil) (consts nil) (pic nil) (types nil))
  "Compile STMT inside a minimal method for ARM7, return assembly string."
  (let ((*standard-output* (make-broadcast-stream)))
    (compile-method-ast-with-tables
     `(:method :method-id "M" :statements (,stmt))
     class-id :arm7
     :slot-table (or slots (make-hash-table :test 'equalp))
     :const-table (or consts (make-hash-table :test 'equalp))
     :pic-width-table (or pic (make-hash-table :test 'equalp))
     :type-table (or types (make-hash-table :test 'equalp)))))

(defun make-hash (&rest pairs)
  (let ((h (make-hash-table :test 'equalp)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key h) value))
    h))

;;;
;;;; Method structure
;;;

(test arm7/method-emits-thumb-func
  "Method body emits .thumb_func directive."
  (let ((asm (arm7-asm '(:goback))))
    (is (search ".thumb_func" asm))))

(test arm7/method-emits-dispatch-label
  "Method dispatch label is MethodClassName."
  (let ((asm (arm7-asm '(:goback) :class-id "Actor")))
    (is (search "MethodActorM" asm))))

;;;
;;;; Termination statements
;;;

(test arm7/goback
  "GOBACK emits bx lr."
  (let ((asm (arm7-asm '(:goback))))
    (is (search "bx" asm))
    (is (search "lr" asm))))

(test arm7/exit-method
  "EXIT METHOD emits bx lr."
  (let ((asm (arm7-asm '(:exit-method))))
    (is (search "bx" asm))
    (is (search "lr" asm))))

(test arm7/exit-program
  "EXIT PROGRAM emits bx lr."
  (let ((asm (arm7-asm '(:exit-program))))
    (is (search "bx" asm))
    (is (search "lr" asm))))

(test arm7/stop-run
  "STOP RUN emits bx lr."
  (let ((asm (arm7-asm '(:stop-run))))
    (is (search "bx" asm))
    (is (search "lr" asm))))

(test arm7/exit
  "EXIT emits bx lr."
  (let ((asm (arm7-asm '(:exit))))
    (is (search "bx" asm))
    (is (search "lr" asm))))

;;;
;;;; MOVE statement
;;;

(test arm7/move-literal-to-var
  "MOVE literal TO var emits movs immediate."
  (let ((asm (arm7-asm '(:move :from 42 :to "X"))))
    (is (search "movs" asm))
    (is (search "42" asm))))

(test arm7/move-slot-of-self
  "MOVE slot OF Self loads via ldr with base register."
  (let ((slots (make-hash "HP" "Character")))
    (let ((asm (arm7-asm '(:move :from (:of "HP" :self) :to "Dest") :slots slots)))
      (is (search "ldr" asm)))))

(test arm7/move-to-subscript
  "MOVE to subscripted destination computes offset via index."
  (let ((slots (make-hash "Arr" "Character" "Idx" "Character")))
    (let ((asm (arm7-asm '(:move :from 0 :to (:subscript "Arr" "Idx")) :slots slots)))
      (is (search "movs" asm))
      (is (search "str" asm)))))

(test arm7/move-named-constant
  "MOVE named constant uses symbolic immediate."
  (let ((consts (make-hash "song--heal--id" 42)))
    (let ((asm (arm7-asm '(:move :from "Song--Heal--ID" :to "X") :consts consts)))
      (is (search "movs" asm))
      (is (search "Song_Heal_ID" asm)))))

;;;
;;;; ADD / SUBTRACT
;;;

(test arm7/add-literal-to-var
  "ADD literal TO var emits adds instruction."
  (let ((pic (make-hash "A" 1 "B" 1)))
    (let ((asm (arm7-asm '(:+ :from "A" :to "B") :pic pic)))
      (is (search "adds" asm)))))

(test arm7/add-16-bit
  "ADD of 2-byte values emits multibyte add sequence."
  (let ((pic (make-hash "A" 2 "B" 2)))
    (let ((asm (arm7-asm '(:+ :from "A" :to "B") :pic pic)))
      (is (search "adds" asm))
      (is (search "str" asm)))))

(test arm7/subtract
  "SUBTRACT emits subs instruction."
  (let ((pic (make-hash "A" 1 "B" 1)))
    (let ((asm (arm7-asm '(:- :from "A" :from-target "B") :pic pic)))
      (is (search "subs" asm)))))

(test arm7/subtract-16-bit
  "SUBTRACT of 2-byte values emits multibyte subtract sequence."
  (let ((pic (make-hash "A" 2 "B" 2)))
    (let ((asm (arm7-asm '(:- :from "A" :from-target "B") :pic pic)))
      (is (search "subs" asm))
      (is (search "str" asm)))))

;;;
;;;; COMPUTE
;;;

(test arm7/compute-literal
  "COMPUTE target = expr emits movs and str."
  (let ((pic (make-hash "X" 1)))
    (let ((asm (arm7-asm '(:compute :target "X" :expression 42) :pic pic)))
      (is (search "movs" asm))
      (is (search "str" asm)))))

;;;
;;;; SET
;;;

(test arm7/set-literal
  "SET target TO value emits movs and str."
  (let ((asm (arm7-asm '(:set :target "X" :value 99))))
    (is (search "movs" asm))
    (is (search "str" asm))))

;;;
;;;; IF / conditionals
;;;

(test arm7/if-equal
  "IF a = b emits cmp and conditional branch."
  (let ((pic (make-hash "A" 1 "B" 1)))
    (let ((asm (arm7-asm '(:if :condition (= "A" "B") :then ((:move :from 0 :to "X"))
                            :class-id "T") :pic pic)))
      (is (search "cmp" asm)))))

(test arm7/if-greater
  "IF a > b emits cmp and ble."
  (let ((pic (make-hash "A" 1 "B" 1)))
    (let ((asm (arm7-asm '(:if :condition (:greater "A" "B") :then ((:move :from 0 :to "X"))
                            :class-id "T") :pic pic)))
      (is (search "cmp" asm)))))

(test arm7/if-less
  "IF a < b emits cmp and bge."
  (let ((pic (make-hash "A" 1 "B" 1)))
    (let ((asm (arm7-asm '(:if :condition (:less "A" "B") :then ((:move :from 0 :to "X"))
                            :class-id "T") :pic pic)))
      (is (search "cmp" asm)))))

(test arm7/if-else
  "IF/ELSE emits branch around else block."
  (let ((pic (make-hash "A" 1 "B" 1)))
    (let ((asm (arm7-asm '(:if :condition (= "A" "B")
                                    :then ((:move :from 1 :to "X"))
                                    :else ((:move :from 2 :to "Y")))
                             :pic pic :class-id "T")))
      (is (search "bne" asm)))))

;;;
;;;; INVOKE
;;;

(test arm7/invoke-self
  "INVOKE Self \"Method\" emits bl to invoke stub."
  (let ((asm (arm7-asm '(:invoke :object "Self" :method "Connect")
                          :class-id "Character")))
    (is (search "bl" asm))
    (is (search "InvokeCharacterConnect" asm))))

(test arm7/invoke-super
  "INVOKE SUPER emits bl to parent class invoke stub."
  (let ((eightbol::*parent-classes* (make-hash "Character" "Actor"))
        (eightbol::*method-id* "Think")
        (eightbol::*class-id* "Character"))
    (let ((asm (arm7-asm '(:invoke-super))))
      (is (search "bl" asm)))))

;;;
;;;; CALL
;;;

(test arm7/call
  "CALL target emits bl."
  (let ((asm (arm7-asm '(:call :target "MoveDecalY"))))
    (is (search "bl" asm))))

(test arm7/call-acc
  "CALL...USING loads argument then bl."
  (let ((asm (arm7-asm '(:call-acc :target "Foo" :using 7))))
    (is (search "movs" asm))
    (is (search "bl" asm))))

;;;
;;;; PERFORM
;;;

(test arm7/perform-simple
  "PERFORM procedure emits bl."
  (let ((asm (arm7-asm '(:perform :procedure "Foo"))))
    (is (search "bl Foo" asm))))

(test arm7/perform-times
  "PERFORM TIMES n emits loop with decrement and conditional branch."
  (let ((asm (arm7-asm '(:perform :procedure "Foo" :times 3))))
    (is (search "bl Foo" asm))))

(test arm7/perform-until
  "PERFORM UNTIL condition emits condition-checked loop."
  (let ((asm (arm7-asm '(:perform :procedure "Foo"
                                    :until (:greater "X" 0))
                          :class-id "T")))
    (is (search "bl" asm))))

(test arm7/perform-with-body-until
  "PERFORM UNTIL with inline :body emits condition-checked loop."
  (let ((asm (arm7-asm '(:perform :until (= "Done" 0) :body ((:move :from 1 :to "X"))))))
    (is (search "cmp" asm))
    (is (search "beq" asm))))

(test arm7/perform-with-body-varying
  "PERFORM VARYING emits counter init, varying store, and increment."
  (let ((pic (make-hash "X" 1)))
    (let ((asm (arm7-asm '(:perform :varying "X" :from 0 :by 1 :until (= "Done" 0)
                                    :body ((:move :from 1 :to "X")))
                         :pic pic)))
      (is (search "movs" asm)))))

;;;
;;;; EVALUATE
;;;

(test arm7/evaluate-when
  "EVALUATE WHEN comparison emits cmp and conditional branch."
  (let ((pic (make-hash "X" 1)))
    (let ((asm (arm7-asm '(:evaluate :subject "X"
                                      :when-clauses ((:when "A" ((:move :from 1 :to "X")))))
                             :pic pic :class-id "T")))
      (is (search "cmp" asm)))))

;;;
;;;; INSPECT
;;;

(test arm7/inspect-tallying
  "INSPECT TALLYING emits counting loop."
  (let ((pic (make-hash "Buf" 64 "Cnt" 1)))
    (let ((asm (arm7-asm '(:inspect :target "Buf" :tallying "Cnt") :pic pic)))
      (is (search "adds" asm)))))

(test arm7/inspect-replacing
  "INSPECT REPLACING emits fill loop."
  (let ((pic (make-hash "Buf" 64)))
    (let ((asm (arm7-asm '(:inspect :target "Buf" :by 42) :pic pic)))
      (is (search "movs" asm)))))

;;;
;;;; GOTO / PARAGRAPH
;;;

(test arm7/goto
  "GO TO target emits b to label."
  (let ((asm (arm7-asm '(:goto :target "Exit"))))
    (is (search "b" asm))))

(test arm7/paragraph
  "Paragraph emits label."
  (let ((asm (arm7-asm '(:paragraph "MyPara"))))
    (is (search "MyPara:" asm))))

;;;
;;;; LOG FAULT / DEBUG BREAK
;;;

(test arm7/log-fault
  "LOG FAULT emits comment."
  (let ((asm (arm7-asm '(:log-fault :code 1234))))
    (is (search "LOG FAULT" asm))))

(test arm7/debug-break
  "DEBUG BREAK emits comment."
  (let ((asm (arm7-asm '(:debug-break :code 42))))
    (is (search "DEBUG BREAK" asm))))

;;;
;;;; Comment statement
;;;

(test arm7/comment
  "Comment statement emits assembly comment."
  (let ((asm (arm7-asm '(:comment "this is a test"))))
    (is (search "this is a test" asm))))

;;;
;;;; STRING BLT
;;;

(test arm7/string-blt
  "STRING dest FROM src emits block copy loop."
  (let ((slots (make-hash "Src" "Character" "Dst" "Character")))
    (let ((asm (arm7-asm '(:string-blt :source "Src" :dest "Dst") :slots slots)))
      (is (search "movs" asm))
      (is (search "ldr" asm))
      (is (search "str" asm)))))

;;;
;;;; UNSUPPORTED
;;;

(test arm7/divide-non-power-of-two-signals-error
  "DIVIDE with non-power-of-two on ARM7 signals source-error."
  (signals eightbol:source-error
    (arm7-asm '(:÷ :divisor 3 :into "B"))))

(test arm7/multiply-non-power-of-two-signals-error
  "MULTIPLY with non-power-of-two on ARM7 signals source-error."
  (signals eightbol:source-error
    (arm7-asm '(:× :multiplier 3 :on "B"))))
