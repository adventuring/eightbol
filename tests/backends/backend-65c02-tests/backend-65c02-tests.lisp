;; tests/backend-65c02-tests.lisp - 65C02 backend statement tests
;;
;; Verifies 65C02-specific statement emission via compile-method-ast-with-tables.
;; The 65C02 extends the 6502 with additional instructions (bra, phx/plx, etc.).

(in-package :eightbol/test)

(fiveam:def-suite :backend-65c02
  :description "65C02 backend statement emission")
(in-suite :backend-65c02)

(defun 65c02-asm (stmt &key (class-id "Character") (slots nil) (consts nil) (pic nil) (types nil))
  "Compile STMT inside a minimal method for 65C02, return assembly string."
  (let ((*standard-output* (make-broadcast-stream)))
    (compile-method-ast-with-tables
     `(:method :method-id "M" :statements (,stmt))
     class-id :65c02
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

(test 65c02/method-emits-block-structure
  "Method body opens with .block and ends with .bend."
  (let ((asm (65c02-asm '(:goback))))
    (is (search ".block" asm))
    (is (search ".bend" asm))))

(test 65c02/method-emits-dispatch-label
  "Method dispatch label is MethodClassName."
  (let ((asm (65c02-asm '(:goback) :class-id "Actor")))
    (is (search "MethodActorM" asm))))

;;;
;;;; Termination statements
;;;

(test 65c02/goback
  "GOBACK emits rts."
  (let ((asm (65c02-asm '(:goback))))
    (is (search "rts" asm))))

(test 65c02/exit-method
  "EXIT METHOD emits rts."
  (let ((asm (65c02-asm '(:exit-method))))
    (is (search "rts" asm))))

(test 65c02/exit-program
  "EXIT PROGRAM emits rts."
  (let ((asm (65c02-asm '(:exit-program))))
    (is (search "rts" asm))))

(test 65c02/stop-run
  "STOP RUN emits rts."
  (let ((asm (65c02-asm '(:stop-run))))
    (is (search "rts" asm))))

;;;
;;;; MOVE statement
;;;

(test 65c02/move-literal-to-var
  "MOVE literal TO var emits lda # immediate and sta."
  (let ((asm (65c02-asm '(:move :from 42 :to "X"))))
    (is (search "lda" asm))
    (is (search "42" asm))
    (is (search "sta" asm))))

(test 65c02/move-slot-of-self
  "MOVE slot OF Self loads via Self-relative address."
  (let ((slots (make-hash "HP" "Character")))
    (let ((asm (65c02-asm '(:move :from (:of "HP" :self) :to "Dest") :slots slots)))
      (is (search "ldy #CharacterHP" asm))
      (is (search "lda (Self), y" asm)))))

(test 65c02/move-to-subscript
  "MOVE to subscripted destination computes offset via index."
  (let ((slots (make-hash "Arr" "Character" "Idx" "Character")))
    (let ((asm (65c02-asm '(:move :from 0 :to (:subscript "Arr" "Idx")) :slots slots)))
      (is (search "lda" asm))
      (is (search "sta" asm)))))

(test 65c02/move-named-constant
  "MOVE named constant uses symbolic immediate."
  (let ((consts (make-hash "song--heal--id" 42)))
    (let ((asm (65c02-asm '(:move :from "Song--Heal--ID" :to "X") :consts consts)))
      (is (search "lda # Song_Heal_ID" asm)))))

;;;
;;;; ADD / SUBTRACT
;;;

(test 65c02/add-literal-to-var
  "ADD literal TO var emits clc and adc."
  (let ((pic (make-hash "A" 1 "B" 1)))
    (let ((asm (65c02-asm '(:add :from "A" :to "B") :pic pic)))
      (is (search "clc" asm))
      (is (search "adc" asm)))))

(test 65c02/add-16-bit
  "ADD of 2-byte values emits multibyte add sequence."
  (let ((pic (make-hash "A" 2 "B" 2)))
    (let ((asm (65c02-asm '(:add :from "A" :to "B") :pic pic)))
      (is (search "adc" asm))
      (is (search "sta" asm)))))

(test 65c02/subtract
  "SUBTRACT emits sec and sbc."
  (let ((pic (make-hash "A" 1 "B" 1)))
    (let ((asm (65c02-asm '(:subtract :from "A" :from-target "B") :pic pic)))
      (is (search "sec" asm))
      (is (search "sbc" asm)))))

(test 65c02/subtract-16-bit
  "SUBTRACT of 2-byte values emits multibyte subtract sequence."
  (let ((pic (make-hash "A" 2 "B" 2)))
    (let ((asm (65c02-asm '(:subtract :from "A" :from-target "B") :pic pic)))
      (is (search "sbc" asm))
      (is (search "sta" asm)))))

;;;
;;;; COMPUTE
;;;

(test 65c02/compute-literal
  "COMPUTE target = expr emits load and store."
  (let ((pic (make-hash "X" 1)))
    (let ((asm (65c02-asm '(:compute :target "X" :expression 42) :pic pic)))
      (is (search "lda #42" asm))
      (is (search "sta" asm)))))

;;;
;;;; SET
;;;

(test 65c02/set-literal
  "SET target TO value emits lda and sta."
  (let ((asm (65c02-asm '(:set :target "X" :value 99))))
    (is (search "lda #99" asm))
    (is (search "sta" asm))))

;;;
;;;; IF / conditionals
;;;

(test 65c02/if-equal
  "IF a = b emits compare and branch."
  (let ((pic (make-hash "A" 1 "B" 1)))
    (let ((asm (65c02-asm '(:if :condition (= "A" "B") :then ((:move :from 0 :to "X")) :class-id "T")
                          :pic pic)))
      (is (search "cmp" asm)))))

(test 65c02/if-greater
  "IF a > b emits compare and branch."
  (let ((pic (make-hash "A" 1 "B" 1)))
    (let ((asm (65c02-asm '(:if :condition (:greater "A" "B") :then ((:move :from 0 :to "X")) :class-id "T")
                          :pic pic)))
      (is (search "cmp" asm)))))

(test 65c02/if-less
  "IF a < b emits compare and branch."
  (let ((pic (make-hash "A" 1 "B" 1)))
    (let ((asm (65c02-asm '(:if :condition (:less "A" "B") :then ((:move :from 0 :to "X")) :class-id "T")
                          :pic pic)))
      (is (search "cmp" asm)))))

(test 65c02/if-else
  "IF/ELSE emits branch around else block."
  (let ((pic (make-hash "A" 1 "B" 1)))
    (let ((asm (65c02-asm '(:if :condition (= "A" "B")
                                  :then ((:move :from 1 :to "X"))
                                  :else ((:move :from 2 :to "Y")))
                           :pic pic :class-id "T")))
      (is (search "bne" asm)))))

(test 65c02/uses-bra-for-branches
  "65C02 uses bra (not jmp) for unconditional branches."
  (let ((asm (65c02-asm '(:if :condition (= "A" "B")
                            :then ((:goback))
                            :else ((:goback)))
                          :class-id "T")))
    (is (search "bra" asm))))

;;;
;;;; INVOKE
;;;

(test 65c02/invoke-self
  "INVOKE Self \"Method\" emits .CallMethod."
  (let ((asm (65c02-asm '(:invoke :object "Self" :method "Connect")
                          :class-id "Character")))
    (is (search ".CallMethod" asm))
    (is (search "InvokeCharacterConnect" asm))))

(test 65c02/invoke-super
  "INVOKE SUPER emits .CallMethod to parent class."
  (let ((eightbol::*parent-classes* (make-hash "Character" "Actor"))
        (eightbol::*method-id* "Think")
        (eightbol::*class-id* "Character"))
    (let ((asm (65c02-asm '(:invoke-super))))
      (is (search ".CallMethod" asm)))))

;;;
;;;; CALL
;;;

(test 65c02/call
  "CALL target emits jsr."
  (let ((asm (65c02-asm '(:call :target "MoveDecalY"))))
    (is (search "jsr" asm))))

(test 65c02/call-acc
  "CALL...USING loads argument then jsr."
  (let ((asm (65c02-asm '(:call-acc :target "Foo" :using 7))))
    (is (search "lda #7" asm))
    (is (search "jsr" asm))))

;;;
;;;; PERFORM
;;;

(test 65c02/perform-simple
  "PERFORM procedure emits jsr."
  (let ((asm (65c02-asm '(:perform :procedure "Foo"))))
    (is (search "jsr Foo" asm))))

(test 65c02/perform-times
  "PERFORM TIMES n emits loop with decrement and branch."
  (let ((asm (65c02-asm '(:perform :procedure "Foo" :times 3))))
    (is (search "jsr Foo" asm))))

(test 65c02/perform-until
  "PERFORM UNTIL condition emits condition-checked loop."
  (let ((asm (65c02-asm '(:perform :procedure "Foo"
                                    :until (:greater "X" 0))
                          :class-id "T")))
    (is (search "jsr" asm))))

(test 65c02/perform-with-body-until
  "PERFORM UNTIL with inline :body emits condition-checked loop."
  (let ((asm (65c02-asm '(:perform :until (= "Done" 0) :body ((:move :from 1 :to "X"))))))
    (is (search "lda Done" asm))
    (is (search "bne" asm))))

(test 65c02/perform-with-body-varying
  "PERFORM VARYING emits counter init, varying store, and increment."
  (let ((pic (make-hash "X" 1)))
    (let ((asm (65c02-asm '(:perform :varying "X" :from 0 :by 1 :until (= "Done" 0)
                                    :body ((:move :from 1 :to "X")))
                         :pic pic)))
      (is (search "lda" asm)))))

;;;
;;;; EVALUATE
;;;

(test 65c02/evaluate-when
  "EVALUATE WHEN comparison emits compare and branch."
  (let ((pic (make-hash "X" 1)))
    (let ((asm (65c02-asm '(:evaluate :subject "X"
                                      :when-clauses ((:when "A" ((:move :from 1 :to "X")))))
                           :pic pic :class-id "T")))
      (is (search "cmp" asm)))))

;;;
;;;; INSPECT
;;;

(test 65c02/inspect-tallying
  "INSPECT TALLYING emits counting loop."
  (let ((pic (make-hash "Buf" 64 "Cnt" 1)))
    (let ((asm (65c02-asm '(:inspect :target "Buf" :tallying "Cnt") :pic pic)))
      (is (search "lda" asm)))))

(test 65c02/inspect-replacing
  "INSPECT REPLACING emits fill loop."
  (let ((pic (make-hash "Buf" 64)))
    (let ((asm (65c02-asm '(:inspect :target "Buf" :by 42) :pic pic)))
      (is (search "lda" asm)))))

;;;
;;;; GOTO / PARAGRAPH
;;;

(test 65c02/goto
  "GO TO target emits bra to label (65C02 has BRA, unlike 6502's JMP)."
  (let ((asm (65c02-asm '(:goto :target "Exit"))))
    (is (search "bra" asm))))

(test 65c02/paragraph
  "Paragraph emits label."
  (let ((asm (65c02-asm '(:paragraph "MyPara"))))
    (is (search "MyPara:" asm))))

;;;
;;;; LOG FAULT / DEBUG BREAK
;;;

(test 65c02/log-fault
  "LOG FAULT emits comment."
  (let ((asm (65c02-asm '(:log-fault :code 1234))))
    (is (search "LOG FAULT" asm))))

(test 65c02/debug-break
  "DEBUG BREAK emits comment."
  (let ((asm (65c02-asm '(:debug-break :code 42))))
    (is (search "DEBUG BREAK" asm))))

;;;
;;;; Comment statement
;;;

(test 65c02/comment
  "Comment statement emits assembly comment."
  (let ((asm (65c02-asm '(:comment "this is a test"))))
    (is (search "this is a test" asm))))

;;;
;;;; STRING BLT
;;;

(test 65c02/string-blt
  "STRING dest FROM src emits block copy loop."
  (let ((slots (make-hash "Src" "Character" "Dst" "Character")))
    (let ((asm (65c02-asm '(:string-blt :source "Src" :dest "Dst") :slots slots)))
      (is (search "ldy #$00" asm))
      (is (search "lda" asm))
      (is (search "sta" asm)))))

;;;
;;;; UNSUPPORTED
;;;

(test 65c02/divide-signals-error
  "DIVIDE on 65C02 signals backend-error."
  (signals eightbol::backend-error
    (65c02-asm '(:divide :from "A" :into "B"))))

(test 65c02/multiply-signals-error
  "MULTIPLY on 65C02 signals backend-error."
  (signals eightbol::backend-error
    (65c02-asm '(:multiply :by "A" :on "B"))))
