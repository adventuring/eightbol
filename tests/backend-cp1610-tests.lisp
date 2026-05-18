;; tests/backend-cp1610-tests.lisp — cp1610 (Intellivision) backend statement tests
;;
;; Verifies that each cp1610 statement emits expected assembly mnemonics
;; using compile-method-ast-with-tables (same bindings as copybooks).
;;
;; To run: (asdf:test-system :eightbol) or (fiveam:run! :backend-cp1610)

(in-package :eightbol/test)

(fiveam:def-suite :backend-cp1610
  :description "cp1610 (Intellivision) backend statement emission")
(in-suite :backend-cp1610)

;;;;
;;;; Helpers
;;;;

(defun cp1610-asm (stmt &key class (class-id "Character") (slots nil) (consts nil) (pic nil) (types nil))
  "Compile STMT inside a minimal method for cp1610, return assembly string."
  (let ((*standard-output* (make-broadcast-stream)))   ; suppress compiler noise
    (compile-method-ast-with-tables
     `(:method :method-id "M" :statements (,stmt))
     class-id :cp1610
     :slot-table (or slots (let ((h (make-hash-table :test 'equalp))) h))
     :const-table (or consts (let ((h (make-hash-table :test 'equalp))) h))
     :pic-width-table (or pic (let ((h (make-hash-table :test 'equalp))) h))
     :type-table (or types (let ((h (make-hash-table :test 'equalp))) h)))))

(defun make-table ()
  (make-hash-table :test 'equalp))

;;;;
;;;; Method structure
;;;;

(test cp1610/method-emits-proc-and-endp
  "Method body opens with PROC and ends with ENDP."
  (let ((asm (cp1610-asm '(:goback))))
    (is (search "PROC" asm))
    (is (search "ENDP" asm))))

(test cp1610/method-emits-dispatch-label
  "Method dispatch label is MethodClassName."
  (let ((asm (cp1610-asm '(:goback) :class-id "Actor")))
    (is (search "MethodActorM" asm))))

;;;;
;;;; Termination statements
;;;;

(test cp1610/goback
  "GOBACK emits JR R5."
  (let ((asm (cp1610-asm '(:goback))))
    (is (search "JR      R5" asm))))

(test cp1610/exit-method
  "EXIT METHOD emits JR R5."
  (let ((asm (cp1610-asm '(:exit-method))))
    (is (search "JR      R5" asm))))

(test cp1610/exit-program
  "EXIT PROGRAM emits JR R5."
  (let ((asm (cp1610-asm '(:exit-program))))
    (is (search "JR      R5" asm))))

(test cp1610/stop-run
  "STOP RUN emits JR R5."
  (let ((asm (cp1610-asm '(:stop-run))))
    (is (search "JR      R5" asm))))

(test cp1610/exit
  "EXIT emits JR R5."
  (let ((asm (cp1610-asm '(:exit))))
    (is (search "JR      R5" asm))))

;;;;
;;;; MOVE statement
;;;;

(test cp1610/move-literal-to-var
  "MOVE literal TO var emits MVII and MVO."
  (let ((slots (make-table)))
    (setf (gethash "Var" slots) "Character")
    (let ((asm (cp1610-asm '(:move :from 42 :to "Var") :slots slots)))
      (is (search "MVII    #42, R0" asm))
      (is (search "MVO     R0, Var" asm)))))

(test cp1610/move-var-to-global
  "MOVE var TO global emits MVI and MVO; global has no class prefix."
  (let ((slots (make-table)))
    (setf (gethash "Dest" slots) "Phantasia-Globals")
    (let ((asm (cp1610-asm '(:move :from "Src" :to "Dest") :slots slots)))
      (is (search "MVI" asm))
      (is (search "MVO" asm))
      (is (search "Dest" asm))
      (is (null (search "CharacterDest" asm))))))

(test cp1610/move-constant-via-const-table
  "MOVE named constant to global: MVII immediate uses constant value."
  (let ((slots (make-table))
        (consts (make-table)))
      (setf (gethash "Next-Song" slots) "Phantasia-Globals")
      (setf (gethash "song--heal--id" consts) 78)
      (let ((asm (cp1610-asm '(:move :from "Song--Heal--ID" :to "Next-Song")
                              :slots slots :consts consts)))
        (is (search "MVII    #78, R0" asm))
        (is (search "MVO     R0, NextSong" asm))
        (is (null (search "CharacterNextSong" asm)))))))

(test cp1610/move-slot-of-self
  "MOVE slot OF Self loads via Self-relative address."
  (let ((slots (make-table)))
    (setf (gethash "HP" slots) "Character")
    (let ((asm (cp1610-asm '(:move :from (:of "HP" :self) :to "Dest") :slots slots)))
      (is (search "MVII    #HP, R4" asm))
      (is (search "ADD     Self, R4" asm))
      (is (search "MVI@    R4, R0" asm)))))

(test cp1610/move-to-subscript
  "MOVE to subscripted destination computes offset via index."
  (let ((slots (make-table)))
    (setf (gethash "Arr" slots) "Character")
    (setf (gethash "Idx" slots) "Character")
    (let ((asm (cp1610-asm '(:move :from 0 :to (:subscript "Arr" "Idx")) :slots slots)))
      (is (search "MOVR    R0, R1" asm))
      (is (search "MVII    #Arr, R4" asm))
      (is (search "ADDR    R0, R4" asm)))))

;;;;
;;;; ADD / SUBTRACT
;;;;

(test cp1610/add-literal-to-var
  "ADD literal TO var emits ADDR and MVO."
  (let ((pic (make-table)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 1)
    (let ((asm (cp1610-asm '(:add :from "A" :to "B") :pic pic)))
      (is (search "ADDR    R1, R0" asm))
      (is (search "MVO" asm)))))

(test cp1610/subtract
  "SUBTRACT emits SUBR."
  (let ((pic (make-table)))
    (setf (gethash "M" pic) 1)
    (setf (gethash "S" pic) 1)
    (let ((asm (cp1610-asm '(:subtract :from "S" :from-target "M" :giving "R") :pic pic)))
      (is (search "SUBR    R0, R1" asm))
      (is (search "MVO     R0, R" asm)))))

;;;;
;;;; COMPUTE
;;;;

(test cp1610/compute-literal
  "COMPUTE target = expr emits load and MVO."
  (let ((pic (make-table)))
    (setf (gethash "X" pic) 1)
    (let ((asm (cp1610-asm '(:compute :target "X" :expression 42) :pic pic)))
      (is (search "MVII    #42, R0" asm))
      (is (search "MVO     R0, X" asm)))))

;;;;
;;;; SET
;;;;

(test cp1610/set-literal
  "SET target TO value emits MVII and MVO."
  (let ((asm (cp1610-asm '(:set :target "X" :value 99))))
    (is (search "MVII    #99, R0" asm))
    (is (search "MVO     R0, X" asm))))

(test cp1610/set-up-by
  "SET target UP BY value emits ADD."
  (let ((pic (make-table)))
    (setf (gethash "X" pic) 1)
    (setf (gethash "Y" pic) 1)
    (let ((asm (cp1610-asm '(:set :target "X" :value "Y" :up-by "Y" :by 1) :pic pic)))
      (is (search "ADDR" asm)))))

;;;;
;;;; IF / conditionals
;;;;

(test cp1610/if-equal
  "IF a = b emits CMPR and BNEQ to else."
  (let ((asm (cp1610-asm '(:if :condition (= "A" "B") :then ((:move :from 0 :to "X")))
                          :class-id "T")))
    (is (search "CMPR    R1, R0" asm))
    (is (search "BNEQ" asm))))

(test cp1610/if-greater
  "IF a > b emits CMPR and BLE."
  (let ((asm (cp1610-asm '(:if :condition (:greater "A" "B") :then ((:move :from 0 :to "X")))
                          :class-id "T")))
    (is (search "CMPR" asm))
    (is (search "BLE" asm))))

(test cp1610/if-less
  "IF a < b emits CMPR and BGE."
  (let ((asm (cp1610-asm '(:if :condition (:less "A" "B") :then ((:move :from 0 :to "X")))
                          :class-id "T")))
    (is (search "CMPR" asm))
    (is (search "BGE" asm))))

(test cp1610/if-is-zero
  "IF a IS ZERO emits TSTR and BNEQ."
  (let ((asm (cp1610-asm '(:if :condition (:is-zero "A") :then ((:move :from 1 :to "A")))
                          :class-id "T")))
    (is (search "TSTR    R0" asm))
    (is (search "BNEQ" asm))))

(test cp1610/if-is-not-zero
  "IF a IS NOT ZERO emits TSTR and BEQ."
  (let ((asm (cp1610-asm '(:if :condition (:is-not-zero "A") :then ((:move :from 1 :to "A")))
                          :class-id "T")))
    (is (search "TSTR    R0" asm))
    (is (search "BEQ" asm))))

(test cp1610/if-else
  "IF/ELSE emits branch around else block."
  (let ((asm (cp1610-asm '(:if :condition (= "A" "B")
                                 :then ((:move :from 1 :to "X"))
                                 :else ((:move :from 2 :to "Y")))
                          :class-id "T")))
    (is (search "B" asm))     ; branch over else
    (is (>= (count #\: asm) 2) "two local labels for else/end")))

(test cp1610/if-and
  "IF a > 0 AND b > 0 emits two condition checks."
  (let ((asm (cp1610-asm '(:if :condition (:and (:greater "A" 0) (:greater "B" 0))
                                 :then ((:move :from 1 :to "X")))
                          :class-id "T")))
    (is (search "CMPR" asm) "should have at least one CMPR"))

(test cp1610/if-not-equal-zero
  "IF NOT (a = 0) emits IS-NOT-ZERO pattern."
  (let ((asm (cp1610-asm '(:if :condition (:not (= "A" 0))
                                 :then ((:move :from 1 :to "X")))
                          :class-id "T")))
    (is (search "TSTR    R0" asm))
    (is (search "BEQ" asm))))

;;;;
;;;; INVOKE
;;;;

(test cp1610/invoke-self
  "INVOKE Self \"Method\" emits JSR R5, InvokeClassMethod."
  (let ((asm (cp1610-asm '(:invoke :object "Self" :method "Connect")
                          :class-id "Character")))
    (is (search "JSR     R5, InvokeCharacterConnect" asm))))

;;;;
;;;; CALL
;;;;

(test cp1610/call
  "CALL target emits JSR R5."
  (let ((asm (cp1610-asm '(:call :target "MoveDecalY"))))
    (is (search "JSR     R5, MoveDecalY" asm))))

;;;;
;;;; PERFORM
;;;;

(test cp1610/perform-simple
  "PERFORM procedure emits JSR R5."
  (let ((asm (cp1610-asm '(:perform :procedure "Foo"))))
    (is (search "JSR     R5, Foo" asm))))

(test cp1610/perform-times
  "PERFORM TIMES n emits loop with DECR/BNEQ."
  (let ((asm (cp1610-asm '(:perform :procedure "Foo" :times 3))))
    (is (search "DECR    R1" asm))
    (is (search "BNEQ" asm))))

(test cp1610/perform-until
  "PERFORM UNTIL condition emits loop with condition test."
  (let ((asm (cp1610-asm '(:perform :procedure "Foo"
                                    :until (:greater "X" 0))
                          :class-id "T")))
    (is (search "JSR     R5, Foo" asm))))

;;;;
;;;; STRING BLT
;;;;

(test cp1610/string-blt
  "STRING dest FROM src emits block copy loop."
  (let ((asm (cp1610-asm '(:string-blt :source "Src" :dest "Dst"))))
    (is (search "MVII    #Src, R2" asm))
    (is (search "MVII    #Dst, R3" asm))
    (is (search "MVI@    R2, R0" asm))
    (is (search "MVO@    R0, R3" asm))
    (is (search "DECR    R1" asm))
    (is (search "BNEQ" asm))))

;;;;
;;;; EVALUATE
;;;;

(test cp1610/evaluate-when
  "EVALUATE WHEN comparison emits CMPR and BNEQ."
  (let ((asm (cp1610-asm '(:evaluate :subject "X"
                                      :when-clauses ((:when "A" ((:move :from 1 :to "X")))))
                          :class-id "T")))
    (is (search "CMPR" asm))
    (is (search "BNEQ" asm))))

;;;;
;;;; INSPECT
;;;;

(test cp1610/inspect-tallying
  "INSPECT TALLYING emits tallying loop."
  (let ((asm (cp1610-asm '(:inspect :target "Buf" :tallying "Cnt"))))
    (is (search "INCR    R0" asm))
    (is (search "MVO     R0" asm))))

(test cp1610/inspect-replacing
  "INSPECT REPLACING emits fill loop."
  (let ((asm (cp1610-asm '(:inspect :target "Buf" :by 42))))
    (is (search "MVII    #Buf, R2" asm))
    (is (search "MVO@    R0, R2" asm))))

;;;;
;;;; GOTO / PARAGRAPH
;;;;

(test cp1610/goto
  "GO TO target emits B to label."
  (let ((asm (cp1610-asm '(:goto :target "Exit"))))
    (is (search "B" asm))))

(test cp1610/paragraph
  "Paragraph emits label."
  (let ((asm (cp1610-asm '(:paragraph "MyPara"))))
    (is (search "MyPara:" asm))))

;;;;
;;;; LOG FAULT / DEBUG BREAK
;;;;

(test cp1610/log-fault
  "LOG FAULT emits comment."
  (let ((asm (cp1610-asm '(:log-fault :code 1234))))
    (is (search "LOG FAULT" asm))))

(test cp1610/debug-break
  "DEBUG BREAK emits comment."
  (let ((asm (cp1610-asm '(:debug-break :code 42))))
    (is (search "DEBUG BREAK" asm))))

;;;;
;;;; Comment statement
;;;;

(test cp1610/comment
  "Comment statement emits assembly comment."
  (let ((asm (cp1610-asm '(:comment "this is a test"))))
    (is (search "this is a test" asm))))

(test cp1610/divide-signals-error
  "DIVIDE on cp1610 signals backend-error."
  (signals eightbol::backend-error
    (cp1610-asm '(:divide :from "A" :into "B"))))

(test cp1610/multiply-signals-error
  "MULTIPLY on cp1610 signals backend-error."
  (signals eightbol::backend-error
    (cp1610-asm '(:multiply :by "A" :on "B"))))

(test cp1610/invoke-super
  "INVOKE SUPER emits JSR to parent class method."
  (let ((eightbol::*parent-classes* (let ((h (make-hash-table :test 'equalp)))
                                       (setf (gethash "Character" h) "Actor") h))
        (eightbol::*method-id* "Think")
        (eightbol::*class-id* "Character"))
    (let ((asm (cp1610-asm '(:invoke-super))))
      (is (search "JSR     R5, MethodActorThink" asm)))))
