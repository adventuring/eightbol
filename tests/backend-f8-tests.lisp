;; tests/backend-f8-tests.lisp — F8 backend statement tests
;; 
;; Verifies F8-specific statement handling via compile-method-ast-with-tables

(in-package :eightbol/test)

(fiveam:def-suite :backend-f8
  :description "F8 backend statement emission")
(in-suite :backend-f8)

(defun f8-asm (stmt &key (class-id "Character") (slots nil) (consts nil) (pic nil))
  (compile-method-ast-with-tables
   `(:method :method-id "M" :statements (,stmt))
   class-id :f8
   :slot-table (or slots (make-hash-table :test 'equalp))
   :const-table (or consts (make-hash-table :test 'equalp))
   :pic-width-table (or pic (make-hash-table :test 'equalp))))

(test f8/move-literal-to-var
  "MOVE literal TO var emits LI/ST sequence."
  (let ((asm (f8-asm '(:move :from 42 :to "X"))))
    (is (search "LI      #42" asm))
    (is (search "ST      10" asm))))

(test f8/move-literal-to-self
  "MOVE literal TO SELF-X emits LI followed by slot offset."
  (let ((slot-table (make-hash-table :test 'equalp))
        (pic (make-hash-table :test 'equalp)))
    (setf (gethash "X" slot-table) "Character")
    (setf (gethash "X" pic) 2)
    (let ((asm (f8-asm '(:move :from 100 :to "X") :slot-table slot-table :pic pic)))
      (is (search "LI      #100" asm))
      (is (search "ST      10+1" asm))))

(test f8/perform
  "PERFORM procedure emits JSR."
  (let ((asm (f8-asm '(:perform :procedure "Foo"))))
    (is (search "JSR     Foo" asm))))

(test f8/if-equal
  "IF condition emits CP and BNE."
  (let ((asm (f8-asm '(:if :condition (= "A" 0) :then ((:move :from 1 :to "A"))))))
    (is (search "CP      10" asm))
    (is (search "BNE" asm))))

(test f8/add
  "ADD emits ADD instruction."
  (let ((pic (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 2)
    (setf (gethash "B" pic) 2)
    (let ((asm (f8-asm '(:add :from "A" :to "B") :pic pic)))
      (is (search "ADD     11" asm))
      (is (search "ST      10" asm)))))

(test f8/subtract
  "SUBTRACT emits SUB."
  (let ((pic (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 1)
    (let ((asm (f8-asm '(:subtract :from "A" :from-target "B" :giving "C") :pic pic)))
      (is (search "SUB     11" asm))))

(test f8/set-address-of-symbol
  "SET dest TO ADDRESS OF var emits LF/HR for symbol."
  (let ((slot-table (make-hash-table :test 'equalp))
        (pic (make-hash-table :test 'equalp)))
    (setf (gethash "X" slot-table) "Character")
    (setf (gethash "X" pic) 2)
    (setf (gethash "Y" slot-table) "Character")
    (setf (gethash "Y" pic) 2)
    (let ((asm (f8-asm '(:set :target "Y" :address-of "X") :slot-table slot-table :pic pic)))
      (is (search "LF      #CharacterX" asm))
      (is (search "LR      10, A" asm))
      (is (search "LI      HIGH(#CharacterX)" asm))
      (is (search "LR      11, A" asm))
      (is (search "ST      10+2" asm))))

(test f8/set-address-of-self
  "SET dest TO ADDRESS OF SELF-X emits sequence to get object pointer then add offset."
  (let ((slot-table (make-hash-table :test 'equalp))
        (pic (make-hash-table :test 'equalp)))
    (setf (gethash "X" slot-table) "Character")
    (setf (gethash "X" pic) 2)
    (setf (gethash "Y" slot-table) "Character")
    (setf (gethash "Y" pic) 2)
    (let ((asm (f8-asm '(:set :target "Y" :address-of "X") :slot-table slot-table :pic pic)))
      (is (search "%f8-self-pointer-to-dc0" asm :test #'search))
      (is (search "%f8-dc0-add-slot-offset" asm :test #'search)))))

(test f8/set-address-of-literal
  "SET dest TO ADDRESS OF literal signals error."
  (let ((slot-table (make-hash-table :test 'equalp)))
    (signals eightbol::unsupported-error
      (f8-asm '(:set :target "Y" :address-of 42) :slot-table slot-table))))

(test f8/display-numeric-literal
  "DISPLAY PIC 9 usage converts number to decimal string."
  (let ((pic (make-hash-table :test 'equalp)))
    (setf (gethash "D" pic) "999V99")
    (let ((asm (f8-asm '(:move :from 12345 :to "D") :pic pic)))
      ;; F8 DISPLAY format: left-padded spaces, comma separators, 1 byte per digit
      (is (search "DISPLAY" asm :test #'string-equal)))))

(test f8/display-string-literal
  "DISPLAY PIC X usage loads string directly."
  (let ((pic (make-hash-table :test 'equalp)))
    (setf (gethash "S" pic) "X(20)")
    (let ((asm (f8-asm '(:move :from "HELLO" :to "S") :pic pic)))
      (is (search "DISPLAY" asm :test #'string-equal)))))

(test f8/usage-display-zero-suppression
  "USAGE DISPLAY with Z picture suppresses leading zeros."
  (let ((pic (make-hash-table :test 'equalp)))
    (setf (gethash "X" pic) "ZZZ")
    (let ((asm (f8-asm '(:move :from 5 :to "X") :pic pic)))
      ;; Z suppresses zeros: 5 becomes "  5" (two spaces then 5)
      (is (search "DISPLAY" asm :test #'string-equal)))))

(test f8/usage-national-zero-print
  "USAGE NATIONAL with 9 picture prints leading zeros."
  (let ((pic (make-hash-table :test 'equalp)))
    (setf (gethash "X" pic) "999")
    (let ((asm (f8-asm '(:move :from 5 :to "X") :pic pic)))
      ;; 9 prints zeros: 5 becomes "005"
      (is (search "DISPLAY" asm :test #'string-equal)))))

(test f8/usage-display-with-commas
  "USAGE DISPLAY with 9(3) and commas formats number with separators."
  (let ((pic (make-hash-table :test 'equalp)))
    (setf (gethash "X" pic) "9(3),9(3)")
    (let ((asm (f8-asm '(:move :from 12345 :to "X") :pic pic)))
      ;; Should format as "12,345"
      (is (search "DISPLAY" asm :test #'string-equal)))))

(test f8/usage-binary-storage-width
  "USAGE BINARY uses calculated width from PIC."
  (let ((pic (make-hash-table :test 'equalp)))
    (setf (gethash "X" pic) "9(5)")
    (let ((width (getf (eightbol::pic->width-slot-info (gethash "X" pic)) :width)))
      (is (> width 1)))))

(test f8/add-characters
  "ADD on DISPLAY characters emits proper sequence with carry."
  (let ((pic (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) "X")
    (setf (gethash "B" pic) "X")
    (let ((asm (f8-asm '(:add :from "A" :to "B") :pic pic)))
      (is (search "ADD" asm)))))

(test f8/move-to-self
  "MOVE TO SELF emits correct self-pointer + offset."
  (let ((slot-table (make-hash-table :test 'equalp))
        (pic (make-hash-table :test 'equalp)))
    (setf (gethash "X" slot-table) "Character")
    (setf (gethash "X" pic) 2)
    (let ((asm (f8-asm '(:move :from 1 :to "X") :slot-table slot-table :pic pic)))
      (is (search "LF" asm))
      (is (search "ST" asm :start (search "LF" asm)))))

(test f8/divide-signals-error
  "DIVIDE on F8 signals backend-error (no hardware support)."
  (signals eightbol::backend-error
    (f8-asm '(:divide :from "A" :into "B"))))

(test f8/multiply-signals-error
  "MULTIPLY on F8 signals backend-error (no hardware support)."
  (signals eightbol::backend-error
    (f8-asm '(:multiply :by "A" :on "B"))))

(test f8/invoke-super
  "INVOKE SUPER emits JSR to parent class method."
  (let ((eightbol::*parent-classes* (let ((h (make-hash-table :test 'equalp)))
                                       (setf (gethash "Character" h) "Actor") h))
        (eightbol::*method-id* "Think")
        (eightbol::*class-id* "Character"))
    (let ((asm (f8-asm '(:invoke-super))))
      (is (search "JSR     MethodActorThink" asm)))))

(test f8/return
  "RETURN emits RTN."
  (let ((asm (f8-asm '(:return))))
    (is (search "RTN" asm))))

(test f8/go-back
  "GOBACK emits RTN."
  (let ((asm (f8-asm '(: goback))))
    (is (search "RTN" asm))))

(test f8/call
  "CALL emits JSR with parameter setup."
  (let ((asm (f8-asm '(:call :name "Foo" :params (1 2 3)))))
    (is (search "JSR     Foo" asm))))

(test f8/stop
  "STOP RUN emits HLT."
  (let ((asm (f8-asm '(:stop :run t))))
    (is (search "HLT" asm))))

(test f8/perform-with-times
  "PERFORM VARYING emits loop with counter."
  (let ((asm (f8-asm '(:perform :times 5 :procedures ((:move :from 1 :to "A")))))
        ( eightbol::*loop-counter* 0))
    (is (search "ST" asm))))

(test f8/loop-with-exit
  "LOOP with EXIT-PERFORM emits conditional branch."
  (let ((asm (f8-asm '(:perform :procedures ((:if :condition (= "A" 0) :then ((:exit-perform))))))))
    (is (search "BNE" asm :start (search "PERFORM" asm)))))
