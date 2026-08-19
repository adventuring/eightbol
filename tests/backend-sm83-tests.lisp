;; tests/backend-sm83-tests.lisp - SM83 backend statement tests
;;
;; Verifies SM83-specific statement emission via compile-method-ast-with-tables,
;; in particular inline PERFORM bodies and CALL...USING.

(in-package :eightbol/test)

(fiveam:def-suite :backend-sm83
  :description "SM83 backend statement emission")
(in-suite :backend-sm83)

(defun sm83-asm (stmt &key (class-id "Character") (slot-table nil) (consts nil) (pic nil))
  (compile-method-ast-with-tables
   `(:method :method-id "M" :statements (,stmt))
   class-id :sm83
   :slot-table (or slot-table (make-hash-table :test 'equalp))
   :const-table (or consts (make-hash-table :test 'equalp))
   :pic-width-table (or pic (make-hash-table :test 'equalp))))

(defun sm83-hash (&rest pairs)
  (let ((table (make-hash-table :test 'equalp)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key table) value))
    table))

(test sm83/perform
  "PERFORM procedure emits call."
  (let ((asm (sm83-asm '(:perform :procedure "Foo"))))
    (is (search "call    Foo" asm))))

(test sm83/perform-with-body-until
  "PERFORM UNTIL with inline :body emits condition-checked loop."
  (let ((asm (sm83-asm '(:perform :until (= "Done" 0) :body ((:move :from 1 :to "X"))))))
    (is (search "jr      nz" asm))
    (is (search "jr      .perfloop" asm))
    (is (search "ld      a, 1" asm))))

(test sm83/perform-with-body-times
  "PERFORM TIMES with inline :body emits counter loop around body."
  (let ((asm (sm83-asm '(:perform :times 3 :body ((:move :from 1 :to "X"))))))
    (is (search "ld      a, 3" asm))
    (is (search "ld      b, a" asm))
    (is (search "dec     b" asm))
    (is (search "jr      nz, .perfloop" asm))))

(test sm83/perform-with-body-varying
  "PERFORM VARYING with inline :body emits counter init, varying store, and increment."
  (let ((slot-table (sm83-hash "X" "Character"))
        (pic (sm83-hash "X" 1)))
    (let ((asm (sm83-asm '(:perform :varying "X" :from 0 :by 1 :until (= "Done" 0)
                                    :body ((:move :from 1 :to "X")))
                         :slot-table slot-table :pic pic)))
      (is (search "ld      [CharacterX], a" asm))
      (is (search "add     a, 1" asm))
      (is (search "ld      b, a" asm)))))

(test sm83/perform-with-body-requires-bounder
  "PERFORM with inline :body and no TIMES/UNTIL/VARYING signals error."
  (signals error
    (sm83-asm '(:perform :body ((:move :from 1 :to "X"))))))

(test sm83/call-acc
  "CALL...USING loads :using then calls target."
  (let ((asm (sm83-asm '(:call-acc :target "Foo" :using 7))))
    (is (search "ld      a, 7" asm))
    (is (search "call    Foo" asm))))

(test sm83/call
  "CALL emits call to target."
  (let ((asm (sm83-asm '(:call :target "Foo"))))
    (is (search "call    Foo" asm))))

(test sm83/move-literal-to-var
  "MOVE literal TO var emits store to bare symbol."
  (let ((asm (sm83-asm '(:move :from 42 :to "X"))))
    (is (search "ld      a, 42" asm))))
