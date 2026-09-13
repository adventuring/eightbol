;; tests/calling-convention-and-complexity-tests.lisp
;;; Tests for Part A (calling conventions) and Part B (expression complexity)
(in-package :eightbol/test)

(fiveam:def-suite :calling-convention-and-complexity
  :description "Tests for calling convention AST distinction and expression complexity handling")
(in-suite :calling-convention-and-complexity)

;;; PART A: Calling Convention Tests
;;; ================================

(test calling-convention-call-type-subroutine
  "Test that local subroutine calls have :type :subroutine"
  (let ((call-node (make-call-node 'MyRoutine)))
    (is (eq (getf (rest call-node) :type) :subroutine))
    (is (eq (first call-node) :call))
    (is (eq (getf (rest call-node) :target) 'MyRoutine))))

(test calling-convention-call-type-library
  "Test that library calls can have :type :library"
  (let ((call-node (make-call-node 'LibFunc :library t)))
    (is (eq (getf (rest call-node) :type) :library))
    (is (getf (rest call-node) :library))))

(test calling-convention-call-type-far-service
  "Test that far service calls have :type :far-service"
  (let ((call-node (make-call-node 'ServiceFunc :bank 42)))
    (is (eq (getf (rest call-node) :type) :far-service))
    (is (= (getf (rest call-node) :bank) 42))))

(test calling-convention-call-explicit-type
  "Test that explicit :type parameter overrides inference"
  (let ((call-node (make-call-node 'Func :bank 42 :type :subroutine)))
    ;; Explicit type is honored even with bank specified
    (is (eq (getf (rest call-node) :type) :subroutine))))

(test calling-convention-invoke-no-type
  "Test that :invoke nodes don't have :type (always no-accumulator)"
  (let ((invoke-node (make-invoke-node 'myObject '"Kill")))
    (is (not (getf (rest invoke-node) :type)))
    (is (eq (first invoke-node) :invoke))
    (is (eq (getf (rest invoke-node) :object) 'myObject))))

(test calling-convention-invoke-with-args
  "Test :invoke with arguments"
  (let ((invoke-node (make-invoke-node 'myObject '"Damage" :args '(10))))
    (is (not (getf (rest invoke-node) :type)))
    (is (equal (getf (rest invoke-node) :args) '(10)))))

(test calling-convention-all-call-forms
  "Test all call node variations include :type"
  (let ((calls (list
                (make-call-node 'Func)
                (make-call-node 'Func :args '(1 2))
                (make-call-node 'Func :returning 'result)
                (make-call-node 'Func :library t)
                (make-call-node 'Func :bank 1)
                (make-call-node 'Func :args '(1) :returning 'r :bank 2))))
    (dolist (call calls)
      (is (eq (first call) :call))
      (is (getf (rest call) :type))
      (is (member (getf (rest call) :type) '(:subroutine :library :far-service))))))

;;; PART B: Expression Complexity Tests
;;; ===================================

(test declare-node-creation
  "Test creation of :declare nodes"
  (let ((decl (make-declare-node :temp :variables '(|TempVar1| |TempVar2|))))
    (is (eq (first decl) :declare))
    (is (eq (getf (rest decl) :type) :temp))
    (is (equal (getf (rest decl) :variables) '(|TempVar1| |TempVar2|)))))

(test declare-node-empty
  "Test creation of :declare with no variables"
  (let ((decl (make-declare-node :temp)))
    (is (eq (first decl) :declare))
    (is (eq (getf (rest decl) :type) :temp))
    (is (not (getf (rest decl) :variables)))))

(test available-temps-basic
  "Test basic available temps retrieval"
  (let ((temps (get-available-temps)))
    ;; At minimum, should have reserved temporaries
    (is (member '|MathTemp| temps))
    (is (member '|MultiplyTemp| temps))))

(test available-temps-6502
  "Test available temps for 6502 (no additional registers)"
  (let ((temps (get-available-temps :backend-cpu :6502)))
    (is (member '|MathTemp| temps))
    (is (member '|MultiplyTemp| temps))
    ;; 6502 has no additional CPU registers
    (is (not (member :HL temps)))))

(test available-temps-z80
  "Test available temps for Z80 (has HL, BC, DE)"
  (let ((temps (get-available-temps :backend-cpu :z80)))
    (is (member '|MathTemp| temps))
    (is (member '|MultiplyTemp| temps))
    (is (member :HL temps))
    (is (member :BC temps))
    (is (member :DE temps))))

(test available-temps-with-declared
  "Test available temps with declared variables"
  (let ((*declared-temp-variables* '(|TempA| |TempB|)))
    (let ((temps (get-available-temps :backend-cpu :z80)))
      (is (member '|MathTemp| temps))
      (is (member '|TempA| temps))
      (is (member '|TempB| temps)))))

(test extract-declare-node-present
  "Test extracting :declare node when present"
  (let ((stmts (list
                (make-declare-node :temp :variables '(|T1| |T2|))
                (make-move-node 1 2))))
    (multiple-value-bind (decl rest) (extract-declare-node stmts)
      (is (not (null decl)))
      (is (eq (first decl) :declare))
      (is (= (length rest) 1)))))

(test extract-declare-node-absent
  "Test extracting when :declare not present"
  (let ((stmts (list
                (make-move-node 1 2)
                (make-move-node 3 4))))
    (multiple-value-bind (decl rest) (extract-declare-node stmts)
      (is (null decl))
      (is (= (length rest) 2)))))

(test extract-declared-temps-valid
  "Test extracting temps from valid :declare node"
  (let ((decl (make-declare-node :temp :variables '(|A| |B| |C|))))
    (let ((temps (extract-declared-temps decl)))
      (is (equal temps '(|A| |B| |C|))))))

(test extract-declared-temps-non-temp
  "Test extracting temps from non-temp :declare returns nil"
  (let ((decl (make-declare-node :other :variables '(|A| |B|))))
    (let ((temps (extract-declared-temps decl)))
      (is (null temps)))))

(test expression-context-binding
  "Test expression context macro binds variables"
  (with-expression-context (:backend-cpu :z80 :declared-temps '(|T1| |T2|))
    (let ((temps (get-available-temps :backend-cpu :z80)))
      ;; Should include Z80 registers and declared temps
      (is (member :HL temps))
      (is (member '|T1| temps)))))

(test cpu-registers-6502
  "Test CPU register retrieval for 6502"
  (let ((regs (get-cpu-additional-registers :6502)))
    (is (null regs))))

(test cpu-registers-z80
  "Test CPU register retrieval for Z80"
  (let ((regs (get-cpu-additional-registers :z80)))
    (is (member :HL regs))
    (is (member :BC regs))
    (is (member :DE regs))))

(test cpu-registers-i286
  "Test CPU register retrieval for i286"
  (let ((regs (get-cpu-additional-registers :i286)))
    (is (member :AX regs))
    (is (member :BX regs))))

(test cpu-registers-arm7
  "Test CPU register retrieval for ARM7"
  (let ((regs (get-cpu-additional-registers :arm7)))
    (is (> (length regs) 0))))

(test cpu-registers-unknown
  "Test CPU register retrieval for unknown CPU"
  (let ((regs (get-cpu-additional-registers :unknown-cpu)))
    (is (null regs))))

;;; Signal Condition Tests

(test expression-too-complex-error
  "Test that expression-too-complex signals error"
  (signals error
    (signal-expression-too-complex '(:+ :from 1 :to 2))))

(test allocate-temp-with-fallback-success
  "Test allocate-temp succeeds with available temps"
  (let ((*used-temporaries* '()))
    (let ((temp (allocate-temp-with-fallback 
                 '(:+ :from 1 :to 2)
                 :z80
                 nil)))
      ;; Should return a valid temporary
      (is (or (member temp *reserved-temporaries*)
              (member temp (get-cpu-additional-registers :z80)))))))

;;; Integration Tests

(test call-node-roundtrip
  "Test that call nodes can be created and inspected"
  (let ((call1 (make-call-node 'Func))
        (call2 (make-call-node 'Lib :library t))
        (call3 (make-call-node 'Service :bank 99)))
    (is (eq (getf (rest call1) :type) :subroutine))
    (is (eq (getf (rest call2) :type) :library))
    (is (eq (getf (rest call3) :type) :far-service))))

(test declare-node-roundtrip
  "Test that declare nodes preserve variable names"
  (let ((decl (make-declare-node :temp :variables '(|X| |Y| |Z|))))
    (let ((temps (extract-declared-temps decl)))
      (is (equal temps '(|X| |Y| |Z|))))))

(test combined-call-and-declare
  "Test using both call type distinction and declare nodes"
  (let ((call (make-call-node 'Worker :type :subroutine))
        (decl (make-declare-node :temp :variables '(|Temp1|))))
    (is (eq (getf (rest call) :type) :subroutine))
    (is (equal (extract-declared-temps decl) '(|Temp1|)))))
