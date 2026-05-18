;;; tests/s-decimal-tests.lisp — S nybble width for DECIMAL usage (Step B)
(in-package :eightbol/test)

(defvar *s-decimal-suite*
  (fiveam:def-suite :s-decimal
    :description "S sign nybble: DECIMAL usage adds 1 nybble for S, BINARY does not"
    :in :eightbol))

(fiveam:in-suite :s-decimal)

(test s-decimal/has-s-p
  "%pic-has-s-p detects S in PIC strings."
  (is-true (eightbol::%pic-has-s-p "S99"))
  (is-true (eightbol::%pic-has-s-p "s9"))
  (is-true (eightbol::%pic-has-s-p "S9(4)"))
  (is-true (eightbol::%pic-has-s-p "9S9"))
  (is (null (eightbol::%pic-has-s-p "99")))
  (is (null (eightbol::%pic-has-s-p "9(4)V99")))
  (is (null (eightbol::%pic-has-s-p "1(8)"))))

(test s-decimal/digits-to-width-binary-unchanged
  "pic-digits-to-width without :usage or with :usage :binary keeps current S behaviour."
  ;; No usage — S does not add nybble (same as binary)
  (is (= 1 (eightbol::pic-digits-to-width "PIC S9")))
  (is (= 1 (eightbol::pic-digits-to-width "PIC S99")))
  (is (= 2 (eightbol::pic-digits-to-width "PIC S9(4)")))
  ;; Explicit :binary — S does not add nybble
  (is (= 1 (eightbol::pic-digits-to-width "PIC S9" :usage :binary)))
  (is (= 1 (eightbol::pic-digits-to-width "PIC S99" :usage :binary)))
  (is (= 2 (eightbol::pic-digits-to-width "PIC S9(4)" :usage :binary))))

(test s-decimal/digits-to-width-decimal-adds-nybble
  "pic-digits-to-width with :usage :decimal adds 1 nybble for S."
  (is (= 1 (eightbol::pic-digits-to-width "PIC S9" :usage :decimal)))    ; (1+1)=2/2=1
  (is (= 2 (eightbol::pic-digits-to-width "PIC S99" :usage :decimal)))   ; (2+1)=3/2=2
  (is (= 2 (eightbol::pic-digits-to-width "PIC S999" :usage :decimal)))  ; (3+1)=4/2=2
  (is (= 3 (eightbol::pic-digits-to-width "PIC S9(4)" :usage :decimal))) ; (4+1)=5/2=3
  (is (= 1 (eightbol::pic-digits-to-width "PIC 9" :usage :decimal)))     ; no S, unchanged
  (is (= 1 (eightbol::pic-digits-to-width "PIC 99" :usage :decimal))))   ; no S, unchanged

(test s-decimal/digits-to-width-mixed-with-1-slots
  "S nybble combines with 1-slots correctly for DECIMAL usage."
  (is (= 4 (eightbol::pic-digits-to-width "PIC S9(4)V1(8)" :usage :decimal)))  ; ceil(5/2)=3 + ceil(8/8)=1 = 4
  (is (= 3 (eightbol::pic-digits-to-width "PIC S9(2)V1(6)" :usage :decimal)))  ; ceil(3/2)=2 + ceil(6/8)=1 = 3
  )

(test s-decimal/nybble-semantics-unchanged
  "Existing pic-nybble-semantics-p behaviour is preserved."
  (is-true (eightbol::pic-nybble-semantics-p "PIC 9 USAGE BINARY."))
  (is (null (eightbol::pic-nybble-semantics-p "PIC 99 USAGE BINARY.")))
  (is (null (eightbol::pic-nybble-semantics-p "PIC 9(4) USAGE BINARY.")))
  ;; S9 with SINGLE digit should still be nybble-semantics
  (is-true (eightbol::pic-nybble-semantics-p "PIC S9 USAGE BINARY.")))

(test s-decimal/pic-1-bit-tests-still-pass
  "Step A pic-1-bit tests still pass with updated pic-digits-to-width."
  (is (= 1 (eightbol::pic-digits-to-width "PIC 99")))
  (is (= 2 (eightbol::pic-digits-to-width "PIC 9999")))
  (is (= 3 (eightbol::pic-digits-to-width "PIC 99.9999")))
  (is (= 1 (eightbol::pic-digits-to-width "PIC 1")))
  (is (= 1 (eightbol::pic-digits-to-width "PIC 1(8)")))
  (is (= 2 (eightbol::pic-digits-to-width "PIC 1(10)")))
  (is (= 2 (eightbol::pic-digits-to-width "PIC 1(6)V1(10)")))
  (is (= 2 (eightbol::pic-digits-to-width "PIC 9(2)V1(6)")))
  (is (= 3 (eightbol::pic-digits-to-width "PIC 9(4)V1(8)"))))
