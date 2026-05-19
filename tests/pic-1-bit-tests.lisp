;;; tests/pic-1-bit-tests.lisp — PIC `1` bit-width tests (Step A)
(in-package :eightbol/test)

(fiveam:def-suite :pic-1-bit
  :description "PIC `1` bit-width: %pic-count-bit-slots, pic-fractional-bits, pic-width-in-bytes, pic-digits-to-width"
  :in :eightbol)

(defvar *pic-suite*)

(fiveam:in-suite :pic-1-bit)

(test pic-1-bit/count-bit-slots
  "%pic-count-bit-slots counts 1 and 1(n) patterns, ignores 9/X/punctuation."
  (is (zerop (eightbol::%pic-count-bit-slots "")))
  (is (zerop (eightbol::%pic-count-bit-slots "99")))
  (is (zerop (eightbol::%pic-count-bit-slots "99.99")))
  (is (zerop (eightbol::%pic-count-bit-slots "S99")))
  (is (zerop (eightbol::%pic-count-bit-slots "(6)")))
  (is (= 1 (eightbol::%pic-count-bit-slots "1")))
  (is (= 6 (eightbol::%pic-count-bit-slots "1(6)")))
  (is (= 8 (eightbol::%pic-count-bit-slots "1(8)")))
  (is (= 10 (eightbol::%pic-count-bit-slots "1(10)")))
  (is (= 16 (eightbol::%pic-count-bit-slots "1(6)V1(10)")))
  (is (= 6 (eightbol::%pic-count-bit-slots "9(4)V1(6)")))
  (is (= 2 (eightbol::%pic-count-bit-slots "1(2)"))))

(test pic-1-bit/fractional-bits
  "pic-fractional-bits counts 1-slots right of V."
  (is (zerop (eightbol::pic-fractional-bits nil)))
  (is (zerop (eightbol::pic-fractional-bits "")))
  (is (zerop (eightbol::pic-fractional-bits "PIC 99")))
  (is (zerop (eightbol::pic-fractional-bits "PIC 1(8)")))
  (is (zerop (eightbol::pic-fractional-bits "PIC 99.99")))
  (is (zerop (eightbol::pic-fractional-bits "PIC 9999.99")))
  (is (= 10 (eightbol::pic-fractional-bits "PIC 1(6)V1(10)")))
  (is (= 8 (eightbol::pic-fractional-bits "PIC 9(4)V1(8)")))
  (is (= 6 (eightbol::pic-fractional-bits "1(6)V1(6)")))
  (is (zerop (eightbol::pic-fractional-bits "1(4)V99"))))

(test pic-1-bit/width-in-bytes
  "pic-width-in-bytes returns total byte width from 9/x and 1-slots."
  (is (= 1 (eightbol::pic-width-in-bytes "PIC 99")))
  (is (= 2 (eightbol::pic-width-in-bytes "PIC 9999")))
  (is (= 1 (eightbol::pic-width-in-bytes "PIC 1")))
  (is (= 1 (eightbol::pic-width-in-bytes "PIC 1(8)")))
  (is (= 1 (eightbol::pic-width-in-bytes "PIC 1(6)")))
  (is (= 2 (eightbol::pic-width-in-bytes "PIC 1(10)")))
  (is (= 2 (eightbol::pic-width-in-bytes "PIC 1(6)V1(10)")))
  (is (= 2 (eightbol::pic-width-in-bytes "PIC 9(2)V1(6)")))
  (is (= 3 (eightbol::pic-width-in-bytes "PIC 9(4)V1(8)")))
  (is (= 2 (eightbol::pic-width-in-bytes "PIC 99.99")))
  (is (= 6 (eightbol::pic-width-in-bytes "PIC 999999.999999"))))

(test pic-1-bit/digits-to-width
  "pic-digits-to-width handles 1-slots, 9-slots, mixed, and parenthesized PIC(n)."
  ;; Existing 9/x patterns unchanged
  (is (= 1 (eightbol::pic-digits-to-width "PIC 99")))
  (is (= 2 (eightbol::pic-digits-to-width "PIC 9999")))
  (is (= 3 (eightbol::pic-digits-to-width "PIC 99.9999")))
  (is (= 3 (eightbol::pic-digits-to-width "PIC 9999.99")))
  (is (= 6 (eightbol::pic-digits-to-width "PIC 999999.999999")))
  (is (= 1 (eightbol::pic-digits-to-width "PIC S9")))
  (is (= 1 (eightbol::pic-digits-to-width "PIC S99")))
  (is (= 2 (eightbol::pic-digits-to-width "PIC S9(4)")))
  ;; Parenthesized PIC(n) form
  (is (= 50 (eightbol::pic-digits-to-width "PIC(99)")))
  ;; 1-bit slot patterns
  (is (= 1 (eightbol::pic-digits-to-width "PIC 1")))
  (is (= 1 (eightbol::pic-digits-to-width "PIC 1(8)")))
  (is (= 2 (eightbol::pic-digits-to-width "PIC 1(10)")))
  (is (= 2 (eightbol::pic-digits-to-width "PIC 1(6)V1(10)")))
  (is (= 2 (eightbol::pic-digits-to-width "PIC 9(2)V1(6)")))
  (is (= 3 (eightbol::pic-digits-to-width "PIC 9(4)V1(8)")))
  ;; &key usage accepted but unused in Step A
  (is (= 1 (eightbol::pic-digits-to-width "PIC 99" :usage :binary)))
  (is (= 2 (eightbol::pic-digits-to-width "PIC 1(6)V1(10)" :usage :binary)))
  (is (= 1 (eightbol::pic-digits-to-width "PIC 99" :usage :decimal))))

(test pic-1-bit/nybble-semantics-unchanged
  "Existing pic-nybble-semantics-p behaviour is preserved."
  (is-true (eightbol::pic-nybble-semantics-p "PIC 9 USAGE BINARY."))
  (is (null (eightbol::pic-nybble-semantics-p "PIC 99 USAGE BINARY.")))
  (is (null (eightbol::pic-nybble-semantics-p "PIC 9(4) USAGE BINARY."))))

(test pic-1-bit/fractional-decimal-digits-unchanged
  "Existing pic-fractional-decimal-digits behaviour is preserved."
  (is (= 4 (eightbol::pic-fractional-decimal-digits "PIC 99.9999")))
  (is (= 2 (eightbol::pic-fractional-decimal-digits "PIC 9999.99")))
  (is (= 6 (eightbol::pic-fractional-decimal-digits "PIC 999999.999999")))
  (is (= 4 (eightbol::pic-fractional-decimal-digits "99V9999")))
  (is (zerop (eightbol::pic-fractional-decimal-digits "PIC 9999")))
  (is (= 2 (eightbol::pic-fractional-decimal-digits "PIC 9(3)V99"))))
