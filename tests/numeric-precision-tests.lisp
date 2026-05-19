;; tests/numeric-precision-tests.lisp — PIC DECIMAL, BINARY, and precision-mixed arithmetic
;;
;; Verifies storage sizing, sign handling, implied-decimal V/., and cross-usage moves
;; in accordance with the numeric precision specification.

(in-package :eightbol/test)

(fiveam:def-suite :numeric-precision
  :description "PIC DECIMAL/BINARY precision, sign, V scaling, and mixed arithmetic")
(in-suite :numeric-precision)

;;;;
;;;; PIC DECIMAL storage rules
;;;;
;;;; S9  = 1 byte  (sign nybble + 1 digit nybble)
;;;; 99  = 1 byte  (2 digit nybbles, no sign)
;;;; S99 = 2 bytes (sign + 2 digit nybbles in 1.5 → ceil to 2)
;;;; 999 = 2 bytes (3 digit nybbles in 1.5 → ceil to 2)
;;;; 9999 = 2 bytes (4 digit nybbles in 2)
;;;;

(test decimal-storage/s9-is-one-byte
  "S9 USAGE DECIMAL → 1 byte (1 sign nybble + 1 digit nybble)."
  (is (= 1 (eightbol::pic-digits-to-width "s9" :usage :decimal))))

(test decimal-storage/99-is-one-byte
  "99 USAGE DECIMAL → 1 byte (2 digit nybbles, no sign)."
  (is (= 1 (eightbol::pic-digits-to-width "99" :usage :decimal))))

(test decimal-storage/s99-is-two-bytes
  "S99 USAGE DECIMAL → 2 bytes (sign + 2 digits = 3 nybbles → ceil 1.5 = 2)."
  (is (= 2 (eightbol::pic-digits-to-width "s99" :usage :decimal))))

(test decimal-storage/999-is-two-bytes
  "999 USAGE DECIMAL → 2 bytes (3 digit nybbles → ceil 1.5 = 2)."
  (is (= 2 (eightbol::pic-digits-to-width "999" :usage :decimal))))

(test decimal-storage/9999-is-two-bytes
  "9999 USAGE DECIMAL → 2 bytes (4 digit nybbles = 2 full bytes)."
  (is (= 2 (eightbol::pic-digits-to-width "9999" :usage :decimal))))

(test decimal-storage/s9999-is-three-bytes
  "S9999 USAGE DECIMAL → 3 bytes (sign + 4 digits = 5 nybbles → ceil 2.5 = 3)."
  (is (= 3 (eightbol::pic-digits-to-width "s9999" :usage :decimal))))

;;;;
;;;; PIC BINARY storage rules
;;;;
;;;; Each 9 = one nybble (4 bits). Each 1 = one bit.
;;;; S changes interpretation between two's-complement and unsigned.
;;;; V represents an implied fixed-point decimal separator.
;;;;

(test binary-storage/9-is-one-nybble
  "9 USAGE BINARY → 1 byte (1 digit nybble = 4 bits → ceil to 1)."
  (is (= 1 (eightbol::pic-digits-to-width "9" :usage :binary))))

(test binary-storage/s9-does-not-add-sign-nybble
  "S9 USAGE BINARY → 1 byte (no sign nybble for BINARY; 1 digit nybble = 4 bits)."
  (is (= 1 (eightbol::pic-digits-to-width "s9" :usage :binary))))

(test binary-storage/99-is-one-byte
  "99 USAGE BINARY → 1 byte (2 digit nybbles = 8 bits)."
  (is (= 1 (eightbol::pic-digits-to-width "99" :usage :binary))))

(test binary-storage/s99-is-still-one-byte
  "S99 USAGE BINARY → 1 byte (2 digit nybbles = 8 bits; no sign nybble for BINARY)."
  (is (= 1 (eightbol::pic-digits-to-width "s99" :usage :binary))))

(test binary-storage/1-4-is-half-byte
  "1(4) USAGE BINARY → 1 byte (4 bits → ceil to 1)."
  (is (= 1 (eightbol::pic-digits-to-width "1(4)" :usage :binary))))

(test binary-storage/1-8-is-one-byte
  "1(8) USAGE BINARY → 1 byte (8 bits)."
  (is (= 1 (eightbol::pic-digits-to-width "1(8)" :usage :binary))))

(test binary-storage/1-16-is-two-bytes
  "1(16) USAGE BINARY → 2 bytes (16 bits)."
  (is (= 2 (eightbol::pic-digits-to-width "1(16)" :usage :binary))))

;;;;
;;;; V implied decimal separator — fractional digit counts
;;;;

(test decimal-v/s99v99-fractional-digits
  "S99V99 USAGE DECIMAL → 2 fractional digits after V."
  (is (= 2 (eightbol::pic-fractional-decimal-digits "s99v99"))))

(test decimal-v/dot-works-like-v
  "99.99 works like 99V99."
  (is (= 2 (eightbol::pic-fractional-decimal-digits "99.99"))))

(test binary-v/1-4v1-4-fractional-bits
  "1(4)V1(4) USAGE BINARY → 4 fractional bits after V."
  (is (= 4 (eightbol::pic-fractional-bits "1(4)V1(4)"))))

(test binary-v/1-8v1-8-fractional-bits
  "1(8)V1(8) USAGE BINARY → 8 fractional bits after V."
  (is (= 8 (eightbol::pic-fractional-bits "1(8)V1(8)"))))
