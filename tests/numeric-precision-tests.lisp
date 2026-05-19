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

;;;;
;;;; MOVE with sign extension / zero fill / truncation
;;;;
;;;; When source is narrower than destination and both USAGE BINARY:
;;;;   signed source → sign extend
;;;;   unsigned source → zero fill
;;;; When source is wider than destination: truncate (low bytes kept)
;;;;

(defun cp1610-move-asm (from to &key (class-id "T") pic ws)
  "Compile MOVE from TO via compile-method-ast-with-tables for cp1610."
  (compile-method-ast-with-tables
   `(:method :method-id "M" :statements ((:move :from ,from :to ,to)))
   class-id :cp1610
   :pic-width-table (or pic (make-hash-table :test 'equalp))
   :working-storage (or ws (make-hash-table :test 'equalp))))

(defun z80-move-asm (from to &key (class-id "T") pic ws)
  "Compile MOVE from TO via compile-method-ast-with-tables for Z80."
  (compile-method-ast-with-tables
   `(:method :method-id "M" :statements ((:move :from ,from :to ,to)))
   class-id :z80
   :pic-width-table (or pic (make-hash-table :test 'equalp))
   :working-storage (or ws (make-hash-table :test 'equalp))))

(test move/cp1610-sign-extend-1-to-2-bytes
  "cp1610: MOVE signed 8-bit BINARY to 16-bit BINARY sign-extends via SLL/SARC."
  (let ((pic (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 2)
    (setf (gethash "A" ws) (list :usage :binary :signed t :pic "s9"))
    (setf (gethash "B" ws) (list :usage :binary :signed t :pic "s9999"))
    (let ((asm (cp1610-move-asm "A" "B" :pic pic :ws ws)))
      (is (search "SLL     R0, 8" asm))
      (is (search "SARC    R0, 8" asm)))))

(test move/cp1610-zero-fill-1-to-2-bytes
  "cp1610: MOVE unsigned 8-bit BINARY to 16-bit BINARY zero-fills via ANDI."
  (let ((pic (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 2)
    (setf (gethash "A" ws) (list :usage :binary :signed nil :pic "99"))
    (setf (gethash "B" ws) (list :usage :binary :signed nil :pic "9999"))
    (let ((asm (cp1610-move-asm "A" "B" :pic pic :ws ws)))
      (is (search "ANDI    #$FF, R0" asm)))))

(test move/z80-sign-extend-1-to-2-bytes
  "Z80: MOVE signed 8-bit BINARY to 16-bit BINARY sign-extends via sbc a,a."
  (let ((pic (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 2)
    (setf (gethash "A" ws) (list :usage :binary :signed t :pic "s9"))
    (setf (gethash "B" ws) (list :usage :binary :signed t :pic "s9999"))
    (let ((asm (z80-move-asm "A" "B" :pic pic :ws ws)))
      (is (search "sbc a, a" asm))
      (is (search "ld h, a" asm)))))

(test move/z80-zero-fill-1-to-2-bytes
  "Z80: MOVE unsigned 8-bit BINARY to 16-bit BINARY zero-fills via ld h,0."
  (let ((pic (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 2)
    (setf (gethash "A" ws) (list :usage :binary :signed nil :pic "99"))
    (setf (gethash "B" ws) (list :usage :binary :signed nil :pic "9999"))
    (let ((asm (z80-move-asm "A" "B" :pic pic :ws ws)))
      (is (search "ld h, 0" asm)))))

(test move/cp1610-same-width-no-extension
  "cp1610: MOVE same-width BINARY has no SLL/SARC extension instructions."
  (let ((pic (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 1)
    (setf (gethash "A" ws) (list :usage :binary :signed nil :pic "99"))
    (setf (gethash "B" ws) (list :usage :binary :signed nil :pic "99"))
    (let ((asm (cp1610-move-asm "A" "B" :pic pic :ws ws)))
      (is (null (search "SLL" asm)))
      (is (null (search "SARC" asm)))
      (is (null (search "ANDI" asm))))))

(test move/z80-same-width-no-extension
  "Z80: MOVE same-width BINARY has no sign-extension instructions."
  (let ((pic (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 1)
    (setf (gethash "A" ws) (list :usage :binary :signed nil :pic "99"))
    (setf (gethash "B" ws) (list :usage :binary :signed nil :pic "99"))
    (let ((asm (z80-move-asm "A" "B" :pic pic :ws ws)))
      (is (null (search "sbc a, a" asm)))
      (is (null (search "ld h, 0" asm))))))

;;;;
;;;; DECIMAL ↔ BINARY MOVE conversion
;;;;

(test move/cp1610-decimal-to-binary
  "cp1610: MOVE USAGE DECIMAL to BINARY emits BCD-to-binary conversion."
  (let ((pic (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 1)
    (setf (gethash "A" ws) (list :usage :decimal :signed nil :pic "99"))
    (setf (gethash "B" ws) (list :usage :binary :signed nil :pic "99"))
    (let ((asm (cp1610-move-asm "A" "B" :pic pic :ws ws)))
      (is (plusp (length asm)))
      (is (search "SARC    R0, 4" asm)))))

(test move/z80-decimal-to-binary
  "Z80: MOVE USAGE DECIMAL to BINARY emits BCD-to-binary conversion."
  (let ((pic (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 1)
    (setf (gethash "A" ws) (list :usage :decimal :signed nil :pic "99"))
    (setf (gethash "B" ws) (list :usage :binary :signed nil :pic "99"))
    (let ((asm (z80-move-asm "A" "B" :pic pic :ws ws)))
      (is (plusp (length asm)))
      (is (search "rrca" asm)))))

(test move/cp1610-binary-to-decimal
  "cp1610: MOVE USAGE BINARY to DECIMAL emits binary-to-BCD conversion."
  (let ((pic (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 1)
    (setf (gethash "A" ws) (list :usage :binary :signed nil :pic "99"))
    (setf (gethash "B" ws) (list :usage :decimal :signed nil :pic "99"))
    (let ((asm (cp1610-move-asm "A" "B" :pic pic :ws ws)))
      (is (plusp (length asm)))
      (is (search "SUBR    R1, R2" asm)))))

(test move/z80-binary-to-decimal
  "Z80: MOVE USAGE BINARY to DECIMAL emits binary-to-BCD conversion."
  (let ((pic (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 1)
    (setf (gethash "B" pic) 1)
    (setf (gethash "A" ws) (list :usage :binary :signed nil :pic "99"))
    (setf (gethash "B" ws) (list :usage :decimal :signed nil :pic "99"))
    (let ((asm (z80-move-asm "A" "B" :pic pic :ws ws)))
      (is (plusp (length asm)))
      (is (search "sbc" asm)))))

(test move/cp1610-bcd-error-for-wide
  "cp1610: MOVE BCD wider than 1 byte signals backend-error."
  (let ((pic (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 2)
    (setf (gethash "B" pic) 2)
    (setf (gethash "A" ws) (list :usage :decimal :signed t :pic "s9999"))
    (setf (gethash "B" ws) (list :usage :binary :signed t :pic "s9999"))
    (signals eightbol::backend-error
      (cp1610-move-asm "A" "B" :pic pic :ws ws))))

(test move/z80-bcd-error-for-wide
  "Z80: MOVE BCD wider than 1 byte signals backend-error."
  (let ((pic (make-hash-table :test 'equalp))
        (ws (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 2)
    (setf (gethash "B" pic) 2)
    (setf (gethash "A" ws) (list :usage :decimal :signed t :pic "s9999"))
    (setf (gethash "B" ws) (list :usage :binary :signed t :pic "s9999"))
    (signals eightbol::backend-error
      (z80-move-asm "A" "B" :pic pic :ws ws))))
