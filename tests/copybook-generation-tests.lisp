;; tests/copybook-generation-tests.lisp — Unit tests for per-class and per-platform copybook generation
;;
;; Tests parsing helpers from globals-copybook.lisp (per-platform) and oops.lisp (per-class).
;; Uses inline minimal fixtures. Platform loop for future expansion; currently 7800 only.
;;
;; To run: (asdf:test-system :eightbol)
;;        — or — (fiveam:run! :copybook-generation)

(in-package :eightbol/test)

(defparameter *test-platforms* '(7800)
  "Platforms to iterate in platform-dependent tests. Expand for multi-platform coverage.")

;;;; ---------------------------------------------------------------
;;;; pascal-to-eightbol-name (shared by globals-copybook and oops)
;;;; ---------------------------------------------------------------

(fiveam:def-suite :copybook-generation
  :description "Unit tests for copybook generation parsing helpers")
(in-suite :copybook-generation)

(test pascal-to-eightbol-name/simple
  "PascalCase converts to UPPERCASE-HYPHENATED."
  (is (string= "NAME-LENGTH" (skyline-tool::pascal-to-eightbol-name "NameLength")))
  (is (string= "MAX-HP" (skyline-tool::pascal-to-eightbol-name "MaxHP")))
  (is (string= "CLASS-ID" (skyline-tool::pascal-to-eightbol-name "ClassID"))))

(test pascal-to-eightbol-name/multi-caps
  "Multiple uppercase transitions get hyphens."
  (is (string= "ABSOLUTE-DELTA-X" (skyline-tool::pascal-to-eightbol-name "AbsoluteDeltaX"))))

(test pascal-to-eightbol-name/single-word
  "Single word uppercases."
  (is (string= "HP" (skyline-tool::pascal-to-eightbol-name "HP"))))

(test pascal-to-eightbol-name/already-hyphenated
  "Already hyphenated input normalizes."
  (is (string= "WORK-FITNESS" (skyline-tool::pascal-to-eightbol-name "work-fitness"))))

(test pascal-to-copybook-filename
  "PascalCase → Title-And-Hyphens for Class-Name-Slots.cpy."
  (is (string= "Character" (skyline-tool::pascal-to-copybook-filename "Character")))
  (is (string= "Non-Player-Character" (skyline-tool::pascal-to-copybook-filename "NonPlayerCharacter"))))

;;;; ---------------------------------------------------------------
;;;; parse-asm-annotation (globals-copybook — per-platform)
;;;; ---------------------------------------------------------------

(test parse-asm-annotation/object-ref
  "@ClassName produces (:object-ref \"ClassName\")."
  (let ((a (skyline-tool::parse-asm-annotation "@Actor")))
    (is (eq :object-ref (first a)))
    (is (string= "Actor" (second a)))))

(test parse-asm-annotation/pic
  "= PIC X(20) produces (:pic \"PIC X(20)\")."
  (let ((a (skyline-tool::parse-asm-annotation "= PIC X(20)")))
    (is (eq :pic (first a)))
    (is (search "PIC" (second a)))))

(test parse-asm-annotation/varchar
  "= VARCHAR(n) DEPENDING ON Field produces (:varchar n field)."
  (let ((a (skyline-tool::parse-asm-annotation "= VARCHAR(12) DEPENDING ON NameLength")))
    (is (eq :varchar (first a)))
    (is (= 12 (second a)))
    (is (string= "NameLength" (third a)))))

(test parse-asm-annotation/nil
  "Plain comment returns NIL."
  (is (null (skyline-tool::parse-asm-annotation "just a comment"))))

;;;; ---------------------------------------------------------------
;;;; parse-asm-line (globals-copybook — per-platform)
;;;; ---------------------------------------------------------------

(test parse-asm-line/byte
  "Label: .byte ? parses as :byte kind."
  (let ((p (skyline-tool::parse-asm-line "Counter: .byte ?")))
    (is (string= "Counter" (getf p :name)))
    (is (eq :byte (getf p :kind)))
    (is (= 1 (getf p :size)))))

(test parse-asm-line/word
  "Label: .word ?, ? parses as :word kind."
  (let ((p (skyline-tool::parse-asm-line "Ptr: .word ?, ?")))
    (is (string= "Ptr" (getf p :name)))
    (is (eq :word (getf p :kind)))
    (is (= 4 (getf p :size)))))

(test parse-asm-line/fill
  "Label: .fill 8, ? parses as :fill with size 8."
  (let ((p (skyline-tool::parse-asm-line "Buffer: .fill 8, ?")))
    (is (string= "Buffer" (getf p :name)))
    (is (eq :fill (getf p :kind)))
    (is (= 8 (getf p :size)))))

(test parse-asm-line/const
  "Name = 255 parses as :const with value."
  (let ((p (skyline-tool::parse-asm-line "MaxHP = 255")))
    (is (string= "MaxHP" (getf p :name)))
    (is (eq :const (getf p :kind)))
    (is (= 255 (getf p :value)))))

(test parse-asm-line/const-hex
  "Name = $FF parses as hex constant."
  (let ((p (skyline-tool::parse-asm-line "Mask = $FF")))
    (is (string= "Mask" (getf p :name)))
    (is (eq :const (getf p :kind)))
    (is (= 255 (getf p :value)))))

(test parse-asm-line/with-annotation
  "Trailing ; @ClassName attaches annotation."
  (let ((p (skyline-tool::parse-asm-line "Target: .word ?, ? ; @Actor")))
    (is (string= "Target" (getf p :name)))
    (is (eq :object-ref (first (getf p :annotation))))
    (is (string= "Actor" (second (getf p :annotation))))))

(test parse-asm-line/skips-directive
  ".if / .block etc. return NIL."
  (is (null (skyline-tool::parse-asm-line "  .if DEBUG")))
  (is (null (skyline-tool::parse-asm-line "  .block"))))

;;;; ---------------------------------------------------------------
;;;; var-to-eightbol-pic (globals-copybook — per-platform)
;;;; ---------------------------------------------------------------

(test var-to-eightbol-pic/byte-1
  ":byte size 1 → PIC 99 USAGE BINARY."
  (is (string= "PIC 99 USAGE BINARY"
              (skyline-tool::var-to-eightbol-pic :byte 1 nil nil))))

(test var-to-eightbol-pic/word-2
  ":word size 2 → PIC 9999 USAGE BINARY."
  (is (string= "PIC 9999 USAGE BINARY"
              (skyline-tool::var-to-eightbol-pic :word 2 nil nil))))

(test var-to-eightbol-pic/object-ref
  ":object-ref annotation → OBJECT REFERENCE ClassName."
  (is (search "OBJECT REFERENCE"
              (skyline-tool::var-to-eightbol-pic :word 2 '(:object-ref "Actor") nil)))
  (is (search "Actor"
              (skyline-tool::var-to-eightbol-pic :word 2 '(:object-ref "Actor") nil))))

(test var-to-eightbol-pic/varchar
  ":varchar annotation → PIC X OCCURS 0 TO n TIMES DEPENDING ON size-field (correct COBOL)."
  (let ((pic (skyline-tool::var-to-eightbol-pic :fill 12 '(:varchar 12 "NameLength") nil)))
    (is (search "PIC X" pic))
    (is (search "OCCURS 0 TO 12 TIMES" pic))
    (is (search "DEPENDING ON" pic))
    (is (search "NAME-LENGTH" pic))))

(test var-to-eightbol-pic/const-nil
  ":const kind returns NIL (skip constants in var section)."
  (is (null (skyline-tool::var-to-eightbol-pic :const 0 nil nil))))

;;;; ---------------------------------------------------------------
;;;; parse-slot-annotation (oops — per-class)
;;;; ---------------------------------------------------------------

(test parse-slot-annotation/object-ref
  "@ClassName in parts produces (:object-ref \"ClassName\")."
  (let ((a (skyline-tool::parse-slot-annotation '("@Actor"))))
    (is (eq :object-ref (first a)))
    (is (string= "Actor" (second a)))))

(test parse-slot-annotation/pic
  "= PIC string in parts produces (:pic spec)."
  (let ((a (skyline-tool::parse-slot-annotation '("=" "PIC" "9999" "USAGE" "BINARY"))))
    (is (eq :pic (first a)))
    (is (search "PIC" (second a)))))

(test parse-slot-annotation/varchar
  "= VARCHAR(n) DEPENDING ON Field produces (:varchar n field)."
  (let ((a (skyline-tool::parse-slot-annotation
            '("=" "VARCHAR(12)" "DEPENDING" "ON" "NameLength"))))
    (is (eq :varchar (first a)))
    (is (= 12 (second a)))
    (is (string= "NameLength" (third a)))))

(test parse-slot-annotation/nil
  "Empty or no annotation returns NIL."
  (is (null (skyline-tool::parse-slot-annotation nil)))
  (is (null (skyline-tool::parse-slot-annotation '("1")))))

;;;; ---------------------------------------------------------------
;;;; slot-annotation-to-eightbol-pic (oops — per-class)
;;;; ---------------------------------------------------------------

(test slot-annotation-to-eightbol-pic/default-size-1
  "NIL annotation, size 1 → PIC 99 USAGE BINARY."
  (is (string= "PIC 99 USAGE BINARY"
              (skyline-tool::slot-annotation-to-eightbol-pic nil 1))))

(test slot-annotation-to-eightbol-pic/default-size-2
  "NIL annotation, size 2 → PIC 9999 USAGE BINARY."
  (is (string= "PIC 9999 USAGE BINARY"
              (skyline-tool::slot-annotation-to-eightbol-pic nil 2))))

(test slot-annotation-to-eightbol-pic/default-size-n
  "NIL annotation, size 5 → PIC 99 USAGE BINARY OCCURS 5 TIMES (correct COBOL order)."
  (let ((pic (skyline-tool::slot-annotation-to-eightbol-pic nil 5)))
    (is (search "PIC 99 USAGE BINARY" pic))
    (is (search "OCCURS 5 TIMES" pic))))

(test slot-annotation-to-eightbol-pic/object-ref
  ":object-ref → OBJECT REFERENCE ClassName."
  (is (search "OBJECT REFERENCE"
              (skyline-tool::slot-annotation-to-eightbol-pic '(:object-ref "Actor") 2)))
  (is (search "Actor"
              (skyline-tool::slot-annotation-to-eightbol-pic '(:object-ref "Actor") 2))))

(test slot-annotation-to-eightbol-pic/varchar
  ":varchar → PIC X OCCURS 0 TO n TIMES DEPENDING ON size-field (correct COBOL)."
  (let ((pic (skyline-tool::slot-annotation-to-eightbol-pic
              '(:varchar 12 "NameLength") 12)))
    (is (search "PIC X" pic))
    (is (search "OCCURS 0 TO 12 TIMES" pic))
    (is (search "DEPENDING ON" pic))
    (is (search "NAME-LENGTH" pic))))

;;;; ---------------------------------------------------------------
;;;; pointer-size-for-machine (architecture-specific @ slot sizes)
;;;; ---------------------------------------------------------------

(test pointer-size-for-machine/16-bit
  "6502, Z80, cp1610 etc. use 2-byte pointers."
  (is (= 2 (skyline-tool::pointer-size-for-machine 7800)) "7800 (6502) = 2 bytes")
  (is (= 2 (skyline-tool::pointer-size-for-machine 81)) "ZX81 (Z80) = 2 bytes"))

(test pointer-size-for-machine/32-bit
  "m68k, ARM7, SH2 use 4-byte pointers."
  (is (= 4 (skyline-tool::pointer-size-for-machine 9)) "NG (m68k) = 4 bytes")
  (is (= 4 (skyline-tool::pointer-size-for-machine 3296)) "GBA (ARM7) = 4 bytes"))

;;;; ---------------------------------------------------------------
;;;; Platform iteration (for future expansion)
;;;; ---------------------------------------------------------------

(test copybook-generation/platform-list
  "Platform list is non-empty and includes 7800."
  (is (not (null *test-platforms*)))
  (is (member 7800 *test-platforms*))
  (dolist (m *test-platforms*)
    (is (integerp m) "Platform should be machine number")))
