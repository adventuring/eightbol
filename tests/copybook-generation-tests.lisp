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

;;;; pascal-to-eightbol-name (shared by globals-copybook and oops)

(fiveam:def-suite :copybook-generation
  :description "Unit tests for copybook generation parsing helpers")
(in-suite :copybook-generation)

(test pascal-to-eightbol-name/simple
  "PascalCase converts to UPPERCASE-HYPHENATED."
  (skip "Not yet implemented"))

(test pascal-to-eightbol-name/multi-caps
  "Multiple uppercase transitions get hyphens."
  (skip "Not yet implemented"))

(test pascal-to-eightbol-name/single-word
  "Single word uppercases."
  (skip "Not yet implemented"))

(test pascal-to-eightbol-name/already-hyphenated
  "Already hyphenated input normalizes."
  (skip "Not yet implemented"))

(test pascal-to-copybook-filename
  "PascalCase → Title-And-Hyphens for Class-Name-Slots.cpy."
  (skip "Not yet implemented"))

(test classes-defs-comment-line-p/semicolon
  "Lines starting with ; are comments (Lisp/Emacs style)."
  (skip "Not yet implemented"))

(test classes-defs-comment-line-p/asterisk
  "Lines starting with * are comments (COBOL style)."
  (skip "Not yet implemented"))

(test classes-defs-comment-line-p/blank-and-content
  "Blank lines and content lines are not comments."
  (skip "Not yet implemented"))

(test parse-asm-annotation/object-ref
  "@ClassName produces (:object-ref \"ClassName\")."
  (skip "Not yet implemented"))

(test parse-asm-annotation/pic
  "= PIC X(20) produces (:pic \"PIC X(20)\")."
  (skip "Not yet implemented"))

(test parse-asm-annotation/varchar
  "= VARCHAR(n) DEPENDING ON Field produces (:varchar n field)."
  (skip "Not yet implemented"))

(test parse-asm-annotation/nil
  "Plain comment returns NIL."
  (skip "Not yet implemented"))

(test parse-asm-line/byte
  "Label: .byte ? parses as :byte kind."
  (skip "Not yet implemented"))

(test parse-asm-line/word
  "Label: .word ?, ? parses as :word kind."
  (skip "Not yet implemented"))

(test parse-asm-line/fill
  "Label: .fill 8, ? parses as :fill with size 8."
  (skip "Not yet implemented"))

(test parse-asm-line/const
  "Name = 255 parses as :const with value."
  (skip "Not yet implemented"))

(test parse-asm-line/const-hex
  "Name = $FF parses as hex constant."
  (skip "Not yet implemented"))

(test parse-asm-line/with-annotation
  "Trailing ; @ClassName attaches annotation."
  (skip "Not yet implemented"))

(test parse-asm-line/skips-directive
  ".if / .block etc. return NIL."
  (skip "Not yet implemented"))

(test var-to-eightbol-pic/byte-1
  ":byte size 1 → PIC 99 USAGE BINARY."
  (skip "Not yet implemented"))

(test var-to-eightbol-pic/word-2
  ":word size 2 → PIC 9999 USAGE BINARY."
  (skip "Not yet implemented"))

(test var-to-eightbol-pic/object-ref
  ":object-ref annotation → OBJECT REFERENCE ClassName (EIGHTBOL uppercase-hyphenated)."
  (skip "Not yet implemented"))

(test var-to-eightbol-pic/varchar
  ":varchar annotation → PIC X OCCURS 0 TO n TIMES DEPENDING ON size-field (correct COBOL)."
  (skip "Not yet implemented"))

(test var-to-eightbol-pic/const-nil
  ":const kind returns NIL (skip constants in var section)."
  (skip "Not yet implemented"))

(test parse-slot-annotation/object-ref
  "@ClassName in parts produces (:object-ref \"ClassName\")."
  (skip "Not yet implemented"))

(test parse-slot-annotation/pic
  "= PIC string in parts produces (:pic spec)."
  (skip "Not yet implemented"))

(test parse-slot-annotation/varchar
  "= VARCHAR(n) DEPENDING ON Field produces (:varchar n field)."
  (skip "Not yet implemented"))

(test parse-slot-annotation/nil
  "Empty or no annotation returns NIL."
  (skip "Not yet implemented"))

(test slot-annotation-to-eightbol-pic/default-size-1
  "NIL annotation, size 1 → PIC 99 USAGE BINARY."
  (skip "Not yet implemented"))

(test slot-annotation-to-eightbol-pic/default-size-2
  "NIL annotation, size 2 → PIC 9999 USAGE BINARY."
  (skip "Not yet implemented"))

(test slot-annotation-to-eightbol-pic/default-size-n
  "NIL annotation, size 5 → PIC 99 USAGE BINARY OCCURS 5 TIMES (correct COBOL order)."
  (skip "Not yet implemented"))

(test slot-annotation-to-eightbol-pic/object-ref
  ":object-ref → OBJECT REFERENCE ClassName."
  (skip "Not yet implemented"))

(test slot-annotation-to-eightbol-pic/varchar
  ":varchar → PIC X OCCURS 0 TO n TIMES DEPENDING ON size-field (correct COBOL)."
  (skip "Not yet implemented"))

(test class-ancestry-chain/single
  "Single class returns singleton list."
  (skip "Not yet implemented"))

(test class-ancestry-chain/inheritance
  "Chain from root to leaf."
  (skip "Not yet implemented"))

(test compute-class-size-during-parse/basic-object
  "BasicObject returns 0 (no parent)."
  (skip "Not yet implemented"))

(test compute-class-size-during-parse/with-slots
  "Child with slots sums parent + own."
  (skip "Not yet implemented"))

(test compute-class-size-during-parse/nil-slot-sizes
  "Class with no *slot-sizes* entry (child that adds no own slots) does not signal TYPE-ERROR."
  (skip "Not yet implemented"))

(test pointer-size-for-machine/16-bit
  "6502, Z80, cp1610 etc. use 2-byte pointers."
  (skip "Not yet implemented"))

(test pointer-size-for-machine/32-bit
  "m68k, ARM7, SH2 use 4-byte pointers."
  (skip "Not yet implemented"))

(test copybook-generation/platform-list
  "Platform list is non-empty and includes 7800."
  (is (not (null *test-platforms*)))
  (is (member 7800 *test-platforms*))
  (dolist (m *test-platforms*)
    (is (integerp m) "Platform should be machine number")))

(test make-classes-for-oops/completes-with-minimal-defs
  "make-classes-for-oops completes without TYPE-ERROR when *class-bases* etc. are in scope."
  (skip "Not yet implemented"))

(test make-eightbol-copybooks/generates-basic-object-slots
  "make-eightbol-copybooks generates Basic-Object-Slots.cpy for BasicObject."
  (skip "Not yet implemented"))

(test make-classes-for-oops/requires-machine
  "make-classes-for-oops needs *machine* set (via load-project.json or --port)."
  (skip "Not yet implemented"))

(test write-globals-copybook/game-name-override
  "When *game-title* is bound, output filename uses it."
  (skip "Not yet implemented"))

(test write-globals-copybook/game-title-from-json
  "When *game-title* is bound (from JSON :game key), uses it."
  (skip "Not yet implemented"))

(test write-globals-copybook/game-title-nil-signals-error
  "When game title is nil (missing game key in JSON), signals error (not NIL-Globals.cpy)."
  (skip "Not yet implemented"))
