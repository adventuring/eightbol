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
;;;; classes-defs-comment-line-p (oops — Classes.Defs parsing)
;;;; ---------------------------------------------------------------

(test classes-defs-comment-line-p/semicolon
  "Lines starting with ; are comments (Lisp/Emacs style)."
  (is (skyline-tool::classes-defs-comment-line-p ";;; Class Definitions"))
  (is (skyline-tool::classes-defs-comment-line-p "; single semicolon"))
  (is (skyline-tool::classes-defs-comment-line-p ";;;; -*- fundamental -*-"))
  (is (not (skyline-tool::classes-defs-comment-line-p "Entity < BasicObject"))))

(test classes-defs-comment-line-p/asterisk
  "Lines starting with * are comments (COBOL style)."
  (is (skyline-tool::classes-defs-comment-line-p "* Own slots (Character):"))
  (is (skyline-tool::classes-defs-comment-line-p "* Inherited from BasicObject:")))

(test classes-defs-comment-line-p/blank-and-content
  "Blank lines and content lines are not comments."
  (is (not (skyline-tool::classes-defs-comment-line-p "")))
  (is (not (skyline-tool::classes-defs-comment-line-p "   ")))
  (is (not (skyline-tool::classes-defs-comment-line-p ".Decal 1")))
  (is (not (skyline-tool::classes-defs-comment-line-p "#GetBounds"))))

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
;;;; class-ancestry-chain (oops)
;;;; ---------------------------------------------------------------

(test class-ancestry-chain/single
  "Single class returns singleton list."
  (let ((bases (make-hash-table :test 'equal)))
    (setf (gethash "BasicObject" bases) nil)
    (is (equal '("BasicObject")
              (skyline-tool::class-ancestry-chain "BasicObject" bases)))))

(test class-ancestry-chain/inheritance
  "Chain from root to leaf."
  (let ((bases (make-hash-table :test 'equal)))
    (setf (gethash "BasicObject" bases) nil
          (gethash "Entity" bases) "BasicObject"
          (gethash "Actor" bases) "Entity"
          (gethash "Character" bases) "Actor")
    (is (equal '("BasicObject" "Entity" "Actor" "Character")
              (skyline-tool::class-ancestry-chain "Character" bases)))))

;;;; ---------------------------------------------------------------
;;;; compute-class-size-during-parse (oops — uses dynamics)
;;;; ---------------------------------------------------------------

(test compute-class-size-during-parse/basic-object
  "BasicObject returns 0 (no parent)."
  (let ((*class-bases* (make-hash-table :test 'equal))
        (*class-size* (make-hash-table :test 'equal))
        (*slot-sizes* (make-hash-table :test 'equal))
        (*class-slots-order* (make-hash-table :test 'equal)))
    (setf (gethash "BasicObject" *class-bases*) nil)
    (is (= 0 (skyline-tool::compute-class-size-during-parse "BasicObject")))))

(test compute-class-size-during-parse/with-slots
  "Child with slots sums parent + own."
  (let ((*class-bases* (make-hash-table :test 'equal))
        (*class-size* (make-hash-table :test 'equal))
        (*slot-sizes* (make-hash-table :test 'equal))
        (*class-slots-order* (make-hash-table :test 'equal)))
    (setf (gethash "BasicObject" *class-bases*) nil
          (gethash "Entity" *class-bases*) "BasicObject"
          (gethash "Entity" *class-slots-order*) '("ClassID" "Decal")
          (gethash "Entity" *slot-sizes*)
          (let ((h (make-hash-table :test 'equal)))
            (setf (gethash "ClassID" h) 1 (gethash "Decal" h) 1) h)))
    (is (= 2 (skyline-tool::compute-class-size-during-parse "Entity")))))

(test compute-class-size-during-parse/nil-slot-sizes
  "Class with no *slot-sizes* entry (child that adds no own slots) does not signal TYPE-ERROR.
Regression: gethash with nil hash-table signaled. Fix: use (or (gethash ...) (make-hash-table))."
  (let ((*class-bases* (make-hash-table :test 'equal))
        (*class-size* (make-hash-table :test 'equal))
        (*slot-sizes* (make-hash-table :test 'equal))
        (*class-slots-order* (make-hash-table :test 'equal)))
    (setf (gethash "BasicObject" *class-bases*) nil
          (gethash "BasicObject" *class-size*) 1
          (gethash "Entity" *class-bases*) "BasicObject"
          (gethash "Entity" *class-slots-order*) '()
          ;; No *slot-sizes* for Entity — previously caused (gethash s nil 0) TYPE-ERROR
          )
    (is (= 1 (skyline-tool::compute-class-size-during-parse "Entity")))))

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

(test make-classes-for-oops/completes-with-minimal-defs
  "make-classes-for-oops completes without TYPE-ERROR when *class-bases* etc. are in scope.
Regression: ClassInheritance/ClassSizes blocks were outside the let binding *class-bases*."
  (uiop:with-temporary-file (:pathname tmp :directory (uiop:pathname-directory-pathname
                                                       (asdf:system-source-directory :eightbol)))
    (let ((defs (merge-pathnames "Classes.Defs" (uiop:pathname-parent-pathname tmp))))
      (with-open-file (out defs :direction :output :if-exists :supersede)
        (write-line ";;; Minimal" out)
        (write-line "Entity < BasicObject" out)
        (write-line ".Decal 1" out))
      (let ((skyline-tool::*machine* 7800))
        (finish-output)
        (is (not (null (nth-value 1 (ignore-errors
                                     (skyline-tool:make-classes-for-oops defs))))))
        "make-classes-for-oops completes (or signals non-TYPE-ERROR)"))))

(test make-eightbol-copybooks/generates-basic-object-slots
  "make-eightbol-copybooks generates Basic-Object-Slots.cpy for BasicObject.
Regression: BasicObject was seeded but not in all-classes, so Basic-Object-Slots.cpy was never written."
  (let* ((tmp (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "basic-object-slots-test-~a/" (get-internal-real-time))
                               (uiop:temporary-directory))))
         (defs (merge-pathnames "Source/Classes/Classes.Defs" tmp))
         (out-dir (merge-pathnames "Source/Generated/7800/Classes/" tmp))
         (basic-slots (merge-pathnames (make-pathname :name "Basic-Object-Slots" :type "cpy") out-dir)))
    (ensure-directories-exist defs)
    (with-open-file (out defs :direction :output :if-exists :supersede)
      (write-line ";;; Minimal" out)
      (write-line "Entity < BasicObject" out)
      (write-line ".Decal 1" out))
    (unwind-protect
         (let ((skyline-tool::*machine* 7800))
           (uiop:call-with-current-directory tmp
             (lambda ()
               (skyline-tool:make-eightbol-copybooks defs)
               (is (probe-file basic-slots)
                   "Basic-Object-Slots.cpy must exist after make-eightbol-copybooks")))
      (uiop:delete-directory-tree tmp :validate t :if-does-not-exist :ignore))))

(test make-classes-for-oops/requires-machine
  "make-classes-for-oops needs *machine* set (via load-project.json or --port).
When *machine* is unbound, pointer-size-for-machine and machine-directory-name fail.
Make rules export PLATFORM when invoking bin/skyline-tool."
  (let ((skyline-tool::*machine* 7800))
    (is (= 7800 skyline-tool::*machine*) "*machine* bound for 7800")
    (is (= 2 (skyline-tool::pointer-size-for-machine))
        "pointer-size-for-machine uses *machine* when no arg")
    (is (string= "7800" (skyline-tool::machine-directory-name))
        "machine-directory-name uses *machine* when no arg")))

;;;; ---------------------------------------------------------------
;;;; make-globals-copybook game-name (TDD: regression fix for NIL-Globals.cpy)
;;;; ---------------------------------------------------------------

(test make-globals-copybook/game-name-override
  "When :game-name is passed, output filename uses it (not *game*).
Regression: nil game name produced NIL-Globals.cpy."
  (uiop:with-temporary-file (:pathname out :type "cpy")
    (let ((skyline-tool::*machine* 7800)
          (skyline-tool::*game* nil))
      (let ((result (skyline-tool:make-globals-copybook
                     :game-name "Phantasia"
                     :output-path out
                     :root-dir (asdf:system-source-directory :eightbol))))
        (is (equal (pathname-name result) "Phantasia-Globals")
            "Output pathname should be Phantasia-Globals.cpy, not NIL-Globals.cpy")
        (is (probe-file result) "Result file should exist")))))

(test make-globals-copybook/game-name-from-json
  "When *game* is bound (from JSON :*game key) and :game-name not passed, uses it."
  (uiop:with-temporary-file (:pathname out :type "cpy")
    (let ((skyline-tool::*machine* 7800)
          (skyline-tool::*game* "Phantasia"))
      (let ((result (skyline-tool:make-globals-copybook
                     :output-path out
                     :root-dir (asdf:system-source-directory :eightbol))))
        (is (equal (pathname-name result) "Phantasia-Globals")
            "Output pathname should be Phantasia-Globals.cpy")
        (is (probe-file result) "Result file should exist")))))

(test make-globals-copybook/game-name-nil-signals-error
  "When game name is nil (missing :*game key in JSON), signals error (not NIL-Globals.cpy)."
  (let ((skyline-tool::*machine* 7800)
        (skyline-tool::*game* nil))
    (signals error
      (skyline-tool:make-globals-copybook
       :root-dir (asdf:system-source-directory :eightbol)))))
