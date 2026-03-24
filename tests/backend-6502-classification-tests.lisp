;; tests/backend-6502-classification-tests.lisp — TDD for 6502 operand kinds and PIC widths
;;
;; Exercises: global bare names (Phantasia-Globals), 77/78 constants (*const-table*),
;; implicit instance slots (class origin in *slot-table*), explicit (:of "Slot" :self),
;; and unsigned compares / MOVE / ADD across 1–4+ byte *pic-width-table* widths.

(in-package :eightbol/test)

(fiveam:def-suite :backend-6502-classification
  :description
  "6502: variable vs constant vs slot addressing; multi-byte PIC widths (8–32+ bit).")
(in-suite :backend-6502-classification)

(defun %count-substring (needle haystack)
  "Count non-overlapping occurrences of NEEDLE in HAYSTACK."
  (loop with n = 0
        for start = 0 then (+ pos (length needle))
        for pos = (search needle haystack :start2 start)
        while pos
        do (incf n)
        finally (return n)))

(defun %ht ()
  (make-hash-table :test 'equalp))

(test b6502-class/global-bare-name-uses-direct-label-not-self
  "Bare data item with global copybook origin uses sta Label (no ldy Character… / (Self),y)."
  (let ((slots (%ht))
        (pw (%ht)))
    (setf (gethash "Scratch-Pad" slots) "Phantasia-Globals")
    (setf (gethash "Scratch-Pad" pw) 1)
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:move :from 7 :to "Scratch-Pad")))
                "Character" :6502
                :slot-table slots :pic-width-table pw)))
      (is (search "sta ScratchPad" asm))
      (is (null (search "(Self)" asm))))))

(test b6502-class/named-constant-to-global-never-class-prefixed-slot
  "MOVE 78 constant to global: lda # SymbolicName; sta GlobalName; no Character* slot labels."
  (let ((slots (%ht))
        (consts (%ht)))
    (setf (gethash "decal-animation-on-tick" consts) #x40)
    (setf (gethash "Decal-Animation-State" slots) "Phantasia-Globals")
    (setf (gethash "Decal" slots) "Character")
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:move :from "Decal-Animation-On-Tick"
                               :to (:subscript "Decal-Animation-State"
                                     (:of "Decal" :self)))))
                "Character" :6502
                :slot-table slots :const-table consts)))
      (is (search "lda # DecalAnimationOnTick" asm))
      (is (search "DecalAnimationState" asm))
      (is (null (search "CharacterDecalanimationontick" asm))
          "constant must not use implicit instance slot load")
      (is (null (search "CharacterDecalAnimationOnTick" asm))))))

(test b6502-class/implicit-instance-slot-uses-self-indirect
  "Bare name with class slot origin (not global, not constant) uses ldy Character… / lda (Self),y."
  (let ((slots (%ht))
        (pw (%ht)))
    (setf (gethash "Zephyr" slots) "Character")
    (setf (gethash "Out-Scratch" slots) "Phantasia-Globals")
    (setf (gethash "Zephyr" pw) 1)
    (setf (gethash "Out-Scratch" pw) 1)
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:move :from "Zephyr" :to "Out-Scratch")))
                "Character" :6502
                :slot-table slots :pic-width-table pw)))
      (is (search "lda (Self), y" asm))
      (is (search "CharacterZephyr" asm))
      (is (search "sta OutScratch" asm)))))

(test b6502-class/explicit-of-self-and-16-bit-if-compare
  "Explicit (:of Slot :self) with width 2: IF > emits two cmp (unsigned multi-byte)."
  (let ((pw (%ht)))
    (setf (gethash "HP" pw) 2)
    (setf (gethash "Max-HP" pw) 2)
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:if :condition (> (:of "HP" :self) (:of "Max-HP" :self))
                               :then ((:goback))
                               :else ((:goback)))))
                "Character" :6502
                :pic-width-table pw)))
      (is (>= (%count-substring "cmp" asm) 2)))))

(test b6502-class/parser-hp-of-self-16-bit-if-compare
  "Parser (data-name OF data-name) triplet (same shape as COBOL source): IF > uses 2-byte unsigned compare."
  (let ((pw (%ht)))
    (setf (gethash "HP" pw) 2)
    (setf (gethash "Max-HP" pw) 2)
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:if :condition (> ("HP" "OF" "Self") ("Max-HP" "OF" "Self"))
                               :then ((:goback))
                               :else ((:goback)))))
                "Character" :6502
                :pic-width-table pw)))
      (is (>= (%count-substring "cmp" asm) 2)))))

(test b6502-class/is-zero-2-byte-loads-all-bytes
  "IF IS ZERO on PIC 9999 slot checks every byte (not low byte only)."
  (let ((pw (%ht)))
    (setf (gethash "HP" pw) 2)
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "Think"
                  :statements ((:if :condition (:is-zero (:of "HP" :self))
                               :then ((:goback))
                               :else ((:goback)))))
                "Character" :6502
                :pic-width-table pw)))
      (is (>= (%count-substring "lda (Self)" asm) 2)))))

(test b6502-class/pic-width-maxhp-key-matches-max-hyphen-source
  "Copybook keys @code{MAXHP} (from @code{05 MAX-HP}) must match source @code{Max-HP} for PIC width."
  (let ((pw (%ht)))
    (setf (gethash "MAXHP" pw) 2)
    (setf (gethash "HP" pw) 2)
    (is (= 2 (eightbol::pic-width-table-lookup "Max-HP" pw)))
    (is (= 2
         (let ((eightbol::*pic-width-table* pw)
               (eightbol::*class-id* "Character"))
           (eightbol::operand-width '("Max-HP" "OF" "Self")))))))

(test b6502-class/move-3-byte-globals-copies-three-bytes
  "MOVE between 3-byte globals emits at least three load/store pairs (little-endian bytes)."
  (let ((slots (%ht))
        (pw (%ht)))
    (setf (gethash "Triple-Src" slots) "Phantasia-Globals")
    (setf (gethash "Triple-Dst" slots) "Phantasia-Globals")
    (setf (gethash "Triple-Src" pw) 3)
    (setf (gethash "Triple-Dst" pw) 3)
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:move :from "Triple-Src" :to "Triple-Dst")))
                "Character" :6502
                :slot-table slots :pic-width-table pw)))
      (is (>= (%count-substring "lda" asm) 3))
      (is (>= (%count-substring "sta" asm) 3)))))

(test b6502-class/if-3-byte-unsigned-slots-three-cmps
  "IF (:of A :self) > (:of B :self) with PIC width 3 emits three cmp (MSB-first compare)."
  (let ((pw (%ht)))
    (setf (gethash "Wide-A" pw) 3)
    (setf (gethash "Wide-B" pw) 3)
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:if :condition (> (:of "Wide-A" :self) (:of "Wide-B" :self))
                               :then ((:goback))
                               :else ((:goback)))))
                "Character" :6502
                :pic-width-table pw)))
      (is (= 3 (%count-substring "cmp" asm))))))

(test b6502-class/if-4-byte-unsigned-slots-four-cmps
  "IF with PIC width 4 on both sides emits four cmp sequences (32-bit unsigned)."
  (let ((pw (%ht)))
    (setf (gethash "Quad-A" pw) 4)
    (setf (gethash "Quad-B" pw) 4)
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:if :condition (> (:of "Quad-A" :self) (:of "Quad-B" :self))
                               :then ((:goback))
                               :else ((:goback)))))
                "Character" :6502
                :pic-width-table pw)))
      (is (= 4 (%count-substring "cmp" asm))))))

(test b6502-class/add-3-byte-slot-with-carry-chain
  "ADD to 3-byte slot OF Self uses general multi-byte path (w>2): adc per byte, sta per byte."
  (let ((pw (%ht)))
    (setf (gethash "Acc3" pw) 3)
    (setf (gethash "Bump3" pw) 3)
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:add :from "Bump3" :to (:of "Acc3" :self))))
                "Character" :6502
                :pic-width-table pw)))
      (is (>= (%count-substring "adc" asm) 3))
      (is (>= (%count-substring "sta (Self), y" asm) 3)))))

(test b6502-class/operand-width-resolves-from-pic-table-24-bit
  "operand-width returns 3 when *pic-width-table* maps name to 3 (24-bit numeric)."
  (let ((pw (%ht)))
    (setf (gethash "Day-Time-Counter" pw) 3)
    (is (= 3 (let ((eightbol::*pic-width-table* pw))
               (eightbol::operand-width "Day-Time-Counter"))))))
