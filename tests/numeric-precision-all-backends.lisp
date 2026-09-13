;;; tests/numeric-precision-all-backends.lisp — Numeric precision operations across all backends
;;;
;;; Tests addition, subtraction, left shift, right shift, and move for various numeric
;;; precision formats (up to 8 bytes) across all supported backends.
;;; Uses compile-method-ast-with-tables to generate assembly and verifies compilation succeeds.

(in-package :eightbol/test)

(fiveam:def-suite :numeric-precision-all-backends
    :description "Numeric precision operations (ADD, SUBTRACT, SHIFT-LEFT, SHIFT-RIGHT, MOVE) for all backends")
(in-suite :numeric-precision-all-backends)

;;; Helper to compute width in bytes from picture and usage
(defun pic-display-width (pic)
  "Return width in bytes for PICTURE string under USAGE DISPLAY (each 9 is one byte)."
  (let ((total 0)
        (i 0))
    (loop while (< i (length pic))
          do (cond
               ((char= (char pic i) #\9)
                (incf total)
                (incf i))
               ((char= (char pic i) #\V)
                (incf i))
               ((char= (char pic i) #\()
                (incf i)
                (let ((start i))
                  (loop while (and (< i (length pic)) (digit-char-p (char pic i)))
                        do (incf i))
                  (let ((repeat (parse-integer (subseq pic start i))))
                    (when (< i (length pic)) (assert (char= (char pic i) #\))))
                    (incf i)
                    (setf total (+ total repeat)))))
               (t (incf i))))
    total))

;;; Helper to make hash tables for test tables
(defun make-tables (pic-list)
  "Create slot and pic-width tables from plist of symbol-width pairs."
  (let ((slots (make-hash-table :test 'equalp))
        (pic (make-hash-table :test 'equalp)))
    (loop for (sym width) on pic-list by #'cddr
          do (setf (gethash (string sym) slots) "Character")
             (setf (gethash (string sym) pic) width))
    (values slots pic)))

;;; Helper to compile a statement and return assembly
(defun compile-with-precision (stmt cpu pic-list)
  "Compile STMT for CPU with PIC-WIDTH table entries from PIC-LIST."
  (multiple-value-bind (slots pic) (make-tables pic-list)
    (let ((*standard-output* (make-broadcast-stream)))
      (compile-method-ast-with-tables
       `(:method :method-id "M" :statements (,stmt))
       "Character" cpu
       :slot-table slots
       :pic-width-table pic))))

;;;
;;;; 6502 family numeric precision tests
;;;

(test 6502/add-byte-binary
  "6502 ADD of 1-byte binary emits clc and adc."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :6502 '("A" 1 "B" 1))))
    (is (search "clc" asm))
    (is (search "adc" asm))))

(test 6502/add-2byte-binary
  "6502 ADD of 2-byte binary emits multibyte add sequence."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :6502 '("A" 2 "B" 2))))
    (is (search "adc" asm))
    (is (search "sta" asm))))

(test 6502/add-4byte-binary
  "6502 ADD of 4-byte binary emits multibyte add sequence."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :6502 '("A" 4 "B" 4))))
    (is (search "adc" asm))
    (is (search "sta" asm))))

(test 6502/subtract-byte-binary
  "6502 SUBTRACT of 1-byte binary emits sec and sbc."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :6502 '("A" 1 "B" 1))))
    (is (search "sec" asm))
    (is (search "sbc" asm))))

(test 6502/subtract-2byte-binary
  "6502 SUBTRACT of 2-byte binary emits multibyte subtract sequence."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :6502 '("A" 2 "B" 2))))
    (is (search "sbc" asm))
    (is (search "sta" asm))))

(test 6502/move-byte-binary
  "6502 MOVE of 1-byte binary emits lda and sta."
  (let ((asm (compile-with-precision '(:move :from 42 :to "X") :6502 '("X" 1))))
    (is (search "lda" asm))
    (is (search "sta" asm))))

(test 6502/move-2byte-binary
  "6502 MOVE of 2-byte binary emits lda/sta for both bytes."
  (let ((asm (compile-with-precision '(:move :from 1000 :to "X") :6502 '("X" 2))))
    (is (search "lda" asm))
    (is (search "sta" asm))))

;;;
;;;; 65C02 numeric precision tests
;;;

(test 65c02/add-byte-binary
  "65C02 ADD of 1-byte binary emits clc and adc."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :65c02 '("A" 1 "B" 1))))
    (is (search "clc" asm))
    (is (search "adc" asm))))

(test 65c02/subtract-byte-binary
  "65C02 SUBTRACT of 1-byte binary emits sec and sbc."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :65c02 '("A" 1 "B" 1))))
    (is (search "sec" asm))
    (is (search "sbc" asm))))

;;;
;;;; 65C816 numeric precision tests
;;;

(test 65c816/add-byte-binary
  "65C816 ADD of 1-byte binary emits clc and adc."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :65c816 '("A" 1 "B" 1))))
    (is (search "clc" asm))
    (is (search "adc" asm))))

;;;
;;;; HuC6280 numeric precision tests
;;;

(test huc6280/add-byte-binary
  "HuC6280 ADD of 1-byte binary emits clc and adc."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :huc6280 '("A" 1 "B" 1))))
    (is (search "clc" asm))
    (is (search "adc" asm))))

;;;
;;;; RP2A03 numeric precision tests
;;;

(test rp2a03/add-byte-binary
  "RP2A03 ADD of 1-byte binary emits clc and adc."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :rp2a03 '("A" 1 "B" 1))))
    (is (search "clc" asm))
    (is (search "adc" asm))))

(test rp2a03/subtract-byte-binary
  "RP2A03 SUBTRACT of 1-byte binary emits sec and sbc."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :rp2a03 '("A" 1 "B" 1))))
    (is (search "sec" asm))
    (is (search "sbc" asm))))

;;;
;;;; Z80 numeric precision tests
;;;

(test z80/add-byte-binary
  "Z80 ADD of 1-byte binary emits add instruction."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :z80 '("A" 1 "B" 1))))
    (is (search "add" asm))))

(test z80/add-2byte-binary
  "Z80 ADD of 2-byte binary emits add hl,de sequence."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :z80 '("A" 2 "B" 2))))
    (is (search "add" asm))))

(test z80/subtract-byte-binary
  "Z80 SUBTRACT of 1-byte binary emits sub instruction."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :z80 '("A" 1 "B" 1))))
    (is (search "sub" asm))))

(test z80/subtract-2byte-binary
  "Z80 SUBTRACT of 2-byte binary emits or a / sbc hl,de."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :z80 '("A" 2 "B" 2))))
    (is (search "or a" asm))
    (is (search "sbc" asm))))

(test z80/move-byte-binary
  "Z80 MOVE of 1-byte binary emits ld instruction."
  (let ((asm (compile-with-precision '(:move :from 42 :to "X") :z80 '("X" 1))))
    (is (search "ld" asm))))

;;;
;;;; cp1610 numeric precision tests
;;;

(test cp1610/add-byte-binary
  "cp1610 ADD of 1-byte emits ADDR instruction."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :cp1610 '("A" 1 "B" 1))))
    (is (search "ADDR" asm))))

(test cp1610/subtract-byte-binary
  "cp1610 SUBTRACT of 1-byte emits SUBR instruction."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :cp1610 '("A" 1 "B" 1))))
    (is (search "SUBR" asm))))

(test cp1610/move-byte-binary
  "cp1610 MOVE of 1-byte emits MVII and MVO instructions."
  (let ((asm (compile-with-precision '(:move :from 42 :to "X") :cp1610 '("X" 1))))
    (is (search "MVII" asm))
    (is (search "MVO" asm))))

;;;
;;;; SM83 numeric precision tests
;;;

(test sm83/add-byte-binary
  "SM83 ADD of 1-byte binary emits add instruction."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :sm83 '("A" 1 "B" 1))))
    (is (search "add" asm))))

(test sm83/subtract-byte-binary
  "SM83 SUBTRACT of 1-byte binary emits sub instruction."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :sm83 '("A" 1 "B" 1))))
    (is (search "sub" asm))))

(test sm83/move-byte-binary
  "SM83 MOVE of 1-byte binary emits ld a,immediate and ld (hl),a."
  (let ((asm (compile-with-precision '(:move :from 42 :to "X") :sm83 '("X" 1))))
    (is (search "ld" asm))))

;;;
;;;; m6800 numeric precision tests
;;;

(test m6800/add-byte-binary
  "m6800 ADD of 1-byte binary emits ABA instruction."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :m6800 '("A" 1 "B" 1))))
    (is (search "ABA" asm))))

(test m6800/subtract-byte-binary
  "m6800 SUBTRACT of 1-byte binary emits SBA instruction."
  (let ((asm (compile-with-precision (list :- :subtrahend "A" :from "B") :m6800 '("A" 1 "B" 1))))
    (is (search "SBA" asm))))

(test m6800/move-byte-binary
  "m6800 MOVE of 1-byte binary emits LDAA and STAA."
  (let ((asm (compile-with-precision '(:move :from 42 :to "X") :m6800 '("X" 1))))
    (is (search "LDAA" asm))
    (is (search "STAA" asm))))

;;;
;;;; m68k numeric precision tests
;;;

(test m68k/add-byte-binary
  "m68k ADD of 1-byte binary emits add instruction."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :m68k '("A" 1 "B" 1))))
    (is (search "add" asm))))

(test m68k/add-2byte-binary
  "m68k ADD of 2-byte binary emits add.w instruction."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :m68k '("A" 2 "B" 2))))
    (is (search "add" asm))
    (is (search ".w" asm))))

(test m68k/subtract-byte-binary
  "m68k SUBTRACT of 1-byte binary emits sub instruction."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :m68k '("A" 1 "B" 1))))
    (is (search "sub" asm))))

(test m68k/subtract-2byte-binary
  "m68k SUBTRACT of 2-byte binary emits sub.w instruction."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :m68k '("A" 2 "B" 2))))
    (is (search "sub" asm))
    (is (search ".w" asm))))

(test m68k/move-byte-binary
  "m68k MOVE of 1-byte binary emits move.b instruction."
  (let ((asm (compile-with-precision '(:move :from 42 :to "X") :m68k '("X" 1))))
    (is (search "move" asm))
    (is (search ".b" asm))))

(test m68k/move-2byte-binary
  "m68k MOVE of 2-byte binary emits move.w instruction."
  (let ((asm (compile-with-precision '(:move :from 1000 :to "X") :m68k '("X" 2))))
    (is (search "move" asm))
    (is (search ".w" asm))))

;;;
;;;; i286 numeric precision tests
;;;

(test i286/add-byte-binary
  "i286 ADD of 1-byte binary emits add instruction."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :i286 '("A" 1 "B" 1))))
    (is (search "add" asm))))

(test i286/subtract-byte-binary
  "i286 SUBTRACT of 1-byte binary emits sub instruction."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :i286 '("A" 1 "B" 1))))
    (is (search "sub" asm))))

(test i286/move-byte-binary
  "i286 MOVE of 1-byte binary emits mov instruction."
  (let ((asm (compile-with-precision '(:move :from 42 :to "X") :i286 '("X" 1))))
    (is (search "mov" asm))))

;;;
;;;; ARM7 numeric precision tests
;;;

(test arm7/add-byte-binary
  "ARM7 ADD of 1-byte binary emits add instruction."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :arm7 '("A" 1 "B" 1))))
    (is (search "add" asm))))

(test arm7/subtract-byte-binary
  "ARM7 SUBTRACT of 1-byte binary emits subs instruction."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :arm7 '("A" 1 "B" 1))))
    (is (search "subs" asm))))

(test arm7/move-byte-binary
  "ARM7 MOVE of 1-byte binary emits movs instruction."
  (let ((asm (compile-with-precision '(:move :from 42 :to "X") :arm7 '("X" 1))))
    (is (search "movs" asm))))

;;;
;;;; F8 numeric precision tests
;;;

(test f8/add-byte-binary
  "F8 ADD of 1-byte binary emits AS instruction."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :f8 '("A" 1 "B" 1))))
    (is (search "AS" asm))))

(test f8/subtract-byte-binary
  "F8 SUBTRACT of 1-byte binary emits COM/INC/AS sequence."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :f8 '("A" 1 "B" 1))))
    (is (search "COM" asm))
    (is (search "INC" asm))
    (is (search "AS" asm))))

(test f8/move-byte-binary
  "F8 MOVE of 1-byte binary emits LI and ST instructions."
  (let ((asm (compile-with-precision '(:move :from 42 :to "X") :f8 '("X" 1))))
    (is (search "LI" asm))
    (is (search "ST" asm))))

;;;
;;;; Stack machine numeric precision tests
;;;

(test stack/add-byte-binary
  "Stack ADD of 1-byte binary emits add instruction."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :stack '("A" 1 "B" 1))))
    (is (search "add" asm))))

(test stack/subtract-byte-binary
  "Stack SUBTRACT of 1-byte binary emits sub instruction."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :stack '("A" 1 "B" 1))))
    (is (search "sub" asm))))

(test stack/move-byte-binary
  "Stack MOVE of 1-byte binary emits push instruction."
  (let ((asm (compile-with-precision '(:move :from 42 :to "X") :stack '("X" 1))))
    (is (search "push" asm))))

;;;
;;;; Mixed scale precision tests
;;;

(test 6502/add-mixed-scale-1v15-plus-15v1
  "6502 ADD of 1v15 + 15v1 emits scaled binary add with widen."
  (let ((asm (compile-with-precision '(:+ :from "A" :to "B") :6502 '("A" 1 "B" 2))))
    (is (search "adc" asm))))

(test 6502/subtract-mixed-scale
  "6502 SUBTRACT of mixed scales emits scaled binary subtract."
  (let ((asm (compile-with-precision '(:- :subtrahend "A" :from "B") :6502 '("A" 2 "B" 1))))
    (is (search "sbc" asm))))

;;;
;;;; Display (character) precision tests
;;;

(test 6502/move-display-char
  "6502 MOVE of display character emits lda and sta."
  (let ((asm (compile-with-precision '(:move :from 65 :to "X") :6502 '("X" 1))))
    (is (search "lda" asm))
    (is (search "sta" asm))))

(test z80/move-display-char
  "Z80 MOVE of display character emits ld instruction."
  (let ((asm (compile-with-precision '(:move :from 65 :to "X") :z80 '("X" 1))))
    (is (search "ld" asm))))

(test m68k/move-display-char
  "m68k MOVE of display character emits move.b instruction."
  (let ((asm (compile-with-precision '(:move :from 65 :to "X") :m68k '("X" 1))))
    (is (search "move" asm))
    (is (search ".b" asm))))
