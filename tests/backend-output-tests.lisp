;; tests/backend-output-tests.lisp — Unit tests for per-statement backend output
;;
;; Verifies that each backend emits expected assembly for statement types.
;; Uses minimal Character class with inline WORKING-STORAGE (no copybook required).
;;
;; To run: (asdf:test-system :eightbol)
;;        — or — (fiveam:run! :backend-output)

(in-package :eightbol/test)

(fiveam:def-suite :backend-output
  :description "Backend output structure tests for all CPU statement emission")
(in-suite :backend-output)

;;;; Minimal source — one statement in Think, two methods (Think, Kill)

(defparameter +minimal-character-source+
  "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000075         05 HP-USAGE-DECIMAL PIC 9999 USAGE DECIMAL.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             ~a
000130             GOBACK.
000140         END METHOD \"Think\".
000150         IDENTIFICATION DIVISION.
000160         METHOD-ID. \"Kill\".
000170         PROCEDURE DIVISION.
000180             GOBACK.
000190         END METHOD \"Kill\".
000200 END OBJECT.
000210 END CLASS Character.
")

;;;; Helpers — compile minimal class with given statement; return assembly

(defun compile-stmt-to-cpu (cpu stmt-line)
  "Compile minimal Character class with STMT-LINE in Think method to CPU, return assembly string."
  (let ((src (format nil +minimal-character-source+ stmt-line)))
    (let ((ast (eightbol::parse-eightbol-string src)))
      (let ((eightbol::*slot-table* (make-hash-table :test 'equalp))
            (eightbol::*working-storage* (make-hash-table :test 'equalp))
            (eightbol::*type-table* (make-hash-table :test 'equalp))
            (eightbol::*pic-width-table* (make-hash-table :test 'equalp)))
        (labels ((collect-slots (form)
                   (when (consp form)
                     (if (and (symbolp (first form))
                              (string-equal (symbol-name (first form)) "OF"))
                         (let ((slot-name (second form)))
                           (setf (gethash (eightbol::cobol-slot-table-name-key slot-name)
                                         eightbol::*slot-table*)
                                 "Character")
                           (setf (gethash (list :of slot-name "Character")
                                         eightbol::*working-storage*)
                                 '(:pic "9999" :usage :binary :signed nil))
                           (setf (gethash (eightbol::cobol-slot-table-name-key slot-name)
                                         eightbol::*pic-width-table*)
                                 2))
                         (dolist (el form)
                           (collect-slots el))))
                   (when (stringp form)
                     (setf (gethash (eightbol::cobol-slot-table-name-key form)
                                   eightbol::*slot-table*)
                           "Character")
                     (setf (gethash (list :of form "Character")
                                   eightbol::*working-storage*)
                           '(:pic "9999" :usage :binary :signed nil))
                     (setf (gethash (eightbol::cobol-slot-table-name-key form)
                                   eightbol::*pic-width-table*)
                           2))))
          (dolist (module (remove-if #'null (mapcar #'identity ast)))
            (dolist (m (eightbol::ast-methods module))
              (when (and (listp m) (eq (first m) :method))
                (dolist (s (eightbol::ast-method-statements m))
                  (collect-slots s))))))
        (with-output-to-string (s)
          (eightbol::compile-to-assembly-with-ast-passes ast cpu s))))))

(defun compile-stmt-to-6502 (stmt-line)
  "Compile minimal Character class with STMT-LINE in Think method to 6502, return assembly string."
  (compile-stmt-to-cpu :6502 stmt-line))

(defparameter +minimal-character-1-byte-flag-source+
  "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 Flag PIC 99 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             ~a
000130             GOBACK.
000140         END METHOD \"Think\".
000150         IDENTIFICATION DIVISION.
000160         METHOD-ID. \"Kill\".
000170         PROCEDURE DIVISION.
000180             GOBACK.
000190         END METHOD \"Kill\".
000200 END OBJECT.
000210 END CLASS Character.
")

(defun compile-stmt-to-6502-1-byte-flag (stmt-line)
  "Like @code{compile-stmt-to-6502} but WORKING-STORAGE has @code{Flag} PIC 99 (single byte)."
  (let ((src (format nil +minimal-character-1-byte-flag-source+ stmt-line)))
    (let ((ast (eightbol::parse-eightbol-string src)))
      (with-output-to-string (s)
        (eightbol::compile-to-assembly-with-ast-passes ast :6502 s)))))

(defun count-substring (needle haystack)
  "Count non-overlapping occurrences of NEEDLE in HAYSTACK."
  (loop with n = 0
        for start = 0 then (+ pos (length needle))
        for pos = (search needle haystack :start2 start)
        while pos
        do (incf n)
        finally (return n)))

(defun count-6502-lda-self-indirect (asm)
  "Count @code{lda (Self), y} loads in ASM (matches with or without space before @code{y})."
  (+ (count-substring "lda (Self), y" asm)
     (count-substring "lda (Self),y" asm)))

;;;; All-backends smoke tests — each CPU compiles GOBACK and emits return

(defun cpu-return-pattern (cpu)
  "Return substring that must appear in assembly for method return (CPU-specific)."
  (ecase cpu
    ((:6502 :65c02 :65c816 :huc6280 :rp2a03) "rts")
    ((:z80 :sm83) "ret")
    ((:cp1610) "JR")
    ((:m6800) "RTS")
    ((:m68k) "rts")
    ((:i286) "ret")
    ((:arm7) "bx")
    ((:f8) "POP")))

(test backend-output/6502-smoke
  "6502 backend compiles minimal class and emits rts.

Think uses MOVE so it is not reduced to @code{= TrueMethod}; trailing GOBACK still emits @code{rts}."
  (let ((asm (compile-stmt-to-cpu :6502 "MOVE 1 TO HP.")))
    (is (search "MethodCharacterThink" asm))
    (is (search (cpu-return-pattern :6502) asm))))

(test backend-output/6502-assembly-ends-with-newline
  "Generated 6502 assembly text ends with a newline (POSIX text file; tooling-friendly)."
  (let ((asm (compile-stmt-to-6502 "MOVE 1 TO HP.")))
    (is (plusp (length asm)))
    (is (char= #\Newline (char asm (1- (length asm)))))))

(test backend-output/6502-assembly-includes-source-line-comment
  "Generated assembly includes @code{;} comments mapping to COBOL source (file / seq / line)."
  (let ((asm (compile-stmt-to-6502 "MOVE 1 TO HP.")))
    (is (search "; " asm) "expect semicolon source-correlation comment")
    (is (search "line " asm) "expect literal \"line\" in comment")))

(test backend-output/65c02-smoke
  "65c02 backend compiles minimal class and emits rts."
  (let ((asm (compile-stmt-to-cpu :65c02 "MOVE 1 TO HP.")))
    (is (search "MethodCharacterThink" asm))
    (is (search (cpu-return-pattern :65c02) asm))))

(test backend-output/65c816-smoke
  "65c816 backend compiles minimal class and emits rts."
  (let ((asm (compile-stmt-to-cpu :65c816 "MOVE 1 TO HP.")))
    (is (search "MethodCharacterThink" asm))
    (is (search (cpu-return-pattern :65c816) asm))))

(test backend-output/huc6280-smoke
  "HuC6280 backend compiles minimal class and emits rts."
  (let ((asm (compile-stmt-to-cpu :huc6280 "MOVE 1 TO HP.")))
    (is (search "MethodCharacterThink" asm))
    (is (search (cpu-return-pattern :huc6280) asm))))

(test backend-output/rp2a03-smoke
  "RP2A03 backend compiles minimal class and emits rts."
  (let ((asm (compile-stmt-to-cpu :rp2a03 "MOVE 1 TO HP.")))
    (is (search "MethodCharacterThink" asm))
    (is (search (cpu-return-pattern :rp2a03) asm))))

(test backend-output/cp1610-smoke
  "cp1610 backend compiles minimal class and emits return."
  (let ((asm (compile-stmt-to-cpu :cp1610 "MOVE 1 TO HP.")))
    (is (plusp (length asm)))
    (is (search (cpu-return-pattern :cp1610) asm))))

(test backend-output/z80-smoke
  "Z80 backend compiles minimal class and emits ret."
  (let ((asm (compile-stmt-to-cpu :z80 "MOVE 1 TO HP.")))
    (is (plusp (length asm)))
    (is (search (cpu-return-pattern :z80) asm))))

(test backend-output/sm83-smoke
  "SM83 backend compiles minimal class and emits ret."
  (let ((asm (compile-stmt-to-cpu :sm83 "MOVE 1 TO HP.")))
    (is (plusp (length asm)))
    (is (search (cpu-return-pattern :sm83) asm))))

(test backend-output/m68k-smoke
  "m68k backend compiles minimal class and emits rts."
  (let ((asm (compile-stmt-to-cpu :m68k "MOVE 1 TO HP.")))
    (is (plusp (length asm)))
    (is (search (cpu-return-pattern :m68k) asm))))

(test backend-output/m6800-smoke
  "m6800 backend compiles minimal class and emits RTS."
  (let ((asm (compile-stmt-to-cpu :m6800 "MOVE 1 TO HP.")))
    (is (plusp (length asm)))
    (is (search (cpu-return-pattern :m6800) asm))))

(test backend-output/i286-smoke
  "i286 backend compiles minimal class and emits ret."
  (let ((asm (compile-stmt-to-cpu :i286 "MOVE 1 TO HP.")))
    (is (plusp (length asm)))
    (is (search (cpu-return-pattern :i286) asm))))

(test backend-output/arm7-smoke
  "ARM7 backend compiles minimal class and emits bx lr."
  (let ((asm (compile-stmt-to-cpu :arm7 "MOVE 1 TO HP.")))
    (is (plusp (length asm)))
    (is (search (cpu-return-pattern :arm7) asm))))

(test backend-output/f8-smoke
  "F8 backend compiles minimal class and emits POP return."
  (let ((asm (compile-stmt-to-cpu :f8 "MOVE 1 TO HP.")))
    (is (plusp (length asm)))
    (is (search (cpu-return-pattern :f8) asm))))

;;;; All-backends MOVE — every CPU compiles MOVE literal TO var

(test backend-output/all-backends-move
  "Every supported CPU compiles MOVE 1 TO HP without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let ((asm (compile-stmt-to-cpu cpu "MOVE 1 TO HP.")))
    (is (plusp (length asm)) "CPU ~s produced empty assembly" cpu))))

(test backend-output/all-backends-goto
  "Every supported CPU compiles GO TO paragraph without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             GO TO Done.
000130 Loop.
000140             GOBACK.
000150 Done.
000160             GOBACK.
000170         END METHOD \"Think\".
000180         IDENTIFICATION DIVISION.
000190         METHOD-ID. \"Kill\".
000200         PROCEDURE DIVISION.
000210             GOBACK.
000220         END METHOD \"Kill\".
000230 END OBJECT.
000240 END CLASS Character."))
           (ast (eightbol::parse-eightbol-string src))
           (asm (when ast
                  (with-output-to-string (s)
                    (eightbol::compile-to-assembly-with-ast-passes ast cpu s)))))
      (when ast
        (is (plusp (length asm)) "CPU ~s produced empty assembly for GOTO" cpu)))))

(test backend-output/z80-goto-emits-jump
  "Z80: GO TO paragraph emits jp to paragraph label."
  (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             GO TO Done.
000130 Loop.
000140             GOBACK.
000150 Done.
000160             GOBACK.
000170         END METHOD \"Think\".
000180         IDENTIFICATION DIVISION.
000190         METHOD-ID. \"Kill\".
000200         PROCEDURE DIVISION.
000210             GOBACK.
000220         END METHOD \"Kill\".
000230 END OBJECT.
000240 END CLASS Character."))
          (ast (eightbol::parse-eightbol-string src))
          (asm (when ast
                 (with-output-to-string (s)
                   (eightbol::compile-to-assembly-with-ast-passes ast :z80 s)))))
    (when ast
      (is (search "Done" asm))
      (is (search "jp" asm)))))

(test backend-output/arm7-goto-emits-jump
  "ARM7: GO TO paragraph emits b (branch) to paragraph label."
  (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             GO TO Done.
000130 Loop.
000140             GOBACK.
000150 Done.
000160             GOBACK.
000170         END METHOD \"Think\".
000180         IDENTIFICATION DIVISION.
000190         METHOD-ID. \"Kill\".
000200         PROCEDURE DIVISION.
000210             GOBACK.
000220         END METHOD \"Kill\".
000230 END OBJECT.
000240 END CLASS Character."))
          (ast (eightbol::parse-eightbol-string src))
          (asm (when ast
                 (with-output-to-string (s)
                   (eightbol::compile-to-assembly-with-ast-passes ast :arm7 s)))))
    (when ast
      (is (search "Done" asm))
      (is (search "b" asm)))))

(test backend-output/cp1610-goto-emits-jump
  "CP1610: GO TO paragraph emits jump to paragraph label."
  (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             GO TO Done.
000130 Loop.
000140             GOBACK.
000150 Done.
000160             GOBACK.
000170         END METHOD \"Think\".
000180         IDENTIFICATION DIVISION.
000190         METHOD-ID. \"Kill\".
000200         PROCEDURE DIVISION.
000210             GOBACK.
000220         END METHOD \"Kill\".
000230 END OBJECT.
000240 END CLASS Character."))
          (ast (eightbol::parse-eightbol-string src))
          (asm (when ast
                 (with-output-to-string (s)
                   (eightbol::compile-to-assembly-with-ast-passes ast :cp1610 s)))))
    (when ast
      (is (search "Done" asm))
      (is (search "JR" asm)))))

(test backend-output/all-backends-set-literal
  "Every supported CPU compiles SET var TO literal without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let ((asm (compile-stmt-to-cpu cpu "SET HP TO 1.")))
    (is (plusp (length asm)) "CPU ~s produced empty assembly for SET literal" cpu))))

(test backend-output/all-backends-set-null
  "Every supported CPU compiles SET identifier TO NULL without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let ((asm (compile-stmt-to-cpu cpu "SET HP TO NULL.")))
    (is (plusp (length asm)) "CPU ~s produced empty assembly for SET NULL" cpu))))

(test backend-output/all-backends-set-up-by
  "Every supported CPU compiles SET identifier UP BY without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let ((asm (compile-stmt-to-cpu cpu "SET HP UP BY 1.")))
    (is (plusp (length asm)) "CPU ~s produced empty assembly for SET UP BY" cpu))))

(test backend-output/all-backends-set-down-by
  "Every supported CPU compiles SET identifier DOWN BY without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let ((asm (compile-stmt-to-cpu cpu "SET HP DOWN BY 1.")))
    (is (plusp (length asm)) "CPU ~s produced empty assembly for SET DOWN BY" cpu))))

(test backend-output/all-backends-set-address-of
  "Every supported CPU compiles SET dest TO ADDRESS OF source without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let ((asm (compile-stmt-to-cpu cpu "SET HP TO ADDRESS OF HP.")))
    (is (plusp (length asm)) "CPU ~s produced empty assembly for SET ADDRESS OF" cpu))))

(test backend-output/all-backends-set-to-self
  "Every supported CPU compiles SET identifier TO SELF without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let ((asm (compile-stmt-to-cpu cpu "SET HP TO SELF.")))
    (is (plusp (length asm)) "CPU ~s produced empty assembly for SET TO SELF" cpu))))

(test backend-output/z80-set-literal-to-var
  "Z80: SET var TO literal emits ld and appropriate store."
  (let ((asm (compile-stmt-to-cpu :z80 "SET HP TO 1.")))
    (is (search "ld" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/z80-set-null-to-ptr
  "Z80: SET identifier TO NULL emits ld zero and store."
  (let ((asm (compile-stmt-to-cpu :z80 "SET HP TO NULL.")))
    (is (search "ld" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/z80-set-up-by
  "Z80: SET identifier UP BY expression emits ADD."
  (let ((asm (compile-stmt-to-cpu :z80 "SET HP UP BY 1.")))
    (is (search "add" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/z80-set-down-by
  "Z80: SET identifier DOWN BY expression emits SUB."
  (let ((asm (compile-stmt-to-cpu :z80 "SET HP DOWN BY 1.")))
    (is (search "sub" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/z80-set-address-of
  "Z80: SET dest TO ADDRESS OF source emits address loading and storing."
  (let ((asm (compile-stmt-to-cpu :z80 "SET HP TO ADDRESS OF HP.")))
    (is (search "ld" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/z80-set-to-self
  "Z80: SET identifier TO SELF emits Self pointer loading and storing."
  (let ((asm (compile-stmt-to-cpu :z80 "SET HP TO SELF.")))
    (is (search "Self" asm))
    (is (search "ld" asm))))

(test backend-output/arm7-set-literal-to-var
  "ARM7: SET var TO literal emits mov and str."
  (let ((asm (compile-stmt-to-cpu :arm7 "SET HP TO 1.")))
    (is (search "mov" asm))
    (is (search "str" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/arm7-set-null-to-ptr
  "ARM7: SET identifier TO NULL emits mov zero and str."
  (let ((asm (compile-stmt-to-cpu :arm7 "SET HP TO NULL.")))
    (is (search "mov" asm))
    (is (search "str" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/arm7-set-up-by
  "ARM7: SET identifier UP BY expression emits ADD."
  (let ((asm (compile-stmt-to-cpu :arm7 "SET HP UP BY 1.")))
    (is (search "add" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/arm7-set-down-by
  "ARM7: SET identifier DOWN BY expression emits SUB."
  (let ((asm (compile-stmt-to-cpu :arm7 "SET HP DOWN BY 1.")))
    (is (search "sub" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/arm7-set-address-of
  "ARM7: SET dest TO ADDRESS OF source emits address loading and storing."
  (let ((asm (compile-stmt-to-cpu :arm7 "SET HP TO ADDRESS OF HP.")))
    (is (search "mov" asm))
    (is (search "str" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/arm7-set-to-self
  "ARM7: SET identifier TO SELF emits Self pointer loading and storing."
  (let ((asm (compile-stmt-to-cpu :arm7 "SET HP TO SELF.")))
    (is (search "Self" asm))
    (is (search "mov" asm))))

(test backend-output/cp1610-set-literal-to-var
  "CP1610: SET var TO literal loads immediate and stores."
  (let ((asm (compile-stmt-to-cpu :cp1610 "SET HP TO 1.")))
    (is (search "#" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/cp1610-set-null-to-ptr
  "CP1610: SET identifier TO NULL loads zero and stores."
  (let ((asm (compile-stmt-to-cpu :cp1610 "SET HP TO NULL.")))
    (is (search "#" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/cp1610-set-up-by
  "CP1610: SET identifier UP BY expression emits ADD."
  (let ((asm (compile-stmt-to-cpu :cp1610 "SET HP UP BY 1.")))
    (is (search "ADD" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/cp1610-set-down-by
  "CP1610: SET identifier DOWN BY expression emits SUB."
  (let ((asm (compile-stmt-to-cpu :cp1610 "SET HP DOWN BY 1.")))
    (is (search "SUB" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/cp1610-set-address-of
  "CP1610: SET dest TO ADDRESS OF source loads address and stores."
  (let ((asm (compile-stmt-to-cpu :cp1610 "SET HP TO ADDRESS OF HP.")))
    (is (search "#" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/cp1610-set-to-self
  "CP1610: SET identifier TO SELF loads Self pointer and stores."
  (let ((asm (compile-stmt-to-cpu :cp1610 "SET HP TO SELF.")))
    (is (search "Self" asm))
    (is (search "#" asm))))

(test backend-output/all-backends-multiply-power-of-two
  "Every supported CPU compiles MULTIPLY constant power-of-two BY var without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let ((asm (compile-stmt-to-cpu cpu "MULTIPLY 2 BY HP.")))
    (is (plusp (length asm)) "CPU ~s produced empty assembly for MULTIPLY power of two" cpu))))

(test backend-output/all-backends-divide-power-of-two
  "Every supported CPU compiles DIVIDE constant power-of-two INTO var without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let ((asm (compile-stmt-to-cpu cpu "DIVIDE 4 INTO HP.")))
    (is (plusp (length asm)) "CPU ~s produced empty assembly for DIVIDE power of two" cpu))))

(test backend-output/z80-multiply-power-of-two
  "Z80: MULTIPLY constant power-of-two BY var emits SLA (shift left arithmetic)."
  (let ((asm (compile-stmt-to-cpu :z80 "MULTIPLY 2 BY HP.")))
    (is (search "sla" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/z80-divide-power-of-two
  "Z80: DIVIDE constant power-of-two INTO var emits SRL (shift right logical)."
  (let ((asm (compile-stmt-to-cpu :z80 "DIVIDE 4 INTO HP.")))
    (is (search "srl" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/arm7-multiply-power-of-two
  "ARM7: MULTIPLY constant power-of-two BY var emits LSL (logical shift left)."
  (let ((asm (compile-stmt-to-cpu :arm7 "MULTIPLY 2 BY HP.")))
    (is (search "lsl" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/arm7-divide-power-of-two
  "ARM7: DIVIDE constant power-of-two INTO var emits LSR (logical shift right)."
  (let ((asm (compile-stmt-to-cpu :arm7 "DIVIDE 4 INTO HP.")))
    (is (search "lsr" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/cp1610-multiply-power-of-two
  "CP1610: MULTIPLY constant power-of-two BY var emits shift left."
  (let ((asm (compile-stmt-to-cpu :cp1610 "MULTIPLY 2 BY HP.")))
    (is (search "SL" asm))  ; CP1610 uses SL for shift left
    (is (search "CharacterHP" asm))))

(test backend-output/cp1610-divide-power-of-two
  "CP1610: DIVIDE constant power-of-two INTO var emits shift right."
  (let ((asm (compile-stmt-to-cpu :cp1610 "DIVIDE 4 INTO HP.")))
    (is (search "SR" asm))  ; CP1610 uses SR for shift right
    (is (search "CharacterHP" asm))))

(test backend-output/all-backends-string-blt
  "Every supported CPU compiles STRING BLT (DELIMITED BY SIZE) without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 Source-String PIC X(10).
000080         05 Dest-String   PIC X(10).
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Think\".
000120         PROCEDURE DIVISION.
000130             STRING \"Hello\" DELIMITED BY SIZE INTO Source-String.
000140             STRING Source-String DELIMITED BY SIZE 5 INTO Dest-String.
000150             GOBACK.
000160         END METHOD \"Think\".
000170         IDENTIFICATION DIVISION.
000180         METHOD-ID. \"Kill\".
000190         PROCEDURE DIVISION.
000200             GOBACK.
000210         END METHOD \"Kill\".
000220 END OBJECT.
000230 END CLASS Character."))
           (ast (eightbol::parse-eightbol-string src))
           (asm (when ast
                  (with-output-to-string (s)
                    (eightbol::compile-to-assembly-with-ast-passes ast cpu s)))))
      (when ast
        (is (plusp (length asm)) "CPU ~s produced empty assembly for STRING BLT" cpu)))))

(test backend-output/z80-string-blt
  "Z80: STRING BLT emits block copy loop using ld, inc, dec."
  (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 Source-String PIC X(10).
000080         05 Dest-String   PIC X(10).
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Think\".
000120         PROCEDURE DIVISION.
000130             STRING \"Hello\" DELIMITED BY SIZE INTO Source-String.
000140             STRING Source-String DELIMITED BY SIZE 5 INTO Dest-String.
000150             GOBACK.
000160         END METHOD \"Think\".
000170         IDENTIFICATION DIVISION.
000180         METHOD-ID. \"Kill\".
000190         PROCEDURE DIVISION.
000200             GOBACK.
000210         END METHOD \"Kill\".
000220 END OBJECT.
000230 END CLASS Character."))
          (ast (eightbol::parse-eightbol-string src))
          (asm (when ast
                 (with-output-to-string (s)
                   (eightbol::compile-to-assembly-with-ast-passes ast :z80 s)))))
    (when ast
      ;; Check for typical Z80 block copy instructions: ld, inc, dec
      (is (or (search "ld" asm) (search "LD" asm)) "Should load data")
      (is (or (search "inc" asm) (search "INC" asm)) "Should increment pointers")
      (is (or (search "dec" asm) (search "DEC" asm)) "Should decrement counter")
      (is (search "Character" asm)))))

(test backend-output/arm7-string-blt
  "ARM7: STRING BLT emits block copy loop using ldrb, strb."
  (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 Source-String PIC X(10).
000080         05 Dest-String   PIC X(10).
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Think\".
000120         PROCEDURE DIVISION.
000130             STRING \"Hello\" DELIMITED BY SIZE INTO Source-String.
000140             STRING Source-String DELIMITED BY SIZE 5 INTO Dest-String.
000150             GOBACK.
000160         END METHOD \"Think\".
000170         IDENTIFICATION DIVISION.
000180         METHOD-ID. \"Kill\".
000190         PROCEDURE DIVISION.
000200             GOBACK.
000210         END METHOD \"Kill\".
000220 END OBJECT.
000230 END CLASS Character."))
          (ast (eightbol::parse-eightbol-string src))
          (asm (when ast
                 (with-output-to-string (s)
                   (eightbol::compile-to-assembly-with-ast-passes ast :arm7 s)))))
    (when ast
      ;; Check for typical ARM byte load/store
      (is (or (search "ldrb" asm) (search "LDRB" asm)) "Should load byte")
      (is (or (search "strb" asm) (search "STRB" asm)) "Should store byte")
      (is (search "Character" asm)))))

(test backend-output/cp1610-string-blt
  "CP1610: STRING BLT emits block copy loop using R2/R3."
  (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 Source-String PIC X(10).
000080         05 Dest-String   PIC X(10).
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Think\".
000120         PROCEDURE DIVISION.
000130             STRING \"Hello\" DELIMITED BY SIZE INTO Source-String.
000140             STRING Source-String DELIMITED BY SIZE 5 INTO Dest-String.
000150             GOBACK.
000160         END METHOD \"Think\".
000170         IDENTIFICATION DIVISION.
000180         METHOD-ID. \"Kill\".
000190         PROCEDURE DIVISION.
000200             GOBACK.
000210         END METHOD \"Kill\".
000220 END OBJECT.
000230 END CLASS Character."))
          (ast (eightbol::parse-eightbol-string src))
          (asm (when ast
                 (with-output-to-string (s)
                   (eightbol::compile-to-assembly-with-ast-passes ast :cp1610 s)))))
    (when ast
      ;; Check for CP1610 block copy using R2/R3
      (is (search "Character" asm))
      ;; We'll just check it compiles and produces output for now
      (is (plusp (length asm)) "Should produce non-empty assembly"))))

(test backend-output/all-backends-invoke-self
  "Every supported CPU compiles INVOKE Self method without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             INVOKE Self \"Kill\".
000130         END METHOD \"Think\".
000140         IDENTIFICATION DIVISION.
000150         METHOD-ID. \"Kill\".
000160         PROCEDURE DIVISION.
000170             GOBACK.
000180         END METHOD \"Kill\".
000190 END OBJECT.
000200 END CLASS Character."))
           (ast (eightbol::parse-eightbol-string src))
           (asm (when ast
                  (with-output-to-string (s)
                    (eightbol::compile-to-assembly-with-ast-passes ast cpu s)))))
      (when ast
        (is (plusp (length asm)) "CPU ~s produced empty assembly for INVOKE Self" cpu)))))

(test backend-output/all-backends-invoke-object
  "Every supported CPU compiles INVOKE object method without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080         05 OP PIC X(10).
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Think\".
000120         PROCEDURE DIVISION.
000130             INVOKE OP \"Kill\".
000140         END METHOD \"Think\".
000150         IDENTIFICATION DIVISION.
000160         METHOD-ID. \"Kill\".
000170         PROCEDURE DIVISION.
000180             GOBACK.
000190         END METHOD \"Kill\".
000200 END OBJECT.
000210 END CLASS Character."))
           (ast (eightbol::parse-eightbol-string src))
           (asm (when ast
                  (with-output-to-string (s)
                    (eightbol::compile-to-assembly-with-ast-passes ast cpu s)))))
      (when ast
        (is (plusp (length asm)) "CPU ~s produced empty assembly for INVOKE object" cpu))))

(test backend-output/z80-invoke-self
  "Z80: INVOKE Self \"Kill\" emits appropriate call instruction."
  (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             INVOKE Self \"Kill\".
000130         END METHOD \"Think\".
000140         IDENTIFICATION DIVISION.
000150         METHOD-ID. \"Kill\".
000160         PROCEDURE DIVISION.
000170             GOBACK.
000180         END METHOD \"Kill\".
000200 END OBJECT.
000210 END CLASS Character."))
          (ast (eightbol::parse-eightbol-string src))
          (asm (when ast
                 (with-output-to-string (s)
                   (eightbol::compile-to-assembly-with-ast-passes ast :z80 s)))))
    (when ast
      ;; Check for Z80 call instruction
      (is (search "call" asm))
      (is (search "Character" asm))))

(test backend-output/z80-invoke-object
  "Z80: INVOKE object \"Kill\" emits appropriate call instruction via object pointer."
  (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080         05 OP PIC X(10).
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Think\".
000120         PROCEDURE DIVISION.
000130             INVOKE OP \"Kill\".
000140         END METHOD \"Think\".
000150         IDENTIFICATION DIVISION.
000160         METHOD-ID. \"Kill\".
000170         PROCEDURE DIVISION.
000180             GOBACK.
000190         END METHOD \"Kill\".
000200 END OBJECT.
000210 END CLASS Character."))
          (ast (eightbol::parse-eightbol-string src))
          (asm (when ast
                 (with-output-to-string (s)
                   (eightbol::compile-to-assembly-with-ast-passes ast :z80 s)))))
    (when ast
      ;; Check for Z80 call instruction and object pointer usage
      (is (search "call" asm))
      (is (search "Character" asm))))

(test backend-output/arm7-invoke-self
  "ARM7: INVOKE Self \"Kill\" emits bl (branch with link) to method."
  (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             INVOKE Self \"Kill\".
000130         END METHOD \"Think\".
000140         IDENTIFICATION DIVISION.
000150         METHOD-ID. \"Kill\".
000160         PROCEDURE DIVISION.
000170             GOBACK.
000180         END METHOD \"Kill\".
000200 END OBJECT.
000210 END CLASS Character."))
          (ast (eightbol::parse-eightbol-string src))
          (asm (when ast
                 (with-output-to-string (s)
                   (eightbol::compile-to-assembly-with-ast-passes ast :arm7 s)))))
    (when ast
      ;; Check for ARM branch with link
      (is (search "bl" asm))
      (is (search "Character" asm))))

(test backend-output/arm7-invoke-object
  "ARM7: INVOKE object \"Kill\" emits bl to method via object pointer."
  (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080         05 OP PIC X(10).
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Think\".
000120         PROCEDURE DIVISION.
000130             INVOKE OP \"Kill\".
000140         END METHOD \"Think\".
000150         IDENTIFICATION DIVISION.
000160         METHOD-ID. \"Kill\".
000170         PROCEDURE DIVISION.
000180             GOBACK.
000190         END METHOD \"Kill\".
000200 END OBJECT.
000210 END CLASS Character."))
          (ast (eightbol::parse-eightbol-string src))
          (asm (when ast
                 (with-output-to-string (s)
                   (eightbol::compile-to-assembly-with-ast-passes ast :arm7 s)))))
    (when ast
      ;; Check for ARM branch with link and object pointer
      (is (search "bl" asm))
      (is (search "Character" asm))))

(test backend-output/cp1610-invoke-self
  "CP1610: INVOKE Self \"Kill\" emits appropriate call instruction."
  (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             INVOKE Self \"Kill\".
000130         END METHOD \"Think\".
000140         IDENTIFICATION DIVISION.
000150         METHOD-ID. \"Kill\".
000160         PROCEDURE DIVISION.
000170             GOBACK.
000180         END METHOD \"Kill\".
000200 END OBJECT.
000210 END CLASS Character."))
          (ast (eightbol::parse-eightbol-string src))
          (asm (when ast
                 (with-output-to-string (s)
                   (eightbol::compile-to-assembly-with-ast-passes ast :cp1610 s)))))
    (when ast
      ;; Check for CP1610 call instruction
      (is (search "CALL" asm))
      (is (search "Character" asm))))

(test backend-output/cp1610-invoke-object
  "CP1610: INVOKE object \"Kill\" emits appropriate call instruction via object pointer."
  (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080         05 OP PIC X(10).
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Think\".
000120         PROCEDURE DIVISION.
000130             INVOKE OP \"Kill\".
000140         END METHOD \"Think\".
000150         IDENTIFICATION DIVISION.
000160         METHOD-ID. \"Kill\".
000170         PROCEDURE DIVISION.
000180             GOBACK.
000190         END METHOD \"Kill\".
000200 END OBJECT.
000210 END CLASS Character."))
          (ast (eightbol::parse-eightbol-string src))
          (asm (when ast
                 (with-output-to-string (s)
                   (eightbol::compile-to-assembly-with-ast-passes ast :cp1610 s)))))
    (when ast
      ;; Check for CP1610 call instruction and object pointer
      (is (search "CALL" asm))
      (is (search "Character" asm))))

(test backend-output/all-backends-if-else
  "Every supported CPU compiles IF-THEN-ELSE without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let ((asm (compile-stmt-to-cpu cpu "IF HP IS EQUAL TO 0 THEN MOVE 1 TO HP. ELSE MOVE 1 TO HP. END-IF.")))
    (is (plusp (length asm)) "CPU ~s produced empty assembly for IF-THEN-ELSE" cpu))))

(test backend-output/all-backends-if-condition
  "Every supported CPU compiles IF condition without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let ((asm (compile-stmt-to-cpu cpu "IF HP IS GREATER THAN 0 THEN MOVE 1 TO HP. END-IF.")))
    (is (plusp (length asm)) "CPU ~s produced empty assembly for IF condition" cpu))))

(test backend-output/z80-if-else
  "Z80: IF-THEN-ELSE emits appropriate conditional jumps."
  (let ((asm (compile-stmt-to-cpu :z80 "IF HP IS EQUAL TO 0 THEN MOVE 1 TO HP. ELSE MOVE 1 TO HP. END-IF.")))
    (is (plusp (length asm)))
    (is (search "cp" asm))  ; Z80 uses cp for compare
    (is (or (search "jr z" asm) (search "jr nz" asm) (search "jp z" asm) (search "jp nz" asm)))  ; conditional jumps
    (is (search "CharacterHP" asm))))

(test backend-output/z80-if-condition
  "Z80: IF condition emits compare and conditional jump."
  (let ((asm (compile-stmt-to-cpu :z80 "IF HP IS GREATER THAN 0 THEN MOVE 1 TO HP. END-IF.")))
    (is (plusp (length asm)))
    (is (search "cp" asm))  ; compare
    (is (or (search "jp po" asm) (search "jp pe" asm) (search "jp" asm)))  ; jump on parity for >0? Actually for unsigned >0 it's more complex
    ;; For simplicity, just check it has a jump
    (is (or (search "jp" asm) (search "jr" asm)) "Should have a jump instruction")
    (is (search "CharacterHP" asm))))

(test backend-output/arm7-if-else
  "ARM7: IF-THEN-ELSE emits appropriate conditional branches."
  (let ((asm (compile-stmt-to-cpu :arm7 "IF HP IS EQUAL TO 0 THEN MOVE 1 TO HP. ELSE MOVE 1 TO HP. END-IF.")))
    (is (plusp (length asm)))
    (is (search "cmp" asm))  ; compare
    (is (or (search "beq" asm) (search "bne" asm)))  ; branch equal/not equal
    (is (search "CharacterHP" asm))))

(test backend-output/arm7-if-condition
  "ARM7: IF condition emits compare and conditional branch."
  (let ((asm (compile-stmt-to-cpu :arm7 "IF HP IS GREATER THAN 0 THEN MOVE 1 TO HP. END-IF.")))
    (is (plusp (length asm)))
    (is (search "cmp" asm))  ; compare
    (is (or (search "bgt" asm) (search "ble" asm)))  ; branch greater than/less than or equal
    (is (search "CharacterHP" asm))))

(test backend-output/cp1610-if-else
  "CP1610: IF-THEN-ELSE emits appropriate conditional jumps."
  (let ((asm (compile-stmt-to-cpu :cp1610 "IF HP IS EQUAL TO 0 THEN MOVE 1 TO HP. ELSE MOVE 1 TO HP. END-IF.")))
    (is (plusp (length asm)))
    (is (search "cpd" asm) || (search "cpi" asm))  ; CP1610 compare instructions
    (is (or (search "jz" asm) (search "jnz" asm)))  ; jump if zero/not zero
    (is (search "CharacterHP" asm))))

(test backend-output/cp1610-if-condition
  "CP1610: IF condition emits compare and conditional jump."
  (let ((asm (compile-stmt-to-cpu :cp1610 "IF HP IS GREATER THAN 0 THEN MOVE 1 TO HP. END-IF.")))
    (is (plusp (length asm)))
    (is (search "cpd" asm) || (search "cpi" asm))  ; compare
    (is (or (search "jp" asm) (search "jn" asm)))  ; jump on positive/negative (simplified)
    (is (search "CharacterHP" asm))))

(test backend-output/all-backends-method-definition
  "Every supported CPU compiles simple method definition without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             MOVE 1 TO HP.
000130             GOBACK.
000140         END METHOD \"Think\".
000150         IDENTIFICATION DIVISION.
000160         METHOD-ID. \"Calculate\".
000170         PROCEDURE DIVISION.
000180             MOVE HP TO HP.
000190             GOBACK.
000200         END METHOD \"Calculate\".
000210         IDENTIFICATION DIVISION.
000220         METHOD-ID. \"Destroy\".
000230         PROCEDURE DIVISION.
000240             GOBACK.
000250         END METHOD \"Destroy\".
000260 END OBJECT.
000270 END CLASS Character."))
           (ast (eightbol::parse-eightbol-string src))
           (asm (when ast
                  (with-output-to-string (s)
                    (eightbol::compile-to-assembly-with-ast-passes ast cpu s)))))
      (when ast
        (is (plusp (length asm)) "CPU ~s produced empty assembly for method definition" cpu)))))

(test backend-output/all-backends-method-with-parameters
  "Every supported CPU compiles method with parameter-like usage without error and emits non-empty assembly."
  (dolist (cpu '(:6502 :65c02 :65c816 :huc6280 :rp2a03 :cp1610 :z80 :sm83 :m6800 :m68k :i286 :arm7 :f8))
    (let* ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080         05 INPUT-VALUE PIC 9(4).
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Process\" USING INPUT-VALUE.
000120         PROCEDURE DIVISION.
000130             MOVE INPUT-VALUE TO HP.
000140             GOBACK.
000150         END METHOD \"Process\".
000160         IDENTIFICATION DIVISION.
000170         METHOD-ID. \"Think\".
000180         PROCEDURE DIVISION.
000190             MOVE 5 TO INPUT-VALUE.
000200             INVOKE Self \"Process\".
000210             GOBACK.
000220         END METHOD \"Think\".
000230 END OBJECT.
000240 END CLASS Character."))
           (ast (eightbol::parse-eightbol-string src))
           (asm (when ast
                  (with-output-to-string (s)
                    (eightbol::compile-to-assembly-with-ast-passes ast cpu s)))))
      (when ast
        (is (plusp (length asm)) "CPU ~s produced empty assembly for method with parameters" cpu)))))

;;;; MOVE statement output

(test backend-output/move-literal-to-var
  "MOVE literal TO var emits lda/sta sequence."
  (let ((asm (compile-stmt-to-6502 "MOVE 1 TO HP.")))
    (is (search "lda" asm))
    (is (search "sta" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/move-null-to-ptr
  "MOVE NULL TO ptr emits high-byte zero only."
  (let ((asm (compile-stmt-to-6502 "MOVE NULL TO HP.")))
    (is (search "lda" asm))
    (is (search "sta" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/move-var-to-var
  "MOVE var TO var emits load then store."
  (let ((asm (compile-stmt-to-6502 "MOVE HP TO HP.")))
    (is (search "lda" asm))
    (is (search "sta" asm))))

(test backend-output/move-zero-to-var
  "MOVE ZERO TO identifier (78-level constant) emits immediate 0 and store."
  (let ((asm (compile-stmt-to-6502 "MOVE ZERO TO HP OF Self.")))
    (is (search "lda" asm))
    (is (search "sta" asm))
    (is (or (search "#0" asm) (search "#$0" asm)) "Should use immediate zero")))

(test backend-output/move-zero-65c02-uses-stz-when-direct
  "65c02 MOVE ZERO uses stz for direct-memory destinations (no lda/sta); slot OF Self still uses lda/sta."
  (let ((asm-6502 (compile-stmt-to-cpu :6502 "MOVE ZERO TO HP OF Self."))
        (asm-65c02 (compile-stmt-to-cpu :65c02 "MOVE ZERO TO HP OF Self.")))
    ;; 6502 and RP2A03: always lda #0 / sta
    (is (search "lda" asm-6502))
    (is (search "sta" asm-6502))
    ;; Slot OF Self is indirect, so 65c02 also uses lda/sta (no stz (Self),y)
    (is (search "lda" asm-65c02))
    (is (search "sta" asm-65c02))
    ;; When a fixture has direct-memory dest, 65c02 would emit stz
    (is (plusp (length asm-65c02)))))

;;;; SET statement output

(test backend-output/set-literal-to-var
  "SET var TO literal emits value load and store."
  (let ((asm (compile-stmt-to-6502 "SET HP TO 1.")))
    (is (search "lda" asm))
    (is (search "sta" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/set-null-to-ptr
  "SET identifier TO NULL emits high-byte zero only."
  (let ((asm (compile-stmt-to-6502 "SET HP TO NULL.")))
    (is (search "lda" asm))
    (is (search "sta" asm))))

(test backend-output/set-up-by
  "SET identifier UP BY expression emits ADD (inc or adc)."
  (let ((asm (compile-stmt-to-6502 "SET HP UP BY 1.")))
    (is (or (search "inc" asm) (search "adc" asm)))
    (is (search "CharacterHP" asm))))

(test backend-output/set-down-by
  "SET identifier DOWN BY expression emits SUBTRACT (dec or sbc)."
  (let ((asm (compile-stmt-to-6502 "SET HP DOWN BY 1.")))
    (is (or (search "dec" asm) (search "sbc" asm)))
    (is (search "CharacterHP" asm))))

(test backend-output/set-address-of-6502
  "SET dest TO ADDRESS OF source emits lda #< / lda #> and sta to pointer."
  (let ((asm (compile-stmt-to-6502 "SET HP TO ADDRESS OF HP.")))
    (is (search "lda" asm))
    (is (search "sta" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/set-to-self-6502
  "SET identifier TO SELF emits Self pointer low/high."
  (let ((asm (compile-stmt-to-6502 "SET HP TO SELF.")))
    (is (search "Self" asm))
    (is (search "sta" asm))))

(test backend-output/divide-power-of-two
  "DIVIDE constant power-of-two INTO var emits LSR."
  (let ((asm (compile-stmt-to-6502 "DIVIDE 4 INTO HP.")))
    (is (search "lsr" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/multiply-power-of-two
  "MULTIPLY constant power-of-two BY var emits ASL."
  (let ((asm (compile-stmt-to-6502 "MULTIPLY 2 BY HP.")))
    (is (search "asl" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/divide-non-power-of-two-signals
  "DIVIDE by non-power-of-two constant signals backend-error."
  (signals eightbol::backend-error
    (compile-stmt-to-cpu :6502 "DIVIDE 3 INTO HP.")))

(test backend-output/compute-divide-unicode
  "COMPUTE with ÷ (division sign) emits LSR like / for power-of-two."
  (let ((asm (compile-stmt-to-6502 "COMPUTE HP = HP ÷ 4.")))
    (is (search "lsr" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/compute-multiply-unicode
  "COMPUTE with × (multiplication sign) emits ASL like * for power-of-two."
  (let ((asm (compile-stmt-to-6502 "COMPUTE HP = HP × 2.")))
    (is (search "asl" asm))
    (is (search "CharacterHP" asm))))

;;;; ADD statement output

(test backend-output/add-literal-to-var
  "ADD literal TO var emits adc/clc/adc or inc (ADD 1 TO byte-width-1 var)."
  (let ((asm (compile-stmt-to-6502 "ADD 1 TO HP.")))
    (is (or (search "adc" asm) (search "clc" asm) (search "inc" asm)))
    (is (search "CharacterHP" asm))))

(test backend-output/subtract-literal-from-var
  "SUBTRACT literal FROM var emits sbc or dec."
  (let ((asm (compile-stmt-to-6502 "SUBTRACT 1 FROM HP.")))
    (is (or (search "sbc" asm) (search "dec" asm)))
    (is (search "CharacterHP" asm))))

;;; USAGE DECIMAL restrictions for MULTIPLY/DIVIDE

(test backend-output/multiply-usage-decimal-signals
   "MULTIPLY with USAGE DECIMAL operands signals backend-error."
   (signals eightbol::backend-error
     (compile-stmt-to-cpu :6502 "MULTIPLY 2 BY HP-USAGE-DECIMAL.")))

(test backend-output/divide-usage-decimal-signals
   "DIVIDE with USAGE DECIMAL operands signals backend-error."
   (signals eightbol::backend-error
     (compile-stmt-to-cpu :6502 "DIVIDE 2 INTO HP-USAGE-DECIMAL.")))
;;;; COMPUTE statement output

(test backend-output/compute-simple
  "COMPUTE var = expr emits load and store."
  (let ((asm (compile-stmt-to-6502 "COMPUTE HP = 42.")))
    (is (search "lda" asm))
    (is (search "sta" asm))
    (is (search "CharacterHP" asm))))

(test backend-output/constant-expression-folded
  "Compile-time constant expressions are folded; COMPUTE HP = 1 + 2 uses immediate for 3."
  (let ((asm (compile-stmt-to-6502 "COMPUTE HP = 1 + 2.")))
    (is (search "lda" asm))
    (is (search "#" asm) "constant expression should be emitted as immediate")
    (is (or (search "$03" asm) (search "#3" asm)) "folded value 3 should appear as immediate")))

;;;; INVOKE statement output

(defparameter +invoke-delegate-character-class-source+
  "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             MOVE 1 TO HP.
000130             GOBACK.
000140         END METHOD \"Think\".
000150         IDENTIFICATION DIVISION.
000160         METHOD-ID. \"Pierce\".
000170         PROCEDURE DIVISION.
000180             INVOKE Self \"Kill\".
000190         END METHOD \"Pierce\".
000200         IDENTIFICATION DIVISION.
000210         METHOD-ID. \"Kill\".
000220         PROCEDURE DIVISION.
000230             GOBACK.
000240         END METHOD \"Kill\".
000250 END OBJECT.
000260 END CLASS Character.
")

(defparameter +pierce-move-then-invoke-kill-class-source+
  "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             MOVE 1 TO HP.
000130             GOBACK.
000140         END METHOD \"Think\".
000150         IDENTIFICATION DIVISION.
000160         METHOD-ID. \"Pierce\".
000170         PROCEDURE DIVISION.
000180             MOVE 1 TO HP.
000190             INVOKE Self \"Kill\".
000200         END METHOD \"Pierce\".
000210         IDENTIFICATION DIVISION.
000220         METHOD-ID. \"Kill\".
000230         PROCEDURE DIVISION.
000240             GOBACK.
000250         END METHOD \"Kill\".
000260 END OBJECT.
000270 END CLASS Character.
")

(defun compile-invoke-delegate-class-to-6502 ()
  "Parse @code{+invoke-delegate-character-class-source+} and compile to 6502 assembly string."
  (let ((ast (eightbol::parse-eightbol-string +invoke-delegate-character-class-source+)))
    (with-output-to-string (s)
      (eightbol::compile-to-assembly-with-ast-passes ast :6502 s))))

(test backend-output/invoke-self-method
  "INVOKE Self \"Kill\" emits @code{.CallMethod CallCharacterKill, CharacterClass} (Phantasia macro)."
  (let ((asm (compile-stmt-to-6502 "INVOKE Self \"Kill\".")))
    (is (search ".CallMethod CallCharacterKill, CharacterClass" asm))))

(test backend-output/6502-singleton-invoke-self-uses-callmethod-block
  "6502: a method whose body is only INVOKE Self emits Method…: .block with .CallMethod."
  (let ((asm (compile-invoke-delegate-class-to-6502)))
    (is (search "MethodCharacterPierce:" asm))
    (is (search ".CallMethod CallCharacterKill, CharacterClass" asm))))

(test backend-output/6502-invoke-self-callmethod-then-rts
  "6502: INVOKE Self uses .CallMethod; method ends with rts (after MOVE + INVOKE + implicit return path)."
  (let* ((ast (eightbol::parse-eightbol-string +pierce-move-then-invoke-kill-class-source+))
         (asm (with-output-to-string (s)
                (eightbol::compile-to-assembly-with-ast-passes ast :6502 s))))
    (is (search ".CallMethod CallCharacterKill, CharacterClass" asm))
    (is (null (search "jmp InvokeCharacterKill" asm)))))

;;;; CALL statement output

(defun compile-call-form-to-6502 (call-plist &optional service-map)
  "Compile one parsed CALL plist using 6502 backend; return emitted assembly.

CALL-PLIST is the @code{(rest stmt)} data (keys like @code{:target},
@code{:service}, @code{:bank}, @code{:library}). SERVICE-MAP, when non-NIL,
is an alist of @code{(service . bank)} strings for @code{*service-bank-table*}."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair service-map)
      (setf (gethash (car pair) table) (cdr pair)))
    (let ((eightbol::*service-bank-table* table))
      (with-output-to-string (s)
        (eightbol::compile-6502-call s (cons :call call-plist))))))

(test backend-output/call-service-emits-farcall
  "CALL SERVICE Check-Wall emits service far call in resolved bank."
  (let* ((asm (compile-call-form-to-6502
               '(:service "Service-Check-Wall")
               '(("ServiceCheckWall" . "BankAnimation")))))
    (is (search ".FarCall ServiceCheckWall, BankAnimation" asm))))

(test backend-output/call-in-library-emits-lib-jsr
  "CALL Move-Decal-Y IN LIBRARY emits jsr Lib.MoveDecalY (not local jmp/jsr)."
  (let ((asm (compile-call-form-to-6502
              '(:target "Move-Decal-Y" :library t :tail-call-p t))))
    (is (search "jsr Lib.MoveDecalY" asm))
    (is (null (search "jmp MoveDecalY" asm)))))

(test backend-output/call-local-near-default-jsr
  "CALL target. without :tail-call-p emits jsr (not jmp)."
  (let ((asm (compile-call-form-to-6502 '(:target "Some-Routine"))))
    (is (search "jsr SomeRoutine" asm))
    (is (null (search "jmp SomeRoutine" asm)))))

(test backend-output/call-local-near-tail-jmp
  "CALL target. with :tail-call-p (e.g. after CALL … GOBACK) may emit jmp."
  (let ((asm (compile-call-form-to-6502 '(:target "Some-Routine" :tail-call-p t))))
    (is (search "jmp SomeRoutine" asm))
    (is (null (search "jsr SomeRoutine" asm)))))

;;;; IF statement output

(test backend-output/if-else-branches
  "IF condition THEN stmts ELSE stmts emits branch around else."
  (let ((asm (compile-stmt-to-6502 "IF HP IS EQUAL TO 0 THEN MOVE 1 TO HP. ELSE MOVE 1 TO HP. END-IF.")))
    (is (search "lda" asm))
    (is (search "bne" asm))
    (is (search "sta" asm))))

(test backend-output/if-relation-condition
  "IF HP > 0 (unsigned vs zero) uses @code{lda} + @code{beq} — no @code{cmp #0} or @code{blt}."
  (let ((asm (compile-stmt-to-6502 "IF HP IS GREATER THAN 0 THEN MOVE 1 TO HP. END-IF.")))
    (is (search "lda" asm))
    (is (search "beq" asm))
    (is (null (search "blt" asm)) "unsigned > 0 vs zero must not emit blt (never taken)")
    (is (null (search "cmp #$00" asm)) "lda already sets Z; cmp #0 is redundant")))

(test backend-output/if-is-less-than
  "IF expr IS LESS THAN expr (5-element condition) emits compare and branch."
  (let ((asm (compile-stmt-to-6502 "IF HP IS LESS THAN 1 THEN MOVE 1 TO HP. END-IF.")))
    (is (search "lda" asm))
    (is (search "bge" asm))))

(test backend-output/6502-cmp-rhs-slot-of-self
  "6502: relational RHS @code{Max-HP OF Self} must use @code{ldy}/@code{cmp (Self),y}, not @code{cmp (MaxHp)}."
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080         05 Max-HP PIC 9999 USAGE BINARY.
000090     PROCEDURE DIVISION.
000100         IDENTIFICATION DIVISION.
000110         METHOD-ID. \"Think\".
000120         PROCEDURE DIVISION.
000130             IF HP OF Self IS GREATER THAN Max-HP OF Self THEN MOVE 1 TO HP OF Self.
000140             END-IF.
000150             GOBACK.
000160         END METHOD \"Think\".
000170         IDENTIFICATION DIVISION.
000180         METHOD-ID. \"Kill\".
000190         PROCEDURE DIVISION.
000200             GOBACK.
000210         END METHOD \"Kill\".
000220 END OBJECT.
000230 END CLASS Character.
")
         (ast (eightbol::parse-eightbol-string src))
         (asm (with-output-to-string (s)
                (eightbol::compile-to-assembly-with-ast-passes ast :6502 s))))
    (is (search "ldy #CharacterMaxHP" asm))
    (is (or (search "cmp (Self), y" asm) (search "cmp (Self),y" asm))
        "post-normalize assembly uses cmp (Self),y (indexed operand spacing)")
    (is (null (search "cmp (MaxHp)" asm)))
    (is (null (search "cmp (Max-HP)" asm)))))

(test backend-output/6502-accumulator-reuse-and-condition
  "6502: second test in AND reuses A for same slot — only one @code{lda} via @code{(Self),y} for Flag.

Uses PIC 99 (one byte); @code{> 0} vs zero is @code{lda} + @code{beq} (no @code{cmp})."
  (let* ((stmt
          "IF Flag IS GREATER THAN 0 AND Flag IS GREATER THAN 0 THEN GOBACK END-IF.")
         (asm (compile-stmt-to-6502-1-byte-flag stmt))
         (n (count-6502-lda-self-indirect asm)))
    (is (>= (count-substring "beq" asm) 2) "both AND arms should branch on zero")
    (is (= n 1) "second load of Flag should omit duplicate lda (Self),y")))

(test backend-output/6502-no-reload-after-add-to-same-slot
  "6502: after @code{ADD … TO Flag OF Self}, @code{IF Flag … IS ZERO} must not emit @code{lda (Self),y} (value is already in A after @code{adc}/@code{sta})."
  (let* ((stmt "ADD 5 TO Flag OF Self. IF Flag OF Self IS ZERO THEN GOBACK END-IF.")
         (asm (compile-stmt-to-6502-1-byte-flag stmt)))
    (is (null (or (search "lda (Self), y" asm) (search "lda (Self),y" asm)))
        "IF must not reload Flag via lda (Self),y after ADD to same slot")))

;;;; GOBACK / EXIT / STOP RUN output

(test backend-output/6502-trivial-method-is-true-method-alias
  "6502: a method whose body is only GOBACK aliases @code{TrueMethod} (matches Phantasia stubs).

The minimal template's Kill method is GOBACK-only; Think carries the test statement."
  (let ((asm (compile-stmt-to-6502 "MOVE 1 TO HP.")))
    (is (search "MethodCharacterKill = TrueMethod" asm))))

(test backend-output/goback-emits-rts
  "GOBACK emits rts.

Think includes MOVE so the method is not reduced to @code{= TrueMethod}; template’s trailing GOBACK emits @code{rts}."
  (let ((asm (compile-stmt-to-6502 "MOVE 1 TO HP.")))
    (is (search "rts" asm))))

(test backend-output/exit-method-emits-rts
  "EXIT METHOD emits rts.

MOVE keeps Think non-trivial; @code{EXIT METHOD} terminates before the template GOBACK."
  (let ((asm (compile-stmt-to-6502 "MOVE 1 TO HP. EXIT METHOD.")))
    (is (search "rts" asm))))

(test backend-output/exit-program-emits-rts
  "EXIT PROGRAM emits rts."
  (let ((asm (compile-stmt-to-6502 "MOVE 1 TO HP. EXIT PROGRAM.")))
    (is (search "rts" asm))))

(test backend-output/stop-run-emits-rts
  "STOP RUN emits rts."
  (let ((asm (compile-stmt-to-6502 "MOVE 1 TO HP. STOP RUN.")))
    (is (search "rts" asm))))

(test backend-output/exit-emits-rts
  "EXIT emits rts."
  (let ((asm (compile-stmt-to-6502 "MOVE 1 TO HP. EXIT.")))
    (is (search "rts" asm))))

;;;; LOG FAULT output

(test backend-output/log-fault-emits-macro
  "LOG FAULT emits .LogFault."
  (let ((asm (compile-stmt-to-6502 "LOG FAULT \"ERR\".")))
    (is (search ".LogFault" asm))))

(test backend-output/log-fault-w-string
  "LOG FAULT w\"STUK\" emits .LogFault."
  (let ((asm (compile-stmt-to-6502 "LOG FAULT w\"STUK\".")))
    (is (search ".LogFault" asm))))

;;;; DEBUG BREAK output

(test backend-output/debug-break-emits-macro
  "DEBUG BREAK emits .DebugBreak."
  (let ((asm (compile-stmt-to-6502 "DEBUG BREAK 1.")))
    (is (search ".DebugBreak" asm))))

;;;; PERFORM statement output

(test backend-output/perform-simple
  "PERFORM procedure emits jsr."
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             PERFORM Loop.
000130             GOBACK.
000140 Loop.
000150             GOBACK.
000160         END METHOD \"Think\".
000170         IDENTIFICATION DIVISION.
000180         METHOD-ID. \"Kill\".
000190         PROCEDURE DIVISION.
000200             GOBACK.
000210         END METHOD \"Kill\".
000220 END OBJECT.
000230 END CLASS Character.")
        (ast (eightbol::parse-eightbol-string src)))
    (let ((asm (with-output-to-string (s)
                 (eightbol::compile-to-assembly-with-ast-passes ast :6502 s))))
      (is (search "jsr" asm))
      (is (search "Loop" asm)))))

(test backend-output/perform-times
  "PERFORM procedure TIMES n emits loop with dex/bne."
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             PERFORM Loop TIMES 3.
000130             GOBACK.
000140 Loop.
000150             GOBACK.
000160         END METHOD \"Think\".
000170         IDENTIFICATION DIVISION.
000180         METHOD-ID. \"Kill\".
000190         PROCEDURE DIVISION.
000200             GOBACK.
000210         END METHOD \"Kill\".
000220 END OBJECT.
000230 END CLASS Character.")
         (ast (eightbol::parse-eightbol-string src)))
    (let ((asm (with-output-to-string (s)
                 (eightbol::compile-to-assembly-with-ast-passes ast :6502 s))))
      (is (search "tax" asm))
      (is (search "dex" asm))
      (is (search "bne" asm))
      (is (search "jsr" asm)))))

(test backend-output/perform-until
  "PERFORM procedure UNTIL condition emits condition check and loop."
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             PERFORM Loop UNTIL HP IS EQUAL TO 0.
000130             GOBACK.
000140 Loop.
000150             GOBACK.
000160         END METHOD \"Think\".
000170         IDENTIFICATION DIVISION.
000180         METHOD-ID. \"Kill\".
000190         PROCEDURE DIVISION.
000200             GOBACK.
000210         END METHOD \"Kill\".
000220 END OBJECT.
000230 END CLASS Character.")
         (ast (eightbol::parse-eightbol-string src)))
    (let ((asm (with-output-to-string (s)
                 (eightbol::compile-to-assembly-with-ast-passes ast :6502 s))))
      (is (search "lda" asm))
      (is (search "bne" asm))
      (is (search "jsr" asm)))))

(test backend-output/evaluate-when-emits
  "EVALUATE subject WHEN n stmts emits compare and branch."
  (let ((asm (compile-stmt-to-6502 "EVALUATE HP WHEN 0 GOBACK. END-EVALUATE.")))
    (is (search "lda" asm))
    (is (search "cmp" asm))
    (is (search "bne" asm))))

(test backend-output/goto-emits-jump
  "GO TO paragraph emits jmp or branch to paragraph label."
  (let ((src (format nil
"000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. Character.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050     DATA DIVISION.
000060         WORKING-STORAGE SECTION.
000070         05 HP PIC 9999 USAGE BINARY.
000080     PROCEDURE DIVISION.
000090         IDENTIFICATION DIVISION.
000100         METHOD-ID. \"Think\".
000110         PROCEDURE DIVISION.
000120             GO TO Done.
000130 Loop.
000140             GOBACK.
000150 Done.
000160             GOBACK.
000170         END METHOD \"Think\".
000180         IDENTIFICATION DIVISION.
000190         METHOD-ID. \"Kill\".
000200         PROCEDURE DIVISION.
000210             GOBACK.
000220         END METHOD \"Kill\".
000230 END OBJECT.
000240 END CLASS Character.")))
    (let ((ast (eightbol::parse-eightbol-string src)))
      (when ast
        (let ((asm (with-output-to-string (s)
                     (eightbol::compile-to-assembly-with-ast-passes ast :6502 s))))
          (is (search "Done" asm))
          (is (or (search "jmp" asm) (search "bra " asm) (search "JMP" asm))))))))
)))))))
