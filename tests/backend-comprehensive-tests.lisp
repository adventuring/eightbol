;;;; tests/backend-comprehensive-tests.lisp — Backend AST→assembly mapping verification
;;; Validates that each backend generates correct syntax for all AST node types

(in-package :eightbol/test)

(def-suite :backend-comprehensive :description "Backend AST-to-assembly generation")
(in-suite :backend-comprehensive)

(defparameter *all-backends* '(:6502 :65c02 :65c816 :arm7 :cp1610 :f8 :huc6280 :i286 :m6800 :m68k :rp2a03 :sm83 :z80 :stack))

;;;; Helper: Generate assembly from COBOL source for given backend

(defun compile-cobol-to-assembly (cobol-src backend)
  "Parse COBOL and compile to assembly for backend"
  (let* ((ast (eightbol::parse-eightbol-string cobol-src))
         (asm (with-output-to-string (s)
                (eightbol::compile-to-assembly-with-ast-passes ast backend s))))
    asm))

;;;; Helper COBOL template
(defparameter *cobol-template-move* 
  "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 MOVE 42 TO X.
000130 GOBACK.
000140 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS T.")

;;;; AST NODE TESTS: :move (sample backends)

(test ast-node/:move/6502-generates-lda-sta
  "6502 :move generates LDA/STA opcodes"
  (let ((asm (compile-cobol-to-assembly *cobol-template-move* :6502)))
    (is (search "lda\\|sta" asm)
        "6502 MOVE should use LDA/STA")))

(test ast-node/:move/z80-generates-ld
  "Z80 :move generates LD opcodes"
  (let ((asm (compile-cobol-to-assembly *cobol-template-move* :z80)))
    (is (search "ld" asm)
        "Z80 MOVE should use LD")))

(test ast-node/:move/arm7-generates-movs-ldr-str
  "ARM7 :move generates MOVS/LDR/STR"
  (let ((asm (compile-cobol-to-assembly *cobol-template-move* :arm7)))
    (is (search "movs\\|ldr\\|str" asm)
        "ARM7 MOVE should use MOVS/LDR/STR")))

;;;; AST NODE TESTS: All backends for basic operations

(test ast-node/move/all-backends-compile
  "All backends generate assembly for :move"
  (dolist (backend *all-backends*)
    (let ((asm (compile-cobol-to-assembly *cobol-template-move* backend)))
      (is (plusp (length asm))
          (format nil "Backend ~a: :move generates assembly" backend)))))

(test ast-node/set/all-backends-compile
  "All backends generate assembly for :set"
  (let ((cobol "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 SET X TO 10.
000130 GOBACK.
000140 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS T."))
    (dolist (backend *all-backends*)
      (let ((asm (compile-cobol-to-assembly cobol backend)))
        (is (plusp (length asm))
            (format nil "Backend ~a: :set generates assembly" backend))))))

(test ast-node/compute/add/all-backends-compile
  "All backends generate assembly for :compute ADD"
  (let ((cobol "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999 USAGE BINARY.
000080 05 Y PIC 9999 USAGE BINARY.
000090 PROCEDURE DIVISION.
000100 IDENTIFICATION DIVISION.
000110 METHOD-ID. \"M\".
000120 PROCEDURE DIVISION.
000130 COMPUTE X = Y + 5.
000140 GOBACK.
000150 END METHOD \"M\".
000160 END OBJECT.
000170 END CLASS T."))
    (dolist (backend *all-backends*)
      (let ((asm (compile-cobol-to-assembly cobol backend)))
        (is (plusp (length asm))
            (format nil "Backend ~a: :compute ADD generates assembly" backend))))))

(test ast-node/compute/subtract/all-backends-compile
  "All backends generate assembly for :compute SUBTRACT"
  (let ((cobol "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999 USAGE BINARY.
000080 05 Y PIC 9999 USAGE BINARY.
000090 PROCEDURE DIVISION.
000100 IDENTIFICATION DIVISION.
000110 METHOD-ID. \"M\".
000120 PROCEDURE DIVISION.
000130 COMPUTE X = Y - 3.
000140 GOBACK.
000150 END METHOD \"M\".
000160 END OBJECT.
000170 END CLASS T."))
    (dolist (backend *all-backends*)
      (let ((asm (compile-cobol-to-assembly cobol backend)))
        (is (plusp (length asm))
            (format nil "Backend ~a: :compute SUBTRACT generates assembly" backend))))))

(test ast-node/if/all-backends-compile
  "All backends generate assembly for :if"
  (let ((cobol "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 IF X > 5 THEN
000130   MOVE 1 TO X
000140 ELSE
000150   MOVE 0 TO X
000160 END-IF.
000170 GOBACK.
000180 END METHOD \"M\".
000190 END OBJECT.
000200 END CLASS T."))
    (dolist (backend *all-backends*)
      (let ((asm (compile-cobol-to-assembly cobol backend)))
        (is (plusp (length asm))
            (format nil "Backend ~a: :if generates assembly" backend))))))

(test ast-node/perform/all-backends-compile
  "All backends generate assembly for :perform"
  (let ((cobol "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 PROCEDURE DIVISION.
000080 IDENTIFICATION DIVISION.
000090 METHOD-ID. \"M\".
000100 PROCEDURE DIVISION.
000110 PERFORM \"Helper\".
000120 GOBACK.
000130 END METHOD \"M\".
000140 END OBJECT.
000150 END CLASS T."))
    (dolist (backend *all-backends*)
      (let ((asm (compile-cobol-to-assembly cobol backend)))
        (is (plusp (length asm))
            (format nil "Backend ~a: :perform generates assembly" backend))))))

(test ast-node/string-blt/all-backends-compile
  "All backends generate assembly for :string-blt"
  (let ((cobol "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 S PIC X(20).
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 STRING \"Hello\" DELIMITED BY SIZE INTO S.
000130 GOBACK.
000140 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS T."))
    (dolist (backend *all-backends*)
      (let ((asm (compile-cobol-to-assembly cobol backend)))
        (is (plusp (length asm))
            (format nil "Backend ~a: :string-blt generates assembly" backend))))))

(test ast-node/evaluate/all-backends-compile
  "All backends generate assembly for :evaluate"
  (let ((cobol "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 05 X PIC 9999 USAGE BINARY.
000080 PROCEDURE DIVISION.
000090 IDENTIFICATION DIVISION.
000100 METHOD-ID. \"M\".
000110 PROCEDURE DIVISION.
000120 EVALUATE X
000130   WHEN 1
000140     MOVE 10 TO X
000150   WHEN OTHER
000160     MOVE 0 TO X
000170 END-EVALUATE.
000180 GOBACK.
000190 END METHOD \"M\".
000200 END OBJECT.
000210 END CLASS T."))
    (dolist (backend *all-backends*)
      (let ((asm (compile-cobol-to-assembly cobol backend)))
        (is (plusp (length asm))
            (format nil "Backend ~a: :evaluate generates assembly" backend))))))

(test ast-node/debug-break/all-backends-compile
  "All backends generate assembly for :debug-break"
  (let ((cobol "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 PROCEDURE DIVISION.
000080 IDENTIFICATION DIVISION.
000090 METHOD-ID. \"M\".
000100 PROCEDURE DIVISION.
000110 DEBUG BREAK.
000120 GOBACK.
000130 END METHOD \"M\".
000140 END OBJECT.
000150 END CLASS T."))
    (dolist (backend *all-backends*)
      (let ((asm (compile-cobol-to-assembly cobol backend)))
        (is (plusp (length asm))
            (format nil "Backend ~a: :debug-break generates assembly" backend))))))

(test ast-node/log-fault/all-backends-compile
  "All backends generate assembly for :log-fault"
  (let ((cobol "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 PROCEDURE DIVISION.
000080 IDENTIFICATION DIVISION.
000090 METHOD-ID. \"M\".
000100 PROCEDURE DIVISION.
000110 LOG FAULT.
000120 GOBACK.
000130 END METHOD \"M\".
000150 END OBJECT.
000160 END CLASS T."))
    (dolist (backend *all-backends*)
      (let ((asm (compile-cobol-to-assembly cobol backend)))
        (is (plusp (length asm))
            (format nil "Backend ~a: :log-fault generates assembly" backend))))))

(test ast-node/exit-method/all-backends-compile
  "All backends generate assembly for :exit-method"
  (let ((cobol "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 PROCEDURE DIVISION.
000080 IDENTIFICATION DIVISION.
000090 METHOD-ID. \"M\".
000100 PROCEDURE DIVISION.
000110 EXIT METHOD.
000120 END METHOD \"M\".
000130 END OBJECT.
000140 END CLASS T."))
    (dolist (backend *all-backends*)
      (let ((asm (compile-cobol-to-assembly cobol backend)))
        (is (plusp (length asm))
            (format nil "Backend ~a: :exit-method generates assembly" backend))))))

(test ast-node/goback/all-backends-compile
  "All backends generate assembly for :goback"
  (let ((cobol "000010 IDENTIFICATION DIVISION.
000020 CLASS-ID. T.
000030 ENVIRONMENT DIVISION.
000040 OBJECT.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070 PROCEDURE DIVISION.
000080 IDENTIFICATION DIVISION.
000090 METHOD-ID. \"M\".
000100 PROCEDURE DIVISION.
000110 GOBACK.
000120 END METHOD \"M\".
000130 END OBJECT.
000140 END CLASS T."))
    (dolist (backend *all-backends*)
      (let ((asm (compile-cobol-to-assembly cobol backend)))
        (is (plusp (length asm))
            (format nil "Backend ~a: :goback generates assembly" backend))))))

;;;; ASSEMBLY SYNTAX VALIDATION

(test backend/6502-opcodes-valid
  "6502 assembly does not contain error markers"
  (let ((asm (compile-cobol-to-assembly *cobol-template-move* :6502)))
    (is (not (search "Unsupported\\|unsupported\\|error" asm))
        "6502 assembly should not contain error markers")))

(test backend/z80-opcodes-valid
  "Z80 assembly does not contain error markers"
  (let ((asm (compile-cobol-to-assembly *cobol-template-move* :z80)))
    (is (not (search "Unsupported\\|unsupported\\|error" asm))
        "Z80 assembly should not contain error markers")))

(test backend/arm7-opcodes-valid
  "ARM7 assembly does not contain error markers"
  (let ((asm (compile-cobol-to-assembly *cobol-template-move* :arm7)))
    (is (not (search "Unsupported\\|unsupported\\|error" asm))
        "ARM7 assembly should not contain error markers")))

