;;;; tests/backend-comprehensive-ast-tests.lisp — Verify core backends handle AST types
;;; Tests core AST node types on selected backends

(in-package :eightbol/test)

(def-suite :backend-ast-comprehensive :description "Backend AST node handling")
(in-suite :backend-ast-comprehensive)

;;;; Helper

(defun safe-compile (cobol-src backend)
  "Safely compile COBOL, return assembly or NIL"
  (handler-case
      (let* ((ast (eightbol::parse-eightbol-string cobol-src))
             (asm (with-output-to-string (s)
                    (eightbol::compile-to-assembly-with-ast-passes ast backend s))))
        asm)
    (error nil)))

;;;; :move on core backends

(defparameter *cobol-move*
  "000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. MoveTest.
000030 DATA DIVISION.
000040 WORKING-STORAGE SECTION.
000050 05 X PIC 9999 USAGE BINARY.
000060 PROCEDURE DIVISION.
000070 MOVE 5 TO X.
000080 STOP RUN.")

(test move-6502-backend
  "6502 :move compiles"
  (is (safe-compile *cobol-move* :6502)))

(test move-z80-backend
  "Z80 :move compiles"
  (is (safe-compile *cobol-move* :z80)))

(test move-arm7-backend
  "ARM7 :move compiles"
  (is (safe-compile *cobol-move* :arm7)))

(test move-stack-backend
  "Stack :move compiles"
  (is (safe-compile *cobol-move* :stack)))

;;;; :set on core backends

(defparameter *cobol-set*
  "000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. SetTest.
000030 DATA DIVISION.
000040 WORKING-STORAGE SECTION.
000050 05 X PIC 9999 USAGE BINARY.
000060 PROCEDURE DIVISION.
000070 SET X TO 10.
000080 STOP RUN.")

(test set-6502-backend
  "6502 :set compiles"
  (is (safe-compile *cobol-set* :6502)))

(test set-z80-backend
  "Z80 :set compiles"
  (is (safe-compile *cobol-set* :z80)))

;;;; :compute on core backends

(defparameter *cobol-compute*
  "000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. AddTest.
000030 DATA DIVISION.
000040 WORKING-STORAGE SECTION.
000050 05 X PIC 9999 USAGE BINARY.
000060 05 Y PIC 9999 USAGE BINARY.
000070 PROCEDURE DIVISION.
000080 COMPUTE X = Y + 5.
000090 STOP RUN.")

(test compute-6502-backend
  "6502 :compute compiles"
  (is (safe-compile *cobol-compute* :6502)))

(test compute-z80-backend
  "Z80 :compute compiles"
  (is (safe-compile *cobol-compute* :z80)))

