;;;; tests/backend-comprehensive-tests.lisp — Backend AST→assembly verification
;;; Validates that each backend generates assembly without errors

(in-package :eightbol/test)

(def-suite :backend-comprehensive :description "Backend AST-to-assembly generation")
(in-suite :backend-comprehensive)

;;;; Helper: compile COBOL to assembly

(defun try-compile-to-assembly (cobol-src backend)
  "Safely compile COBOL to assembly, return NIL on error"
  (handler-case
      (let* ((ast (eightbol::parse-eightbol-string cobol-src))
             (asm (with-output-to-string (s)
                    (eightbol::compile-to-assembly-with-ast-passes ast backend s))))
        asm)
    (error (e) nil)))

;;;; COBOL template for :move

(defparameter *cobol-move*
  "000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. CompileTest.
000030 DATA DIVISION.
000040 WORKING-STORAGE SECTION.
000050 05 X PIC 9999 USAGE BINARY.
000060 PROCEDURE DIVISION.
000070 MOVE 42 TO X.
000080 STOP RUN.")

;;;; Backend Tests

(test backend/6502-move-compiles
  "6502 backend compiles :move"
  (let ((asm (try-compile-to-assembly *cobol-move* :6502)))
    (is (and asm (plusp (length asm)))
        "6502 should compile :move")))

(test backend/z80-move-compiles
  "Z80 backend compiles :move"
  (let ((asm (try-compile-to-assembly *cobol-move* :z80)))
    (is (and asm (plusp (length asm)))
        "Z80 should compile :move")))

(test backend/arm7-move-compiles
  "ARM7 backend compiles :move"
  (let ((asm (try-compile-to-assembly *cobol-move* :arm7)))
    (is (and asm (plusp (length asm)))
        "ARM7 should compile :move")))

(test backend/stack-move-compiles
  "Stack backend compiles :move"
  (let ((asm (try-compile-to-assembly *cobol-move* :stack)))
    (is (and asm (plusp (length asm)))
        "Stack should compile :move")))

