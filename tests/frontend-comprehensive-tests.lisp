;;;; tests/frontend-comprehensive-tests.lisp — Comprehensive frontend tests
;;; Tests language front-end parsing

(in-package :eightbol/test)

(def-suite :frontend-comprehensive :description "Comprehensive frontend language tests")
(in-suite :frontend-comprehensive)

;;;; COBOL Parser Test

(test cobol/parse-simple-program
  "COBOL parses simple program structure"
  (let* ((src "000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. SimpleTest.
000030 DATA DIVISION.
000040 WORKING-STORAGE SECTION.
000050 05 X PIC 9999 USAGE BINARY.
000060 PROCEDURE DIVISION.
000070 MOVE 1 TO X.
000080 STOP RUN."))
    (is (not (null (eightbol::parse-eightbol-string src)))
        "COBOL parser should parse valid program")))

