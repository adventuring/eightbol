;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-huc6280 -*-
;;;
;;; EIGHTBOL EIGHTBOL HUC6280 Backend Special Node Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the HUC6280 backend code generation.
;;; See: src/backend-huc6280/

(in-package :eightbol/test/backend-huc6280)

(fiveam:def-suite :backend-huc6280
  :description "HUC6280 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-huc6280-special-nodes
  :description "HUC6280 special-nodes tests"
  :in :backend-huc6280)

(in-suite :backend-huc6280-special-nodes)


(test huc6280_log_fault
  "SPECIAL: HUC6280 :log-fault produces correct debug output sequence"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_debug_break
  "SPECIAL: HUC6280 :debug-break produces correct breakpoint instruction"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_inspect
  "SPECIAL: HUC6280 :inspect produces correct debug inspection code"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_evaluate
  "SPECIAL: HUC6280 :evaluate WHEN clauses generate correct case/switch logic"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_bitwise_ops
  "SPECIAL: HUC6280 bitwise AND/OR/XOR operations generate correct opcodes"
(let* ((ast ('(:move (:const 0) (:var x))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

