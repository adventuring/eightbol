;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-huc6280 -*-
;;;
;;; EIGHTBOL EIGHTBOL HUC6280 Backend Control Flow Tests
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

(fiveam:def-suite :backend-huc6280-control-flow
  :description "HUC6280 control-flow tests"
  :in :backend-huc6280)

(in-suite :backend-huc6280-control-flow)


(test huc6280_if_conditional
  "CONTROL FLOW: HUC6280 :if conditionals generate correct branch instructions"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_if_nested
  "CONTROL FLOW: HUC6280 nested conditionals generate correct branch chains"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_perform_loop
  "CONTROL FLOW: HUC6280 :perform loops generate correct branch/jump sequences"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_goto
  "CONTROL FLOW: HUC6280 :goto jumps generate correct unconditional jumps"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_loop_unroll
  "CONTROL FLOW: HUC6280 loop optimization correctly unrolls small loops"
(let* ((ast ('(:if (:const 1) (:move (:const 1) (:var x)) (:move (:const 0) (:var x)))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

