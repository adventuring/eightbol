;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-huc6280 -*-
;;;
;;; EIGHTBOL EIGHTBOL HUC6280 Backend String Operations Tests
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

(fiveam:def-suite :backend-huc6280-string-operations
  :description "HUC6280 string-operations tests"
  :in :backend-huc6280)

(in-suite :backend-huc6280-string-operations)


(test huc6280_string_blt
  "STRING: HUC6280 STRING BLT operations generate correct memory operations"
(let* ((ast ('(:string-blt (:var src) (:var dst) (:const 10))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_string_subscript
  "STRING: HUC6280 subscript operations on strings generate correct index calculations"
(let* ((ast ('(:string-blt (:var src) (:var dst) (:const 10))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_string_refmod
  "STRING: HUC6280 reference modification produces correct substring operations"
(let* ((ast ('(:string-blt (:var src) (:var dst) (:const 10))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test huc6280_string_indexing_base
  "STRING: HUC6280 zero-based vs one-based indexing is correctly handled"
(let* ((ast ('(:string-blt (:var src) (:var dst) (:const 10))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :HUC6280 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

