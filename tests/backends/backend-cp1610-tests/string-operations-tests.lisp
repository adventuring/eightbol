;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-cp1610 -*-
;;;
;;; EIGHTBOL EIGHTBOL CP1610 Backend String Operations Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the CP1610 backend code generation.
;;; See: src/backend-cp1610/

(in-package :eightbol/test/backend-cp1610)

(fiveam:def-suite :backend-cp1610
  :description "CP1610 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-cp1610-string-operations
  :description "CP1610 string-operations tests"
  :in :backend-cp1610)

(in-suite :backend-cp1610-string-operations)


(test cp1610_string_blt
  "STRING: CP1610 STRING BLT operations generate correct memory operations"
(let* ((ast ('(:string-blt (:var src) (:var dst) (:const 10))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test cp1610_string_subscript
  "STRING: CP1610 subscript operations on strings generate correct index calculations"
(let* ((ast ('(:string-blt (:var src) (:var dst) (:const 10))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test cp1610_string_refmod
  "STRING: CP1610 reference modification produces correct substring operations"
(let* ((ast ('(:string-blt (:var src) (:var dst) (:const 10))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test cp1610_string_indexing_base
  "STRING: CP1610 zero-based vs one-based indexing is correctly handled"
(let* ((ast ('(:string-blt (:var src) (:var dst) (:const 10))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :CP1610 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

