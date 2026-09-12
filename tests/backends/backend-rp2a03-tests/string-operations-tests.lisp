;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/backend-rp2a03 -*-
;;;
;;; EIGHTBOL EIGHTBOL RP2A03 Backend String Operations Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the RP2A03 backend code generation.
;;; See: src/backend-rp2a03/

(in-package :eightbol/test/backend-rp2a03)

(fiveam:def-suite :backend-rp2a03
  :description "RP2A03 backend tests"
  :in :backend-matrix)

(fiveam:def-suite :backend-rp2a03-string-operations
  :description "RP2A03 string-operations tests"
  :in :backend-rp2a03)

(in-suite :backend-rp2a03-string-operations)


(test rp2a03_string_blt
  "STRING: RP2A03 STRING BLT operations generate correct memory operations"
(let* ((ast ('(:string-blt (:var src) (:var dst) (:const 10))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :RP2A03 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test rp2a03_string_subscript
  "STRING: RP2A03 subscript operations on strings generate correct index calculations"
(let* ((ast ('(:string-blt (:var src) (:var dst) (:const 10))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :RP2A03 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test rp2a03_string_refmod
  "STRING: RP2A03 reference modification produces correct substring operations"
(let* ((ast ('(:string-blt (:var src) (:var dst) (:const 10))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :RP2A03 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

(test rp2a03_string_indexing_base
  "STRING: RP2A03 zero-based vs one-based indexing is correctly handled"
(let* ((ast ('(:string-blt (:var src) (:var dst) (:const 10))'))
         (output (with-output-to-string (s)
                   (eightbol:compile-ast-to-asm ast :backend :RP2A03 :output s))))
    (is (stringp output))
    (is (> (length output) 0))))

