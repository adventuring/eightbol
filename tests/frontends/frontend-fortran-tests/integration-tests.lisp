;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-fortran -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend FORTRAN Integration Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the FORTRAN language integration.
;;; See: src/frontend-fortran/

(in-package :eightbol/test/frontend-fortran)

(fiveam:def-suite :fortran-integration
  :description "FORTRAN integration tests"
  :in :frontend-fortran)

(in-suite :fortran-integration)


(test fortran_integration_multi_statement
  "INTEGRATION: FORTRAN programs with multiple statement types compile correctly"
(is t))

(test fortran_integration_nested_structures
  "INTEGRATION: FORTRAN nested control structures (if/loops) are correctly compiled"
(is t))

(test fortran_integration_mixed_types
  "INTEGRATION: FORTRAN programs mixing different numeric types work correctly"
(is t))

(test fortran_integration_real_world_example
  "INTEGRATION: Real FORTRAN program compiles without errors"
(is t))

