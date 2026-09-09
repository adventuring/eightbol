;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-lua -*-
;;;
;;; EIGHTBOL EIGHTBOL Frontend LUA Numeric Types Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the LUA language numeric-types.
;;; See: src/frontend-lua/lua-parser.lisp

(in-package :eightbol/test/frontend-lua)

(fiveam:def-suite :lua-numeric-types
  :description "LUA numeric-types tests"
  :in :frontend-lua)

(in-suite :lua-numeric-types)


(test lua_numeric_binary
  "NUMERIC: LUA binary integers (8/16/32-bit) are correctly parsed"
  (skip "Implementation pending"))

(test lua_numeric_fixed_point
  "NUMERIC: LUA fixed-point numbers are correctly parsed and scaled"
  (skip "Implementation pending"))

(test lua_numeric_bcd
  "NUMERIC: LUA BCD numbers (if supported) are correctly parsed"
  (skip "Implementation pending"))

(test lua_numeric_arithmetic
  "NUMERIC: LUA arithmetic on mixed numeric types maintains correct precision"
  (skip "Implementation pending"))

(test lua_numeric_overflow
  "NUMERIC: LUA arithmetic overflow is handled correctly"
  (skip "Implementation pending"))

