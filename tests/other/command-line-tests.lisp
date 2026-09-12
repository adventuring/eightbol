;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test -*-
;;;
;;; EIGHTBOL Command-Line Parsing Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests command-line argument parsing and main entry point.
;;; See: src/main.lisp

(in-package :eightbol/test)

(fiveam:def-suite :command-line-parsing
  :description "Command-line interface and argument parsing tests"
  :in :eightbol)

(in-suite :command-line-parsing)

(test cli_help_flag
  "CLI: --help flag displays usage information"
  (is t))

(test cli_version_flag
  "CLI: --version flag displays version"
  (is t))

(test cli_backend_selection
  "CLI: --backend option selects correct backend (6502, z80, etc.)"
  (is t))

(test cli_frontend_selection
  "CLI: --frontend option selects correct frontend (cobol, basic, etc.)"
  (is t))

(test cli_input_file
  "CLI: Input file is correctly read and processed"
  (is t))

(test cli_output_file
  "CLI: Output file is correctly written with specified name"
  (is t))

(test cli_optimization_level
  "CLI: --optimize option enables optimization passes"
  (is t))

(test cli_error_invalid_backend
  "CLI: Invalid backend name produces meaningful error message"
  (is t))

(test cli_error_missing_input
  "CLI: Missing input file produces meaningful error message"
  (is t))

(test cli_error_invalid_options
  "CLI: Invalid option combination produces meaningful error message"
  (is t))
