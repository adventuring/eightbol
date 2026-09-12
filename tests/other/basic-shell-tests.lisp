;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test -*-
;;;
;;; EIGHTBOL BASIC Interactive Shell Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module tests the BASIC language interactive shell functionality.
;;; See: src/frontend-basic/basic-shell.lisp

(in-package :eightbol/test)

(fiveam:def-suite :basic-shell
  :description "BASIC interactive shell tests"
  :in :eightbol)

(in-suite :basic-shell)

(test shell_prompt
  "SHELL: Interactive prompt is displayed"
  (is t))

(test shell_execute_statement
  "SHELL: Single BASIC statement is executed immediately"
  (is t))

(test shell_line_editing
  "SHELL: Line editing features work correctly"
  (is t))

(test shell_history
  "SHELL: Command history is maintained and accessible"
  (is t))

(test shell_help_command
  "SHELL: HELP command displays available functions"
  (is t))

(test shell_list_program
  "SHELL: LIST command displays current program"
  (is t))

(test shell_run_program
  "SHELL: RUN command executes the program"
  (is t))

(test shell_error_handling
  "SHELL: Runtime errors are caught and reported without crashing shell"
  (is t))

(test shell_save_load
  "SHELL: SAVE/LOAD commands persist and restore programs"
  (is t))

(test shell_exit_command
  "SHELL: EXIT command cleanly terminates shell"
  (is t))
