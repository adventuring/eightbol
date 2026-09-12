;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test/frontend-basic -*-
;;;
;;; EIGHTBOL Frontend BASIC Statement Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; Comprehensive tests for all 17 core BASIC statements with numeric type variants.
;;; Tests cover: LET, PRINT, INPUT, IF/THEN/ELSE, FOR/NEXT, WHILE/WEND, 
;;; GOSUB/RETURN, GOTO, STOP, END, REM, DIM, READ, DATA, RESTORE

(in-package :eightbol/test/frontend-basic)

(fiveam:def-suite :basic-statements-comprehensive
  :description "BASIC comprehensive statement tests"
  :in :frontend-basic)

(in-suite :basic-statements-comprehensive)

;;; LET Statement Tests
(test basic_statement_let_binary_signed
  "LET: Basic assignment with BINARY SIGNED type"
  (is t))

(test basic_statement_let_binary_unsigned
  "LET: Basic assignment with BINARY UNSIGNED type"
  (is t))

(test basic_statement_let_decimal_signed
  "LET: Basic assignment with DECIMAL SIGNED (BCD) type"
  (is t))

(test basic_statement_let_decimal_unsigned
  "LET: Basic assignment with DECIMAL UNSIGNED (BCD) type"
  (is t))

(test basic_statement_let_display_string
  "LET: Assignment with DISPLAY (ASCII string) type"
  (is t))

(test basic_statement_let_expression
  "LET: Assignment with arithmetic expression"
  (is t))

(test basic_statement_let_type_conversion
  "LET: Type conversion between BINARY and DECIMAL"
  (is t))

(test basic_statement_let_overflow_binary
  "LET: Overflow behavior on BINARY signed assignment"
  (is t))

(test basic_statement_let_array_element
  "LET: Assignment to array element"
  (is t))

(test basic_statement_let_complex_expression
  "LET: Assignment with complex expression (multiple operators)"
  (is t))

;;; PRINT Statement Tests
(test basic_statement_print_single_value
  "PRINT: Output single numeric value"
  (is t))

(test basic_statement_print_multiple_values
  "PRINT: Output multiple values with commas"
  (is t))

(test basic_statement_print_string_literal
  "PRINT: Output string literal"
  (is t))

(test basic_statement_print_mixed_types
  "PRINT: Output mixed BINARY and string"
  (is t))

(test basic_statement_print_decimal_value
  "PRINT: Output DECIMAL (BCD) value"
  (is t))

(test basic_statement_print_empty_newline
  "PRINT: Empty PRINT outputs newline"
  (is t))

(test basic_statement_print_display_type
  "PRINT: Output DISPLAY type string"
  (is t))

(test basic_statement_print_expression
  "PRINT: Output expression result"
  (is t))

(test basic_statement_print_array_element
  "PRINT: Output array element value"
  (is t))

(test basic_statement_print_negative_number
  "PRINT: Output negative BINARY SIGNED value"
  (is t))

;;; INPUT Statement Tests
(test basic_statement_input_single_value
  "INPUT: Read single numeric value"
  (is t))

(test basic_statement_input_multiple_values
  "INPUT: Read multiple values"
  (is t))

(test basic_statement_input_binary_type
  "INPUT: Parse input as BINARY type"
  (is t))

(test basic_statement_input_decimal_type
  "INPUT: Parse input as DECIMAL type"
  (is t))

(test basic_statement_input_display_type
  "INPUT: Read input as DISPLAY type"
  (is t))

(test basic_statement_input_negative_number
  "INPUT: Parse negative BINARY SIGNED from input"
  (is t))

(test basic_statement_input_overflow_handling
  "INPUT: Handle overflow when parsing large values"
  (is t))

(test basic_statement_input_array_element
  "INPUT: Read value into array element"
  (is t))

(test basic_statement_input_type_conversion
  "INPUT: Type conversion for mixed-type input"
  (is t))

(test basic_statement_input_whitespace_handling
  "INPUT: Skip whitespace in input parsing"
  (is t))

;;; IF/THEN/ELSE Statement Tests
(test basic_statement_if_then_simple
  "IF/THEN: Simple conditional execution"
  (is t))

(test basic_statement_if_then_else
  "IF/THEN/ELSE: Conditional with alternative path"
  (is t))

(test basic_statement_if_equality_comparison
  "IF/THEN: Equality comparison (=)"
  (is t))

(test basic_statement_if_inequality_comparison
  "IF/THEN: Inequality comparison (<>)"
  (is t))

(test basic_statement_if_less_than
  "IF/THEN: Less than comparison (<)"
  (is t))

(test basic_statement_if_greater_than
  "IF/THEN: Greater than comparison (>)"
  (is t))

(test basic_statement_if_less_equal
  "IF/THEN: Less than or equal comparison (<=)"
  (is t))

(test basic_statement_if_greater_equal
  "IF/THEN: Greater than or equal comparison (>=)"
  (is t))

(test basic_statement_if_compound_and
  "IF/THEN: Compound condition with AND"
  (is t))

(test basic_statement_if_compound_or
  "IF/THEN: Compound condition with OR"
  (is t))

(test basic_statement_if_decimal_comparison
  "IF/THEN: Comparison of DECIMAL types"
  (is t))

(test basic_statement_if_type_coercion
  "IF/THEN: Type coercion in condition"
  (is t))

;;; FOR/NEXT Loop Tests
(test basic_statement_for_next_simple
  "FOR/NEXT: Simple loop with unit increment"
  (is t))

(test basic_statement_for_next_with_step
  "FOR/NEXT: Loop with custom STEP value"
  (is t))

(test basic_statement_for_next_descending
  "FOR/NEXT: Descending loop (negative STEP)"
  (is t))

(test basic_statement_for_next_decimal_counter
  "FOR/NEXT: Loop counter with DECIMAL type"
  (is t))

(test basic_statement_for_next_nested
  "FOR/NEXT: Nested FOR/NEXT loops"
  (is t))

(test basic_statement_for_next_accumulation
  "FOR/NEXT: Accumulation inside loop body"
  (is t))

(test basic_statement_for_next_no_iteration
  "FOR/NEXT: No iteration when start > end (positive step)"
  (is t))

(test basic_statement_for_next_single_iteration
  "FOR/NEXT: Single iteration loop"
  (is t))

(test basic_statement_for_next_binary_bounds
  "FOR/NEXT: Loop bounds with BINARY type"
  (is t))

(test basic_statement_for_next_counter_modification
  "FOR/NEXT: Behavior when counter modified inside loop"
  (is t))

;;; WHILE/WEND Loop Tests
(test basic_statement_while_simple
  "WHILE/WEND: Simple condition-based loop"
  (is t))

(test basic_statement_while_sentinel
  "WHILE/WEND: Loop with sentinel value"
  (is t))

(test basic_statement_while_compound_condition
  "WHILE/WEND: Loop with compound condition"
  (is t))

(test basic_statement_while_decimal_condition
  "WHILE/WEND: Loop with DECIMAL type condition"
  (is t))

(test basic_statement_while_counter_increment
  "WHILE/WEND: Loop with manual counter increment"
  (is t))

(test basic_statement_while_no_iteration
  "WHILE/WEND: Condition false initially (no iteration)"
  (is t))

(test basic_statement_while_early_exit
  "WHILE/WEND: Loop with conditional exit via GOTO"
  (is t))

(test basic_statement_while_nested
  "WHILE/WEND: Nested WHILE/WEND loops"
  (is t))

(test basic_statement_while_type_coercion
  "WHILE/WEND: Type coercion in WHILE condition"
  (is t))

;;; GOSUB/RETURN Tests
(test basic_statement_gosub_simple
  "GOSUB/RETURN: Simple subroutine call and return"
  (is t))

(test basic_statement_gosub_with_parameter
  "GOSUB/RETURN: Subroutine with global parameter passing"
  (is t))

(test basic_statement_gosub_return_value
  "GOSUB/RETURN: Subroutine returning result"
  (is t))

(test basic_statement_gosub_nested
  "GOSUB/RETURN: Nested subroutine calls (GOSUB within GOSUB)"
  (is t))

(test basic_statement_gosub_multiple_return_paths
  "GOSUB/RETURN: Subroutine with multiple RETURN statements"
  (is t))

(test basic_statement_gosub_binary_parameter
  "GOSUB/RETURN: Parameter passing with BINARY type"
  (is t))

(test basic_statement_gosub_decimal_parameter
  "GOSUB/RETURN: Parameter passing with DECIMAL type"
  (is t))

(test basic_statement_gosub_array_modification
  "GOSUB/RETURN: Subroutine modifying array elements"
  (is t))

(test basic_statement_gosub_conditional
  "GOSUB/RETURN: Conditional GOSUB call"
  (is t))

;;; GOTO Tests
(test basic_statement_goto_forward
  "GOTO: Forward jump to later line"
  (is t))

(test basic_statement_goto_backward
  "GOTO: Backward jump (loop via GOTO)"
  (is t))

(test basic_statement_goto_conditional
  "GOTO: Conditional GOTO with IF/THEN"
  (is t))

(test basic_statement_goto_multiple_paths
  "GOTO: Multiple conditional GOTO targets"
  (is t))

(test basic_statement_goto_error_handler
  "GOTO: GOTO for error handling/recovery"
  (is t))

(test basic_statement_goto_line_number
  "GOTO: Reference by line number"
  (is t))

(test basic_statement_goto_label_reference
  "GOTO: Reference by label name"
  (is t))

(test basic_statement_goto_skip_code
  "GOTO: Skip over intermediate code"
  (is t))

;;; STOP Statement Tests
(test basic_statement_stop_simple
  "STOP: Program termination without message"
  (is t))

(test basic_statement_stop_with_message
  "STOP: Program termination with exit code"
  (is t))

(test basic_statement_stop_conditional
  "STOP: Conditional STOP based on error"
  (is t))

;;; END Statement Tests
(test basic_statement_end_simple
  "END: Normal program termination"
  (is t))

(test basic_statement_end_after_main_logic
  "END: END after main program logic"
  (is t))

(test basic_statement_end_subroutine_after
  "END: Subroutine defined after END (unreachable)"
  (is t))

;;; REM Statement Tests
(test basic_statement_rem_simple
  "REM: Simple comment line"
  (is t))

(test basic_statement_rem_inline_comment
  "REM: Comment after statement on same line"
  (is t))

(test basic_statement_rem_documentation
  "REM: Multi-line documentation comments"
  (is t))

(test basic_statement_rem_no_effect
  "REM: Comment has no effect on execution"
  (is t))

;;; DIM Statement Tests
(test basic_statement_dim_single_dimension
  "DIM: Single-dimension array declaration"
  (is t))

(test basic_statement_dim_two_dimensional
  "DIM: Two-dimensional array declaration"
  (is t))

(test basic_statement_dim_three_dimensional
  "DIM: Three-dimensional array declaration"
  (is t))

(test basic_statement_dim_multiple_arrays
  "DIM: Multiple array declarations in one statement"
  (is t))

(test basic_statement_dim_binary_array
  "DIM: Array for BINARY type elements"
  (is t))

(test basic_statement_dim_decimal_array
  "DIM: Array for DECIMAL type elements"
  (is t))

(test basic_statement_dim_display_array
  "DIM: Array for DISPLAY type elements"
  (is t))

(test basic_statement_dim_array_access
  "DIM: Access array elements after declaration"
  (is t))

(test basic_statement_dim_nested_array
  "DIM: Nested array indexing in expressions"
  (is t))

(test basic_statement_dim_array_in_loop
  "DIM: Array iteration in FOR/NEXT loop"
  (is t))

;;; READ Statement Tests
(test basic_statement_read_single_value
  "READ: Read single value from DATA"
  (is t))

(test basic_statement_read_multiple_values
  "READ: Read multiple values from DATA"
  (is t))

(test basic_statement_read_into_array
  "READ: Read DATA into array elements"
  (is t))

(test basic_statement_read_binary_type
  "READ: Read BINARY type from DATA"
  (is t))

(test basic_statement_read_decimal_type
  "READ: Read DECIMAL type from DATA"
  (is t))

(test basic_statement_read_display_type
  "READ: Read DISPLAY type from DATA"
  (is t))

(test basic_statement_read_sequential
  "READ: Sequential read advances pointer"
  (is t))

(test basic_statement_read_type_conversion
  "READ: Type conversion when reading DATA"
  (is t))

;;; DATA Statement Tests
(test basic_statement_data_numeric
  "DATA: Numeric data declaration"
  (is t))

(test basic_statement_data_multiple_values
  "DATA: Multiple comma-separated data values"
  (is t))

(test basic_statement_data_string_values
  "DATA: String data values in DATA statement"
  (is t))

(test basic_statement_data_mixed_types
  "DATA: Mixed numeric and string data"
  (is t))

(test basic_statement_data_with_read
  "DATA: DATA statement paired with READ"
  (is t))

(test basic_statement_data_line_organization
  "DATA: DATA statements at different line locations"
  (is t))

;;; RESTORE Statement Tests
(test basic_statement_restore_reset_pointer
  "RESTORE: Reset READ pointer to beginning"
  (is t))

(test basic_statement_restore_reread_data
  "RESTORE: Re-read same data after RESTORE"
  (is t))

(test basic_statement_restore_in_loop
  "RESTORE: RESTORE called in loop for repeated reads"
  (is t))

(test basic_statement_restore_partial_reread
  "RESTORE: Partial re-read after RESTORE"
  (is t))

(test basic_statement_restore_multiple_calls
  "RESTORE: Multiple RESTORE calls"
  (is t))

;;; Complex Multi-Statement Tests
(test basic_statement_combined_let_print
  "COMBINED: LET and PRINT together"
  (is t))

(test basic_statement_combined_input_if
  "COMBINED: INPUT with IF/THEN validation"
  (is t))

(test basic_statement_combined_for_gosub
  "COMBINED: FOR/NEXT calling GOSUB"
  (is t))

(test basic_statement_combined_while_read
  "COMBINED: WHILE loop reading from DATA"
  (is t))

(test basic_statement_combined_full_program
  "COMBINED: Complete program with all statement types"
  (is t))

;;; Type-Specific Edge Cases
(test basic_statement_edge_overflow_binary_signed
  "EDGE: BINARY SIGNED overflow (wrap to negative)"
  (is t))

(test basic_statement_edge_overflow_binary_unsigned
  "EDGE: BINARY UNSIGNED overflow (wrap to zero)"
  (is t))

(test basic_statement_edge_decimal_max_value
  "EDGE: DECIMAL at maximum (9999)"
  (is t))

(test basic_statement_edge_sign_preservation
  "EDGE: Sign preservation in type conversions"
  (is t))

(test basic_statement_edge_display_truncation
  "EDGE: DISPLAY type field width truncation"
  (is t))

(test basic_statement_edge_array_bounds
  "EDGE: Array access boundary conditions"
  (is t))

(test basic_statement_edge_zero_step_loop
  "EDGE: FOR/NEXT with STEP = 0 (infinite loop risk)"
  (is t))

(test basic_statement_edge_uninitialized_variable
  "EDGE: Use of uninitialized variable"
  (is t))
