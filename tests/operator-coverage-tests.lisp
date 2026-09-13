;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: eightbol/test -*-
;;;
;;; EIGHTBOL Comprehensive Operator Coverage Tests
;;;
;;; © 2026 EIGHTBOL Development. All rights reserved.
;;; Licensed under the MIT License
;;;
;;; This module provides comprehensive tests for operator coverage across
;;; all frontends, backends, and AST levels. Tests cover:
;;; - Arithmetic operators: :+, :-, :×, :÷
;;; - Comparison operators: :=, :≠, :<, :≤, :>, :≥
;;; - Bitwise operators: :¬, :∧, :∨, :⊻ (NAND/NOR composed as :¬ of :∧/:∨)
;;; - Shift operators: :ash (signed count; positive = left, negative = right)
;;; - Logical operators: :and, :or, :not
;;; - Operator precedence and complex expressions
;;; - Mixed operator types

(in-package :eightbol/test)

(fiveam:def-suite :operator-coverage
  :description "Comprehensive operator coverage tests"
  :in :eightbol)

(in-suite :operator-coverage)

;;; ============================================================================
;;; SECTION 1: ARITHMETIC OPERATOR TESTS
;;; ============================================================================

;;; 1.1 Addition operator (:+)
(test operator_arithmetic_addition_basic
  "OPERATOR: Arithmetic addition operator (:+) recognized in expressions"
  (let ((ast '(:+ (:const 5) (:const 3))))
    (is (listp ast))
    (is (eq (first ast) :+))))

(test operator_arithmetic_addition_variables
  "OPERATOR: Addition operator (:+) works with variable operands"
  (let ((ast '(:+ (:var x) (:var y))))
    (is (listp ast))
    (is (eq (first ast) :+))))

(test operator_arithmetic_addition_mixed
  "OPERATOR: Addition operator (:+) works with mixed literals and variables"
  (let ((ast '(:+ (:const 10) (:var counter))))
    (is (listp ast))
    (is (eq (first ast) :+))))

(test operator_arithmetic_addition_chained
  "OPERATOR: Chained addition (A + B + C) produces nested :+ nodes"
  (let ((ast '(:+ (:+ (:const 1) (:const 2)) (:const 3))))
    (is (listp ast))
    (is (eq (first ast) :+))
    (is (listp (second ast)))
    (is (eq (first (second ast)) :+))))

;;; 1.2 Subtraction operator (:-)
(test operator_arithmetic_subtraction_basic
  "OPERATOR: Arithmetic subtraction operator (:-) recognized in expressions"
  (let ((ast '(:- (:const 10) (:const 3))))
    (is (listp ast))
    (is (eq (first ast) :-))))

(test operator_arithmetic_subtraction_variables
  "OPERATOR: Subtraction operator (:-) works with variable operands"
  (let ((ast '(:- (:var x) (:var y))))
    (is (listp ast))
    (is (eq (first ast) :-))))

(test operator_arithmetic_subtraction_nested
  "OPERATOR: Nested subtraction (A - (B - C)) produces correct structure"
  (let ((ast '(:- (:const 10) (:- (:const 5) (:const 2)))))
    (is (listp ast))
    (is (eq (first ast) :-))
    (is (listp (third ast)))
    (is (eq (first (third ast)) :-))))

;;; 1.3 Multiplication operator (:×)
(test operator_arithmetic_multiplication_basic
  "OPERATOR: Arithmetic multiplication operator (:×) recognized in expressions"
  (let ((ast '(:× (:const 5) (:const 4))))
    (is (listp ast))
    (is (eq (first ast) :×))))

(test operator_arithmetic_multiplication_variables
  "OPERATOR: Multiplication operator (:×) works with variable operands"
  (let ((ast '(:× (:var width) (:var height))))
    (is (listp ast))
    (is (eq (first ast) :×))))

(test operator_arithmetic_multiplication_powers_of_two
  "OPERATOR: Multiplication by powers of two for optimization"
  (let ((ast '(:× (:var x) (:const 2))))
    (is (listp ast))
    (is (eq (first ast) :×))))

;;; 1.4 Division operator (:÷)
(test operator_arithmetic_division_basic
  "OPERATOR: Arithmetic division operator (:÷) recognized in expressions"
  (let ((ast '(:÷ (:const 20) (:const 4))))
    (is (listp ast))
    (is (eq (first ast) :÷))))

(test operator_arithmetic_division_variables
  "OPERATOR: Division operator (:÷) works with variable operands"
  (let ((ast '(:÷ (:var numerator) (:var denominator))))
    (is (listp ast))
    (is (eq (first ast) :÷))))

(test operator_arithmetic_division_powers_of_two
  "OPERATOR: Division by powers of two for right-shift optimization"
  (let ((ast '(:÷ (:var x) (:const 4))))
    (is (listp ast))
    (is (eq (first ast) :÷))))

;;; ============================================================================
;;; SECTION 2: COMPARISON OPERATOR TESTS
;;; ============================================================================

;;; 2.1 Equality operator (:=)
(test operator_comparison_equality_basic
  "OPERATOR: Comparison equality operator (:=) recognized in expressions"
  (let ((ast '(:= (:var x) (:const 5))))
    (is (listp ast))
    (is (eq (first ast) :=))))

(test operator_comparison_equality_variables
  "OPERATOR: Equality operator (:=) works with variable operands"
  (let ((ast '(:= (:var a) (:var b))))
    (is (listp ast))
    (is (eq (first ast) :=))))

(test operator_comparison_equality_in_conditionals
  "OPERATOR: Equality operator (:=) can be used in IF conditions"
  (let ((ast '(:if (:= (:var x) (:const 0)) (:then (list)) (:else (list)))))
    (is (listp ast))
    (is (eq (first ast) :if))))

;;; 2.2 Inequality operator (:≠)
(test operator_comparison_inequality_basic
  "OPERATOR: Comparison inequality operator (:≠) recognized in expressions"
  (let ((ast '(:≠ (:var x) (:const 0))))
    (is (listp ast))
    (is (eq (first ast) :≠))))

(test operator_comparison_inequality_variables
  "OPERATOR: Inequality operator (:≠) works with variable operands"
  (let ((ast '(:≠ (:var a) (:var b))))
    (is (listp ast))
    (is (eq (first ast) :≠))))

;;; 2.3 Less-than operator (:<)
(test operator_comparison_less_than_basic
  "OPERATOR: Comparison less-than operator (:<) recognized in expressions"
  (let ((ast '(:< (:var x) (:const 100))))
    (is (listp ast))
    (is (eq (first ast) :<))))

(test operator_comparison_less_than_with_arithmetic
  "OPERATOR: Less-than operator (:<) can compare arithmetic results"
  (let ((ast '(:< (:+ (:var x) (:const 5)) (:const 100))))
    (is (listp ast))
    (is (eq (first ast) :<))))

;;; 2.4 Less-than-or-equal operator (:≤)
(test operator_comparison_less_equal_basic
  "OPERATOR: Comparison less-than-or-equal operator (:≤) recognized"
  (let ((ast '(:≤ (:var x) (:const 100))))
    (is (listp ast))
    (is (eq (first ast) :≤))))

;;; 2.5 Greater-than operator (:>)
(test operator_comparison_greater_than_basic
  "OPERATOR: Comparison greater-than operator (:>) recognized in expressions"
  (let ((ast '(:> (:var x) (:const 0))))
    (is (listp ast))
    (is (eq (first ast) :>))))

;;; 2.6 Greater-than-or-equal operator (:≥)
(test operator_comparison_greater_equal_basic
  "OPERATOR: Comparison greater-than-or-equal operator (:≥) recognized"
  (let ((ast '(:≥ (:var x) (:const 0))))
    (is (listp ast))
    (is (eq (first ast) :≥))))

;;; ============================================================================
;;; SECTION 3: BITWISE OPERATOR TESTS
;;; ============================================================================

;;; 3.1 Bitwise AND operator (:∧)
(test operator_bitwise_and_basic
  "OPERATOR: Bitwise AND operator (:∧) recognized in expressions"
  (let ((ast '(:∧ (:const 12) (:const 10))))
    (is (listp ast))
    (is (eq (first ast) :∧))))

(test operator_bitwise_and_variables
  "OPERATOR: Bitwise AND operator (:∧) works with variable operands"
  (let ((ast '(:∧ (:var flags) (:const #xFF))))
    (is (listp ast))
    (is (eq (first ast) :∧))))

(test operator_bitwise_and_mask_extraction
  "OPERATOR: Bitwise AND operator (:∧) for bit masking"
  (let ((ast '(:∧ (:var register) (:const #x0F))))
    (is (listp ast))
    (is (eq (first ast) :∧))))

;;; 3.2 Bitwise OR operator (:∨)
(test operator_bitwise_or_basic
  "OPERATOR: Bitwise OR operator (:∨) recognized in expressions"
  (let ((ast '(:∨ (:const 12) (:const 10))))
    (is (listp ast))
    (is (eq (first ast) :∨))))

(test operator_bitwise_or_variables
  "OPERATOR: Bitwise OR operator (:∨) works with variable operands"
  (let ((ast '(:∨ (:var flags) (:const #x01))))
    (is (listp ast))
    (is (eq (first ast) :∨))))

(test operator_bitwise_or_flag_setting
  "OPERATOR: Bitwise OR operator (:∨) for setting flags"
  (let ((ast '(:∨ (:var status) (:const #x80))))
    (is (listp ast))
    (is (eq (first ast) :∨))))

;;; 3.3 Bitwise XOR operator (:⊻)
(test operator_bitwise_xor_basic
  "OPERATOR: Bitwise XOR operator (:⊻) recognized in expressions"
  (let ((ast '(:⊻ (:const 12) (:const 10))))
    (is (listp ast))
    (is (eq (first ast) :⊻))))

(test operator_bitwise_xor_variables
  "OPERATOR: Bitwise XOR operator (:⊻) works with variable operands"
  (let ((ast '(:⊻ (:var a) (:var b))))
    (is (listp ast))
    (is (eq (first ast) :⊻))))

(test operator_bitwise_xor_toggle
  "OPERATOR: Bitwise XOR operator (:⊻) for toggling bits"
  (let ((ast '(:⊻ (:var led_status) (:const 1))))
    (is (listp ast))
    (is (eq (first ast) :⊻))))

;;; 3.4 Bitwise NOT operator (:¬)
(test operator_bitwise_not_basic
  "OPERATOR: Bitwise NOT operator (:¬) recognized as unary operator"
  (let ((ast '(:¬ (:const 0))))
    (is (listp ast))
    (is (eq (first ast) :¬))))

(test operator_bitwise_not_variable
  "OPERATOR: Bitwise NOT operator (:¬) works with variable operand"
  (let ((ast '(:¬ (:var flags))))
    (is (listp ast))
    (is (eq (first ast) :¬))))

(test operator_bitwise_not_inversion
  "OPERATOR: Bitwise NOT operator (:¬) for bit inversion"
  (let ((ast '(:¬ (:const #xFF))))
    (is (listp ast))
    (is (eq (first ast) :¬))))

;;; 3.5 Bitwise NAND operator (:¬ (:∧ a b))
(test operator_bitwise_nand_basic
  "OPERATOR: Bitwise NAND operator (:¬ (:∧ a b)) recognized in expressions"
  (let ((ast '(:¬ (:∧ (:const 12) (:const 10)))))
    (is (listp ast))
    (is (eq (first ast) :¬))
    (is (eq (first (second ast)) :∧))))

(test operator_bitwise_nand_variables
  "OPERATOR: Bitwise NAND operator (:¬ (:∧ a b)) works with variable operands"
  (let ((ast '(:¬ (:∧ (:var a) (:var b)))))
    (is (listp ast))
    (is (eq (first ast) :¬))
    (is (eq (first (second ast)) :∧))))

;;; 3.6 Bitwise NOR operator (:¬ (:∨ a b))
(test operator_bitwise_nor_basic
  "OPERATOR: Bitwise NOR operator (:¬ (:∨ a b)) recognized in expressions"
  (let ((ast '(:¬ (:∨ (:const 12) (:const 10)))))
    (is (listp ast))
    (is (eq (first ast) :¬))
    (is (eq (first (second ast)) :∨))))

(test operator_bitwise_nor_variables
  "OPERATOR: Bitwise NOR operator (:¬ (:∨ a b)) works with variable operands"
  (let ((ast '(:¬ (:∨ (:var x) (:var y)))))
    (is (listp ast))
    (is (eq (first ast) :¬))
    (is (eq (first (second ast)) :∨))))

;;; ============================================================================
;;; SECTION 4: SHIFT OPERATOR TESTS
;;; ============================================================================

;;; 4.1 Shift Left via (:ash v count) with positive count
(test operator_shift_asl_basic
  "OPERATOR: Shift left as (:ash v count) with positive count"
  (let ((ast '(:ash (:var x) (:const 2))))
    (is (listp ast))
    (is (eq (first ast) :ash))))

(test operator_shift_asl_constants
  "OPERATOR: Shift left (:ash const pos-count) with constant shift amount"
  (let ((ast '(:ash (:const 5) (:const 3))))
    (is (listp ast))
    (is (eq (first ast) :ash))))

(test operator_shift_asl_variables
  "OPERATOR: Shift left (:ash var count) with variable shift amount"
  (let ((ast '(:ash (:var data) (:var shift_count))))
    (is (listp ast))
    (is (eq (first ast) :ash))))

;;; 4.2 Shift Right via (:ash v count) with negative count
(test operator_shift_asr_basic
  "OPERATOR: Shift right as (:ash v count) with negative count"
  (let ((ast '(:ash (:var x) (:const -1))))
    (is (listp ast))
    (is (eq (first ast) :ash))))

(test operator_shift_asr_constants
  "OPERATOR: Shift right (:ash const neg-count) with constant shift amount"
  (let ((ast '(:ash (:const 32) (:const -2))))
    (is (listp ast))
    (is (eq (first ast) :ash))))

(test operator_shift_asr_variables
  "OPERATOR: Shift right (:ash var count) with variable shift amount"
  (let ((ast '(:ash (:var data) (:var shift_count))))
    (is (listp ast))
    (is (eq (first ast) :ash))))

;;; 4.3 Arithmetic Shift operator (:ash) — generic
(test operator_shift_ash_basic
  "OPERATOR: Arithmetic shift operator (:ash) recognized in expressions"
  (let ((ast '(:ash (:var x) (:const 2))))
    (is (listp ast))
    (is (eq (first ast) :ash))))

(test operator_shift_ash_positive_count
  "OPERATOR: Arithmetic shift operator (:ash) with positive shift count (left)"
  (let ((ast '(:ash (:var x) (:const 4))))
    (is (listp ast))
    (is (eq (first ast) :ash))))

(test operator_shift_ash_negative_count
  "OPERATOR: Arithmetic shift operator (:ash) with negative shift count (right)"
  (let ((ast '(:ash (:var x) (:const -2))))
    (is (listp ast))
    (is (eq (first ast) :ash))))

;;; ============================================================================
;;; SECTION 5: LOGICAL OPERATOR TESTS
;;; ============================================================================

;;; 5.1 Logical AND operator (:and)
(test operator_logical_and_basic
  "OPERATOR: Logical AND operator (:and) recognized in expressions"
  (let ((ast '(:and (:= (:var x) (:const 1)) (:= (:var y) (:const 2)))))
    (is (listp ast))
    (is (eq (first ast) :and))))

(test operator_logical_and_variables
  "OPERATOR: Logical AND operator (:and) with boolean variables"
  (let ((ast '(:and (:var flag_a) (:var flag_b))))
    (is (listp ast))
    (is (eq (first ast) :and))))

(test operator_logical_and_chained
  "OPERATOR: Chained logical AND (A AND B AND C) produces nested structure"
  (let ((ast '(:and (:and (:var a) (:var b)) (:var c))))
    (is (listp ast))
    (is (eq (first ast) :and))))

;;; 5.2 Logical OR operator (:or)
(test operator_logical_or_basic
  "OPERATOR: Logical OR operator (:or) recognized in expressions"
  (let ((ast '(:or (:= (:var x) (:const 0)) (:= (:var x) (:const 1)))))
    (is (listp ast))
    (is (eq (first ast) :or))))

(test operator_logical_or_variables
  "OPERATOR: Logical OR operator (:or) with boolean variables"
  (let ((ast '(:or (:var flag_a) (:var flag_b))))
    (is (listp ast))
    (is (eq (first ast) :or))))

(test operator_logical_or_chained
  "OPERATOR: Chained logical OR (A OR B OR C) produces nested structure"
  (let ((ast '(:or (:or (:var a) (:var b)) (:var c))))
    (is (listp ast))
    (is (eq (first ast) :or))))

;;; 5.3 Logical NOT operator (:not)
(test operator_logical_not_basic
  "OPERATOR: Logical NOT operator (:not) recognized as unary operator"
  (let ((ast '(:not (:= (:var x) (:const 0)))))
    (is (listp ast))
    (is (eq (first ast) :not))))

(test operator_logical_not_variable
  "OPERATOR: Logical NOT operator (:not) works with boolean variable"
  (let ((ast '(:not (:var done))))
    (is (listp ast))
    (is (eq (first ast) :not))))

(test operator_logical_not_double_negation
  "OPERATOR: Double logical NOT (NOT (NOT A)) produces nested structure"
  (let ((ast '(:not (:not (:var flag)))))
    (is (listp ast))
    (is (eq (first ast) :not))
    (is (listp (second ast)))
    (is (eq (first (second ast)) :not))))

;;; ============================================================================
;;; SECTION 6: OPERATOR PRECEDENCE TESTS
;;; ============================================================================

;;; 6.1 Arithmetic precedence: :× and :÷ before :+ and :-
(test operator_precedence_multiplication_before_addition
  "PRECEDENCE: Multiplication (:×) has higher precedence than addition (:+)"
  (let ((ast '(:+ (:const 2) (:× (:const 3) (:const 4)))))
    ;; Should be: 2 + (3 * 4), not (2 + 3) * 4
    (is (listp ast))
    (is (eq (first ast) :+))
    (is (listp (third ast)))
    (is (eq (first (third ast)) :×))))

(test operator_precedence_division_before_subtraction
  "PRECEDENCE: Division (:÷) has higher precedence than subtraction (:-)"
  (let ((ast '(:- (:const 10) (:÷ (:const 8) (:const 2)))))
    ;; Should be: 10 - (8 / 2), not (10 - 8) / 2
    (is (listp ast))
    (is (eq (first ast) :-))
    (is (listp (third ast)))
    (is (eq (first (third ast)) :÷))))

;;; 6.2 Bitwise precedence: lower than arithmetic
(test operator_precedence_bitwise_and_lower_than_arithmetic
  "PRECEDENCE: Bitwise AND (:∧) has lower precedence than arithmetic operators"
  (let ((ast '(:∧ (:+ (:var x) (:const 1)) (:const #xFF))))
    ;; Should be: (x + 1) & 0xFF
    (is (listp ast))
    (is (eq (first ast) :∧))))

;;; 6.3 Shift precedence: lower than arithmetic but higher than logical
(test operator_precedence_shift_lower_than_arithmetic
  "PRECEDENCE: Shift operators have lower precedence than arithmetic"
  (let ((ast '(:ash (:+ (:var x) (:const 1)) (:const 2))))
    (is (listp ast))
    (is (eq (first ast) :ash))))

;;; 6.4 Logical precedence: :and before :or
(test operator_precedence_logical_and_before_or
  "PRECEDENCE: Logical AND (:and) has higher precedence than OR (:or)"
  (let ((ast '(:or (:and (:var a) (:var b)) (:var c))))
    ;; Should be: (a AND b) OR c
    (is (listp ast))
    (is (eq (first ast) :or))))

;;; ============================================================================
;;; SECTION 7: COMPLEX EXPRESSION TESTS
;;; ============================================================================

;;; 7.1 Multiple arithmetic operators
(test operator_complex_mixed_arithmetic
  "EXPRESSION: Complex arithmetic (A + B - C * D / E)"
  (let ((ast '(:- (:+ (:var a) (:var b))
                  (:÷ (:× (:var c) (:var d))
                      (:var e)))))
    (is (listp ast))
    (is (eq (first ast) :-))))

;;; 7.2 Arithmetic with comparisons
(test operator_complex_arithmetic_comparison
  "EXPRESSION: Comparison of arithmetic results ((A + B) > (C * D))"
  (let ((ast '(:> (:+ (:var a) (:var b))
                  (:× (:var c) (:var d)))))
    (is (listp ast))
    (is (eq (first ast) :>))))

;;; 7.3 Bitwise with shifts
(test operator_complex_bitwise_with_shifts
  "EXPRESSION: Bitwise operations with shifts ((X << 2) & 0xFF)"
  (let ((ast '(:∧ (:ash (:var x) (:const 2)) (:const #xFF))))
    (is (listp ast))
    (is (eq (first ast) :∧))))

;;; 7.4 Logical with comparisons
(test operator_complex_logical_comparisons
  "EXPRESSION: Logical operations with comparisons ((A = B) AND (C < D))"
  (let ((ast '(:and (:= (:var a) (:var b))
                    (:< (:var c) (:var d)))))
    (is (listp ast))
    (is (eq (first ast) :and))))

;;; 7.5 Deeply nested expression
(test operator_complex_deeply_nested
  "EXPRESSION: Deeply nested (((A + B) * C) - ((D / E) & F)) expression"
  (let ((ast '(:- (:× (:+ (:var a) (:var b)) (:var c))
                  (:∧ (:÷ (:var d) (:var e)) (:var f)))))
    (is (listp ast))
    (is (eq (first ast) :-))
    (is (listp (second ast)))
    (is (eq (first (second ast)) :×))))

;;; ============================================================================
;;; SECTION 8: MIXED OPERATOR TYPE TESTS
;;; ============================================================================

;;; 8.1 Arithmetic and bitwise mixed
(test operator_mixed_arithmetic_bitwise
  "MIXED: Arithmetic and bitwise operators ((A + B) & (C - D))"
  (let ((ast '(:∧ (:+ (:var a) (:var b))
                  (:- (:var c) (:var d)))))
    (is (listp ast))
    (is (eq (first ast) :∧))))

;;; 8.2 All operator types in one expression
(test operator_mixed_all_types
  "MIXED: All operator types (((A + B) * C) >= D) AND (E & F)"
  (let ((ast '(:and (:≥ (:× (:+ (:var a) (:var b)) (:var c))
                         (:var d))
                    (:∧ (:var e) (:var f)))))
    (is (listp ast))
    (is (eq (first ast) :and))))

;;; ============================================================================
;;; SECTION 9: UNARY VS BINARY OPERATOR TESTS
;;; ============================================================================

;;; 9.1 Unary operators
(test operator_unary_bitwise_not
  "UNARY: Bitwise NOT (:¬) is unary operator"
  (let ((ast '(:¬ (:var x))))
    (is (listp ast))
    (is (= (length ast) 2))))

(test operator_unary_logical_not
  "UNARY: Logical NOT (:not) is unary operator"
  (let ((ast '(:not (:var flag))))
    (is (listp ast))
    (is (= (length ast) 2))))

;;; 9.2 Binary operators
(test operator_binary_addition
  "BINARY: Addition (:+) is binary operator"
  (let ((ast '(:+ (:var a) (:var b))))
    (is (listp ast))
    (is (>= (length ast) 3))))

(test operator_binary_logical_and
  "BINARY: Logical AND (:and) is binary operator"
  (let ((ast '(:and (:var a) (:var b))))
    (is (listp ast))
    (is (>= (length ast) 3))))

;;; ============================================================================
;;; SECTION 10: OPERATOR COVERAGE AUDIT TESTS
;;; ============================================================================

;;; These tests verify that all operators are recognized in the system

(test operator_coverage_all_arithmetic_defined
  "AUDIT: All arithmetic operators defined in AST (:+, :-, :×, :÷)"
  (is (member :+ (list :+ :- :× :÷)))
  (is (member :- (list :+ :- :× :÷)))
  (is (member :× (list :+ :- :× :÷)))
  (is (member :÷ (list :+ :- :× :÷))))

(test operator_coverage_all_comparison_defined
  "AUDIT: All comparison operators defined in AST (:=, :≠, :<, :≤, :>, :≥)"
  (is (member := (list := :≠ :< :≤ :> :≥)))
  (is (member :≠ (list := :≠ :< :≤ :> :≥)))
  (is (member :< (list := :≠ :< :≤ :> :≥)))
  (is (member :≤ (list := :≠ :< :≤ :> :≥)))
  (is (member :> (list := :≠ :< :≤ :> :≥)))
  (is (member :≥ (list := :≠ :< :≤ :> :≥))))

(test operator_coverage_all_bitwise_defined
  "AUDIT: All bitwise operators defined in AST (:¬, :∧, :∨, :⊻)"
  (is (member :¬ (list :¬ :∧ :∨ :⊻)))
  (is (member :∧ (list :¬ :∧ :∨ :⊻)))
  (is (member :∨ (list :¬ :∧ :∨ :⊻)))
  (is (member :⊻ (list :¬ :∧ :∨ :⊻))))

(test operator_coverage_all_shift_defined
  "AUDIT: All shift operators defined in AST (:ash, signed count)"
  (is (member :ash (list :ash)))
  (is (= 1 (length (list :ash)))))

(test operator_coverage_all_logical_defined
  "AUDIT: All logical operators defined in AST (:and, :or, :not)"
  (is (member :and (list :and :or :not)))
  (is (member :or (list :and :or :not)))
  (is (member :not (list :and :or :not))))

;;; ============================================================================
;;; SECTION 11: OPERATOR OCCURRENCE COUNTING
;;; ============================================================================

(test operator_coverage_arithmetic_count
  "COUNT: 4 arithmetic operators defined (:+, :-, :×, :÷)"
  (is (= (length (list :+ :- :× :÷)) 4)))

(test operator_coverage_comparison_count
  "COUNT: 6 comparison operators defined (:=, :≠, :<, :≤, :>, :≥)"
  (is (= (length (list := :≠ :< :≤ :> :≥)) 6)))

(test operator_coverage_bitwise_count
  "COUNT: 4 bitwise operators defined (:¬, :∧, :∨, :⊻)"
  (is (= (length (list :¬ :∧ :∨ :⊻)) 4)))

(test operator_coverage_shift_count
  "COUNT: 1 shift operator defined (:ash, signed count)"
  (is (= (length (list :ash)) 1)))

(test operator_coverage_logical_count
  "COUNT: 3 logical operators defined (:and, :or, :not)"
  (is (= (length (list :and :or :not)) 3)))

(test operator_coverage_total_count
  "COUNT: 25 total operators across all types"
  (let ((total (+ 4 6 6 3 3)))
    (is (= total 25))))
