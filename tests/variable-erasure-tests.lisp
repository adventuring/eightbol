;; tests/variable-erasure-tests.lisp — Variable erasure and resolution tests
;;
;; Validates that:
;; 1. All AST variables are qualified (:global or :slot)
;; 2. No bare variable symbols remain in AST
;; 3. Reserved temporaries (MathTemp, MultiplyTemp) are used correctly
;; 4. Undefined variables error at validation time
;;
;; Run: (fiveam:run! :variable-erasure)

(in-package :eightbol/test)

(fiveam:def-suite :variable-erasure
  :description "Variable erasure, resolution, and qualification validation")
(in-suite :variable-erasure)

;;; ============================================================================
;;; Test Helpers
;;; ============================================================================

(defun find-unqualified-vars-in-ast (node)
  "Search AST for any unqualified variable symbols.
   Returns list of found symbols, or empty list if none found."
  (let ((found nil))
    (labels ((walk (n)
               (cond
                 ((null n) nil)
                 ((numberp n) nil)
                 ((stringp n) nil)
                 ((keywordp n) nil)
                 ((symbolp n)
                  (push n found))
                 ((and (listp n) (eq (first n) :global))
                  nil)
                 ((and (listp n) (eq (first n) :slot))
                  nil)
                 ((listp n)
                  (dolist (e n) (walk e))))))
      (walk node))
    (nreverse found)))

(defun create-mock-copybook-tables (var-names)
  "Create mock copybook tables with VAR-NAMES as globals.
   Returns the tuple (values slot-table type-table …) directly."
  (let ((slot-table (make-hash-table :test 'equalp))
        (type-table (make-hash-table :test 'equalp))
        (const-table (make-hash-table :test 'equalp))
        (service-bank-table (make-hash-table :test 'equalp))
        (usage-table (make-hash-table :test 'equalp))
        (sign-table (make-hash-table :test 'equalp))
        (pic-size-table (make-hash-table :test 'equalp))
        (pic-width-table (make-hash-table :test 'equalp))
        (pic-frac-bits-table (make-hash-table :test 'equalp))
        (pic-nybble-semantics-table (make-hash-table :test 'equalp)))
    (dolist (var-name var-names)
      (let ((key (eightbol::cobol-slot-table-name-key var-name)))
        (setf (gethash key slot-table) "Global")))
    ;; Add reserved temporaries
    (dolist (temp '("MathTemp" "MultiplyTemp"))
      (let ((key (eightbol::cobol-slot-table-name-key temp)))
        (setf (gethash key slot-table) "Global")))
    (values slot-table type-table const-table service-bank-table usage-table sign-table
            pic-size-table pic-width-table pic-frac-bits-table pic-nybble-semantics-table)))

;;; ============================================================================
;;; VALIDATION TESTS
;;; ============================================================================

(test variable-erasure/validates-no-bare-symbols-in-simple-move
  "Simple MOVE AST should contain no bare variable symbols."
  (let ((ast `((:move :from (:global "X") :to (:global "Y")))))
    (is (null (find-unqualified-vars-in-ast ast))
        "No bare symbols in qualified MOVE")))

(test variable-erasure/detects-bare-symbol-in-expression
  "Find bare symbols in unqualified AST."
  (let ((ast `((:move :from X :to Y))))
    (let ((found (find-unqualified-vars-in-ast ast)))
      (is (member 'X found))
      (is (member 'Y found)))))

(test variable-erasure/detects-bare-symbol-in-compute
  "Find bare symbols in :compute expression."
  (let ((ast `((:compute :target Z :expression (+ X Y)))))
    (let ((found (find-unqualified-vars-in-ast ast)))
      (is (not (null found)))
      (is (member 'X found))
      (is (member 'Y found))
      (is (member 'Z found)))))

(test variable-erasure/validates-qualified-compute
  "Qualified :compute AST should have no bare symbols."
  (let ((ast `((:compute :target (:global "Z") :expression (:+ (:global "X") (:global "Y"))))))
    (is (null (find-unqualified-vars-in-ast ast))
        "No bare symbols in qualified COMPUTE")))

(test variable-erasure/validates-qualified-if-condition
  "IF with qualified condition variables should pass."
  (let ((ast `((:if :condition (:= (:global "X") 0)
                    :then ((:move :from (:global "A") :to (:global "B")))
                    :else ((:move :from (:global "C") :to (:global "D")))))))
    (is (null (find-unqualified-vars-in-ast ast))
        "No bare symbols in qualified IF")))

(test variable-erasure/detects-bare-vars-in-if
  "IF with unqualified variables should be detected."
  (let ((ast `((:if :condition (:= X 0)
                    :then ((:move :from A :to B))
                    :else ((:move :from C :to D))))))
    (let ((found (find-unqualified-vars-in-ast ast)))
      (is (not (null found)))
      (is (every (lambda (sym) (member sym '(X A B C D))) found)))))

;;; ============================================================================
;;; RESOLUTION TESTS
;;; ============================================================================

(test variable-erasure/resolve-variable-finds-global
  "Resolve global variable from copybook."
  (multiple-value-bind (slot-table _t _c _s _u _sig _ps _pw _pf _pn) (create-mock-copybook-tables '("X" "Y" "Z"))
    (let ((result (eightbol::resolve-variable "X" slot-table)))
      (is (and (listp result) (eq (first result) :global))))))

(test variable-erasure/resolve-variable-reserved-temp
  "Resolve reserved temporary MathTemp."
  (multiple-value-bind (slot-table _t _c _s _u _sig _ps _pw _pf _pn) (create-mock-copybook-tables nil)
    (let ((result (eightbol::resolve-variable "MathTemp" slot-table)))
      (is (and (listp result) (eq (first result) :global))))))

(test variable-erasure/resolve-variable-multiply-temp
  "Resolve reserved temporary MultiplyTemp."
  (multiple-value-bind (slot-table _t _c _s _u _sig _ps _pw _pf _pn) (create-mock-copybook-tables nil)
    (let ((result (eightbol::resolve-variable "MultiplyTemp" slot-table)))
      (is (and (listp result) (eq (first result) :global))))))

(test variable-erasure/resolve-variable-undefined-errors
  "Resolving undefined variable signals error."
  (multiple-value-bind (slot-table _t _c _s _u _sig _ps _pw _pf _pn) (create-mock-copybook-tables '("X" "Y"))
    (signals eightbol::compiler-error
      (eightbol::resolve-variable "UNDEFINED" slot-table))))

(test variable-erasure/resolve-expression-simple-number
  "Resolve numeric literal (unchanged)."
  (multiple-value-bind (slot-table _t _c _s _u _sig _ps _pw _pf _pn) (create-mock-copybook-tables '("X"))
    (let ((result (eightbol::resolve-expression 42 slot-table)))
      (is (= 42 result)))))

(test variable-erasure/resolve-expression-string
  "Resolve string literal (unchanged)."
  (multiple-value-bind (slot-table _t _c _s _u _sig _ps _pw _pf _pn) (create-mock-copybook-tables '("X"))
    (let ((result (eightbol::resolve-expression "HELLO" slot-table)))
      (is (string-equal "HELLO" result)))))

(test variable-erasure/resolve-expression-bare-symbol
  "Resolve bare symbol to qualified reference."
  (multiple-value-bind (slot-table _t _c _s _u _sig _ps _pw _pf _pn) (create-mock-copybook-tables '("X"))
    (let ((result (eightbol::resolve-expression 'X slot-table)))
      (is (and (listp result) (eq (first result) :global))))))

(test variable-erasure/resolve-expression-qualified-already
  "Qualified reference passes through unchanged."
  (multiple-value-bind (slot-table _t _c _s _u _sig _ps _pw _pf _pn) (create-mock-copybook-tables '("X"))
    (let ((result (eightbol::resolve-expression '(:global "X") slot-table)))
      (is (equal '(:global "X") result)))))

(test variable-erasure/resolve-expression-subscript
  "Resolve subscript expression."
  (multiple-value-bind (slot-table _t _c _s _u _sig _ps _pw _pf _pn) (create-mock-copybook-tables '("ARRAY" "IDX"))
    (let ((result (eightbol::resolve-expression '(:subscript ARRAY IDX) slot-table)))
      (is (and (listp result)
               (eq (first result) :subscript)
               (listp (second result))
               (eq (first (second result)) :global))))))

;;; ============================================================================
;;; ERASURE TESTS
;;; ============================================================================

(test variable-erasure/erase-locals-simple-move
  "Erase locals in simple MOVE statement."
  (multiple-value-bind (slot-table _t _c _s _u _sig _ps _pw _pf _pn) (create-mock-copybook-tables '("X" "Y"))
    (let ((ast `((:move :from X :to Y))))
      (let ((result (eightbol::erase-locals (first ast) slot-table)))
        (is (null (find-unqualified-vars-in-ast result)))))))

(test variable-erasure/erase-locals-compute
  "Erase locals in :compute statement."
  (multiple-value-bind (slot-table _t _c _s _u _sig _ps _pw _pf _pn) (create-mock-copybook-tables '("X" "Y" "Z"))
    (let ((ast `((:compute :target Z :expression (:+ X Y)))))
      (let ((result (eightbol::erase-locals (first ast) slot-table)))
        (is (null (find-unqualified-vars-in-ast result)))))))

(test variable-erasure/erase-locals-if-statement
  "Erase locals in :if statement with condition."
  (multiple-value-bind (slot-table _t _c _s _u _sig _ps _pw _pf _pn) (create-mock-copybook-tables '("X" "Y" "A" "B"))
    (let ((ast `((:if :condition (:= X 0)
                      :then ((:move :from A :to B))
                      :else nil))))
      (let ((result (eightbol::erase-locals (first ast) slot-table)))
        (is (null (find-unqualified-vars-in-ast result)))))))

(test variable-erasure/erase-locals-preserves-structure
  "Erase locals preserves AST structure."
  (multiple-value-bind (slot-table _t _c _s _u _sig _ps _pw _pf _pn) (create-mock-copybook-tables '("X" "Y"))
    (let ((ast `((:move :from X :to Y))))
      (let ((result (eightbol::erase-locals (first ast) slot-table)))
        (is (eq :move (first result)))
        (is (eightbol::safe-getf (rest result) :from))
        (is (eightbol::safe-getf (rest result) :to))))))

(test variable-erasure/allocate-temp-byte
  "Allocate byte temporary for small operations."
  (let ((result (eightbol::allocate-temp-for-intermediate :arithmetic 8)))
    (is (string-equal "MathTemp" result))))

(test variable-erasure/allocate-temp-word
  "Allocate word temporary for 16-bit operations."
  (let ((result (eightbol::allocate-temp-for-intermediate :× 16)))
    (is (string-equal "MultiplyTemp" result))))

(test variable-erasure/allocate-temp-too-large-errors
  "Allocate temporary for >16-bit errors."
  (signals eightbol::compiler-error
    (eightbol::allocate-temp-for-intermediate :arithmetic 32)))

;;; ============================================================================
;;; VALIDATION PHASE TESTS
;;; ============================================================================

(test variable-erasure/validate-qualified-ast-passes
  "Qualified AST passes validation."
  (let ((ast `(:program :class-id "Test" :data nil
               :methods ((:method :method-id "T"
                          :statements ((:move :from (:global "X") :to (:global "Y"))))))))
    (is (eightbol::validate-no-unqualified-variables ast))))

(test variable-erasure/validate-unqualified-ast-signals
  "Unqualified AST signals validation error."
  (let ((ast `(:program :class-id "Test" :data nil
               :methods ((:method :method-id "T"
                          :statements ((:move :from X :to Y)))))))
    (signals eightbol::compiler-error
      (eightbol::validate-no-unqualified-variables ast))))

;;; ============================================================================
;;; End of file
;;; ============================================================================
