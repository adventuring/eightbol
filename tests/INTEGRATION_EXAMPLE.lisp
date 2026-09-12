;;; Integration Example: Complete System with Calling Conventions, Declarations, & Complexity

;;; This example demonstrates how all three features work together
;;; in a complete compilation pipeline.

;;; ============================================================================
;;; EXAMPLE 1: Simple Procedure with Declarations
;;; ============================================================================

;;; Source (COBOL-like):
;;; 
;;;        IDENTIFICATION DIVISION.
;;;        PROGRAM-ID. DamageCalc.
;;;       *> (declare (optimize (speed 3) (space 1)))
;;;        PROCEDURE DIVISION.
;;;            IDENTIFICATION DIVISION.
;;;            METHOD-ID. "Calculate".
;;;            PROCEDURE DIVISION.
;;;                COMPUTE Result = (Base - Armor) * Factor.
;;;                CALL LogDamage USING Result.
;;;                GOBACK.

;;; AST Output (with all three features):

#|
(:procedure 
  :name "Calculate"
  :declare (
    (optimize (speed 3) (space 1))
  )
  :statements (
    (:compute 
      :target Result
      :expression (:× (:- Base Armor) Factor))
    (:call 
      :target "LogDamage"
      :type :library        ; PART A: Calling convention
      :using Result)
    (:goback)
  ))
|#

;;; ============================================================================
;;; EXAMPLE 2: Complex Procedure with Multiple Declarations
;;; ============================================================================

;;; Source:
;;;       *> (declare (optimize (speed 3) (space 2) (safety 0)))
;;;       *> (declare (temp WorkTempA WorkTempB ExprTemp))
;;;        PROCEDURE DIVISION.
;;;            SET IntermediateResult TO ComplexExpression.
;;;            PERFORM Process IntermediateResult
;;;                TIMES 10.
;;;            GOBACK.

#|
(:procedure
  :name "ComplexProcessing"
  :declare (
    (optimize (speed 3) (space 2) (safety 0))  ; PART B: Optimization hints
    (temp WorkTempA WorkTempB ExprTemp)         ; PART B: Extra temporaries
  )
  :statements (
    (:set
      :target IntermediateResult
      :value ComplexExpression)
    (:perform
      :procedure "Process"
      :times 10
      :body (
        (:call
          :target "Process"
          :type :subroutine                     ; PART A: Local call
          :using IntermediateResult)
      ))
    (:goback)
  ))
|#

;;; ============================================================================
;;; EXAMPLE 3: Service Call with Far Convention
;;; ============================================================================

#|
(:procedure
  :name "CallService"
  :body (
    (:call
      :target "RemoteHandler"
      :bank ServiceBank
      :type :far-service                        ; PART A: Service bank call
      :using Param1)))                          ;    (no accumulator return)
|#

;;; ============================================================================
;;; EXAMPLE 4: Integration Showing Compiler Processing
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun integration-example-parse-cobol-with-declarations ()
    "Example: Parse COBOL with declarations, emit AST with all features."
    
    ;; Step 1: Frontend parses COBOL
    (let* ((cobol-source "
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DamageCalc.
      *> (declare (optimize (speed 3) (space 1) (safety 0)))
      *> (declare (temp DamageTemp FactorTemp))
       PROCEDURE DIVISION.
           IDENTIFICATION DIVISION.
           METHOD-ID. \"CalculateDamage\".
           PROCEDURE DIVISION.
               COMPUTE DamageTemp = (Base - Armor) * DamageFactor.
               CALL LogDamage IN LIBRARY USING DamageTemp.
               GOBACK.
    ")
           
           ;; Parse with declarations
           (ast (eightbol::parse-cobol cobol-source)))
      
      ;; Step 2: Extract first method
      (let* ((methods (eightbol::ast-methods ast))
             (method (first methods))
             (declarations (eightbol::safe-getf (rest method) :declare)))
        
        ;; Step 3: Verify all three features present
        (format t "~&Method: ~s~%" (eightbol::safe-getf (rest method) :method-id))
        
        ;; PART B: Show declarations
        (when declarations
          (format t "~&Declarations:~%")
          (dolist (decl declarations)
            (format t "  ~s~%" decl)))
        
        ;; PART A: Show call with :type
        (let* ((stmts (eightbol::ast-method-statements method))
               (call-stmt (find-if (lambda (s) (eq (first s) :call)) stmts)))
          (when call-stmt
            (format t "~&Call Statement:~%")
            (format t "  Target: ~s~%" (eightbol::safe-getf (rest call-stmt) :target))
            (format t "  Type: ~s~%" (eightbol::safe-getf (rest call-stmt) :type))
            (format t "  Library: ~s~%" (eightbol::safe-getf (rest call-stmt) :library))))))))

;;; ============================================================================
;;; EXAMPLE 5: Backend Using Declaration Information
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun backend-example-use-declarations (proc-node)
    "Example: Backend uses declarations and calling convention info."
    
    (let* ((name (eightbol::safe-getf (rest proc-node) :name))
           (declarations (eightbol::safe-getf (rest proc-node) :declare))
           (stmts (eightbol::safe-getf (rest proc-node) :statements)))
      
      ;; Extract optimization hints
      (let ((optimize-hints (assoc 'optimize declarations :test #'string-equal)))
        (when optimize-hints
          (let ((speed (getf (rest optimize-hints) :speed 1))
                (space (getf (rest optimize-hints) :space 1)))
            
            ;; Decision: Inline vs. Call
            (if (> speed space)
                (format t "~&Procedure ~s: Inlining enabled (speed ~d > space ~d)~%" 
                        name speed space)
                (format t "~&Procedure ~s: Size optimization (space ~d >= speed ~d)~%" 
                        name space speed)))))
      
      ;; Extract temporary declarations
      (let ((temp-decl (assoc 'temp declarations :test #'string-equal)))
        (when temp-decl
          (format t "~&Available temporaries: ~s~%" (rest temp-decl))))
      
      ;; Process statements with calling convention info
      (format t "~&Compiling statements:~%")
      (dolist (stmt stmts)
        (when (eq (first stmt) :call)
          (let ((type (eightbol::safe-getf (rest stmt) :type))
                (target (eightbol::safe-getf (rest stmt) :target)))
            (ecase type
              (:library
               (format t "  CALL ~s IN LIBRARY (accumulator may have return value)~%" target))
              (:subroutine
               (format t "  JSR ~s (local call, accumulator may have return value)~%" target))
              (:far-service
               (format t "  FAR-CALL ~s (service bank, no accumulator)~%" target)))))))))

;;; ============================================================================
;;; EXAMPLE 6: AST Optimization Using Declarations
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun optimizer-example-use-hints (proc-node)
    "Example: Optimizer uses declaration hints for aggressiveness."
    
    (let* ((declarations (eightbol::safe-getf (rest proc-node) :declare))
           (optimize (assoc 'optimize declarations :test #'string-equal)))
      
      (if (null optimize)
          (format t "~&Optimizer: Running with default settings~%")
          
          (let ((speed (or (getf (rest optimize) :speed) 1))
                (space (or (getf (rest optimize) :space) 1))
                (safety (or (getf (rest optimize) :safety) 1)))
            
            (format t "~&Optimizer: Settings (speed=~d space=~d safety=~d)~%"
                    speed space safety)
            
            ;; Apply optimization passes based on hints
            (when (>= speed 3)
              (format t "  Enabling: Loop unrolling~%")
              (format t "  Enabling: Instruction scheduling~%")
              (format t "  Enabling: Aggressive inlining~%"))
            
            (when (>= speed 2)
              (format t "  Enabling: Constant folding~%")
              (format t "  Enabling: Dead code elimination~%"))
            
            (when (>= space 3)
              (format t "  Enabling: Maximal compression~%")
              (format t "  Enabling: Strength reduction~%"))
            
            (when (<= safety 1)
              (format t "  Disabling: Bounds checking~%")
              (format t "  Disabling: Overflow checks~%")))))))

;;; ============================================================================
;;; EXAMPLE 7: Complete Compilation Pipeline
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun complete-pipeline-example ()
    "Walk through complete compilation with all three features."
    
    (format t "~&~%=== EIGHTBOL INTEGRATION EXAMPLE ===~%~%")
    
    ;; Step 1: Parse
    (format t "STEP 1: Frontend Parsing~%")
    (format t "  Input: COBOL source with declaration comments~%")
    (format t "  Output: AST with :declare and :type keywords~%~%")
    
    ;; Step 2: Optimize
    (format t "STEP 2: AST Optimization~%")
    (format t "  Examine: (:declare (optimize ...)) hints~%")
    (format t "  Decision: Aggressiveness level~%")
    (format t "  Action: Apply optimization passes~%~%")
    
    ;; Step 3: Codegen
    (format t "STEP 3: Backend Code Generation~%")
    (format t "  Examine: (:declare (temp ...)) declarations~%")
    (format t "  Examine: (:call :type ...) for return handling~%")
    (format t "  Action: Emit optimal assembly~%~%")
    
    (format t "=== FEATURES INTEGRATED ===~%~%")
    (format t "PART A: Calling Convention Distinction~%")
    (format t "  ✓ :call :type :subroutine/:library/:far-service~%")
    (format t "  ✓ :invoke (no accumulator)~%~%")
    
    (format t "PART B: Pragmatic Declarations~%")
    (format t "  ✓ (optimize (speed N) (space N) (safety N))~%")
    (format t "  ✓ (temp Var1 Var2 Var3)~%~%")
    
    (format t "PART C: Full Integration~%")
    (format t "  ✓ All information available to backends~%")
    (format t "  ✓ Optimizers can use hints (optional)~%")
    (format t "  ✓ Expression complexity solved~%~%")))

;;; Run the complete example
(integration-example-parse-cobol-with-declarations)
(optimizer-example-use-hints '(:procedure :name "Test" :declare ((optimize (speed 3) (space 1)))))
(complete-pipeline-example)
