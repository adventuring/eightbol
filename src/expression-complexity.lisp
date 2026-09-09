;; src/expression-complexity.lisp — Expression complexity fallback allocation framework
;;; Copyright © 2026 Interworldly Adventuring, LLC
(in-package :eightbol)

;;; Expression Complexity Management
;;;
;;; Backend strategies for handling complex expressions with limited temporaries:
;;; 1. Try reserved temporaries (MathTemp, MultiplyTemp)
;;; 2. Try CPU additional registers (if available)
;;; 3. Try declared (declare (temp ...)) variables
;;; 4. Error: Signal 'expression too complex' if all exhausted

(defvar *reserved-temporaries* '(|MathTemp| |MultiplyTemp|)
  "List of reserved temporary variables available on all backends.")

(defvar *cpu-additional-registers* nil
  "Per-CPU list of additional registers available for temporary storage.
   Bound in backend implementations per CPU. Examples:
   - 6502: () ;; only MathTemp, MultiplyTemp available
   - i286: (:AX :BX :CX :DX)
   - Z80: (:HL :BC :DE)
   - ARM7: (:R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7)")

(defvar *declared-temp-variables* nil
  "List of declared temporary variables from (declare (temp ...)) annotations.
   Bound during method compilation from :declare AST nodes.")

(defun get-available-temps (&key backend-cpu)
  "Return list of available temporaries for compilation.
   
   Returns temporaries in order of preference:
   1. Reserved temporaries (always available)
   2. CPU additional registers (if backend-cpu provided and available)
   3. Declared temporary variables (if any)
   
   BACKEND-CPU: keyword symbol identifying the CPU (:6502, :z80, :i286, etc.)
   Returns: list of variable/register names suitable for temporary allocation"
  
  (append
   ;; Always available reserved temporaries
   *reserved-temporaries*
   ;; CPU-specific additional registers
   (when backend-cpu
     (get-cpu-additional-registers backend-cpu))
   ;; User-declared temporary variables
   *declared-temp-variables*))

(defun get-cpu-additional-registers (cpu)
  "Return list of CPU-specific additional registers for temporary storage.
   
   Examples:
   - :6502 / :65c02 / :65c816: () — only reserved temporaries
   - :i286: (:AX :BX :CX :DX :SI :DI) — 6 general registers
   - :z80: (:HL :BC :DE :IX :IY) — 5 16-bit register pairs
   - :sm83: (:BC :DE :HL) — 3 16-bit register pairs (Game Boy Z80 variant)
   - :m68k: (:D0 :D1 :D2-:D7 :A0 :A1-:A5) — 8+ general regs
   - :arm7: (:R0-:R7) — user-available regs (R8-R12 reserved)
   
   Returns: list of register keywords"
  
  ;; This will be extended per backend implementation
  (case cpu
    (:6502 '())
    (:65c02 '())
    (:65c816 '())
    (:huc6280 '())
    (:rp2a03 '())
    (:cp1610 '())
    (:z80 '(:HL :BC :DE))
    (:sm83 '(:BC :DE :HL))
    (:m68k '(:D0 :D1))
    (:i286 '(:AX :BX :CX :DX))
    (:arm7 '(:R0 :R1 :R2 :R3))
    (:f8 '())
    (:forth '())
    (:stack '())
    (otherwise '())))

(defun allocate-temp-with-fallback (expr backend-cpu context)
  "Allocate temporary storage for an expression with fallback strategy.
   
   Attempts to find suitable temporary storage:
   1. Try reserved temporaries (MathTemp, MultiplyTemp)
   2. Try CPU additional registers
   3. Try declared temporary variables
   4. Signal error if none available
   
   EXPR: Expression AST node requiring temporary storage
   BACKEND-CPU: CPU keyword (:6502, :z80, etc.)
   CONTEXT: Backend compilation context (for tracking used temporaries)
   
   Returns: (values temp-var used-temps) where
   - temp-var: allocated temporary variable/register name
   - used-temps: list of temporaries already in use by caller
   
   Raises: error condition 'expression-too-complex if no space available"
  
  (declare (ignore context))
  (let* ((available (get-available-temps :backend-cpu backend-cpu))
         (used-temps (or *used-temporaries* '()))
         (free-temps (set-difference available used-temps)))
    
    (if free-temps
        (first free-temps)
        (signal-expression-too-complex expr))))

(defun signal-expression-too-complex (expr)
  "Signal compiler error with helpful message about expression complexity.
   
   Suggests solutions:
   - Split complex expressions into multiple statements
   - Use (declare (temp VarName ...)) to declare extra temporaries
   - Assign intermediate results to variables
   
   EXPR: Expression AST that was too complex to compile"
  
  (let ((available (get-available-temps)))
    (error "Expression too complex for available temporary storage.~
~%  Expression: ~s~
~%  Available temporaries: ~s~
~%  Solutions:~
~%    1. Split into multiple statements:~
~%       LET Intermediate = Sub-Expression~
~%       LET Result = Intermediate + OtherPart~
~%    2. Declare extra temporaries with:~
~%       (declare (temp TempVar1 TempVar2 ...))~
~%    3. Simplify the expression"
           expr available)))

(defun extract-declare-node (statements)
  "Extract and remove first :declare node from STATEMENTS if present.
   
   Returns: (values declare-node remaining-statements)
   
   The :declare node (if present) should be the first statement in a
   method body to declare extra temporary variables for that method."
  
  (if (and (listp (first statements))
           (eq :declare (first (first statements))))
      (values (first statements) (rest statements))
      (values nil statements)))

(defun extract-declared-temps (declare-node)
  "Extract temporary variable list from a :declare node.
   
   DECLARE-NODE: AST node of form (:declare :type :temp :variables (var1 var2 ...))
   
   Returns: list of variable names, or NIL if not a :temp declaration"
  
  (when (and (listp declare-node)
             (eq :declare (first declare-node)))
    (when (eq :temp (getf (rest declare-node) :type))
      (getf (rest declare-node) :variables))))

(defmacro with-expression-context ((&key backend-cpu declared-temps) &body body)
  "Bind context for expression compilation with temporary management.
   
   BACKEND-CPU: CPU keyword for register availability lookup
   DECLARED-TEMPS: List of temporary variables declared with (declare (temp ...))
   
   Within BODY, *cpu-additional-registers* and *declared-temp-variables*
   are bound appropriately."
  
  `(let ((*cpu-additional-registers* (get-cpu-additional-registers ,backend-cpu))
         (*declared-temp-variables* (or ,declared-temps '())))
     ,@body))

(defvar *used-temporaries* nil
  "Track which temporaries are currently in use during expression compilation.
   Bound during backend-specific expression compilation.")
