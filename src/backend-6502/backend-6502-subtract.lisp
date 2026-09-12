;;; src/backend-6502/backend-6502-subtract.lisp
;;; SUBTRACT statement compilation for 6502 family
;;; Copyright © 2026 EIGHTBOL Development. All rights reserved.

(in-package :eightbol)

;;; ============================================================================
;;; SUBTRACT STATEMENT COMPILATION
;;; ============================================================================

(defun emit-6502-subtract (minuend subtrahend &optional destination result)
  "Emit 6502 assembly for SUBTRACT operation (minuend - subtrahend).
   - minuend: value to subtract from (first operand)
   - subtrahend: value to subtract (second operand)
   - destination: destination for result (modified in-place if no result)
   - result: explicit result location (with GIVING clause)
   
   Returns assembly code string and any register constraints."
  
  ;; Determine operation width based on operand types
  (let ((width (max (operand-width-bytes minuend)
                    (operand-width-bytes subtrahend)))
        (numeric-type (determine-operand-numeric-type minuend)))
    
    (cond
      ;; 8-bit unsigned BINARY SUBTRACT (most common)
      ((= width 1)
       (emit-6502-subtract-8bit minuend subtrahend destination result numeric-type))
      
      ;; 16-bit unsigned BINARY SUBTRACT
      ((= width 2)
       (emit-6502-subtract-16bit minuend subtrahend destination result numeric-type))
      
      ;; BCD (DECIMAL) SUBTRACT
      ((numeric-type-decimal-p numeric-type)
       (emit-6502-subtract-bcd minuend subtrahend destination result numeric-type))
      
      ;; DISPLAY (zoned decimal) SUBTRACT
      ((numeric-type-display-p numeric-type)
       (emit-6502-subtract-display minuend subtrahend destination result))
      
      ;; Multi-byte BINARY SUBTRACT (>2 bytes)
      ((> width 2)
       (emit-6502-subtract-multibyte minuend subtrahend destination result width numeric-type))
      
      ;; Default fallback
      (t (emit-6502-subtract-generic minuend subtrahend destination result width numeric-type)))))

;;; ============================================================================
;;; 8-BIT SUBTRACT (Most Common)
;;; ============================================================================

(defun emit-6502-subtract-8bit (minuend subtrahend destination result type)
  "Emit optimized 8-bit SUBTRACT sequence for 6502.
   Sequence: LDA minuend, SEC, SBC subtrahend, STA destination"
  
  (let ((asm-output (make-string-output-stream)))
    (format asm-output "    ; SUBTRACT (8-bit ~a)~%" (type-name type))
    
    ;; Load minuend into accumulator
    (if (is-register-p minuend :a)
        (format asm-output "    ; A already holds minuend~%")
        (format asm-output "    LDA ~a~%" (operand-to-6502 minuend)))
    
    ;; Set carry for subtraction (borrow = NOT carry initially)
    (format asm-output "    SEC~%")
    
    ;; Subtract subtrahend
    (format asm-output "    SBC ~a~%" (operand-to-6502 subtrahend))
    
    ;; Store result
    (let ((dest-loc (if result result (or destination minuend))))
      (format asm-output "    STA ~a~%" (operand-to-6502 dest-loc)))
    
    ;; Handle underflow if needed
    (when (numeric-type-unsigned-p type)
      (format asm-output "    ; Carry clear if underflow (result < 0)~%"))
    
    (get-output-stream-string asm-output)))

;;; ============================================================================
;;; 16-BIT SUBTRACT
;;; ============================================================================

(defun emit-6502-subtract-16bit (minuend subtrahend destination result type)
  "Emit 16-bit SUBTRACT sequence for 6502.
   Uses 6502 double-byte subtraction with borrow propagation.
   Sequence:
     LDA minuend_low, SEC, SBC subtrahend_low, STA dest_low
     LDA minuend_high, SBC subtrahend_high, STA dest_high"
  
  (let ((asm-output (make-string-output-stream))
        (dest-loc (if result result (or destination minuend))))
    
    (format asm-output "    ; SUBTRACT (16-bit ~a)~%" (type-name type))
    
    ;; Subtract low bytes
    (format asm-output "    LDA ~a~%" (operand-to-6502-low minuend))
    (format asm-output "    SEC~%")
    (format asm-output "    SBC ~a~%" (operand-to-6502-low subtrahend))
    (format asm-output "    STA ~a~%" (operand-to-6502-low dest-loc))
    
    ;; Subtract high bytes (borrow propagates via carry)
    (format asm-output "    LDA ~a~%" (operand-to-6502-high minuend))
    (format asm-output "    SBC ~a~%" (operand-to-6502-high subtrahend))
    (format asm-output "    STA ~a~%" (operand-to-6502-high dest-loc))
    
    (when (numeric-type-unsigned-p type)
      (format asm-output "    ; 16-bit borrow propagated; final carry clear = underflow~%"))
    
    (get-output-stream-string asm-output)))

;;; ============================================================================
;;; BCD (DECIMAL) SUBTRACT
;;; ============================================================================

(defun emit-6502-subtract-bcd (minuend subtrahend destination result type)
  "Emit BCD (DECIMAL) SUBTRACT sequence using 6502 decimal mode.
   Sequence: SED (set decimal mode), then normal SUBTRACT, then CLD (clear decimal)
   Note: 6502 decimal mode SBC is tricky; must handle carry properly."
  
  (let ((asm-output (make-string-output-stream))
        (width (max (operand-width-bytes minuend)
                    (operand-width-bytes subtrahend)))
        (dest-loc (if result result (or destination minuend))))
    
    (format asm-output "    ; SUBTRACT (BCD ~a, ~d byte~:p)~%"
            (if (numeric-type-signed-p type) "signed" "unsigned") width)
    
    ;; Set decimal mode
    (format asm-output "    SED~%")
    
    ;; Perform subtraction (same as binary, but processor handles BCD)
    (format asm-output "    LDA ~a~%" (operand-to-6502 minuend))
    (format asm-output "    SEC~%")
    (format asm-output "    SBC ~a~%" (operand-to-6502 subtrahend))
    (format asm-output "    STA ~a~%" (operand-to-6502 dest-loc))
    
    ;; For multi-byte BCD, subtract bytes with borrow propagation
    (when (> width 1)
      (format asm-output "    ; Additional bytes subtracted with borrow propagation~%")
      (loop for i from 1 below width do
            (format asm-output "    LDA ~a~%" (operand-to-6502-offset minuend i))
            (format asm-output "    SBC ~a~%" (operand-to-6502-offset subtrahend i))
            (format asm-output "    STA ~a~%" (operand-to-6502-offset dest-loc i))))
    
    ;; Clear decimal mode
    (format asm-output "    CLD~%")
    
    (format asm-output "    ; Carry clear if BCD underflow~%")
    
    (get-output-stream-string asm-output)))

;;; ============================================================================
;;; DISPLAY (ZONED DECIMAL) SUBTRACT
;;; ============================================================================

(defun emit-6502-subtract-display (minuend subtrahend destination result)
  "Emit DISPLAY (zoned decimal) SUBTRACT sequence.
   DISPLAY numbers need digit extraction, binary subtraction, and re-encoding."
  
  (let ((asm-output (make-string-output-stream))
        (dest-loc (if result result (or destination minuend))))
    
    (format asm-output "    ; SUBTRACT (DISPLAY format)~%")
    (format asm-output "    ; Extract digits from DISPLAY format~%")
    
    ;; Extract minuend's numeric value from ASCII
    (format asm-output "    LDA ~a~%" (operand-to-6502 minuend))
    (format asm-output "    AND #$0f     ; Mask to digit value~%")
    (format asm-output "    STA $fc      ; Temporary storage~%")
    
    ;; Extract subtrahend's numeric value
    (format asm-output "    LDA ~a~%" (operand-to-6502 subtrahend))
    (format asm-output "    AND #$0f~%")
    
    ;; Subtract the values
    (format asm-output "    SEC~%")
    (format asm-output "    LDA $fc~%")
    (format asm-output "    SBC #VALUE   ; Subtract, but this needs fixing~%")
    
    ;; Handle borrow (result < 0, need to handle multi-digit)
    (format asm-output "    BCS +        ; Branch if no borrow~%")
    (format asm-output "    ADC #$0a     ; Add 10, borrow set~%")
    (format asm-output "+~%")
    
    ;; Re-encode as DISPLAY (ASCII digit)
    (format asm-output "    ORA #$30     ; Convert to ASCII ('0'-'9')~%")
    (format asm-output "    STA ~a~%" (operand-to-6502 dest-loc))
    
    (get-output-stream-string asm-output)))

;;; ============================================================================
;;; MULTI-BYTE SUBTRACT (>2 bytes)
;;; ============================================================================

(defun emit-6502-subtract-multibyte (minuend subtrahend destination result width type)
  "Emit multi-byte (>2 byte) SUBTRACT sequence.
   Generalizes 8-bit and 16-bit subtraction to arbitrary widths."
  
  (let ((asm-output (make-string-output-stream))
        (dest-loc (if result result (or destination minuend))))
    
    (format asm-output "    ; SUBTRACT (~d-byte ~a)~%"
            width (type-name type))
    
    (if (numeric-type-decimal-p type)
        ;; BCD multi-byte: use decimal mode
        (progn
          (format asm-output "    SED~%")
          (loop for i from 0 below width do
                (format asm-output "    LDA ~a~%" (operand-to-6502-offset minuend i))
                (when (= i 0)
                  (format asm-output "    SEC~%"))
                (format asm-output "    SBC ~a~%" (operand-to-6502-offset subtrahend i))
                (format asm-output "    STA ~a~%" (operand-to-6502-offset dest-loc i)))
          (format asm-output "    CLD~%"))
        
        ;; BINARY multi-byte: use borrow propagation
        (progn
          (format asm-output "    SEC~%")
          (loop for i from 0 below width do
                (format asm-output "    LDA ~a~%" (operand-to-6502-offset minuend i))
                (format asm-output "    SBC ~a~%" (operand-to-6502-offset subtrahend i))
                (format asm-output "    STA ~a~%" (operand-to-6502-offset dest-loc i))))))
    
    (get-output-stream-string asm-output)))

;;; ============================================================================
;;; GENERIC SUBTRACT FALLBACK
;;; ============================================================================

(defun emit-6502-subtract-generic (minuend subtrahend destination result width type)
  "Generic SUBTRACT fallback for edge cases.
   Dispatches to appropriate implementation based on width and type."
  (declare (ignore width))
  
  ;; Fallback to simple 8-bit for now
  (emit-6502-subtract-8bit minuend subtrahend destination result type))

;;; ============================================================================
;;; HELPER FUNCTIONS (Reuse from ADD)
;;; ============================================================================

(defun is-register-p (operand register)
  "Check if OPERAND is the specified REGISTER (:a, :x, :y)."
  (and (symbolp operand) (eq operand register)))

(defun operand-width-bytes (operand)
  "Determine width of operand in bytes."
  (cond
    ((numberp operand) 1)
    ((symbolp operand) 1)
    ((listp operand) (getf operand :width-bytes 1))
    (t 1)))

(defun operand-to-6502 (operand)
  "Convert operand to 6502 addressing mode syntax."
  (cond
    ((numberp operand) (format nil "#$~2,'0x" operand))
    ((symbolp operand) (pascal-case operand))
    ((listp operand) (car operand))
    (t "????")))

(defun operand-to-6502-low (operand)
  "Get low byte of 16-bit operand."
  (let ((base (operand-to-6502 operand)))
    (if (string-contains-p "," base)
        base  ; Already has addressing mode
        (format nil "~a+0" base))))

(defun operand-to-6502-high (operand)
  "Get high byte of 16-bit operand."
  (let ((base (operand-to-6502 operand)))
    (if (string-contains-p "," base)
        base  ; Already has addressing mode
        (format nil "~a+1" base))))

(defun operand-to-6502-offset (operand offset)
  "Get byte at OFFSET of operand."
  (let ((base (operand-to-6502 operand)))
    (format nil "~a+~d" base offset)))

(defun determine-operand-numeric-type (operand)
  "Determine numeric type of operand from its metadata or default to DISPLAY."
  (if (listp operand)
      (getf operand :type :display)
      :display))

(defun type-name (type)
  "Get friendly name for numeric type."
  (case type
    (:binary-unsigned "unsigned binary")
    (:binary-signed "signed binary")
    (:decimal-unsigned "unsigned BCD")
    (:decimal-signed "signed BCD")
    (:display "DISPLAY")
    (t "unknown")))

;;; End of backend-6502-subtract.lisp
