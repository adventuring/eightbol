;;; src/backend-6502/backend-6502-add.lisp
;;; ADD statement compilation for 6502 family
;;; Copyright © 2026 EIGHTBOL Development. All rights reserved.

(in-package :eightbol)

;;; ============================================================================
;;; ADD STATEMENT COMPILATION
;;; ============================================================================

(defun emit-6502-add (operand1 operand2 &optional destination result)
  "Emit 6502 assembly for ADD operation.
   - operand1: first addend (can be register or memory)
   - operand2: second addend
   - destination: destination for result (modified in-place if no result)
   - result: explicit result location (with GIVING clause)
   
   Returns assembly code string and any register constraints."
  
  ;; Determine operation width based on operand types
  (let ((width (max (operand-width-bytes operand1)
                    (operand-width-bytes operand2)))
        (numeric-type (determine-operand-numeric-type operand1)))
    
    (cond
      ;; 8-bit unsigned BINARY ADD (most common case)
      ((= width 1)
       (emit-6502-add-8bit operand1 operand2 destination result numeric-type))
      
      ;; 16-bit unsigned BINARY ADD
      ((= width 2)
       (emit-6502-add-16bit operand1 operand2 destination result numeric-type))
      
      ;; BCD (DECIMAL) ADD
      ((numeric-type-decimal-p numeric-type)
       (emit-6502-add-bcd operand1 operand2 destination result numeric-type))
      
      ;; DISPLAY (zoned decimal) ADD
      ((numeric-type-display-p numeric-type)
       (emit-6502-add-display operand1 operand2 destination result))
      
      ;; Multi-byte BINARY ADD (>2 bytes)
      ((> width 2)
       (emit-6502-add-multibyte operand1 operand2 destination result width numeric-type))
      
      ;; Default fallback
      (t (emit-6502-add-generic operand1 operand2 destination result width numeric-type)))))

;;; ============================================================================
;;; 8-BIT ADD (Most Common)
;;; ============================================================================

(defun emit-6502-add-8bit (operand1 operand2 destination result type)
  "Emit optimized 8-bit ADD sequence for 6502.
   Sequence: LDA operand1, CLC, ADC operand2, STA destination"
  
  (let ((asm-output (make-string-output-stream)))
    (format asm-output "    ; ADD (8-bit ~a)~%" (type-name type))
    
    ;; Load first operand into accumulator
    (if (is-register-p operand1 :a)
        (format asm-output "    ; A already holds first operand~%")
        (format asm-output "    LDA ~a~%" (operand-to-6502 operand1)))
    
    ;; Clear carry for addition
    (format asm-output "    CLC~%")
    
    ;; Add second operand
    (format asm-output "    ADC ~a~%" (operand-to-6502 operand2))
    
    ;; Store result
    (let ((dest-loc (if result result (or destination operand1))))
      (format asm-output "    STA ~a~%" (operand-to-6502 dest-loc)))
    
    ;; Handle overflow if needed (set carry flag notes in code comment)
    (when (numeric-type-unsigned-p type)
      (format asm-output "    ; Carry flag set if overflow (>255)~%"))
    
    (get-output-stream-string asm-output)))

;;; ============================================================================
;;; 16-BIT ADD
;;; ============================================================================

(defun emit-6502-add-16bit (operand1 operand2 destination result type)
  "Emit 16-bit ADD sequence for 6502.
   Uses 6502 double-byte addition with carry propagation.
   Sequence:
     LDA operand1_low, CLC, ADC operand2_low, STA dest_low
     LDA operand1_high, ADC operand2_high, STA dest_high"
  
  (let ((asm-output (make-string-output-stream))
        (dest-loc (if result result (or destination operand1))))
    
    (format asm-output "    ; ADD (16-bit ~a)~%" (type-name type))
    
    ;; Add low bytes
    (format asm-output "    LDA ~a~%" (operand-to-6502-low operand1))
    (format asm-output "    CLC~%")
    (format asm-output "    ADC ~a~%" (operand-to-6502-low operand2))
    (format asm-output "    STA ~a~%" (operand-to-6502-low dest-loc))
    
    ;; Add high bytes (carry automatically propagates)
    (format asm-output "    LDA ~a~%" (operand-to-6502-high operand1))
    (format asm-output "    ADC ~a~%" (operand-to-6502-high operand2))
    (format asm-output "    STA ~a~%" (operand-to-6502-high dest-loc))
    
    (when (numeric-type-unsigned-p type)
      (format asm-output "    ; 16-bit carry propagated; final carry indicates 16-bit overflow~%"))
    
    (get-output-stream-string asm-output)))

;;; ============================================================================
;;; BCD (DECIMAL) ADD
;;; ============================================================================

(defun emit-6502-add-bcd (operand1 operand2 destination result type)
  "Emit BCD (DECIMAL) ADD sequence using 6502 decimal mode.
   Sequence: SED (set decimal mode), then normal ADD, then CLD (clear decimal)"
  
  (let ((asm-output (make-string-output-stream))
        (width (max (operand-width-bytes operand1)
                    (operand-width-bytes operand2)))
        (dest-loc (if result result (or destination operand1))))
    
    (format asm-output "    ; ADD (BCD ~a, ~d byte~:p)~%"
            (if (numeric-type-signed-p type) "signed" "unsigned") width)
    
    ;; Set decimal mode
    (format asm-output "    SED~%")
    
    ;; Perform addition (same as binary, but processor handles BCD)
    (format asm-output "    LDA ~a~%" (operand-to-6502 operand1))
    (format asm-output "    CLC~%")
    (format asm-output "    ADC ~a~%" (operand-to-6502 operand2))
    (format asm-output "    STA ~a~%" (operand-to-6502 dest-loc))
    
    ;; For multi-byte BCD, add bytes with carry propagation
    (when (> width 1)
      (format asm-output "    ; Additional bytes added with carry propagation~%")
      (loop for i from 1 below width do
            (format asm-output "    LDA ~a~%" (operand-to-6502-offset operand1 i))
            (format asm-output "    ADC ~a~%" (operand-to-6502-offset operand2 i))
            (format asm-output "    STA ~a~%" (operand-to-6502-offset dest-loc i))))
    
    ;; Clear decimal mode
    (format asm-output "    CLD~%")
    
    (format asm-output "    ; Carry flag set if BCD overflow~%")
    
    (get-output-stream-string asm-output)))

;;; ============================================================================
;;; DISPLAY (ZONED DECIMAL) ADD
;;; ============================================================================

(defun emit-6502-add-display (operand1 operand2 destination result)
  "Emit DISPLAY (zoned decimal) ADD sequence.
   DISPLAY numbers need digit extraction, binary addition, and re-encoding."
  
  (let ((asm-output (make-string-output-stream))
        (dest-loc (if result result (or destination operand1))))
    
    (format asm-output "    ; ADD (DISPLAY format)~%")
    (format asm-output "    ; Extract digits from DISPLAY format~%")
    
    ;; Extract first operand's numeric value from ASCII
    (format asm-output "    LDA ~a~%" (operand-to-6502 operand1))
    (format asm-output "    AND #$0f     ; Mask to digit value~%")
    (format asm-output "    STA $fc      ; Temporary storage~%")
    
    ;; Extract second operand's numeric value
    (format asm-output "    LDA ~a~%" (operand-to-6502 operand2))
    (format asm-output "    AND #$0f~%")
    
    ;; Add the values
    (format asm-output "    CLC~%")
    (format asm-output "    ADC $fc~%")
    
    ;; Handle carry (sum > 9, need to handle multi-digit)
    (format asm-output "    CMP #$0a     ; Check if >= 10~%")
    (format asm-output "    BCC +        ; Branch if < 10 (no carry needed)~%")
    (format asm-output "    SEC~%")
    (format asm-output "    SBC #$0a     ; Subtract 10, set carry~%")
    (format asm-output "+~%")
    
    ;; Re-encode as DISPLAY (ASCII digit)
    (format asm-output "    ORA #$30     ; Convert to ASCII ('0'-'9')~%")
    (format asm-output "    STA ~a~%" (operand-to-6502 dest-loc))
    
    (get-output-stream-string asm-output)))

;;; ============================================================================
;;; MULTI-BYTE ADD (>2 bytes)
;;; ============================================================================

(defun emit-6502-add-multibyte (operand1 operand2 destination result width type)
  "Emit multi-byte (>2 byte) ADD sequence.
   Generalizes 8-bit and 16-bit addition to arbitrary widths."
  
  (let ((asm-output (make-string-output-stream))
        (dest-loc (if result result (or destination operand1))))
    
    (format asm-output "    ; ADD (~d-byte ~a)~%"
            width (type-name type))
    
    (if (numeric-type-decimal-p type)
        ;; BCD multi-byte: use decimal mode
        (progn
          (format asm-output "    SED~%")
          (loop for i from 0 below width do
                (format asm-output "    LDA ~a~%" (operand-to-6502-offset operand1 i))
                (when (= i 0)
                  (format asm-output "    CLC~%"))
                (format asm-output "    ADC ~a~%" (operand-to-6502-offset operand2 i))
                (format asm-output "    STA ~a~%" (operand-to-6502-offset dest-loc i)))
          (format asm-output "    CLD~%"))
        
        ;; BINARY multi-byte: use carry propagation
        (progn
          (format asm-output "    CLC~%")
          (loop for i from 0 below width do
                (format asm-output "    LDA ~a~%" (operand-to-6502-offset operand1 i))
                (format asm-output "    ADC ~a~%" (operand-to-6502-offset operand2 i))
                (format asm-output "    STA ~a~%" (operand-to-6502-offset dest-loc i))))))
    
    (get-output-stream-string asm-output)))

;;; ============================================================================
;;; GENERIC ADD FALLBACK
;;; ============================================================================

(defun emit-6502-add-generic (operand1 operand2 destination result width type)
  "Generic ADD fallback for edge cases.
   Dispatches to appropriate implementation based on width and type."
  (declare (ignore width))
  
  ;; Fallback to simple 8-bit for now
  (emit-6502-add-8bit operand1 operand2 destination result type))

;;; ============================================================================
;;; HELPER FUNCTIONS
;;; ============================================================================

(defun is-register-p (operand register)
  "Check if OPERAND is the specified REGISTER (:a, :x, :y)."
  (and (symbolp operand) (eq operand register)))

(defun operand-width-bytes (operand)
  "Determine width of operand in bytes."
  (cond
    ((numberp operand) 1)
    ((symbolp operand) 1)
    ((listp operand) (get (car operand) :width-bytes))
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

;;; End of backend-6502-add.lisp
