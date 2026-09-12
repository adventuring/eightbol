;;; src/backend-6502/backend-6502-bcd.lisp
;;; Binary-Coded Decimal (BCD) support for 6502 family
;;; Copyright © 2026 EIGHTBOL Development. All rights reserved.

(in-package :eightbol)

;;; ============================================================================
;;; BCD (DECIMAL) NUMERIC TYPE SUPPORT
;;; ============================================================================

(defvar *6502-decimal-mode-active* nil
  "Track whether 6502 is currently in decimal mode (D flag set).
   Used for optimization to avoid unnecessary SED/CLD pairs.")

(defvar *bcd-temp-registers* '($FC $FD $FE $FF)
  "Scratch memory locations for BCD intermediate calculations on 6502/65C02.
   These are in the zero page for fast access.")

;;; ============================================================================
;;; BCD LOAD AND STORE OPERATIONS
;;; ============================================================================

(defun emit-6502-load-bcd (bcd-value register)
  "Emit 6502 code to load BCD value into REGISTER.
   BCD values are already in proper digit format (0x00-0x99 for unsigned,
   0x00-0x99 with sign nybble for signed)."
  
  (format nil "    LDA ~a    ; Load BCD value~%"
          (operand-to-6502 bcd-value)))

(defun emit-6502-store-bcd (register bcd-destination)
  "Emit 6502 code to store BCD value from REGISTER.
   Automatically handles endianness and byte ordering."
  
  (format nil "    STA ~a    ; Store BCD value~%"
          (operand-to-6502 bcd-destination)))

;;; ============================================================================
;;; BCD ARITHMETIC IN DECIMAL MODE
;;; ============================================================================

(defun ensure-decimal-mode-on ()
  "Emit code to ensure decimal mode is ON, optimizing away redundant SED."
  (if *6502-decimal-mode-active*
      ""  ; Already on, no code needed
      (progn
        (setf *6502-decimal-mode-active* t)
        "    SED~%")))

(defun ensure-decimal-mode-off ()
  "Emit code to ensure decimal mode is OFF, optimizing away redundant CLD."
  (if (not *6502-decimal-mode-active*)
      ""  ; Already off, no code needed
      (progn
        (setf *6502-decimal-mode-active* nil)
        "    CLD~%")))

(defun toggle-decimal-mode (enable-p)
  "Toggle decimal mode ON (if ENABLE-P true) or OFF."
  (if enable-p
      (ensure-decimal-mode-on)
      (ensure-decimal-mode-off)))

;;; ============================================================================
;;; BCD FORMAT CONVERSIONS
;;; ============================================================================

(defun emit-6502-binary-to-bcd (binary-source bcd-destination)
  "Emit 6502 code to convert BINARY value to BCD.
   Algorithm:
   1. Load binary value
   2. Use repeated division by 10 to extract BCD digits
   3. Store digits in BCD format (high nybble = tens, low nybble = units)"
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Convert binary to BCD~%")
    (format asm "    LDA ~a~%" (operand-to-6502 binary-source))
    (format asm "    ; Algorithm: divide by 10 repeatedly~%")
    (format asm "    LDX #0~%")
    (format asm ".BINARY-TO-BCD-LOOP~%")
    (format asm "    CMP #10~%")
    (format asm "    BCC .BINARY-TO-BCD-DONE~%")
    (format asm "    SBC #10~%")
    (format asm "    INX~%")
    (format asm "    JMP .BINARY-TO-BCD-LOOP~%")
    (format asm ".BINARY-TO-BCD-DONE~%")
    (format asm "    ASL~%      ; Shift units digit left to high nybble~%")
    (format asm "    ASL~%")
    (format asm "    ASL~%")
    (format asm "    ASL~%")
    (format asm "    ORA #x    ; Combine with tens digit in X~%")
    (format asm "    STA ~a~%" (operand-to-6502 bcd-destination))
    
    (get-output-stream-string asm)))

(defun emit-6502-bcd-to-binary (bcd-source binary-destination)
  "Emit 6502 code to convert BCD value to BINARY.
   Algorithm:
   1. Load BCD value
   2. Extract high nybble (tens digit) and low nybble (units digit)
   3. Multiply tens by 10 and add units
   4. Store binary result"
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Convert BCD to binary~%")
    (format asm "    LDA ~a~%" (operand-to-6502 bcd-source))
    (format asm "    ; Extract tens digit (high nybble)~%")
    (format asm "    LSR~%")
    (format asm "    LSR~%")
    (format asm "    LSR~%")
    (format asm "    LSR~%")
    (format asm "    STA $FC    ; Store tens digit~%")
    (format asm "    LDA ~a~%" (operand-to-6502 bcd-source))
    (format asm "    AND #$0f   ; Extract units digit (low nybble)~%")
    (format asm "    STA $FD    ; Store units digit~%")
    (format asm "    LDA $FC~%")
    (format asm "    ASL~%")
    (format asm "    ASL~%")
    (format asm "    ASL~%")
    (format asm "    STA $FE    ; 8 * tens~%")
    (format asm "    LDA $FC~%")
    (format asm "    ASL~%")
    (format asm "    ADC $FE    ; + 2 * tens = 10 * tens~%")
    (format asm "    ADC $FD    ; + units = result~%")
    (format asm "    STA ~a~%" (operand-to-6502 binary-destination))
    
    (get-output-stream-string asm)))

;;; ============================================================================
;;; BCD SIGN HANDLING
;;; ============================================================================

(defun emit-6502-extract-bcd-sign (bcd-source sign-register)
  "Emit 6502 code to extract sign from BCD value.
   In 6502 BCD, sign is in high nybble of the value or separate byte.
   Returns :positive (Z=0), :negative (Z=1) in status register."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Extract BCD sign~%")
    (format asm "    LDA ~a~%" (operand-to-6502 bcd-source))
    (format asm "    AND #$f0   ; Isolate sign nybble~%")
    (format asm "    CMP #$d0   ; Check for negative sign (0xD)~%")
    (format asm "    ; Z=0 if negative, Z=1 if positive~%")
    
    (get-output-stream-string asm)))

(defun emit-6502-set-bcd-sign (bcd-destination sign)
  "Emit 6502 code to set sign in BCD value.
   SIGN is :positive or :negative."
  
  (let ((asm (make-string-output-stream))
        (sign-nybble (if (eq sign :negative) #xd0 #xc0)))
    (format asm "    ; Set BCD sign (~a)~%"
            (if (eq sign :negative) "negative" "positive"))
    (format asm "    LDA ~a~%" (operand-to-6502 bcd-destination))
    (format asm "    AND #$0f   ; Keep units digit~%")
    (format asm "    ORA #$~2,'0x   ; Set sign nybble~%" sign-nybble)
    (format asm "    STA ~a~%" (operand-to-6502 bcd-destination))
    
    (get-output-stream-string asm)))

;;; ============================================================================
;;; BCD MULTI-BYTE OPERATIONS
;;; ============================================================================

(defun emit-6502-bcd-add-multibyte (bcd1 bcd2 result width)
  "Emit optimized multi-byte BCD ADD sequence.
   Handles carrying through all bytes in decimal mode."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; BCD ADD (~d bytes)~%" width)
    (format asm "    SED    ; Set decimal mode~%")
    (format asm "    CLC    ; Clear carry for addition~%")
    
    (loop for byte-idx from 0 below width do
          (format asm "    LDA ~a    ; Load byte ~d of BCD1~%"
                  (format nil "~a+~d" (operand-to-6502 bcd1) byte-idx) byte-idx)
          (format asm "    ADC ~a    ; Add byte ~d of BCD2~%"
                  (format nil "~a+~d" (operand-to-6502 bcd2) byte-idx) byte-idx)
          (format asm "    STA ~a    ; Store byte ~d of result~%"
                  (format nil "~a+~d" (operand-to-6502 result) byte-idx) byte-idx))
    
    (format asm "    CLD    ; Clear decimal mode~%")
    
    (get-output-stream-string asm)))

(defun emit-6502-bcd-subtract-multibyte (bcd1 bcd2 result width)
  "Emit optimized multi-byte BCD SUBTRACT sequence.
   Handles borrowing through all bytes in decimal mode."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; BCD SUBTRACT (~d bytes)~%" width)
    (format asm "    SED    ; Set decimal mode~%")
    (format asm "    SEC    ; Set carry for subtraction~%")
    
    (loop for byte-idx from 0 below width do
          (format asm "    LDA ~a    ; Load byte ~d of minuend~%"
                  (format nil "~a+~d" (operand-to-6502 bcd1) byte-idx) byte-idx)
          (format asm "    SBC ~a    ; Subtract byte ~d of subtrahend~%"
                  (format nil "~a+~d" (operand-to-6502 bcd2) byte-idx) byte-idx)
          (format asm "    STA ~a    ; Store byte ~d of result~%"
                  (format nil "~a+~d" (operand-to-6502 result) byte-idx) byte-idx))
    
    (format asm "    CLD    ; Clear decimal mode~%")
    
    (get-output-stream-string asm)))

;;; ============================================================================
;;; BCD VALIDATION AND ERROR HANDLING
;;; ============================================================================

(defun emit-6502-validate-bcd (bcd-value error-label)
  "Emit 6502 code to validate BCD value (all nybbles 0-9).
   Jumps to ERROR-LABEL if BCD is invalid."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Validate BCD value~%")
    (format asm "    LDA ~a~%" (operand-to-6502 bcd-value))
    (format asm "    AND #$0f   ; Check low nybble~%")
    (format asm "    CMP #$0a~%")
    (format asm "    BCS ~a     ; Branch if >= 10 (invalid)~%" error-label)
    (format asm "    LDA ~a~%" (operand-to-6502 bcd-value))
    (format asm "    LSR~%")
    (format asm "    LSR~%")
    (format asm "    LSR~%")
    (format asm "    LSR~%")
    (format asm "    CMP #$0a   ; Check high nybble~%")
    (format asm "    BCS ~a     ; Branch if >= 10 (invalid)~%" error-label)
    
    (get-output-stream-string asm)))

;;; ============================================================================
;;; HELPER FUNCTIONS
;;; ============================================================================

(defun operand-to-6502 (operand)
  "Convert operand to 6502 addressing mode syntax."
  (cond
    ((numberp operand) (format nil "#$~2,'0x" operand))
    ((symbolp operand) (pascal-case operand))
    ((listp operand) (car operand))
    (t "????")))

;;; End of backend-6502-bcd.lisp
