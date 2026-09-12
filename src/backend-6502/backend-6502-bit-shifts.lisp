;;; src/backend-6502/backend-6502-bit-shifts.lisp
;;; Bit shift and rotate operations for 6502 family
;;; Copyright © 2026 EIGHTBOL Development. All rights reserved.

(in-package :eightbol)

;;; ============================================================================
;;; BIT SHIFT OPERATIONS (ASH, LSH, etc.)
;;; ============================================================================

(defun emit-6502-shift-left (value shift-count destination)
  "Emit 6502 code for logical left shift (ASL, LSL).
   Left-shifts VALUE by SHIFT-COUNT bits, storing result in DESTINATION.
   Carry flag set if highest bit shifted out (overflow)."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Shift left by ~d bit~:p~%" shift-count)
    (format asm "    LDA ~a~%" (operand-to-6502 value))
    
    ;; Unroll shifts for efficiency (up to 4 shifts)
    (if (<= shift-count 4)
        (loop for i from 0 below shift-count do
              (format asm "    ASL~%"))
        (progn
          (format asm "    LDX #~d~%" shift-count)
          (format asm ".SHIFT-LEFT-LOOP~%")
          (format asm "    ASL~%")
          (format asm "    DEX~%")
          (format asm "    BNE .SHIFT-LEFT-LOOP~%")))
    
    (format asm "    STA ~a    ; Store shifted result~%" (operand-to-6502 destination))
    
    (get-output-stream-string asm)))

(defun emit-6502-shift-right (value shift-count destination &optional signed)
  "Emit 6502 code for shift right (logical or arithmetic).
   If SIGNED is true, uses arithmetic shift (SRA/ASR) preserving sign bit.
   Otherwise uses logical shift (LSR)."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Shift right by ~d bit~:p (~a)~%"
            shift-count (if signed "arithmetic" "logical"))
    (format asm "    LDA ~a~%" (operand-to-6502 value))
    
    ;; For unsigned, use LSR (logical shift right)
    ;; For signed, use ASL then loop with ASR (arithmetic shift right)
    ;; Note: 6502 doesn't have ASR instruction; use ROR instead
    
    (if (<= shift-count 4)
        (loop for i from 0 below shift-count do
              (if signed
                  (format asm "    CMP #$80   ; Check sign bit~%    BCC +~%    SEC~%    +ROR~%")
                  (format asm "    LSR~%")))
        (progn
          (format asm "    LDX #~d~%" shift-count)
          (format asm ".SHIFT-RIGHT-LOOP~%")
          (if signed
              (format asm "    ROR~%    ; Rotate right preserves carry (sign)~%")
              (format asm "    LSR~%"))
          (format asm "    DEX~%")
          (format asm "    BNE .SHIFT-RIGHT-LOOP~%")))
    
    (format asm "    STA ~a    ; Store shifted result~%" (operand-to-6502 destination))
    
    (get-output-stream-string asm)))

(defun emit-6502-rotate-left (value rotate-count destination &optional through-carry)
  "Emit 6502 code for bit rotation left (ROL).
   If THROUGH-CARRY, rotation passes through carry flag (multi-bit safe).
   Otherwise uses ROL which shifts through carry (normal rotate)."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Rotate left by ~d bit~:p~%" rotate-count)
    (format asm "    LDA ~a~%" (operand-to-6502 value))
    
    (if through-carry
        (format asm "    CLC~%"))  ; Clear carry before rotation
    
    (if (<= rotate-count 4)
        (loop for i from 0 below rotate-count do
              (format asm "    ROL~%"))
        (progn
          (format asm "    LDX #~d~%" rotate-count)
          (format asm ".ROTATE-LEFT-LOOP~%")
          (format asm "    ROL~%")
          (format asm "    DEX~%")
          (format asm "    BNE .ROTATE-LEFT-LOOP~%")))
    
    (format asm "    STA ~a    ; Store rotated result~%" (operand-to-6502 destination))
    
    (get-output-stream-string asm)))

(defun emit-6502-rotate-right (value rotate-count destination &optional through-carry)
  "Emit 6502 code for bit rotation right (ROR).
   If THROUGH-CARRY, rotation passes through carry flag (multi-bit safe).
   Otherwise uses ROR which rotates through carry (normal rotate)."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Rotate right by ~d bit~:p~%" rotate-count)
    (format asm "    LDA ~a~%" (operand-to-6502 value))
    
    (if through-carry
        (format asm "    CLC~%"))  ; Clear carry before rotation
    
    (if (<= rotate-count 4)
        (loop for i from 0 below rotate-count do
              (format asm "    ROR~%"))
        (progn
          (format asm "    LDX #~d~%" rotate-count)
          (format asm ".ROTATE-RIGHT-LOOP~%")
          (format asm "    ROR~%")
          (format asm "    DEX~%")
          (format asm "    BNE .ROTATE-RIGHT-LOOP~%")))
    
    (format asm "    STA ~a    ; Store rotated result~%" (operand-to-6502 destination))
    
    (get-output-stream-string asm)))

;;; ============================================================================
;;; BITWISE LOGICAL OPERATIONS
;;; ============================================================================

(defun emit-6502-bitwise-and (operand1 operand2 destination)
  "Emit 6502 code for bitwise AND operation.
   Result: operand1 AND operand2 → destination"
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Bitwise AND~%")
    (format asm "    LDA ~a~%" (operand-to-6502 operand1))
    (format asm "    AND ~a~%" (operand-to-6502 operand2))
    (format asm "    STA ~a~%" (operand-to-6502 destination))
    
    (get-output-stream-string asm)))

(defun emit-6502-bitwise-or (operand1 operand2 destination)
  "Emit 6502 code for bitwise OR operation.
   Result: operand1 OR operand2 → destination"
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Bitwise OR~%")
    (format asm "    LDA ~a~%" (operand-to-6502 operand1))
    (format asm "    ORA ~a~%" (operand-to-6502 operand2))
    (format asm "    STA ~a~%" (operand-to-6502 destination))
    
    (get-output-stream-string asm)))

(defun emit-6502-bitwise-xor (operand1 operand2 destination)
  "Emit 6502 code for bitwise XOR (exclusive OR) operation.
   Result: operand1 XOR operand2 → destination"
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Bitwise XOR~%")
    (format asm "    LDA ~a~%" (operand-to-6502 operand1))
    (format asm "    EOR ~a~%" (operand-to-6502 operand2))
    (format asm "    STA ~a~%" (operand-to-6502 destination))
    
    (get-output-stream-string asm)))

(defun emit-6502-bitwise-not (operand destination)
  "Emit 6502 code for bitwise NOT (complement/negation) operation.
   Result: NOT operand → destination"
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Bitwise NOT (complement)~%")
    (format asm "    LDA ~a~%" (operand-to-6502 operand))
    (format asm "    EOR #$FF   ; XOR with all 1's to flip all bits~%")
    (format asm "    STA ~a~%" (operand-to-6502 destination))
    
    (get-output-stream-string asm)))

;;; ============================================================================
;;; BIT TESTING AND MANIPULATION
;;; ============================================================================

(defun emit-6502-test-bit (value bit-position)
  "Emit 6502 code to test a single bit at BIT-POSITION.
   Sets zero flag if bit is 0, clears if bit is 1.
   Returns assembly code."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Test bit ~d~%" bit-position)
    (format asm "    LDA ~a~%" (operand-to-6502 value))
    (format asm "    AND #$~2,'0x   ; Mask bit ~d~%"
            (ash 1 bit-position) bit-position)
    (format asm "    ; Z flag clear if bit is set~%")
    
    (get-output-stream-string asm)))

(defun emit-6502-set-bit (value bit-position destination)
  "Emit 6502 code to set bit at BIT-POSITION to 1."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Set bit ~d~%" bit-position)
    (format asm "    LDA ~a~%" (operand-to-6502 value))
    (format asm "    ORA #$~2,'0x   ; Set bit ~d~%"
            (ash 1 bit-position) bit-position)
    (format asm "    STA ~a~%" (operand-to-6502 destination))
    
    (get-output-stream-string asm)))

(defun emit-6502-clear-bit (value bit-position destination)
  "Emit 6502 code to clear bit at BIT-POSITION to 0."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Clear bit ~d~%" bit-position)
    (format asm "    LDA ~a~%" (operand-to-6502 value))
    (format asm "    AND #$~2,'0x   ; Clear bit ~d (AND with 0 for this bit)~%"
            (logxor #xFF (ash 1 bit-position)) bit-position)
    (format asm "    STA ~a~%" (operand-to-6502 destination))
    
    (get-output-stream-string asm)))

(defun emit-6502-toggle-bit (value bit-position destination)
  "Emit 6502 code to toggle (flip) bit at BIT-POSITION."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Toggle bit ~d~%" bit-position)
    (format asm "    LDA ~a~%" (operand-to-6502 value))
    (format asm "    EOR #$~2,'0x   ; XOR with 1 at bit ~d (toggle)~%"
            (ash 1 bit-position) bit-position)
    (format asm "    STA ~a~%" (operand-to-6502 destination))
    
    (get-output-stream-string asm)))

;;; ============================================================================
;;; MULTI-BYTE BIT OPERATIONS
;;; ============================================================================

(defun emit-6502-multibyte-shift-left (value shift-count destination width)
  "Emit 6502 code for multi-byte left shift.
   Shifts entire WIDTH-byte value left by SHIFT-COUNT bits.
   Used for fixed-point arithmetic scaling."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Multi-byte shift left (~d bytes, ~d bits)~%" width shift-count)
    (format asm "    CLC~%")
    
    ;; Shift each byte, propagating carries
    (loop for byte-idx from (- width 1) downto 0 do
          (format asm "    LDA ~a~%" (format nil "~a+~d" (operand-to-6502 value) byte-idx))
          (loop for i from 0 below shift-count do
                (format asm "    ASL~%"))
          (format asm "    STA ~a~%" (format nil "~a+~d" (operand-to-6502 destination) byte-idx)))
    
    (get-output-stream-string asm)))

(defun emit-6502-multibyte-shift-right (value shift-count destination width &optional signed)
  "Emit 6502 code for multi-byte right shift.
   Shifts entire WIDTH-byte value right by SHIFT-COUNT bits.
   SIGNED: if true, uses arithmetic shift (preserves sign bit)."
  
  (let ((asm (make-string-output-stream)))
    (format asm "    ; Multi-byte shift right (~d bytes, ~d bits, ~a)~%"
            width shift-count (if signed "signed" "unsigned"))
    
    ;; Shift each byte from MSB to LSB, preserving borrows
    (loop for byte-idx from 0 below width do
          (format asm "    LDA ~a~%" (format nil "~a+~d" (operand-to-6502 value) byte-idx))
          (loop for i from 0 below shift-count do
                (if (and signed (= byte-idx 0))
                    (format asm "    ROR~%")  ; Arithmetic: rotate through carry (preserves sign)
                    (format asm "    LSR~%")))  ; Logical: shift right
          (format asm "    STA ~a~%" (format nil "~a+~d" (operand-to-6502 destination) byte-idx)))
    
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

;;; End of backend-6502-bit-shifts.lisp
