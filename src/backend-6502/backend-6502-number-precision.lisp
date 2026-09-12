;;; src/backend-6502/backend-6502-number-precision.lisp
;;; Numeric type precision handling for 6502 family
;;; Copyright © 2026 EIGHTBOL Development. All rights reserved.

(in-package :eightbol)

;;; ============================================================================
;;; NUMERIC TYPE DEFINITIONS AND CONSTANTS
;;; ============================================================================

;; Numeric type enumeration
(deftype numeric-type ()
  '(member :binary-unsigned :binary-signed :decimal-unsigned :decimal-signed :display))

;; Precision in bits for each numeric type (per byte)
(defvar *numeric-type-precision* '((:binary-unsigned . 8)
                                   (:binary-signed . 8)
                                   (:decimal-unsigned . 4) ; 2 BCD digits per byte
                                   (:decimal-signed . 4)   ; 2 BCD digits + sign nybble
                                   (:display . 8))         ; 1 ASCII digit per byte
  "Bits of precision per byte for each numeric type.")

;; Sign representations
(defvar *numeric-type-sign-nybble* '((:binary-signed . #xc0)        ; Two's complement high bit
                                     (:decimal-signed . #x0d)       ; BCD sign nybble (0xD = negative)
                                     (:display . #x0d))             ; DISPLAY sign in units digit
  "Sign representation for signed numeric types.")

;;; ============================================================================
;;; NUMERIC TYPE CLASSIFICATION
;;; ============================================================================

(defun numeric-type-binary-p (type)
  "Return true if TYPE is a BINARY numeric type (signed or unsigned)."
  (member type '(:binary-unsigned :binary-signed)))

(defun numeric-type-decimal-p (type)
  "Return true if TYPE is a DECIMAL (BCD) numeric type (signed or unsigned)."
  (member type '(:decimal-unsigned :decimal-signed)))

(defun numeric-type-display-p (type)
  "Return true if TYPE is a DISPLAY (zoned decimal) numeric type."
  (eq type :display))

(defun numeric-type-signed-p (type)
  "Return true if TYPE is a signed numeric type."
  (member type '(:binary-signed :decimal-signed :display)))

(defun numeric-type-unsigned-p (type)
  "Return true if TYPE is an unsigned numeric type."
  (member type '(:binary-unsigned :decimal-unsigned)))

;;; ============================================================================
;;; NUMERIC TYPE DETERMINATION
;;; ============================================================================

(defun determine-numeric-type (pic-string &optional (usage :display))
  "Determine the numeric type from PIC string and USAGE clause.
   Returns one of :binary-unsigned, :binary-signed, :decimal-unsigned,
   :decimal-signed, or :display."
  (let ((has-sign-p (and (stringp pic-string) (string-contains-p "S" pic-string))))
    (case usage
      (:comp          (if has-sign-p :binary-signed :binary-unsigned))
      (:binary        (if has-sign-p :binary-signed :binary-unsigned))
      (:comp-3        (if has-sign-p :decimal-signed :decimal-unsigned))
      (:packed-decimal (if has-sign-p :decimal-signed :decimal-unsigned))
      (:display       (if has-sign-p :display :display))
      (t              (if has-sign-p :display :display)))))

(defun string-contains-p (substring string)
  "Return true if STRING contains SUBSTRING."
  (if (and (stringp string) (stringp substring))
      (> (length (search substring string)) 0)
      nil))

;;; ============================================================================
;;; NUMERIC TYPE CONVERSION
;;; ============================================================================

(defun convert-numeric-value (source-type source-value dest-type dest-width)
  "Convert a numeric value from SOURCE-TYPE to DEST-TYPE.
   Returns optimized assembly sequence and any register constraints.
   - source-type: numeric type of source
   - source-value: AST node or value
   - dest-type: numeric type of destination
   - dest-width: width of destination in bytes"
  (cond
    ;; BINARY to BINARY (same representation, different sign/width)
    ((and (numeric-type-binary-p source-type) (numeric-type-binary-p dest-type))
     (if (= (numeric-type-get-width source-type) dest-width)
         `(:move-direct ,source-value)  ; No conversion needed
         (if (< (numeric-type-get-width source-type) dest-width)
             `(:sign-extend ,source-value)  ; Extend if dest wider
             `(:truncate ,source-value)))) ; Truncate if dest narrower
    
    ;; BINARY to DECIMAL
    ((and (numeric-type-binary-p source-type) (numeric-type-decimal-p dest-type))
     `(:binary-to-decimal ,source-value
                          :signed ,(numeric-type-signed-p source-type)
                          :dest-width ,dest-width))
    
    ;; DECIMAL to BINARY
    ((and (numeric-type-decimal-p source-type) (numeric-type-binary-p dest-type))
     `(:decimal-to-binary ,source-value
                          :signed ,(numeric-type-signed-p source-type)
                          :dest-width ,dest-width))
    
    ;; DECIMAL to DECIMAL (same representation, different sign)
    ((and (numeric-type-decimal-p source-type) (numeric-type-decimal-p dest-type))
     `(:decimal-sign-adjust ,source-value
                            :from-signed ,(numeric-type-signed-p source-type)
                            :to-signed ,(numeric-type-signed-p dest-type)))
    
    ;; DISPLAY conversions
    ((numeric-type-display-p dest-type)
     `(:convert-to-display ,source-value :from-type ,source-type))
    
    ((numeric-type-display-p source-type)
     `(:convert-from-display ,source-value :to-type ,dest-type))
    
    ;; Default: use intermediate conversion through BINARY
    (t `(:intermediate-conversion ,source-value
                                  :from-type ,source-type
                                  :to-type ,dest-type
                                  :dest-width ,dest-width))))

;;; ============================================================================
;;; NUMERIC TYPE RANGE CALCULATION
;;; ============================================================================

(defun numeric-type-min-value (type width)
  "Calculate minimum representable value for TYPE with given WIDTH (in bytes)."
  (let ((bits (* width 8)))
    (cond
      ((numeric-type-unsigned-p type) 0)
      ((numeric-type-binary-p type)
       (- (expt 2 (- bits 1))))  ; -2^(n-1) for two's complement
      ((numeric-type-decimal-p type)
       (- (expt 10 (* width 2))))  ; -10^(2*width) for BCD
      (t 0))))

(defun numeric-type-max-value (type width)
  "Calculate maximum representable value for TYPE with given WIDTH (in bytes)."
  (let ((bits (* width 8)))
    (cond
      ((numeric-type-unsigned-p type)
       (- (expt 2 bits) 1))  ; 2^n - 1
      ((numeric-type-binary-p type)
       (- (expt 2 (- bits 1)) 1))  ; 2^(n-1) - 1 for signed
      ((numeric-type-decimal-p type)
       (- (expt 10 (* width 2)) 1))  ; 10^(2*width) - 1 for BCD
      (t (- (expt 2 bits) 1)))))

;;; ============================================================================
;;; NUMERIC WIDTH CALCULATION
;;; ============================================================================

(defun numeric-type-get-width (type)
  "Get the width in bytes for a given numeric type.
   Returns the minimum width; actual width depends on field size."
  (cond
    ((numeric-type-binary-p type) 1)    ; 8-bit default; can extend to 16/32
    ((numeric-type-decimal-p type) 1)   ; 2 BCD digits per byte
    ((numeric-type-display-p type) 1))) ; 1 ASCII digit per byte

;;; ============================================================================
;;; OVERFLOW AND UNDERFLOW HANDLING
;;; ============================================================================

(defun numeric-type-check-overflow-p (value type width)
  "Return true if VALUE would overflow in TYPE with given WIDTH (in bytes)."
  (let ((max (numeric-type-max-value type width))
        (min (numeric-type-min-value type width)))
    (or (> value max) (< value min))))

(defun numeric-type-handle-overflow (value type width)
  "Handle overflow by wrapping or truncating as appropriate for TYPE.
   Returns the wrapped value and any generated assembly."
  (let ((max (numeric-type-max-value type width))
        (min (numeric-type-min-value type width))
        (range (- (numeric-type-max-value type width)
                  (numeric-type-min-value type width))))
    (cond
      ;; BINARY types: wrap using modulo arithmetic
      ((numeric-type-binary-p type)
       (let ((bits (* width 8)))
         (values (logand value (- (expt 2 bits) 1))
                 `(:modulo ,(expt 2 bits)))))
      
      ;; DECIMAL types: wrap in decimal space
      ((numeric-type-decimal-p type)
       (let ((dec-max (expt 10 (* width 2))))
         (values (mod value dec-max)
                 `(:decimal-wrap ,dec-max))))
      
      ;; DISPLAY: truncate from left (most significant digits)
      ((numeric-type-display-p type)
       (values (mod value (expt 10 width))
               `(:truncate-left ,width))))))

;;; ============================================================================
;;; SIGN HANDLING
;;; ============================================================================

(defun numeric-type-extract-sign (value type)
  "Extract the sign of VALUE in TYPE representation.
   Returns :positive, :negative, or :zero."
  (cond
    ((= value 0) :zero)
    ((numeric-type-unsigned-p type)
     (if (>= value 0) :positive :negative))
    ((numeric-type-binary-p type)
     (if (>= value 0) :positive :negative))
    ((numeric-type-decimal-p type)
     ;; BCD sign is in high nybble of last byte
     (if (logtest value #xf000) :negative :positive))
    (t (if (>= value 0) :positive :negative))))

(defun numeric-type-apply-sign (magnitude type width &optional (sign :positive))
  "Apply SIGN to MAGNITUDE value in TYPE representation.
   Returns the signed value in proper representation."
  (cond
    ((numeric-type-unsigned-p type)
     magnitude)  ; No sign in unsigned
    
    ((numeric-type-binary-p type)
     ;; Two's complement: negate if negative
     (if (eq sign :negative)
         (logand (- (+ magnitude 1)) (- (expt 2 (* width 8)) 1))
         magnitude))
    
    ((numeric-type-decimal-p type)
     ;; BCD: set sign nybble in last byte
     (if (eq sign :negative)
         (logior magnitude #x0d)
         (logand magnitude #xf0)))
    
    ((numeric-type-display-p type)
     ;; DISPLAY: sign in units digit
     (let ((units-digit (mod magnitude 10)))
       (+ (- magnitude units-digit)
          (if (eq sign :negative) (+ units-digit 10) units-digit))))))

;;; ============================================================================
;;; PRECISION MAINTENANCE
;;; ============================================================================

(defun numeric-type-maintain-precision (value type source-type dest-type)
  "Adjust VALUE for precision differences between SOURCE-TYPE and DEST-TYPE.
   Used when MOVing between types to handle fractional parts and scale differences."
  (cond
    ;; BINARY to DECIMAL: scale up
    ((and (numeric-type-binary-p source-type)
          (numeric-type-decimal-p dest-type))
     (values value `(:scale-up-for-bcd)))
    
    ;; DECIMAL to BINARY: scale down with truncation
    ((and (numeric-type-decimal-p source-type)
          (numeric-type-binary-p dest-type))
     (values (truncate value)  ; Truncate fractional part
             `(:truncate-decimal-to-binary)))
    
    ;; DISPLAY to BINARY: convert digit string
    ((and (numeric-type-display-p source-type)
          (numeric-type-binary-p dest-type))
     (values value `(:display-to-binary)))
    
    ;; BINARY to DISPLAY: convert to digit string
    ((and (numeric-type-binary-p source-type)
          (numeric-type-display-p dest-type))
     (values value `(:binary-to-display)))
    
    ;; Default: no change
    (t (values value nil))))

;;; End of backend-6502-number-precision.lisp
