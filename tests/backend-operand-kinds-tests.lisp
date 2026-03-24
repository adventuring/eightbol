;; tests/backend-operand-kinds-tests.lisp — Operand-shape and width coverage for all backends
;;
;; Exercises lvalues/rvalues: scalars, constants, subscripted arrays, slots OF Self,
;; OBJECT REFERENCE (pointer width), and nested arithmetic. Wide numerics are checked
;; on 6502-class backends; Z80/SM83 remain limited to 2-byte operands (errors above).

(in-package :eightbol/test)

(fiveam:def-suite :backend-operand-kinds
  :description
  "Cross-backend tests for MOVE/ADD/COMPUTE operand shapes, composite expressions, and widths.")
(in-suite :backend-operand-kinds)

(defparameter +operand-kinds-matrix-cpus+
  '(:6502 :rp2a03 :65c02 :65c816 :huc6280 :z80 :cp1610 :sm83 :m68k :i286 :arm7 :f8)
  "CPUs exercised by @code{compile-method-ast-with-tables} in @code{eightbol-tests}.")

(defun ht ()
  "Fresh hash table for copybook-style keys (@code{:test equalp})."
  (make-hash-table :test 'equalp))

(defun count-substring (needle haystack)
  "Count non-overlapping occurrences of NEEDLE in HAYSTACK."
  (loop with n = 0
        for start = 0 then (+ pos (length needle))
        for pos = (search needle haystack :start2 start)
        while pos
        do (incf n)
        finally (return n)))

(defun compile-ok-method-ast (method class-id cpu
                              &key (slot-table (ht)) (const-table (ht))
                                   (type-table (ht)) (usage-table (ht))
                                   (sign-table (ht)) (pic-size-table (ht))
                                   (pic-width-table (ht)) (pic-frac-bits-table (ht))
                                   (pic-nybble-semantics-table (ht))
                                   (service-bank-table (ht)))
  "Same contract as @code{compile-method-ast-with-tables} in @code{eightbol-tests}; inlined so this file compiles standalone."
  (with-output-to-string (s)
    (let ((eightbol::*output-stream* s)
          (eightbol::*class-id* class-id)
          (eightbol::*slot-table* slot-table)
          (eightbol::*type-table* type-table)
          (eightbol::*const-table* const-table)
          (eightbol::*service-bank-table* service-bank-table)
          (eightbol::*usage-table* usage-table)
          (eightbol::*sign-table* sign-table)
          (eightbol::*pic-size-table* pic-size-table)
          (eightbol::*pic-width-table* pic-width-table)
          (eightbol::*pic-frac-bits-table* pic-frac-bits-table)
          (eightbol::*pic-nybble-semantics-table* pic-nybble-semantics-table))
      (ecase cpu
        ((:6502) (eightbol::compile-6502-method method class-id :6502))
        ((:rp2a03) (eightbol::compile-rp2a03-method method class-id))
        ((:65c02) (eightbol::compile-6502-method method class-id :65c02))
        ((:65c816) (eightbol::compile-6502-method method class-id :65c816))
        ((:huc6280) (eightbol::compile-6502-method method class-id :huc6280))
        ((:z80) (eightbol::compile-z80-method s method class-id slot-table type-table const-table
                                              pic-size-table pic-width-table))
        ((:cp1610) (eightbol::compile-cp1610-method method class-id))
        ((:sm83) (eightbol::compile-sm83-method s method class-id slot-table type-table const-table
                                                pic-size-table pic-width-table))
        ((:m68k) (eightbol::compile-m68k-method s method class-id slot-table type-table const-table
                                                pic-size-table pic-width-table))
        ((:i286) (eightbol::compile-i286-method s method class-id slot-table type-table const-table
                                                 pic-size-table pic-width-table))
        ((:arm7) (eightbol::compile-arm7-method s method class-id slot-table type-table const-table
                                                 pic-size-table pic-width-table))
        ((:f8) (eightbol::compile-f8-method s method class-id slot-table type-table const-table
                                             pic-size-table pic-width-table))))))

(defun compile-one-stmt (cpu stmt &key (slot-table (ht)) (const-table (ht))
                                    (type-table (ht)) (pic-width-table (ht)))
  "Compile a single-statement method for CLASS-ID Character; return assembly string."
  (compile-ok-method-ast
   `(:method :method-id "Ok" :statements (,stmt))
   "Character" cpu
   :slot-table slot-table
   :const-table const-table
   :type-table type-table
   :pic-width-table pic-width-table))

(test operand-kinds/matrix-move-literal-to-scalar-lvalue
  "Every backend compiles MOVE literal TO scalar; assembly is non-empty with a method label."
  (dolist (cpu +operand-kinds-matrix-cpus+)
    (let ((asm (compile-one-stmt cpu '(:move :from 42 :to "Scratch"))))
      (is (plusp (length asm)) "CPU ~s should emit assembly" cpu)
      (is (search "Method" asm) "CPU ~s should emit method label" cpu))))

(test operand-kinds/matrix-move-named-constant-to-scalar
  "MOVE named constant TO scalar uses const table; all CPUs compile."
  (let ((consts (ht)))
    (setf (gethash "magic-number" consts) 99)
    (dolist (cpu +operand-kinds-matrix-cpus+)
      (let ((asm (compile-one-stmt cpu '(:move :from "Magic-Number" :to "Dest")
                                   :const-table consts)))
        (is (plusp (length asm)) "CPU ~s" cpu)))))

(test operand-kinds/matrix-move-scalar-to-slot-of-self-lvalue
  "MOVE literal TO slot OF Self; 6502 uses (Self),y; others emit their load/store pattern."
  (let ((pw (ht)))
    (setf (gethash "HP" pw) 2)
    (dolist (cpu +operand-kinds-matrix-cpus+)
      (let ((asm (compile-one-stmt cpu '(:move :from 3 :to (:of "HP" :self))
                                   :pic-width-table pw)))
        (is (plusp (length asm)) "CPU ~s" cpu)
        (case cpu
          ((:6502 :rp2a03 :65c02 :65c816 :huc6280)
           (is (search "(Self)" asm) "6502-class CPU ~s should reference Self indirect" cpu))
          ((:z80) (is (search "Self" asm)))
          ((:cp1610) (is (search "MVO" asm)))
          ((:sm83) (is (search "Self" asm)))
          (t (is (search "Self" asm))))))))

(test operand-kinds/matrix-move-from-slot-of-self-rvalue
  "MOVE HP OF Self TO Dest loads from instance slot."
  (let ((pw (ht)))
    (setf (gethash "HP" pw) 2)
    (setf (gethash "Dest" pw) 2)
    (dolist (cpu +operand-kinds-matrix-cpus+)
      (let ((asm (compile-one-stmt cpu '(:move :from (:of "HP" :self) :to "Dest")
                                   :pic-width-table pw)))
        (is (plusp (length asm)) "CPU ~s" cpu)))))

(test operand-kinds/matrix-move-subscript-rvalue-to-scalar
  "MOVE Buf(index) TO Out — indexed load, scalar store."
  (let ((pw (ht)))
    (setf (gethash "Buf" pw) 1)
    (setf (gethash "Out" pw) 1)
    (dolist (cpu +operand-kinds-matrix-cpus+)
      (let ((asm (compile-one-stmt cpu '(:move :from (:subscript "Buf" 2) :to "Out")
                                   :pic-width-table pw)))
        (is (plusp (length asm)) "CPU ~s" cpu)
        (case cpu
          ((:6502 :rp2a03 :65c02 :65c816 :huc6280)
           (is (search "Buf" asm) "CPU ~s should reference array base" cpu)))))))

(test operand-kinds/matrix-move-scalar-to-subscript-lvalue
  "MOVE Val TO Buf(index) — indexed store."
  (let ((pw (ht)))
    (setf (gethash "Buf" pw) 1)
    (setf (gethash "Val" pw) 1)
    (dolist (cpu +operand-kinds-matrix-cpus+)
      (let ((asm (compile-one-stmt cpu '(:move :from "Val" :to (:subscript "Buf" 1))
                                   :pic-width-table pw)))
        (is (plusp (length asm)) "CPU ~s" cpu)))))

(test operand-kinds/matrix-add-three-scalars-giving
  "ADD A TO B GIVING C — three scalar names, width 1."
  (let ((pw (ht)))
    (setf (gethash "A" pw) 1)
    (setf (gethash "B" pw) 1)
    (setf (gethash "C" pw) 1)
    (dolist (cpu +operand-kinds-matrix-cpus+)
      (let ((asm (compile-one-stmt cpu '(:add :from "A" :to "B" :giving "C")
                                   :pic-width-table pw)))
        (is (plusp (length asm)) "CPU ~s" cpu)))))

(test operand-kinds/matrix-compute-nested-add-expr
  "COMPUTE X = A + (B + C) — nested :add-expr; max leaf width drives operand-width."
  (let ((pw (ht)))
    (setf (gethash "A" pw) 1)
    (setf (gethash "B" pw) 1)
    (setf (gethash "C" pw) 1)
    (setf (gethash "X" pw) 1)
    (dolist (cpu +operand-kinds-matrix-cpus+)
      (let ((asm (compile-one-stmt
                  cpu
                  '(:compute :target "X"
                    :expression (:add-expr "A" (:add-expr "B" "C")))
                  :pic-width-table pw)))
        (is (plusp (length asm)) "CPU ~s" cpu)))))

(test operand-kinds/matrix-compute-mixed-expr-with-multiply
  "COMPUTE X = A + (B * 2) — multiply-expr inside add-expr."
  (let ((pw (ht)))
    (setf (gethash "A" pw) 1)
    (setf (gethash "B" pw) 1)
    (setf (gethash "X" pw) 1)
    (dolist (cpu +operand-kinds-matrix-cpus+)
      (let ((asm (compile-one-stmt
                  cpu
                  '(:compute :target "X"
                    :expression (:add-expr "A" (:multiply-expr "B" 2)))
                  :pic-width-table pw)))
        (is (plusp (length asm)) "CPU ~s" cpu)))))

(test operand-kinds/operand-width-object-reference-without-pic-row
  "OBJECT REFERENCE in type-table with no PIC width uses +object-reference-storage-width+ (2)."
  (let ((pw (ht))
        (tt (ht)))
    (setf (gethash "EnemyRef" tt) "Entity")
    (is (= eightbol::+object-reference-storage-width+
           (let ((eightbol::*pic-width-table* pw)
                 (eightbol::*type-table* tt))
             (eightbol::operand-width "EnemyRef"))))))

(test operand-kinds/expression-operand-width-nested-max
  "expression-operand-width takes max width across nested :add-expr leaves."
  (let ((pw (ht)))
    (setf (gethash "A" pw) 1)
    (setf (gethash "B" pw) 1)
    (setf (gethash "Wide" pw) 4)
    (is (= 4
           (let ((eightbol::*pic-width-table* pw))
             (eightbol::expression-operand-width
              '(:add-expr "A" (:add-expr "B" "Wide"))
              pw))))))

(test operand-kinds/6502-family-move-four-byte-operands
  "6502 backends emit multi-byte sequence for PIC width 4 MOVE between scalars."
  (let ((pw (ht)))
    (setf (gethash "LongSrc" pw) 4)
    (setf (gethash "LongDst" pw) 4)
    (dolist (cpu '(:6502 :rp2a03 :65c02 :65c816 :huc6280))
      (let* ((asm (compile-one-stmt cpu '(:move :from "LongSrc" :to "LongDst")
                                    :pic-width-table pw))
             (asm-lower (string-downcase asm)))
        (is (plusp (length asm)) "CPU ~s" cpu)
        ;; At least four byte touches; labels use CharacterLongsrc / CharacterLongdst (case from name mapping).
        (is (plusp (count-substring "longsrc" asm-lower)) "CPU ~s" cpu)
        (is (plusp (count-substring "longdst" asm-lower)) "CPU ~s" cpu)))))

(test operand-kinds/z80-and-sm83-reject-three-byte-add
  "Z80 and SM83 signal error on ADD with 3-byte operands (max 2)."
  (let ((pw (ht)))
    (setf (gethash "A" pw) 3)
    (setf (gethash "B" pw) 3)
    (setf (gethash "C" pw) 3)
    (signals error (compile-one-stmt :z80 '(:add :from "A" :to "B" :giving "C")
                                     :pic-width-table pw))
    (signals error (compile-one-stmt :sm83 '(:add :from "A" :to "B" :giving "C")
                                      :pic-width-table pw))))

(test operand-kinds/6502-set-address-of-slot-of-self-pointer-target
  "SET pointer TO ADDRESS OF HP OF Self stores 16-bit address (pointer lvalue, slot rvalue)."
  (let ((pw (ht))
        (tt (ht)))
    (setf (gethash "Ptr" tt) "Entity")
    (setf (gethash "Ptr" pw) 2)
    (setf (gethash "HP" pw) 2)
    (let ((asm (compile-one-stmt :6502
                                 '(:set :target "Ptr"
                                   :address-of (:of "HP" :self))
                                 :type-table tt :pic-width-table pw)))
      (is (search "#<Self" asm))
      (is (search "CharacterHP" asm)))))

(test operand-kinds/matrix-move-object-reference-to-object-reference
  "MOVE between OBJECT REFERENCE scalars compiles on every backend (2-byte pointer path)."
  (let ((tt (ht)))
    (setf (gethash "SrcRef" tt) "Entity")
    (setf (gethash "DstRef" tt) "Actor")
    (dolist (cpu +operand-kinds-matrix-cpus+)
      (let ((asm (compile-one-stmt cpu '(:move :from "SrcRef" :to "DstRef")
                                   :type-table tt)))
        (is (plusp (length asm)) "CPU ~s" cpu)))))

(test operand-kinds/cp1610-subscript-global-with-slot-table
  "cp1610 MOVE to global subscript OF Self uses bare global label from slot table (regression)."
  (let ((slots (ht))
        (consts (ht)))
    (setf (gethash "decalanimationontick" consts) 0)
    (setf (gethash "Decal-Animation-State" slots) "Phantasia-Globals")
    (setf (gethash "Decal" slots) "Character")
    (let ((asm (compile-one-stmt
                :cp1610
                '(:move :from "DecalAnimationOnTick"
                  :to (:subscript "Decal-Animation-State" (:of "Decal" :self)))
                :slot-table slots
                :const-table consts)))
      (is (search "DecalAnimationState" asm))
      (is (null (search "CharacterDecalAnimationState" asm))))))
