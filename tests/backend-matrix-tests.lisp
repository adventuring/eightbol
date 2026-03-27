;; tests/backend-matrix-tests.lisp — Multi-CPU parity and smoke tests for EIGHTBOL
;;
;; Uses compile-method-ast-with-tables from eightbol-tests.lisp (same bindings as copybooks).

(in-package :eightbol/test)

(fiveam:def-suite :backend-matrix
  :description "Multi-CPU parity (Z80/cp1610) and smoke tests using compile-method-ast-with-tables")
(in-suite :backend-matrix)

(test matrix/z80-move-global-constant-no-class-prefix
  "MOVE named constant to global Next-Song: ld uses NextSong label, not CharacterNextSong."
  (let ((slots (make-hash-table :test 'equalp))
        (consts (make-hash-table :test 'equalp)))
    (setf (gethash "song--heal--id" consts) 42)
    (setf (gethash "Next-Song" slots) "Phantasia-Globals")
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:move :from "Song--Heal--ID" :to "Next-Song")))
                "Character" :z80
                :slot-table slots :const-table consts)))
      (is (search "NextSong" asm))
      (is (null (search "CharacterNextSong" asm))))))

(test matrix/cp1610-move-global-constant-no-class-prefix
  "MOVE constant to global: MVO to NextSong, not CharacterNextSong."
  (let ((slots (make-hash-table :test 'equalp))
        (consts (make-hash-table :test 'equalp)))
    (setf (gethash "song--heal--id" consts) 42)
    (setf (gethash "Next-Song" slots) "Phantasia-Globals")
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:move :from "Song--Heal--ID" :to "Next-Song")))
                "Character" :cp1610
                :slot-table slots :const-table consts)))
      (is (search "MVO" asm))
      (is (search "NextSong" asm))
      (is (null (search "CharacterNextSong" asm))))))

(test matrix/cp1610-subscript-global-base-bare-label
  "Subscript store: base address uses bare global label (DecalAnimationState) from slot table."
  (let ((slots (make-hash-table :test 'equalp))
        (consts (make-hash-table :test 'equalp)))
    (setf (gethash "decalanimationontick" consts) 0)
    (setf (gethash "Decal-Animation-State" slots) "Phantasia-Globals")
    (setf (gethash "Decal" slots) "Character")
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:move :from "DecalAnimationOnTick"
                                          :to (:subscript "Decal-Animation-State"
                                                (:of "Decal" :self)))))
                "Character" :cp1610
                :slot-table slots :const-table consts)))
      (is (search "DecalAnimationState" asm))
      (is (null (search "CharacterDecalAnimationState" asm))))))

(test matrix/z80-add-width-3-signals-error
  "Z80 ADD does not emit w>2; signals a clear EIGHTBOL error."
  (let ((pic (make-hash-table :test 'equalp)))
    (setf (gethash "A" pic) 3)
    (setf (gethash "B" pic) 3)
    (setf (gethash "C" pic) 3)
    (signals error
      (compile-method-ast-with-tables
       '(:method :method-id "M"
         :statements ((:add :from "A" :to "B" :giving "C")))
       "T" :z80 :pic-width-table pic))))

(test matrix/other-cpus-smoke-move
  "SM83, m6800, m68k, i286, ARM7, F8 compile MOVE with tables and emit method label."
  (let ((slots (make-hash-table :test 'equalp))
        (consts (make-hash-table :test 'equalp)))
    (setf (gethash "Next-Song" slots) "Phantasia-Globals")
    (dolist (cpu '(:sm83 :m6800 :m68k :i286 :arm7 :f8))
      (let ((asm (compile-method-ast-with-tables
                  '(:method :method-id "Run"
                    :statements ((:move :from 0 :to "Next-Song")))
                  "Character" cpu
                  :slot-table slots :const-table consts)))
        (is (search "Method" asm) "CPU ~s should emit method label" cpu)
        (is (plusp (length asm)) "CPU ~s should emit non-empty assembly" cpu)))))

(test matrix/z80-bare-dest-name-not-class-prefixed
  "MOVE to bare Dest-X emits DestX, not MummyCourseDestX."
  (let ((slots (make-hash-table :test 'equalp)))
    (setf (gethash "Course-Waypoint-X" slots) "Course")
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:move :from (:of "Course-Waypoint-X" :self)
                                      :to "Dest-X")))
                "MummyCourse" :z80
                :slot-table slots)))
      (is (search "DestX" asm))
      (is (null (search "MummyCourseDestX" asm))))))

(test matrix/cp1610-bare-dest-name-not-class-prefixed
  "MOVE to bare Dest-X emits DestX, not MummyCourseDestX."
  (let ((slots (make-hash-table :test 'equalp)))
    (setf (gethash "Course-Waypoint-X" slots) "Course")
    (let ((asm (compile-method-ast-with-tables
                '(:method :method-id "M"
                  :statements ((:move :from (:of "Course-Waypoint-X" :self)
                                      :to "Dest-X")))
                "MummyCourse" :cp1610
                :slot-table slots)))
      (is (search "DestX" asm))
      (is (null (search "MummyCourseDestX" asm))))))
