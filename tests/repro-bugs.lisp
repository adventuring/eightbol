(in-package :eightbol/test)
(in-suite :eightbol)

(test repro-of-self-type-error
  "Reproduce (:OF \"State\" \"Self\") type error in to-identifier."
  (let ((eightbol::*class-id* "Anenemy")
        (eightbol::*current-cpu* :6502)
        (eightbol::*slot-table* (make-hash-table :test 'equalp)))
    ;; Simulate what Anenemy.cob might produce
    (let ((expr '(:of "State" "Self")))
      ;; This should NOT throw a type error
      (with-output-to-string (out)
        (eightbol::emit-6502-load-expression out expr "Anenemy")))))

(test repro-nested-of-type-error
  "Reproduce nested :OF type error in to-identifier."
  (let ((eightbol::*class-id* "Anenemy")
        (eightbol::*current-cpu* :6502)
        (eightbol::*slot-table* (make-hash-table :test 'equalp)))
    (setf (gethash (eightbol::cobol-slot-table-name-key "Timer") eightbol::*slot-table*) "Anenemy")
    (setf (gethash (eightbol::cobol-slot-table-name-key "Current-Actor") eightbol::*working-storage*)
          '(:usage :object-ref :class "Actor"))
    
    ;; Timer OF THE Anenemy Current-Actor
    (let ((expr '(:of "Timer" "Current-Actor" "Anenemy")))
      (with-output-to-string (out)
        (eightbol::emit-6502-load-expression out expr "Anenemy")))))
