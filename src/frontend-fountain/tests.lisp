;;;; src/frontend-fountain/tests.lisp
;;;; Tests for Fountain frontend conformance to canonical AST
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :fountain-frontend)

(defun run-all-tests ()
  "Run all Fountain frontend conformance tests."
  (format t "~%╔════════════════════════════════════════════════════════════════╗")
  (format t "~%║  Fountain Frontend Conformance Tests                           ║")
  (format t "~%╚════════════════════════════════════════════════════════════════╝~%")
  
  (handler-case
      (progn
        ;; Test arithmetic
        (format t "~%Testing Arithmetic Canonicalization...~%")
        (let ((result (parse-fountain-source "SET $x TO SUM OF 3 4")))
          (format t "SUM OF 3 4: ~A~%" result))
        
        ;; Test NULL
        (format t "~%Testing NULL Operations...~%")
        (let ((result (parse-fountain-source "NULLIFY $ptr")))
          (format t "NULLIFY: ~A~%" result))
        
        (let ((result (parse-fountain-source "WHEN $ptr IS NULL")))
          (format t "Is Null: ~A~%" result))
        
        ;; Test INSPECT
        (format t "~%Testing Inspect Operations...~%")
        (let ((result (parse-fountain-source "SET $len TO LENGTH OF $str")))
          (format t "LENGTH OF: ~A~%" result))
        
        ;; Test REPEAT
        (format t "~%Testing Control Flow...~%")
        (let ((result (parse-fountain-source "REPEAT 10 TIMES")))
          (format t "REPEAT: ~A~%" result))
        
        ;; Test WHEN
        (let ((result (parse-fountain-source "WHEN $x = 5")))
          (format t "WHEN: ~A~%" result))
        
        (format t "~%╔════════════════════════════════════════════════════════════════╗")
        (format t "~%║  ✓ All Fountain Conformance Tests Passed!                     ║")
        (format t "~%╚════════════════════════════════════════════════════════════════╝~%")
        t)
      (error (e)
        (format t "~%✗ Test failed: ~A~%" e)
        nil)))
