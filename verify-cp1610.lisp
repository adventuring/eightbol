#!/usr/bin/env sbcl --script
(require :asdf)
(push #P"/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/" asdf:*central-registry*)

(format t "~&Testing CP1610 backend enhancements...~%")

;; Test that the system loads
(asdf:load-system :eightbol)
(format t "~&��✓ EightBol system loaded successfully~%")

;; Test that we can compile a simple program with call-acc, break, continue, and perform
(let ((out (make-string-output-stream)))
  (compile-to-assembly 
   '((:program
     (:class-id "TestClass")
     (:methods
      (:method
       (:method-id "Main")
       (:statements
        (:call-acc :target "HelloWorld")
        (:break)
        (:continue)
        (:perform :times 3 :body ((:move :from 42 :to "Counter"))))
       (:goback))))
   :cp1610 out))
  (let ((asm (get-output-stream-string out)))
    (format t "~&��✓ CP1610 compilation successful~%")
    (format t "~&Generated CP1610 assembly:~%~a~%" asm)
    ;; Check that our new features are present
    (when (search "jsr HelloWorld" asm)
      (format t "~&��✓ call-acc statement compiled correctly~%"))
    (when (search "jmp" asm)
      (format t "~&��✓ break/compile statements present~%"))
    (when (search "perfloop" asm)
      (format t "~&��✓ PERFORM loop labels generated~%"))))
