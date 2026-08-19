(require :asdf)
(push #P"/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/" asdf:*central-registry*)
(asdf:load-system :eightbol)
(format t "~&Testing CP1610 backend...~%")
(let ((out (make-string-output-stream)))
  (compile-to-assembly '((:program
                          (:class-id "Test")
                          (:methods
                           ((:method
                            (:method-id "Test")
                            (:statements
                             ((:call-acc :target "TestRoutine")
                              (:break)
                              (:continue)
                              (:perform :times 5 :body ((:move :from 1 :to "Var")))))))))
                         :cp1610 out))
  (format t "~&Generated assembly: ~a~%" (get-output-stream-string out)))
