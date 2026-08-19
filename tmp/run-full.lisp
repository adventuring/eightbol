(require :asdf)
(asdf:load-system :eightbol-test :force-not-found t)
(format t "~&Running full test suite...~%")
(asdf:test-system :eightbol-test)