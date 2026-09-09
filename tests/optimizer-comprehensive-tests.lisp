;;;; tests/optimizer-comprehensive-tests.lisp — Comprehensive optimizer tests
;;; Tests all 5 optimizer passes

(in-package :eightbol/test)

(def-suite :optimizer-comprehensive :description "Comprehensive optimizer tests")
(in-suite :optimizer-comprehensive)

;;;; Power-of-Two Tests

(test optimizer/power-of-two-p-2
  "2 is power-of-two"
  (is (eightbol::power-of-two-p 2)))

(test optimizer/power-of-two-p-4
  "4 is power-of-two"
  (is (eightbol::power-of-two-p 4)))

(test optimizer/power-of-two-p-256
  "256 is power-of-two"
  (is (eightbol::power-of-two-p 256)))

(test optimizer/power-of-two-p-3-not
  "3 is not power-of-two"
  (is (not (eightbol::power-of-two-p 3))))

(test optimizer/log2-2
  "log2(2) = 1"
  (is (eql (eightbol::log2 2) 1)))

(test optimizer/log2-4
  "log2(4) = 2"
  (is (eql (eightbol::log2 4) 2)))

(test optimizer/log2-256
  "log2(256) = 8"
  (is (eql (eightbol::log2 256) 8)))

