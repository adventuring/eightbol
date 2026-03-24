;; tests/service-bank-lut-tests.lisp — build-service-bank-lut-from-banks regressions
;;
;; Verifies Enums.s bank symbols map to BANK = lines and .Dispatch Service… entries.

(in-package :eightbol/test)

(fiveam:def-suite :service-bank-lut
  :description "Service→bank LUT built from Source/Code/{machine}/Banks scan")
(in-suite :service-bank-lut)

(defun %service-bank-lut-temp-root ()
  "Return new temporary directory path for LUT fixture tree."
  (uiop:ensure-directory-pathname
   (merge-pathnames
    (make-pathname :directory `(:relative "eightbol-service-lut-"
                                ,(format nil "~a" (get-internal-real-time))))
    (uiop:temporary-directory))))

(defun %write-service-bank-fixture (&key (machine "7800") (bank-num 5))
  "Write minimal Source/Code/{MACHINE}/Common/Enums.s and Banks/BankNN/*.s; return ROOT.
BANK-NUM must match subdir BankNN (hex width 2)."
  (let* ((root (%service-bank-lut-temp-root))
         (common (merge-pathnames
                  (make-pathname :directory `(:relative "Source" "Code" ,machine "Common"))
                  root))
         (banks (merge-pathnames
                 (make-pathname :directory `(:relative "Source" "Code" ,machine "Banks"))
                 root))
         (bank-sub (merge-pathnames
                    (make-pathname :directory
                                   `(:relative ,(format nil "Bank~2,'0x" bank-num)))
                    banks))
         (enums (merge-pathnames "Enums.s" common))
         (stub (merge-pathnames "StubBank.s" bank-sub)))
    (ensure-directories-exist enums)
    (ensure-directories-exist stub)
    (with-open-file (out enums :direction :output :if-exists :supersede)
      (format out ";;; test fixture~%BankFixture = $~2,'0x~%" bank-num))
    (with-open-file (out stub :direction :output :if-exists :supersede)
      (format out ";;; stub~%")
      (format out "BANK = $~2,'0x~%" bank-num)
      (format out ".Dispatch ServiceFixtureTest, DoSomething~%"))
    root))

(test service-bank-lut/build-maps-dispatch-to-enum-symbol
  "build-service-bank-lut-from-banks records Service… → Bank symbol from Enums.s."
  (let* ((root (%write-service-bank-fixture :bank-num #x0a))
         (machine "7800"))
    (eightbol::build-service-bank-lut-from-banks root machine)
    (is (string= "BankFixture"
                 (gethash "ServiceFixtureTest" eightbol::*service-bank-lut*)))))

(test service-bank-lut/load-copybook-merges-lut
  "load-copybook-tables merges *service-bank-lut* when machine inferred from paths."
  (let* ((root (%write-service-bank-fixture :bank-num #x03))
         (copy-dir (merge-pathnames
                    (make-pathname :directory `(:relative "Source" "Generated" "7800" "Classes"))
                    root)))
    (ensure-directories-exist copy-dir)
    (eightbol::build-service-bank-lut-from-banks root "7800")
    (let ((eightbol::*eightbol-root-directory* root)
          (eightbol::*copybook-paths* (list copy-dir)))
      (multiple-value-bind (slot-table type-table const-table service-bank-table
                            usage-table sign-table pic-size-table pic-width-table pic-frac-bits-table
                            pic-nybble-semantics-table)
          (eightbol::load-copybook-tables "Nonesuch" :root-dir root)
        (declare (ignore slot-table type-table const-table usage-table sign-table
                         pic-size-table pic-width-table pic-frac-bits-table pic-nybble-semantics-table))
        (is (string= "BankFixture"
                     (gethash "ServiceFixtureTest" service-bank-table)))))))
