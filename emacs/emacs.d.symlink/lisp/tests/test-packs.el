;; -*- auto-byte-compile: nil -*-
(require 'run-tests)
(require 'vars)
(require 'defs)
(require 'packs)

;;; TESTS
;;
(ert-deftest mars/fetch-exec-in-command ()
  (should
   (equal (mars/fetch-exec-in-command "toto fait du velo; cat | split;; truc ; ; ./should-not") 
          '("toto" "cat" "split" "truc"))))

;;; TEST RUNNER
;;
; (ert-run-tests-batch)

(provide 'test-defs)
