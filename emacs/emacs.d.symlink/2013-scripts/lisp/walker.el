(require 'noaccess)
(require 'vars)
(require 'defs)
(require 'packs-el-get)
;; (require 'packs-melpa)

;;; GENERATE LOAD PATH (add your 'SITE-LISP)
;;
;; Notice:
;;
;; touch a file in a SITE-LISP directory to control visibility:
;; - .nosearch => not in `load-path' / no `autoloads' generated (eg. resources)
;; - .noauto   =>     in `load-path' / no `autoloads' generated (eg. `ecb' and `nxhtml' frameworks)
;; - .cedet    =>     in `load-path' / use `cedet-autoloads' (for `defclass')
;;
(mars/add-to-load-path mars/local-root-dir mars/site-lisp-path)
;; FIXME: use subdirs.el in those two directories

(provide 'walker)
