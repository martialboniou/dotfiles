(require 'walker)
(require 'packs)
;; automagic
;;; TEMPORARY REMOVED (NO VENDOR)
;; (generate-loaddefs)

;; (setq renew-autoloads-at-startup nil)   ; reset to prevent slow reloading

;; (defun update-autoloads-on-kill ()
;;   "Update autoloads on kill iff emacs boots correctly."
;;   (when (require 'kernel)
;;     (update-autoloads-in-package-area)))
;; (add-hook 'kill-emacs-hook #'update-autoloads-on-kill)

;; handmade ;; TODO: 2013/05/23 PURGE THIS!!!
(mars/autoload '(;("iswitchb"                  iswitchb-minibuffer-setup)
                 ("update-autoloads-in-package-area" update-auto-loads) ;; shut up warning TODO: remove quickly
;;                 ("anything-show-completion"  use-anything-show-completion)
 ;;                ("autopair"                  autopair-mode)
                 ;("header2"                   auto-make-header auto-update-file-header)
                 ;("hippie-exp"                hippie-expand he-init-string 
                 ;                     he-substitute-string)
                 ("inf-shen"                  shen-mode)
                 ("pymacs"                    pymacs-apply pymacs-call pymacs-eval 
                                              pymacs-exec pymacs-load)
   ;;              ("yaml-mode"                 yaml-mode)
                 ;; ("emms-source-file"          emms-dired emms-add-directory emms-add-file)
                 ("wl-mailto"                 wl-mailto-compose-message-from-mailto-url)))


(provide 'adapter)
