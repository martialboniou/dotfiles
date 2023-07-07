;;; Viper Mode setup

;; (eval-when-compile (require 'viper))

(setq-default viper-mode t
              viper-inhibit-startup-message 't
              viper-ex-style-editing nil
              viper-expert-level '5
              viper-syntax-preference 'extended
              viper-always t)
;; IMPORTANT: during NO-WINDOW-SYSTEM session:
;;            1/ no `viper' in minibuffer; otherwise arrows = OD,OC.. => `ido' is broken
;;            2/ use C-\ as <meta>-key
(unless window-system
  (setq viper-vi-style-in-minibuffer nil))
;; eshell is not a friend of Viper
(add-hook 'eshell-mode-hook
          (lambda ()
            (when viper-mode
              (setq viper-auto-indent nil))))
