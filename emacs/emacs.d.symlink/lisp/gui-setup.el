(setq use-dialog-box nil)
(set-frame-parameter (selected-frame) 'alpha '(80 . 80))
(add-to-list 'default-frame-alist '(alpha . (80 . 80)))

;;; PACKAGES

;; Hydra
(use-package hydra
  :config
  (defhydra hydra-text-scale (:timeout 4)
            ("k" text-scale-increase "in")
            ("j" text-scale-decrease "out")
            ("f" nil "finished" :exit t)))

(provide 'gui-setup)
