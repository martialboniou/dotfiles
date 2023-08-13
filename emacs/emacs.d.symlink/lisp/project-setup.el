(unless (boundp 'project-directory)
  (setq project-directory "~/Documents/Code"))

;;; PACKAGES

;; Project manager
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p project-directory)
    (setq projectile-project-search-path '(project-directory)))
  (setq projectile-switch-project-action #'projectile-dired))

(provide 'project-setup)
