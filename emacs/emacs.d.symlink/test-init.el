(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(savehist-mode t)
(recentf-mode t)
(save-place-mode t)
(global-auto-revert-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(require 'autorevert)
(setq global-auto-revert-non-file-buffer t
      ;; backup-directory-alist
      ;; `((".*" . ,temporary-file-directory))
      ;; auto-save-file-name-transforms
      ;; `((".*" . ,temporary-file-directory))
      )
(load-theme 'modus-vivendi t)

(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e) (ignore e)))

(defun set-transparency ()
       (interactive)
       (set-face-background 'default "unspecified-bg" (selected-frame))
       (set-face-background 'line-number "unspecified-bg" (selected-frame)))
(add-hook 'emacs-startup-hook 'set-transparency)

(require 'display-line-numbers)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(column-number-mode)

(package-install 'company)

(package-install 'slime) ; that one for coalton
(package-install 'slime-company)

(slime-setup '(slime-fancy slime-company slime-coalton))

(setq lisp-lambda-list-keyword-parameter-alignment t
      lisp-lambda-list-keyword-alignment t
      lisp-align-keywords-in-calls t)

(put 'make-instance 'common-lisp-indent-function 1)

(setq slime-lisp-implementations
      '((sbcl ("sbcl") :coding-system utf-8-unix))) ; ***

(add-to-list 'auto-mode-alist '("\\.ct$" . lisp-mode))
