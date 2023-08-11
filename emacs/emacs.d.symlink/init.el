;;; Simple Emacs for Common Lisp
;;  Emacs version = 29.1
(setq inferior-lisp-program "sbcl") ; used with sbcl 2.3.7 on macOS 13.4

;; Performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Core setup
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq-default tab-width 4
              indent-tabs-mode nil)
(add-hook 'before-save-hook 'whitespace-cleanup)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-buffer-choice (expand-file-name "init.el" user-emacs-directory))
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq global-auto-revert-non-file-buffers t
      backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(load-theme 'modus-vivendi t)

;; Mouse
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; GUI (rarely used b/c my emacsclient is in a terminal)
(setq use-dialog-box nil)
(set-frame-parameter (selected-frame) 'alpha '(80 . 80))
(add-to-list 'default-frame-alist '(alpha . (80 . 80)))

;; Terminal (AKA -nw option)
(defun set-transparency ()
  (interactive)
  (set-face-background 'default "unspecified-bg" (selected-frame))
  (set-face-background 'line-number "unspecified-bg" (selected-frame)))
(add-hook 'emacs-startup-hook 'set-transparency)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(column-number-mode)

;;; REPOSITORY
(require 'cl-lib)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
    use-package-expand-minimally t))

;;; PACKAGES
;; Evil
(use-package evil
  :ensure t
  :config
  (evil-mode)
  (evil-set-undo-system 'undo-redo)
  (cl-loop for (mode . state) in '((inferior-emacs-lisp-mode      . emacs)
                                   (sly-db-mode                   . emacs)
                                   (sly-inspector-mode            . emacs)
                                   (sly-trace-dialog-mode         . emacs)
                                   (sly-stickers--replay-mode     . emacs)
                                   (sly-xref-mode                 . emacs)
                                   (sly-connection-list-mode      . emacs)
                                   (sly-thread-control-mode       . emacs)
                                   (bs-mode                       . emacs)
                                   (dired-mode                    . emacs)
                                   (navi-mode                     . emacs)
                                   (archive-mode                  . emacs)
                                   (comint-mode                   . emacs)
                                   (ebib-entry-mode               . emacs)
                                   (dirtree-mode                  . emacs)
                                   (image-mode                    . emacs)
                                   (ebib-log-mode                 . emacs)
                                   (gtags-select-mode             . emacs)
                                   (shell-mode                    . emacs)
                                   (term-mode                     . emacs)
                                   (bc-menu-mode                  . emacs)
                                   (grep-mode                     . emacs)
                                   (help-mode                     . emacs)
                                   (eww-mode                      . emacs)
                                   (google-contacts-mode          . emacs)
                                   (magit-branch-manager-mode-map . emacs)
                                   (magit-popup-mode              . emacs)
                                   (magit-revision-mode           . emacs)
                                   (magit-mode                    . emacs)
                                   (magit-refs-mode               . emacs)
                                   (magit-popup-sequence-mode     . emacs)
                                   (magit-repolist-mode           . emacs)
                                   (semantic-symref-results-mode  . emacs)
                                   (fundamental-mode              . emacs)
                                   (cider-stacktrace-mode         . emacs)
                                   (diff-mode                     . emacs)
                                   (git-rebase-mode               . emacs)
                                   (git-rebase-mode               . emacs)
                                   (elfeed-show-mode              . emacs)
                                   (elfeed-search-mode            . emacs)
                                   (docker-images-mode            . emacs)
                                   (docker-containers-mode        . emacs)
                                   (cider-docview-mode            . emacs)
                                   (notmuch-tree-mode             . emacs)
                                   (xref--xref-buffer-mode        . emacs)
                                   (eww-mode                      . emacs)
                                   (rdictcc-buffer-mode           . emacs))
      do (evil-set-initial-state mode state))

  (cl-loop for buffer-name in '("\\*sly-macroexpansion\\*" "\\*sly-description\\*"
                                "\\*VC-history\\*" "COMMIT_EDITMSG" "CAPTURE-.*\\.org"
                                "\\*Warnings\\*" "\\*cider-inspect\\*")
           do (add-to-list 'evil-buffer-regexps
                           (cons buffer-name 'emacs))))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Xclip
(unless (and (eq system-type 'gnu/linux)
             (not (executable-find "xclip")))
  (use-package xclip
               :ensure t
               :init (xclip-mode 1)))

;; Better minibuffer completion
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

;; Code completion at point ; TODO: replace me by Corfu if time
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0))

;; Which Key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.300))

;; (Optional) Doom-modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

;; Save minibuffer results
(use-package savehist
  :init
  (savehist-mode))

;; Show lots of useful stuff in the minibuffer
(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

;;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Sly (+ launch it when common lisp found)
(use-package sly
  :init
  (when (executable-find inferior-lisp-program)
        (sly)))
