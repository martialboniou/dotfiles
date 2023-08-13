;;; Simple Emacs for Common Lisp

;;  Emacs version = 29.1
(defconst hondana/want-git t)
(defconst hondana/want-project-assist nil)
(defconst hondana/want-org-mode t)

(defvar inferior-lisp-program "sbcl") ; used with sbcl 2.3.7 on macOS 13,4
(defvar project-directory "~/Documents/Code")

(when (equal system-type 'darwin) ; `brew install coreutils` b/c gnuls != BSD's
  (let ((gnuls "/opt/homebrew/opt/coreutils/libexec/gnubin/ls"))
    (when (file-executable-p gnuls)
      (setq insert-directory-program gnuls))))

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
(global-auto-revert-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(eval-when-compile
  (require 'autorevert))
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
  (defun track-mouse (e) (ignore e)))

;; Terminal (AKA -nw option; normal way to start Emacs here)
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
(eval-when-compile
  (require 'display-line-numbers))
(setq display-line-numbers-type 'relative)
(column-number-mode)

;;; REPOSITORY

(eval-when-compile
  (require 'package))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;;; PACKAGES

;; Packages from sources
(use-package quelpa
  :demand t
  :config
  (quelpa
   '(quelpa-use-package
     :repo "quelpa/quelpa-use-package"
     :fetcher github)))

(eval-when-compile (require 'quelpa-use-package))

;; TOML (used for configuration files)
(use-package 'toml
  :ensure t
  :demand t
  :quelpa (toml
           :repo "gongo/emacs-toml"
           :fetcher github))

;; Shortcuts' manager
(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-create-definer hondana/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")
  (hondana/leader-keys
   "SPC" 'dired-jump))

;; Evil
(use-package evil
  :ensure t
  :after general
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil) ; IMPORTANT: mandatory for evil-collection
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (general-define-key :states 'insert
                      (kbd "C-g") 'evil-normal-state)
  (general-define-key :states 'insert
                      (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (when (require 'toml nil 'noerror)
    (let ((states (toml:read-from-file "evil-emacs-state.toml")))
      (dolist (state states)
        (cond ((equal "modes" (car state))
               (let ((modes (cdr state)))
                 (when (proper-list-p modes)
                   (dolist (mode modes)
                     (add-to-list 'evil-emacs-state-modes (make-symbol mode))))))
              ((equal "buffers" (car state))
               (let ((buffers (cdr state)))
                 (when (proper-list-p buffers)
                   (dolist (buffer buffers)
                     (let* ((bs-filtered (replace-regexp-in-string "\\\\\\\\" "\\\\" buffer))
                            (wildcard-escaped-1 (replace-regexp-in-string "^\\*" "\\\\*" bs-filtered))
                            (filtered-buffer (replace-regexp-in-string "\\([^\\.]\\)\\*" "\\1\\\\*" wildcard-escaped-1)))
                       (add-to-list 'evil-buffer-regexps (cons filtered-buffer 'emacs))))))))))))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Xclip
(unless (and (eq system-type 'gnu/linux)
             (not (executable-find "xclip")))
  (use-package xclip
               :ensure t
               :init (xclip-mode 1)))

;; Buffer history
(use-package savehist
  :init
  (savehist-mode))

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

;; Show lots of useful stuff in the minibuffer
(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Sly (+ launch it when common lisp found)
(use-package sly
  :init
  (when (executable-find inferior-lisp-program)
        (sly)))

;;; EXTENSIONS

;; Additional tools for the graphical version of Emacs
(unless window-system
  (require 'gui-setup))

;; Git manager if you want
(when hondana/want-git
  (use-package magit
    :commands (magit-status magit-get-current-branch)
    :custom
    (magit-display-buffer-function
     #'magit-display-buffer-same-window-except-diff-v1))
  (when (require 'evil-mode nil 'noerror)
    ;; (quelpa '(evil-magit :repo "emacs-evil/evil-magit" :fetcher github :after magit))))
    (use-package evil-magit
      :quelpa (evil-magit
               :repo "emacs-evil/evil-magit"
               :fetcher github)
      :after magit)))

;; Project management if you want
(when hondana/want-project-assist
  (require 'project-setup))

;; Org-mode
(when hondana/want-org-mode
  (require 'org-setup))

(provide 'init)
