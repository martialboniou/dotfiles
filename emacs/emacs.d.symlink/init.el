;;; Simple Emacs for Common Lisp

;;  Emacs version = 29.1
(defconst hondana/want-git t)
(defconst hondana/want-project-assist nil)
(defconst hondana/want-org-mode t)

(defvar inferior-lisp-program "sbcl") ; used with sbcl 2.3.7 on macOS 13,4
(defvar project-directory "~/Documents/Code")
(eval-and-compile
  (defvar evil-want-keybinding nil)) ; IMPORTANT: mandatory for evil-collection

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
      inhibit-startup-message t)
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

;; Relative line numbers
(eval-when-compile
  (require 'display-line-numbers))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(column-number-mode)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; REPOSITORY (`early-init.el' has already done the job)

;;; PACKAGES

;; Packages from sources
(straight-use-package 'use-package)

;; TOML (used for configuration files)
(use-package toml
  :ensure t
  :demand t
  :straight (toml
             :type git
             :host github
             :repo "gongo/emacs-toml"))

;; Shortcuts' manager
(use-package general
  :ensure t
  :config
  (general-evil-setup t))

;; Evil
(use-package evil
  :ensure t
  :after general
  :init
  (setq evil-want-integration t)
  :defines
  (evil-set-undo-systems)
  :functions (evil-mode)
  :preface ; (Optional) calm the compiler
  (declare-function evil-set-undo-system (system))
  (declare-function evil-global-set-key (state key def))
  (declare-function evil-set-initial-state (mode state))
  :config
  ;; leader leader => dired
  (general-define-key :states 'normal
                      "SPC SPC" 'dired-jump)
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  ;; insert mode: C-g => Esc & C-h => good ole backward
  (general-define-key :states 'insert
                      (kbd "C-g") 'evil-normal-state)
  (general-define-key :states 'insert
                      (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (when (require 'toml nil 'noerror)
    (let ((states (toml:read-from-file (expand-file-name
                                        "evil-emacs-state.toml"
                                        user-emacs-directory))))
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
  :functions evil-collection-init
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil
  :functions global-evil-surround-mode
  :config
  (global-evil-surround-mode 1))

;; Xclip
(unless (and (eq system-type 'gnu/linux)
             (not (executable-find "xclip")))
  (use-package xclip
               :ensure t
               :functions xclip-mode
               :init
               (xclip-mode 1)))

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
  :functions marginalia-mode
  :init
  (marginalia-mode))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Sly
(use-package sly)

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
     #'magit-display-buffer-same-window-except-diff-v1)))

;; Project management if you want
(when hondana/want-project-assist
  (require 'project-setup))

;; Org-mode
(when hondana/want-org-mode
  (require 'org-setup))

(provide 'init)
