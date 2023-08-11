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
(setq display-line-numbers-type 'relative)
(column-number-mode)

;;; REPOSITORY
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
;; Key bindings manager
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
  (dolist (mode '(inferior-emacs-lisp-mode
                  sly-db-mode
                  sly-inspector-mode
                  sly-trace-dialog-mode
                  sly-stickers--replay-mode ; not a typo
                  sly-xref-mode
                  sly-connection-list-mode
                  sly-thread-control-mode
                  dired-mode
                  navi-mode
                  comint-mode
                  ebib-entry-mode
                  dirtree-mode
                  image-mode
                  ebib-log-mode
                  gtags-select-mode
                  eshell-mode
                  shell-mode
                  term-mode
                  bc-menu-mode
                  grep-mode
                  help-mode
                  eww-mode
                  google-contacts-mode
                  magit-branch-manager-mode-map
                  magit-repolist-mode
                  semantic-symref-results-mode
                  fundamental-mode
                  cider-stacktrace-mode
                  diff-mode
                  git-rebase-mode
                  git-rebase-mode
                  elfeed-show-mode
                  elfeed-search-mode
                  docker-images-mode
                  docker-containers-mode
                  cider-docview-mode
                  notmuch-tree-mode
                  xref--xref-buffer-mode
                  eww-mode
                  rdictcc-buffer-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  (dolist (buffer-name '("\\*sly-macroexpansion\\*" "\\*sly-description\\*"
                         "\\*VC-history\\*" "COMMIT_EDITMSG" "CAPTURE-.*\\.org"
                         "\\*Warnings\\*" "\\*cider-inspect\\*"))
    (add-to-list 'evil-buffer-regexps
                 (cons buffer-name 'emacs))))

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

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Sly (+ launch it when common lisp found)
(use-package sly
  :init
  (when (executable-find inferior-lisp-program)
        (sly)))

;;; Extensions
(add-to-list 'load-path (file-name-directory user-init-file))
(unless window-system
  (require 'init-gui))

(provide 'init)
