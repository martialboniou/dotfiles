;;; Simple Emacs for Common Lisp -*- lexical-binding: t; -*-

;;  Emacs version = 29.1
(defconst hondana/want-git t)
(defconst hondana/want-project-assist nil)
(defconst hondana/want-org-mode t)

(defvar inferior-lisp-program "sbcl") ; used with sbcl 2.3.7 on macOS 13,4
(defvar project-directory "~/Documents/Code")
(defvar hondana/evil-emacs-states '())
(eval-and-compile
  (defvar evil-want-keybinding nil)) ; IMPORTANT: mandatory for evil-collection

(when (equal system-type 'darwin) ; `brew install coreutils` b/c gnuls != BSD's
  (let ((gnuls "/opt/homebrew/opt/coreutils/libexec/gnubin/ls"))
    (when (file-executable-p gnuls)
      (setq insert-directory-program gnuls))))

;; Core setup
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
(eval-when-compile (require 'autorevert))
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

;;; MACROS

(defmacro hondana/make-evil-states (file)
  `(let ((states
          (condition-case err
              (progn
                (require 'toml)
                (toml:read-from-file (expand-file-name ,file
                                                       user-emacs-directory)))
            (error
             (message "%s in %s"
                      (error-message-string err)
                      ,file)
             nil))))
     (when states
       (fset 'filter (lambda (text a b)
                       (replace-regexp-in-string a b text)))
       (fset 'filter-buf (lambda (buf)
                           (let* ((bs (filter buf "\\\\\\\\" "\\\\"))
                                  (wildc1 (filter bs "^\\*" "\\\\*"))
                                  (clean-buf (filter wildc1
                                                     "\\([^\\.]\\)\\*"
                                                     "\\1\\\\*")))
                             clean-buf)))
       (let ((buffers (cdr (assoc "buffers" states)))
             (filtered-buffers '()))
         (dolist (buffer buffers)
           (push (cons (filter-buf buffer) 'emacs)
                 filtered-buffers))
         (setf (cdr (assoc "buffers" states)) filtered-buffers)
         states))))

;;; REPOSITORY (`early-init.el' has already done the job)

;;; PACKAGES

;; TOML (used for configuration files)

(hondana/use toml
  :straight (toml
             :type git
             :host github
             :repo "gongo/emacs-toml"))

;; Shortcuts' manager
(hondana/use general
             :config
             (general-evil-setup t))

;; Evil
(hondana/use evil
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
  (let ((states (eval-when-compile
                  (hondana/make-evil-states "evil-emacs-states.toml"))))
    (when states
      (let ((modes (cdr (assoc "modes" states)))
            (buffers (cdr (assoc "buffers" states))))
        (dolist (mode modes)
          (add-to-list 'evil-emacs-state-modes (make-symbol mode)))
        (dolist (buffer buffers)
          (add-to-list 'evil-buffer-regexps buffer))))))

(hondana/use evil-collection
  :after evil
  :functions evil-collection-init
  :config
  (evil-collection-init))

(hondana/use evil-surround
  :after evil
  :functions global-evil-surround-mode
  :config
  (global-evil-surround-mode 1))

;; Xclip
(unless (and (eq system-type 'gnu/linux)
             (not (executable-find "xclip")))
  (hondana/use xclip
               :functions xclip-mode
               :init
               (xclip-mode 1)))

;; Buffer history
(hondana/use savehist
  :init
  (savehist-mode))

;; Better minibuffer completion
(hondana/use vertico
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

;; Code completion at point ; TODO: replace me by Corfu if time
(hondana/use company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0))

;; Which Key
(hondana/use which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.300))

;; (Optional) Doom-modeline
(hondana/use doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

;; Show lots of useful stuff in the minibuffer
(hondana/use marginalia
  :after vertico
  :functions marginalia-mode
  :init
  (marginalia-mode))

;; Rainbow Delimiters
(hondana/use rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Sly
(hondana/use sly)

;;; EXTENSIONS

;; Additional tools for the graphical version of Emacs
(unless window-system
  (require 'gui-setup))

;; Git manager if you want
(when hondana/want-git
  (hondana/use magit
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
