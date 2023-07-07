;;; vim-everywhere.el ---
;;
;; Filename: vim-everywhere.el
;; Description: Vim Emulation Setup
;; Author: Martial Boniou
;; Maintainer:
;; Created: Sat Feb 19 18:19:43 2011 (+0100)
;; Version: 0.8
;; Last-Updated: Thu Jan 30 13:19:39 2014 (+0100)
;;           By: Martial Boniou
;;     Update #: 497
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: Evil + your Vim colorscheme in Emacs
;;
;; keep (quite) same syntax highlighting everywhere
;; by hondana@gmx.com 2001-2013
;;
;; key bindings for Evil & Evil-leader in `shortcuts.el'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar *vim-now* t) ; vemacs case
(provide 'emulation-context)
(require 'preamble)

;; (unless (fboundp 'mars/add-to-load-path)
;;   (let ((local-site-lisp-path (mapcar #'(lambda (x) (concat (file-name-as-directory mars/local-root-dir)
;;                                                            (file-name-as-directory x))) mars/site-lisp-path)))
;;     (setq load-path (append local-site-lisp-path load-path))
;;     (dolist (local-site-lisp local-site-lisp-path)
;;       (mapc #'(lambda (x)
;;                 (let ((found-dirs (directory-files local-site-lisp t (symbol-name x))))
;;                   (when found-dirs
;;                     (mapc #'(lambda (found-dir)
;;                               (save-excursion
;;                                 (condition-case nil
;;                                     (progn
;;                                       (cd found-dir)
;;                                       (push (expand-file-name found-dir) load-path))
;;                                   (error nil))))
;;                           found-dirs))))
;;             '(color-theme evil))))
;;   (load-library "color-theme-autoloads"))


;;; VIPER CASE IF REQUIRED
(defvar viper-custom-file-name nil
  "Viper file name")
(defvar viper-toggle-key nil
  "Viper key to switch between Emacs and Vi Normal modes")
(defvar viper-mode nil
  "Boolean flag to run Viper or not")
(let ((vi-conf (conf-locate (concat (user-login-name) ".viper"))))
  (if vi-conf
      (setq viper-custom-file-name (convert-standard-filename vi-conf))
    (let ((default-conf (conf-locate "default.viper")))
      (when default-conf
        (setq viper-custom-file-name (convert-standard-filename default-conf))))))
;; (setq viper-toggle-key "\C-x\C-z" ; no more C-z -- 'minimize' then works on Mac
;;       viper-mode t)
(eval-after-load "viper"                ; UNUSED but kept for 'viper user
  '(progn
     ;; 0- tweaks
     (bind-key viper-vi-global-user-map "C-<kp-delete>" nil)
     ;; 1- ruby case
     (add-to-list 'viper-vi-state-mode-list 'ruby-mode)
     ;; 2- autopair case
     (eval-after-load "autopair"
       '(progn
          (when (locate-library "autopair-viper-compat")
            (require 'autopair-viper-compat))))
     ;; 3- additional keyboard bindings
     (bind-keys '(viper-vi-global-user-map viper-insert-basic-map)
                "<kp-delete>" #'viper-forward-char
                "C-<backspace>" #'viper-backward-word)
     ;; idea: http://stackoverflow.com/users/2797/sebastien-roccaserra
     (bind-keys viper-vi-global-user-map
                "/" #'isearch-forward-regexp
                "?" #'isearch-backward-regexp
                "C-w h" #'windmove-left
                "C-w j" #'windmove-down
                "C-w k" #'windmove-up
                "C-w l" #'windmove-right
                "C-w v" #'(lambda nil (interactive)
                            (split-window-horizontally)
                            (other-window 1)
                            (switch-to-buffer (other-buffer))))
     (bind-key viper-insert-basic-map "C-e" nil) ; IMPORTANT: ANSI C-a/C-e in insert mode
     (bind-key viper-vi-basic-map "C-e" #'viper-scroll-up-one)
     (push '("only"  (delete-other-windows)) ex-token-alist) ; on  in Evil
     (push '("close" (delete-window))        ex-token-alist) ; clo in Evil
     (bind-key viper-vi-global-user-map " k" #'viper-kill-buffer)
     (when *i-am-a-terminator*
       ;; global-unset-key "\C-h"
       (bind-key viper-vi-global-user-map "C-h" 'viper-backward-char)
       (bind-key viper-insert-global-user-map "C-h" 'viper-delete-backward-char))
     ;; 4- colorize <> modes
     (setq viper-vi-state-id
           (concat (propertize "<V>" 'face 'font-lock-string-face) " ")
           viper-insert-state-id
           (concat (propertize "<I>" 'face 'font-lock-string-face) " ")
           viper-replace-state-id
           (concat (propertize "<R>" 'face 'font-lock-string-face) " "))
     (require-if-located 'hi-lock)
     (setq viper-emacs-state-id
           (concat (propertize "<E>" 'face 'hi-red-b) " "))
     (put 'viper-mode-string 'risky-local-variable t)))

;;; EVIL (VIPER FREE)
(condition-case nil
    (global-evil-leader-mode)           ; before 'EVIL to work in all buffers
  (error "vim-everywhere: you should install evil-leader"))
(eval-after-load "evil-leader"
  '(global-evil-leader-mode))
(require-if-located 'evil)
;; add C-w outside Evil -- eg. C-w C-w -> other-window
(eval-after-load "evil"
  '(progn
     ;; 0- Evil, don't touch my cursor, will you?!
     (setq evil-default-cursor t)
     ;; 1- boot Evil & friends properly
     (if-bound-call viper-go-away)      ; FIXME: missing hooks in Evil? to shutdown viper on turn-on
     (evil-mode 1)
     ;; enable C-x C-z as evil-mode toggle key
     (when (window-system)
       (eval-after-load "escreen"
         '(progn
            (evil-set-toggle-key "C-x C-z") ; unset C-z
            (setq escreen-prefix-char "\C-z")
            ;; fast escreen keybindings for Dvorak typists
            ;; (using bottom right diamond combination)
            (when *i-am-a-dvorak-typist*
              (bind-keys escreen-map
                         "C--" #'escreen-goto-next-screen
                         "C-v" #'escreen-goto-prev-screen
                         "C-s" #'escreen-menu))
            (bind-key (current-global-map) escreen-prefix-char #'escreen-prefix)))
       (eval-after-load "elscreen"
         '(progn
            (evil-set-toggle-key "C-x C-z") ; unset C-z
            (elscreen-set-prefix-key "\C-z"))))
     (require-if-located 'surround)      ; via evil-surround
     (eval-after-load "surround"
       '(progn
          ;; from https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-evil.el
          (setq-default surround-pairs-alist '((?\( . ("("  . ")"))
                                               (?\[ . ("["  . "]"))
                                               (?\{ . ("{"  . "}"))
                                               (?\) . ("("  . ")"))
                                               (?\] . ("["  . "]"))
                                               (?\} . ("{"  . "}"))
                                               (?>  . ("<"  . ">"))
                                               (?#  . ("#{" . "}"))
                                               (?p  . ("("  . ")"))
                                               (?b  . ("["  . "]"))
                                               (?B  . ("{"  . "}"))
                                               (?<  . ("<"  . ">"))
                                               (?t  . surround-read-tags)))
          (defun cofi/surround-add-pair (trigger begin-or-fun &optional end)
            "Add a surround pair. If `end' is nil, `begin-or-fun' will be treated as a fun."
            (push (cons (if (stringp trigger)
                            (string-to-char trigger)
                          trigger)
                        (if end
                            (cons begin-or-fun end)
                          begin-or-fun))
                  surround-pairs-alist))
          (global-surround-mode 1)
          (mars/add-hooks '(emacs-lisp-mode-hook lisp-mode-hook)
                          #'(lambda () (cofi/surround-add-pair "`" "`" "'")))
          (eval-after-load "code"
            '(progn
               (mars/add-hooks '(markdown-mode-hook rst-mode-hook python-mode-hook)
                               #'(lambda () (cofi/surround-add-pair "~" "``" "``")))
               (mars/add-hooks '(rst-mode-hook python-mode-hook)
                               #'(lambda ()
                                   (cofi/surround-add-pair "c" ":class:`" "`")
                                   (cofi/surround-add-pair "f" ":func:`"  "`")
                                   (cofi/surround-add-pair "m" ":meth:`"  "`")
                                   (cofi/surround-add-pair "a" ":attr:`"  "`")
                                   (cofi/surround-add-pair "e" ":exc:`"   "`")))
               (add-lambda-hook 'LaTeX-mode-hook
                 (cofi/surround-add-pair "~" "\\texttt{" "}")
                 (cofi/surround-add-pair "=" "\\verb="   "=")
                 (cofi/surround-add-pair "/" "\\emph{"   "}")
                 (cofi/surround-add-pair "*" "\\textbf{" "}")
                 (cofi/surround-add-pair "P" "\\("       ")"))))))
     (require-if-located 'evil-numbers)
     ;; manage special modes where Emacs state should be activated
     (defmacro mars/set-evil-state-in-modes (state &rest mode-list)
       `(progn
          ,@(mapcar #'(lambda (x)
                        `(evil-set-initial-state ',x ',state)) mode-list)))
     (mars/set-evil-state-in-modes normal erc-mode)
     (mars/set-evil-state-in-modes emacs
                                   wl-summary-mode ack-and-a-half-mode grep-mode
                                   bbdb-mode bc-medu-mode
                                   ebib-entry-mode ebib-index-mode ebib-log-mode
                                   shell-mode eshell-mode term-mode
                                   comint-mode inferior-emacs-lisp-mode
                                   inferior-shen-mode pylookup-mode
                                   semantic-symref-results-mode rdictcc-buffer-mode
                                   magit-status-mode magit-log-edit-mode
                                   magit-branch-manager-mode gtags-select-mode)
     (eval-after-load "wl-folder"       ; FIXME: evil-set-initial-state fails!
       `(add-hook 'wl-folder-mode-hook #'evil-emacs-state))
     ;; manage special modes where 'C-w' should be activated
     (fset 'evil-like-window-map (copy-keymap evil-window-map))
     (defmacro mars/activate-C-w-in-modes (&rest context-mode-alist)
       `(progn
          ,@(mapcar #'(lambda (x)
                        `(mars/activate-state-in-modes ,(cadr x)
                                                       ,(car x)
                                                       bind-key
                                                       "C-w" 'evil-like-window-map))
                    context-mode-alist)))
     (mars/activate-C-w-in-modes (dired (dired-mode-map))
                                 (ibuffer (ibuffer-mode-map))
                                 (org-agenda (org-agenda-mode-map)))

     ;; 2- parenface to add a default color to parentheses as Vim does
     (if (locate-library "hl-line+")
         (global-hl-line-mode)
       (progn
         (defface hl-line '((t (:background "grey10"))) "Dummy hl-line.")
         (setq hl-line-face 'hl-line)))
     (require-if-located 'parenface)

     ;; 3- TODO: nothing to add (mis)match parentheses -- check 'show-parens
     ;;    so there's no need to add 'mic-paren

     ;; 4- line numbering
     (or (require-if-located 'linum-relative) ; relative numbers is the default
         (require-if-located 'linum+)) ; remove linum-relative for smart numbers
     (eval-after-load "linum"
       '(global-linum-mode t))      ; TODO: remove wl-summary etc...
     (eval-after-load "linum-off"   ; `linum-off' should be installed
       '(custom-set-variables
         '(linum-disabled-modes-list '(eshell-mode
                                       term-mode
                                       wl-summary-mode
                                       compilation-mode
                                       org-mode
                                       text-mode
                                       ack-and-a-half-mode
                                       grep-mode
                                       dired-mode))))
     ;; personal tweak I use on Vim too
     (eval-after-load "linum-relative"
       '(progn
          ;; show real line number at the zero point
          (setq linum-relative-current-symbol "")
          (custom-set-faces
           '(linum-relative-current-face ((t :inherit font-lock-variable-name-face :weight bold))))
          ;; deactivate in typing states
          (let ((states '(evil-insert-state
                          evil-visual-state
                          evil-replace-state)))
            (cl-flet ((state-hookize (state-list timeline)
                                     (mapcar #'(lambda (x)
                                                 (intern
                                                  (concat (symbol-name x)
                                                          "-"
                                                          (symbol-name timeline)
                                                          "-hook"))) state-list)))
              ;; FIXME: when switching with the mouse, the display offset creates a Visual session
              (mars/add-hooks (state-hookize states 'entry) #'(lambda () (linum-mode 0)))
              (mars/add-hooks (state-hookize states 'exit) #'(lambda () (linum-mode 1)))))))

     ;; 5- colorize numbers, todos & warnings
     (defface font-lock-number-face
       '((((type tty) (class color)) (:foreground "pink"))
         (((type tty) (class mono))  (:inverse-video t))
         (((class color) (background dark))  (:foreground "pink"))
         (((class color) (background light)) (:foreground "grey10"))
         (t ()))
       "Face name to use for numbers."
       :group 'basic-faces)
     (defvar font-lock-number-face 'font-lock-number-face)
     (defun font-lock-fontify-numbers ()
       "Hook function to `font-lock-number-face'-ify numbers."
       (font-lock-add-keywords nil
                               '(("[^a-zA-Z_]\\(0x[0-9a-fA-F]+\\)"     1 font-lock-number-face) ; hexa
                                 ("[^a-zA-Z_]\\(-?[0-9]+\\.[0-9]+\\)"  1 font-lock-number-face) ; float
                                 ("[^a-zA-Z_1-9-]\\(-?[0-9]+U?L?L?\\)" 1 font-lock-number-face)))) ; int
     (defface font-lock-todo-face
       '((((type tty) (class color)) (:foreground "yellow"))
         (((type tty) (class mono))  (:underline t))
         (((class color) (background dark))  (:foreground "gold"))
         (((class color) (background light)) (:foreground "goldenrod"))
         (t ()))
       "Face name to use for todos."
       :group 'basic-faces)
     (defvar font-lock-todo-face 'font-lock-todo-face)
     (defface font-lock-notify-face
       '((((type tty) (class color)) (:foreground "blue"))
         (((type tty) (class mono))  (:inverse-video t :underline t))
         (((class color) (background dark))  (:foreground "DarkSlateGray1"))
         (((class color) (background light)) (:foreground "DarkSlateGray4"))
         (t ()))
       "Face name to use for notifications. As `font-lock-warning-face' may be used as Vim's
ErrorMsg al alternative, Vim's WarningMsg may be mapped to this face."
       :group 'basic-faces)
     (defvar font-lock-notify-face 'font-lock-notify-face)
     (dolist (mode '(lisp-mode
                     emacs-lisp-mode
                     lisp-interaction-mode
                     scheme-mode
                     shen-mode
                     qi-mode
                     c-mode
                     cc-mode
                     js-mode
                     espresso-mode
                     shell-script-mode
                     ruby-mode
                     python-mode
                     php-mode
                     java-mode
                     cperl-mode
                     howm-mode
                     org-mode
                     haskell-mode
                     smalltalk-mode
                     factor-mode
                     erlang-mode
                     caml-mode
                     clojure-mode))     ; TODO: mix in code.el
       (progn
         (add-hook (intern (concat (symbol-name mode) "-hook"))
                   #'font-lock-fontify-numbers)
         (font-lock-add-keywords mode
                                 '(("\\(FIXME:\\|TODO:\\)"
                                    1 font-lock-builtin-face prepend))) ; FIXME: builtin here b/c todo is too grayish in `Wombat256mod'
         (font-lock-add-keywords mode
                                 '(("\\(IMPORTANT:\\|WARNING:\\|NOTE:\\|UNTESTED\\|DEPRECATED\\)"
                                    1 font-lock-notify-face prepend)))
         (font-lock-add-keywords mode
                                 '(("\\(ERROR:\\|XXX\\|OBSOLETE\\)"
                                    1 font-lock-warning-face prepend)))))

     ;; 6- colorize status bar
     (lexical-let ((default-color (cons (face-background 'mode-line)
                                        (face-foreground 'mode-line))))
       (add-hook 'post-command-hook
                 (lambda ()
                   (let ((color (cond ((minibufferp) default-color)
                                      ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                      ((evil-visual-state-p) '("#666666" . "#ffffff"))
                                      ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                      ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                      (t default-color))))
                     (set-face-background 'mode-line (car color))
                     (set-face-foreground 'mode-line (cdr color))))))

     (unless (and (< emacs-major-version 24)
                  (not (fboundp 'color-theme-install)))
       ;;(unless (< emacs-major-version 24) (custom-set-variables '(custom-safe-themes (quote ("6c61100cf9667a6bbc99933f9e49b352147ba5f6918926d317f4a327a7e7de94" default)))))
       ;; choose a theme according to your Vim setup -- ~/.vimrc by default

       (defun mars/extract-colorscheme-from-vimrc (&optional fpath)
         "Check the contents of the vimrc file to get the same theme in emacs.
TODO: case of '''colorscheme' this'' where this is
''this = my_colorscheme''."
         (let ((vimfile (or fpath "~/.vimrc")))
           (if (file-exists-p vimfile)
               ;; locate used colorscheme in your .vimrc
               (with-temp-buffer
                 (insert-file-contents vimfile)
                 (goto-char (point-max))                       ; backward searching
                 (makunbound 'vim-colorscheme-used)
                 (while (not (boundp 'vim-colorscheme-used))
                   (let ((mat
                          (re-search-backward
                           "[^i]colorscheme \\\([-_A-Za-z0-9]+\\\)" nil t))) ; don't get guicolorscheme
                     (if (not (null mat))
                         (let ((str (match-string 1)))
                           (save-excursion
                             (save-restriction ; FIXME: write it w/o narrowing using
                                        ; a 'end instead of nil in 're-search-forward
                               (let ((pt (point)))
                                 (beginning-of-line)
                                 (narrow-to-region (point) pt)
                                 ;; " is a vimscript comment
                                 (if (not (re-search-forward "\"" nil t))
                                     (setq vim-colorscheme-used str))))))
                       vim-colorscheme-used)))))))

       ;; fetch your vim colorscheme and load a clone theme
       (let ((colorscheme (mars/extract-colorscheme-from-vimrc))
             (default-colorscheme "molokai"))
         (cl-flet ((compose-theme-name (theme-head theme-name)
                                       (if (or (null theme-name) (< (length theme-name) 2)) "" ; no short name
                                         (intern (concat theme-head (replace-regexp-in-string (char-to-string ?_) (char-to-string ?-) (remove-last-unwanted-char theme-name)))))))
           (when (or (not (stringp colorscheme))
                     (= (length colorscheme) 0))
             (setq colorscheme default-colorscheme))
           (if (< emacs-major-version 24)
               (let* ((theme-header "custom-vim-colorscheme-")
                      (theme (compose-theme-name theme-header colorscheme)))
                 (require 'deprecated-emacs-themes)
                 (unless (functionp theme) (setq theme (intern (compose-color-theme-name theme-header default-colorscheme))))
                 (when (functionp theme)
                   (funcall theme)))
             (let ((theme-header "vim-"))
               ;; may be overload by `Custom'
               (condition-case nil
                   (load-theme (compose-theme-name theme-header colorscheme) t)
                 (error (condition-case nil
                            (load-theme (compose-theme-name theme-header default-colorscheme) t)
                          (error (message "vim-everywhere: the default theme cannot be loaded."))))))))))

     ;; 7- open the current buffer in Vim (when Emacs modal editing comes short)
     (defun open-with-vim ()
       "Open current buffer with Vim. To ensure buffers synchronization, set 'GLOBAL-AUTO-REVERT-MODE to T."
       (interactive)
       (let* ((os (symbol-name system-type))
              (vim-name (if (string-match "Darwin" os)
                            "mvim"
                          (if (string-match "^Windows.*" os)
                              "gvim.exe"
                            "gvim"))))
         (message "Open %s with %s..." buffer-file-name vim-name)
         (shell-command (concat vim-name " " buffer-file-name))))
     (global-set-key '[(meta \0)] 'open-with-vim)))

;; - some keybindings
;; remember:
;; C-w = Window manipulation in normal modes; a kind of `DELETE-BACKWARD-WORD' elsewhere

(provide 'vim-everywhere)
(unintern 'emulation-context obarray)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vim-everywhere.el ends here
