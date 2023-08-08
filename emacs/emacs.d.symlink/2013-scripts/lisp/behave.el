;;; behave.el ---
;;
;; Filename: behave.el
;; Description: Provides Emacs main behavior
;; Author: Martial Boniou
;; Maintainer:
;; Created: Thu Nov 17 17:30:20 2011 (+0100)
;; Version: 0.6.5
;; Last-Updated: Thu Jan 30 14:51:15 2014 (+0100)
;;           By: Martial Boniou
;;     Update #: 36
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: erasing (kp-delete support) + undoing (UNDO-TREE | REDO+) /
;;              modal editing (aka VIMPULSE) / DESKTOP + AUTOSAVE + SESSION /
;;              buffers (UNIQUIFY + ANYTHING + IBUFFER) / minibuffer (IDO + SMEX) /
;;              files (RECENTF + BOOKMARK-PLUS) / cursor (ACE-JUMP-MODE) /
;;              screen & buffers (ESCREEN + REVIVE+) / system (POSIX to kill emacs
;;              on SIGUSR2 + VT100 to get keys support for iTerm2.app or Terminator)
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

(require 'adapter)

;;; MAIN
;;
(cd "~")                                ; home sweet home
(setq standard-indent 4
      tab-width 4
      dired-use-ls-dired nil
      autosave-interval 50
      undo-limit 50000
      auto-compression-mode t
      backup-by-copying t
      backup-by-copying-when-linked t
      backup-by-copying-when-mismatch t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      completion-ignore-case t
      custom-buffer-done-kill t
      newsticker-display-interval 15.3
      linum+-dynamic-format "%%%dd "
      linum+-smart-format linum+-dynamic-format)
(when (featurep 'ns)
  (setq ns-alternate-modifier nil
        ns-command-modifier 'meta       ; NeXT/Apple computers got real Meta keys
        ns-antialias-text t))

;;; ERASING/UNDOING
;;
;; try 'REDO+ iff no 'UNDO-TREE
(eval-after-load "el-get"
  '(progn
     (unless (el-get-package-is-installed "undo-tree") ; display tree by using C-x u
       (condition-case nil
       (require 'redo+)
     (error (message "behave: install undo-tree or at least redo+ to get a better undo support")))
       (eval-after-load "redo"
     '(progn (setq undo-no-redo t))))))

;;; MODAL EDITING incl. COLOR-THEME & PARENS
;;
(when *i-am-a-vim-user*
  (require 'vim-everywhere))        ; otherwise see 'APPEARANCE

;;; DESKTOP & AUTOSAVE & SESSION
;;
;; - vars
(defvar the-desktop-file nil)           ; desktop
(defvar the-desktop-lock nil)
(defvar desktop-dir nil)
(defvar autosave-dir nil                ; autosave
  "Temporary variable use to make autosaves directory name.
That's where #foo# goes. It should normally be nil if
`user-init-file' is compiled.")
(defvar session-dir nil                 ; session
  "Temporary variable use to record interrupted sessions
for latter recovery. That's where .saves-<pid>-<hostname>
goes. It should normally be nil if `user-init-file' is
compiled. This directory is known as `auto-save-list'.")
(defvar backup-dir   nil                ; backup
  "Temporary variable use to make backups directory name.
That's where foo~ goes. It should normally be nil if
`user-init-file' is compiled.")
(defvar confirm-frame-action-buffer-alist nil ; kill frame alert
  "Associated list of buffer properties in order to get a confirmation alert during action on the frame containing this buffer. A property is a CONS formed by an information and a LIST of parameters for this information. Used by the adviced version of 'DELETE-FRAME defined in KERNEL.
Example: (MAJOR-MODE . (CHESS-MASTER-MODE MAIL-DRAFT-MODE).")
;; - loading
(require 'desktop)
;; - filtering
(setq desktop-buffers-not-to-save "\\(^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|.*_flymake.*\\|^tags\\|^TAGS\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|.*\\.bbdb\\)$"
      desktop-files-not-to-save "^$"
      desktop-minor-mode-table '((auto-fill-function auto-fill-mode)
                                 (vc-mode nil)
                                 (vc-dired-mode nil)
                                 (flymake-mode nil)
                                 (ecb-minor-mode nil)
                                 (semantic-show-unmatched-syntax-mode nil)
                                 (semantic-stickyfunc-mode nil)
                                 (senator-minor-mode nil)
                                 (semantic-idle-scheduler-mode nil)))
(mapc #'(lambda (x)
          (add-to-list 'desktop-modes-not-to-save x))
      '(Info-mode dired-mode image-mode))

;; - path
(unless desktop-dir
  (setq desktop-dir (joindirs mars/local-root-dir mars/personal-data "desktop")))
(unless (file-exists-p desktop-dir)
  (make-directory desktop-dir t))
(setq desktop-path (list desktop-dir)
      history-length 250)
;; - directories
(defmacro define-local-temporary-directory (local-work-directory-symbol)
  "Define the best temporary directory for registering files and sessions."
  (let ((local-tmp-dir (joindirs mars/local-root-dir mars/personal-data "Temporary")))
    (let ((dir-symbol (intern (format "%s-dir"
                                      (symbol-name
                                       local-work-directory-symbol)))))
      (unless (symbol-value dir-symbol)
        ;; creates directory iff unset (eg. `vars' may override)
        ;; this <symbol>-dir must be 'NIL before compiling this macro
        (if (file-exists-p local-tmp-dir)
            `(setq ,dir-symbol ,(expand-file-name
                                 (capitalize
                                  (format "%ss"
                                          (symbol-name
                                           local-work-directory-symbol)))
                                 local-tmp-dir))
          (let ((name (joindirs mars/temporary-dir
                                (format "emacs_%ss" (symbol-name local-work-directory-symbol))
                                (user-login-name))))
            `(progn
               (setq ,dir-symbol ,name)
               (message ,(format "Beware: your autosave directory named `%s' may be publicly accessed. Be sure to make it hidden to other users." name)))))))))
(define-local-temporary-directory autosave) ; #<files>#
(define-local-temporary-directory session)  ; .saves-<pid>-<hostname>
(define-local-temporary-directory backup)   ; !<backup-directory>!<backup-file>!.~<index>~
(setq auto-save-file-name-transforms `(("\\([^/]*/\\)*\\(.*\\)" ,(expand-file-name "\\2" autosave-dir) nil))
      auto-save-list-file-prefix (joindirs session-dir ".saves-")
      backup-directory-alist (list (cons "." backup-dir)))
;; - desktop load
(when (featurep 'emacs-normal-startup)
  (eval-after-load "gtd"
    '(progn
       (when (fboundp 'display-organizer-at-startup)
         (add-hook 'desktop-after-read-hook #'display-organizer-at-startup))))
  (condition-case err
      (desktop-save-mode 1)
    (error (message (format "behave: %s" err)))))
(make-directory autosave-dir t)         ; be sure it exists
(setq the-desktop-file (joindirs desktop-dir desktop-base-file-name)
      the-desktop-lock (joindirs desktop-dir desktop-base-lock-name))
(defun desktop-in-use-p ()
  (and (file-exists-p the-desktop-file) (file-exists-p the-desktop-lock)))
(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))
;; - desktop autosave
(when (featurep 'emacs-normal-startup)
  (add-lambda-hook 'auto-save-hook
    (interactive)
    (when (desktop-in-use-p)
      (if desktop-dirname
          (desktop-save desktop-dirname)
        (call-interactively #'desktop-save)))))
;; - unset temporary directory names
;;   otherwise `define-local-temporary-directory' compilation issue
(eval-when-compile
  (setq autosave-dir nil
        session-dir nil
        backup-dir nil))

;;; BUFFERS
;;
;; - uniquify
(when (locate-library "uniquify")
  (require 'uniquify))                  ; should be in Emacs 24
(eval-after-load "uniquify"
  '(setq uniquify-buffer-name-style 'post-forward-angle-brackets ; name<foo/bar>|name<quux/bar>
         uniquify-after-kill-buffer-p t
         uniquify-ignore-buffers-re "^\\*"
         global-auto-revert-mode t))
;; - ibuffer
(eval-after-load "ibuffer"
  '(bind-key ibuffer-mode-map "'" #'kill-all-dired-buffers))
;; - anything
(eval-after-load "el-get"
  '(when (el-get-package-is-installed "anything")
     (if (locate-library "anything-match-plugin")
     (require 'anything-match-plugin) ; loads 'ANYTHING
       (require 'anything))
     (require-if-located 'anything-config)))
(eval-after-load "anything-config"
  '(progn
     (defvar mars/anything-pers-action-binding "C-."
       "New binding for `anything-execute-persistent-action'. Was originally
the should-be-forbidden C-z.")
     (bind-keys anything-map
                "C-l" #'anything-find-files-down-one-level  ; originally C-. in 'window-system
                "C-z" nil
                mars/anything-pers-action-binding #'anything-execute-persistent-action)
     (setcdr (assoc 'persistent-help anything-c-source-advice)
             (format "Describe function / C-u %s: Toggle advice"
                     mars/anything-pers-action-binding))
     ;; ido case
     (eval-after-load "ido"
       '(progn
          nil
          ;; (anything-lisp-complete-symbol-set-timer 150) ; collect by 150 sec
          ;; (bind-key emacs-lisp-mode "C-M-i" #'anything-lisp-complete-symbol-partial-match)
          ;; (bind-key lisp-interaction-mode-map "C-M-i" #'anything-lisp-complete-symbol-partial-match)
          ;; (anything-read-string-mode 0)
          ))))

;;; MINIBUFFER
;;
;; - ido
(eval-after-load "ido"
  '(progn
     (setq confirm-nonexistent-file-or-buffer nil)
     (ido-mode 1)
     (setq ido-enable-tramp-completion nil
           ido-enable-flex-matching t
           ido-everywhere t
           ido-max-directory-size 100000
           ido-create-new-buffer 'always
           ido-enable-last-directory-history nil
           ido-confirm-unique-completion nil ; wait for RET, even for unique
           ido-show-dot-for-dired t         ; put . as the first item
           ido-use-filename-at-point 'guess ; ido guesses the context
           ido-use-url-at-point nil
           ido-default-file-method 'raise-frame ; you may ask if it should be displayed in the current
                                        ; window via `maybe-frame'. Let `ido-switch-buffer' do this.
           ido-default-buffer-method 'selected-window
           ido-ignore-extensions t)))   ; `completion-ignored-extensions'
;; - smex
(unless (el-get-package-is-installed "smex")
  (message "behave: smex is not installed"))

;;; FILES
;;
;; - recentf
(recentf-mode 1)
(eval-after-load "recentf"
  '(progn
     (setq recentf-max-saved-items 50
           recentf-max-menu-items 30
           recentf-keep '(file-remote-p file-readable-p))
     (defun undo-kill-buffer (arg)
       "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
       (interactive "p")
       (let ((recently-killed-list (copy-sequence recentf-list))
             (buffer-files-list
              (delq nil (mapcar #'(lambda (buf)
                                    (when (buffer-file-name buf)
                                      (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
         (mapc #'(lambda (buf-file)
                   (setq recently-killed-list
                         (delq buf-file recently-killed-list)))
          buffer-files-list)
         ;; (message "echo: %s" (prin1-to-string
         ;;                      (reduce 'cons recently-killed-list
         ;;                              :start 0 :end 10)))
         (find-file
          (if arg (nth arg recently-killed-list)
            (car recently-killed-list)))))
     ;; ido case
     ;; open recent files according to history of mini-buffer (incl. files search
     ;; and management) or according to the list of recently loaded ones.
     (defun ido-recentf-file-name-history ()
       "Find a file in the `file-name-history' using ido for completion. Written by Markus Gattol."
       (interactive)
       (let* ((all-files
               (remove-duplicates
                (mapcar #'expand-file-name
                        file-name-history) :test 'string=)) ; remove dups after expanding
              (file-assoc-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) all-files))
              (filename-list (remove-duplicates (mapcar 'car file-assoc-list) :test 'string=))
              (ido-make-buffer-list-hook
               (lambda ()
                 (setq ido-temp-list filename-list)))
              (filename (ido-read-buffer "File History: "))
              (result-list (delq nil (mapcar
                                      #'(lambda (x)
                                          (when (string= (car x) filename)
                                            (cdr x)))
                                      file-assoc-list)))
              (result-length (length result-list)))
         (find-file
          (cond
           ((= result-length 0) filename)
           ((= result-length 1) (car result-list))
           (t (let ((ido-make-buffer-list-hook
                     (lambda () (setq ido-temp-list result-list))))
                (ido-read-buffer (format "%d matches:" result-length))))))))
     (defun ido-recentf ()
       "Use ido to select a recently opened file from the `recentf-list'. Written by xsteve."
       (interactive)
       (let ((home (expand-file-name (getenv "HOME"))))
         (find-file (ido-completing-read "Recent File: "
                                         (mapcar
                                          (lambda (path)
                                            (replace-regexp-in-string
                                             home "~" path))
                                          recentf-list) nil t))))
     (eval-after-load "ibuffer"
       '(progn
          (defun ibuffer-ido-find-file ()
            "Like `ido-find-file', but default to the directory of the buffer at point."
            (interactive)
            (let ((buf (ibuffer-current-buffer)))
              (when (buffer-live-p buf)
                (setq default-directory
                      (with-current-buffer buf default-directory))))
            (ido-find-file-in-dir default-directory))
          (bind-key ibuffer-mode-map "C-x C-f" #'ibuffer-ido-find-file)))))

;;; BOOKMARKS
;;  el-get features bookmark+

;;; ACE-JUMP-MODE
;;  don't let el-get feature ace-jump-mode
;;  but let it post-initialize key bindings (cf. 'PACKS-EL-GET)
(unless (locate-library "ace-jump-mode")
  (message "behave: ace-jump-mode is recommended but not installed."))

;;; ESCREEN
(require-if-located 'escreen)
(eval-after-load "escreen"
  '(progn
     (escreen-install)
     (require-if-located 'escreen-fancy-display-numbers)))

;;; REVIVE
(when (locate-library "revive+")
  (require 'revive+)
  (eval-after-load "revive+"
    '(progn
       (setq revive-plus:all-frames t)
       (revive-plus:minimal-setup))))

;;; POSIX ENVIRONMENT
(bind-key special-event-map [sigusr2] #'kill-emacs)

;;; VT100 KEYS
(when (and (null window-system)
           (locate-library "term/lk201"))
  (load "term/lk201"))
(eval-after-load "lk201" '(terminal-init-lk201))

(provide 'behave)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; behave.el ends here
