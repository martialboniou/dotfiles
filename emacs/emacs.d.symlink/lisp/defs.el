;;; defs.el ---
;;
;; Filename: defs.el
;; Description: utility functions
;; Author: Martial Boniou
;; Maintainer:
;; Created: Sat Feb 19 18:12:37 2011 (+0100)
;; Version: 0.17
;; Last-Updated: Thu Jan 30 14:46:45 2014 (+0100)
;;           By: Martial Boniou
;;     Update #: 331
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: Most useful functions (should be loaded at the
;;              beginning of .emacs)
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log: require cl-lib (either from 24.3+ recommended GNU Emacs
;;              or using `el-get-install' in 'EL-SELECT to install
;;              compatibility script)
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

;;; The Most Useful Defines:
;;
;;; /// TEXT \\\
;;; M-x show-trailing-whitespace : toggle spacing appearance
;;; M-x dired-2unix-marked-files : repair dired files to Unix encoding
;;; M-x the-the : remove duplicata words in the current file
;;; M-x insert-special : insert special characters
;;; /// FILE/BUFFER \\\
;;; M-x rename-file-and-buffer : rename file/buffer
;;; M-x move-buffer-file : move file/buffer to another directory
;;

;;; INITIALIZATIONS
;;
(unless (boundp 'mars/eternal-buffer-list)
    (setq mars/eternal-buffer-list '("*scratch*")))

(eval-when-compile (require 'cl))

;;; ESSENTIAL UTILITIES
;;;
(defmacro if-bound-call (form &rest args) ; UNTESTED
  "If `FORM' is bound as a function, call it with `ARGS'."
  `(if (fboundp ',form)
       (,form ,@args)))

(defun require-if-located (symbol &optional message) ; UNTESTED
  "If library is located, require it."
  (if (locate-library (symbol-name symbol))
      (require symbol)
    (when message
      (message message))))

(defun mars/generate-mode-hook-list (modes)
  "Create a quoted list of hooks."
  (mapcar (lambda (arg)
            (intern (format "%s-mode-hook" (symbol-name arg))))
          modes))

(defmacro add-lambda-hook (hook &rest body)
  "Add LAMBDA to function to bind to one or many HOOKs. -- mina86@github"
  (declare (indent 1))
  (if (and (listp hook) (eq (car hook) 'quote) (listp (cadr hook)))
      (let ((func (make-symbol "func")))
        `(let
          ((,func (lambda () ,@body)))
          . ,(mapcar (lambda (h) `(add-hook (quote ,h) ,func))
                     (cadr hook))))
    `(add-hook ,hook (function (lambda () ,@body)))))

(defun mars/add-hooks (hooks fun)
  "Attach one FUN to many HOOKS: (mars/add-hook-to-list '(my-hk1 my-hk2 my-hk3) my-fun)."
  (mapc #'(lambda (mode-hook)
            (add-hook mode-hook fun))
        hooks))

(defmacro mars/force-options (&rest conses)
  "Initialize CDR from the value of CAR if CAR is bound.
`CONSES' is one or many CONS of variables."
  `(progn
     ,@(mapcar #'(lambda (x)
                   `(when (boundp ',(car x))
                      (setq ,(cdr x) ,(car x))))
               conses)))

(defun safe-autoloads-load (loaddefs)   ; UNTESTED
  "Secure load a `loaddefs' file. Load additional libraries
if special autoload format (eg: `cedet' autoloads)."
  (condition-case err
      (load loaddefs)
    (error
     (message "Cedet must be loaded to parse `%s' correctly: %s" loaddefs err)
     (safe-load-cedet)
     (load loaddefs))))

(defun safe-load-cedet ()       ; UNTESTED
  "Load `cedet'. Be sure to not load the compiled common file."
  (condition-case err
    (let* ((library      (file-name-directory
                           (locate-library "cedet")))
           (current-name (expand-file-name "cedet-devel-load.el" library)))
     (if (file-exists-p current-name)
        (load-file current-name)
      (load-file (expand-file-name "cedet.el" library))))
    (error (message "error: cedet environment not loaded: %s" err))))

(defun twb/autoload (library &rest functions) ; UNTESTED
  "Autoload LIBRARY when one of the FUNCTIONS is invoked.
twb#emacs at http://paste.lisp.org/display/43546,"
  (when (locate-library library)
    (mapc (lambda (y)
            (autoload y library nil t))
          functions)))

(defun mergeable-to-path-p (dir)    ; UNTESTED
  "Checks if `DIR' is a visitable directory or link."
  (and (file-exists-p dir)
       (save-excursion
         (let ((inspectable t))
           (condition-case nil
               (cd dir)
             (error
              (setq inspectable nil)))
           inspectable))))

(defun mars/add-to-load-path (root &optional &rest pathname) ; UNTESTED
  "Add directories to `load-path' according the two following
patterns:
ROOT (LIST PATH1 PATH2 ...) => ROOT / PATH1 & ROOT / PATH2 & ...
ROOT                        => ROOT"
  (let ((current-directory default-directory)
        (path (if pathname
                (mapcar (lambda (x)
                          (expand-file-name x root))
                        (flatten pathname))
                (list (expand-file-name root)))))
    (mapc
     #'(lambda (dir)
         (when (and (mergeable-to-path-p dir)
                    (not (file-exists-p (expand-file-name ".nosearch" dir)))) ; test exclusion on `dir'
           (let ((default-directory dir)
                 (orig-load-path load-path))
             (setq load-path (list dir))
             (normal-top-level-add-subdirs-to-load-path)
             (setq load-path
                   (mapcar #'(lambda (x)
                               (directory-file-name x)) load-path))
             (nconc load-path orig-load-path)))) ; reverse path construct
     path)
    (delete-dups load-path)
    (cd current-directory)))

(defun mars/autoload (libraries-and-functions) ; UNTESTED
  (mapc (lambda(x)
          (apply 'twb/autoload x))
        libraries-and-functions))

(defmacro add-to-alist (key value alist)
  "add VALUE to an ALIST at KEY."
  `(if (null (assoc ,key ,alist))
      (setq ,alist (cons (list ,key ,value) ,alist))
     (progn
       (setcdr (assoc ,key ,alist)
               (cons ,value (cdr (assoc ,key ,alist))))
       ,alist)))

(defun value-alist (key alist &optional default) ; UNTESTED
  (let ((match (assoc key alist)))
    (if match
        (cdr match)
      default)))

(defun joindirs (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join,
  (dotemacs-joindirs \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c. Code by
  dbr@stackoverflow (neverfear.org).
"
  (if (not dirs)
      (expand-file-name root)
    (apply #'joindirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(defun dont-kill-emacs ()               ; UNTESTED
  "Disallow emacs to kill on the dangerous C-x C-c command."
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

(defun byte-compile-user-init-file ()
  "Compiles .emacs."
  (let ((byte-compile-warnings '(unresolved)))
    ;; in case compilation fails, don't leave the old .elc around:
    (let ((byte-init-file (byte-compile-dest-file user-init-file)))
      (when (file-exists-p byte-init-file)
        (delete-file byte-init-file))
      (byte-compile-file user-init-file))))

(defun smart-tab ()                     ; UNTESTED
  "This smart tab is minibuffer compliant: it acts as usual in
the minibuffer. Else, if mark is active, indents region. Else if
point is at the end of a symbol, expands it. Else indents the
current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (hippie-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (hippie-expand nil)
        (indent-for-tab-command)))))

(defun indent-or-expand (arg)           ; UNTESTED
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
      (indent-according-to-mode)))

(defmacro defun-dummy (configuration-case &rest source-funs) ; UNTESTED
  "Creates dummy functions if not bound and associates a possible
file to load it. Useful for keybindings referring to functions without
autoloads (generally non site-lisp files like `configuration' files
aren't autoloaded). `fun-source' is a suite of alists where CAR is a
source file and CDR is a function name or a list of function name.
If `configuration-case' is T, try to load the file as a configuration
file; display a message otherwise."
  (if configuration-case                ; FIXME: refactor it
      `(progn
         ,@(mapcar
            #'(lambda (x)
                (let ((z (listify (cdr x))))
                  `(progn
                     ,@(mapcar #'(lambda (y)
                                   `(unless (fboundp ',y) (defun ,y () (interactive) (when (y-or-n-p ,(format "%s: function missing. Load the configuration file `%s'? " (symbol-name y) (car x))) (progn (require (intern ,(car x)))(call-interactively ',y)))))) z))))
            source-funs))
    `(progn
       ,@(mapcar
          #'(lambda (x)
              `(unless
                   (fboundp ',x)
                 (defun ,x ()
                   (interactive)
                   (message "%s: function missing." ,(symbol-name x)))))
          source-funs))))

(eval-after-load "cl-lib"
  '(progn

     (defun bind-key (map spec def)       ; UNTESTED
       (let ((key (cl-typecase spec
                    (vector spec)
                    (string (read-kbd-macro spec))
                    (t (error "wrong argument")))))
         (define-key map key def)))

     (defmacro bind-keys (maps &rest bindings) ; UNTESTED
       "Map keys from a list to one or multiple KEYMAPs."
       (if (and (listp maps) (eq (car maps) 'quote)) ; TODO: think about a smarter combinator
           `(progn
              ,@(mapcar #'(lambda (map)
                            `(bind-keys ,map ,@bindings)) (cadr maps)))
         (when (> (length bindings) 1)
           `(progn
              (bind-key ,maps ,(car bindings) ,(cadr bindings))
              (bind-keys ,maps ,@(cddr bindings))))))

     (defun crazycode/indent-and-complete () ; UNTESTED
       "Indent line and Complete if point is at end of left a leave word."
       (interactive)
       (cond
        ;; hippie-expand
        ((looking-at "\\_>")
         ;; skip message output
         (cl-flet ((message (format-string &rest args) nil))
           (hippie-expand nil))))
       ;; always indent line
       (indent-for-tab-command)))) ; for example, for Ruby indent issues

(defun my-tab-expansion-switcher ()     ; UNTESTED
  (local-set-key [tab] 'indent-or-expand))

(defun mars/kill-this-buffer ()         ; UNTESTED
    "Kill the current buffer now"
    (interactive)
    ;; issue with 'frame-live and eternal buffers managed
    (if (member (buffer-name (current-buffer)) mars/eternal-buffer-list)
        (bury-buffer)
        (kill-buffer (current-buffer))))

(defun plain-buffer-list (&optional buflist limit) ; UNTESTED
  "Return a list of non-empty buffers. If BUFLIST exists, filter it.
LIMIT is the minimal buffer size to consider a buffer to be plain."
  (let (plains)
      (dolist (buf (or buflist (buffer-list)))
          (when (> (buffer-size buf) (or limit 0))
            (push buf plains)))
      (nreverse plains)))

(defun first-line-of-buffer ()
  "Return as a string the first line in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (buffer-substring (point-min) (point))))

(defun count-buffers (&optional display-anyway)
  "Display or return the number of buffers."
  (interactive)
  (let
      ((buf-count (length (buffer-list))))
    (if (or (called-interactively-p 'any) display-anyway)
        (message "%d buffers in this Emacs" buf-count))
    buf-count))

(defun kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  ;; deprecated 'save-excursion+'set-buffer => 'with-current-buffer
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))

(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;;; 'previous-line invokes a new line when we reached the top of the buffer
(defadvice previous-line (before next-line-at-end activate) ; UNTESTED
  "Insert an empty line when moving up from the top line."
  (when (and next-line-add-newlines (= arg 1)
             (save-excursion (beginning-of-line) (bobp)))
    (beginning-of-line)
    (newline)))

;;; remove last unwanted char
(defun remove-last-unwanted-char (lookup-string) ; UNTESTED
           "Remove -, _, ? or ! characters at the end of a string."
           (while (and (> (length lookup-string) 0)
                       (string-match-p (substring lookup-string -1) "-_?!=")) ; keep '+'
             (setq lookup-string (substring lookup-string 0 -1)))
           lookup-string)

;;; NOTIFIERS
(defun display-external-pop-up (title msg &optional icon sound) ; UNTESTED
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played.
   libnotifyd version: djcb@http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html"
  (setq title (replace-regexp-in-string "'" "\\'" title t t))
  (setq msg (replace-regexp-in-string "'" "\\'" msg t t))
  (cond
   ((eq mars/notifier 'growl) (shell-command (format "growlnotify -a Emacs -t '%s' -m '%s' 2> /dev/null" title msg)))
   ((eq mars/notifier 'terminal-notifier) (shell-command (format "terminal-notifier -sound default -sender org.gnu.emacs -title '%s' -message $'%s'" title msg)))
   ((eq mars/notifier 'notify-send)
    (progn
      (when sound
        (shell-command
         (format "mplayer -really-quiet %s 2> /dev/null" sound)))
      (when (eq window-system 'x)
        (shell-command (format "notify-send %s '%s' '%s'"
                               (if icon (format "-i %s" icon) "")
                               title  msg)))))
   (t (message (format "defs::notification: %s: %s" title msg)))))

(defun output-to-growl (msg)            ; UNTESTED
  (let ((fname (make-temp-file "/tmp/growlXXXXXX")))
    (with-temp-file fname
      (let ((coding-system-for-write 'utf-16))
        (insert (format "tell application \"GrowlHelperApp\" notify with name \"Emacs\" title \"Emacs alert\" description «data utxt%s» as Unicode text application name \"Emacs\" end tell"
                        (osd-text-to-utf-16-hex msg))))
      (shell-command (format "osascript %s" fname)))
    (delete-file fname)))

(defun osd-text-to-utf-16-hex (text)    ; UNTESTED
  (let* ((utext (encode-coding-string text 'utf-16))
         (ltext (string-to-list utext)))
    (apply #'concat
           (mapcar (lambda (x) (format "%02x" x)) ltext))))

;;; VARS DEPENDENT
;;;
(eval-after-load "vars"
  '(progn
     ;; - safe loaders
     ;;
     (defun safe-load (library)     ; UNTESTED
       "Secure load a library."
       (condition-case err
       (load-library library)
     (error
      (progn
        (message "Failed to load %s: %s" library err)
        (sleep-for emacs/breaktime-on-error)))))

     (defun conf-locate (conf)      ; UNTESTED
       "Locate a configuration file. Normally out of the `LOAD-PATH'."
       (let ((path (mapcar #'(lambda (x) (expand-file-name x mars/local-root-dir)) mars/local-conf-path)))
     (locate-library conf nil path)))

     (defun conf-load (conf)        ; UNTESTED & SOON UNUSED
       "Load a configuration file."
       (let ((found (conf-locate conf)))
     (when found
       (load-file found))))      ; load compiled version if any

     ;; - autoloads generator
     ;;
     (defun generate-loaddefs ()    ; UNTESTED
        "Fetch your 'SITE-LISP 's LOADDEFS or create it."
    ;; FIXME: work for one site-lisp dir for instance!!
    (mapc #'(lambda (x)
          (let ((mars/loaddefs
                 (expand-file-name "loaddefs.el"
                                   (expand-file-name x mars/local-root-dir))))
            (unless (and (file-exists-p mars/loaddefs)
                 (not renew-autoloads-at-startup)) ; force to renew in some case even if `loaddefs' exists
              (load "update-auto-loads")
              (update-autoloads-in-package-area)) ; adds 'update-auto-loads autoloads in loaddefs too
                                        ; updates CEDET autoloads for CEDET directories
            (safe-autoloads-load mars/loaddefs)))
          mars/site-lisp-path))

     ;; - custom builder
     ;;
     (defun build-custom-file-name (subdirectory-in-data &optional general) ; UNTESTED
       "A custom file for different emacsen and system version or `NIL' if
none (and not makeable). If `GENERAL' is true, it will refer to or creates
a simple `custom.el'."
       (joindirs mars/local-root-dir
                 mars/personal-data
                 subdirectory-in-data
                 (if general "custom.el"
                   (format "%s-%s-%s.el"
                           (which-emacs-i-am)
                           (number-to-string emacs-major-version)
                           (replace-regexp-in-string "/" "-" (symbol-name system-type))))))

     (defun safe-build-custom-file (subdirectory-in-data &optional general) ; UNTESTED
       (let ((file (build-custom-file-name subdirectory-in-data general)))
     (unless (file-exists-p file)
       (make-directory (file-name-directory file) t))
     file))

     ;; - byte compile
     (defun auto-byte-compile-file-maybe () ; UNTESTED
       (interactive)
       (when (and auto-byte-compile
                  buffer-file-name)
         (byte-recompile-file buffer-file-name nil 0)))

     (defun byte-compile-new-files-maybe (one-or-more-files) ; UNTESTED
       (interactive)                    ; add find-file entry here
       (let ((files (listify one-or-more-files)))
           (mapc
            #'(lambda (file)
                (when (file-exists-p file)
                  (let ((byte-file (byte-compile-dest-file file)))
                    ;; faster to check compiled file before getting
                    ;; local variables then force byte-recompilation
                    (when (or (not (file-exists-p byte-file))
                              (file-newer-than-file-p
                               file byte-file))
                      (when (with-temp-buffer
                              (insert-file-contents file t)
                              (normal-mode t)
                              (let ((definition (assoc 'auto-byte-compile
                                                       file-local-variables-alist)))
                                (if definition
                                    (cdr definition)
                                  (progn
                                    (message (prin1-to-string (hack-dir-local-variables)))
                                    (hack-dir-local-variables))))) ; check dir locals too
                        (byte-recompile-file file t 0)))))) ; force byte-recompile b/c checked
            files)))

     (defun byte-compile-new-files-in-directories-maybe (one-or-more-directories) ; UNTESTED
       (interactive)
       (let ((dirs (listify one-or-more-directories)))
         (mapc #'(lambda (dir)
                   (byte-compile-new-files-maybe (directory-files dir t "^[^.].*\\.el\\'")))
               dirs)))

     (defun auto-byte-compile-save-hook () ; UNTESTED
       (add-hook 'after-save-hook #'auto-byte-compile-file-maybe t t))))

;;; MISCELLANEOUS UTILITIES
;;;
(defun listify (l)
  (if (listp l) l (list l)))

(defun flatten (list)           ; UNTESTED
  (cond ((atom list) list)
        ((listp (car list)) (append (flatten (car list)) (flatten (cdr list))))
        (t (append (list (car list)) (flatten (cdr list))))))

(defun which-emacs-i-am ()      ; UNTESTED
  (if (string-match "XEmacs\\|Lucid" emacs-version) "xemacs" "gnuemacs"))

(defun make-file-executable-if-script () ; UNTESTED
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (let* ((current-mode (file-modes (buffer-file-name)))
              (add-mode (logand ?\111 (default-file-modes))))
         (or (/= (logand ?\111 current-mode) 0)
             (zerop add-mode)
             (set-file-modes (buffer-file-name)
                             (logior current-mode add-mode))))))

(defun byte-compile-emacs-config ()     ; UNTESTED (FIXME: should work for confs)
  "Byte compile the current file, when saved, if the file is part of
the personal Emacs Lisp configuration directory."
  (let ((current-file (buffer-file-name)))
    (let ((config-path (listify mars/local-conf-path)))
      (dolist (config-dir
               (mapcar #'(lambda (x)
                           (expand-file-name x mars/local-root-dir))
                       config-path))
        (let ((string-length (length config-dir)))
          (when (and (eq (compare-strings config-dir 0 string-length
                                          current-file 0 string-length) 1)
                     (string-match "\\.el\\'" current-file))
            (byte-compile-file current-file)))))))

(defun byte-recompile-home ()           ; UNTESTED
  (interactive)
  (let ((path (listify mars/local-conf-path)))
    (mapcar #'(lambda (x) (progn
                           (message (prin1-to-string path))
                           (byte-recompile-directory
                            (expand-file-name x mars/local-root-dir))))
            path)))

(defmacro disable-eyecandies (&rest modes) ; UNTESTED
  `(progn ,@(mapcar #'(lambda (x) `(if-bound-call ,x -1)) modes)))

 (defmacro mars/activate-state-in-modes (modes context activator &rest state) ; UNTESTED
   `(eval-after-load ',context
      '(progn
         ,@(mapcar #'(lambda (x)
                       `(,activator ,x ,@state)) modes)))) ; see. 'VIM-EVERYWHERE

(defun swap-windows ()                  ; UNUSED
 "If you have 2 windows, it swaps them."
 (interactive)
 (cond ((not (= (count-windows) 2))
        (message "You need exactly 2 windows to do this."))
       (t
        (let* ((w1 (first (window-list)))
               (w2 (second (window-list)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)))))

(defun rename-file-and-buffer (new-name) ; UNTESTED
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
          (progn
            (rename-file name new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)           ; UNTESTED
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
          (filename (buffer-file-name))
          (dir
             (if (string-match dir "\\(?:/\\|\\\\)$")
                       (substring dir 0 -1) dir))
          (newname (expand-file-name name dir)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

(defun the-the ()
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)
  ;; This regexp is not perfect
  ;; but is fairly good over all:
  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
    (message "End of buffer")))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10). --xah"
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun to-unix-eol (fpath)              ; UNTESTED
  "Change file's line ending to unix convention."
  (let (mybuffer)
    (setq mybuffer (find-file fpath))
    (set-buffer-file-coding-system 'unix) ; or 'mac or 'dos
    (save-buffer)
    (kill-buffer mybuffer)))

(defun execvp (&rest args)              ; UNTESTED
  "Simulate C's execvp() function.
Quote each argument separately, join with spaces and call shell-command-to-string to run in a shell."
  (let ((cmd (mapconcat 'shell-quote-argument args " ")))
    (shell-command-to-string cmd)))

(defun dired-2unix-marked-files ()      ; UNTESTED
  "Change to unix line ending for marked (or next arg) files."
  (interactive)
  (mapc 'to-unix-eol (dired-get-marked-files)))

(defun emacs-process-p (pid)            ; UNTESTED
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (format "/proc/%s/cmdline" (int-to-string pid))))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (search-forward "emacs" nil t)
          pid)))))

(defun elisp-files-in-below-directory (directory)
  "Fetch all emacs lisp files in `directory' and all its subdirectories.
Known as FILES-IN-BELOW-DIRECTORY seen in `http://www.rattlesnake.com/intro/Files-List.html'."
  (interactive "DDirectory name: ")
  (let (el-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    (while current-directory-list
      (cond
       ((equal ".el" (substring (car (car current-directory-list)) -3))
        (setq el-files-list
              (cons (car (car current-directory-list)) el-files-list)))
       ((eq t (car (cdr (car current-directory-list))))
        (unless
            (equal "."
                   (substring (car (car current-directory-list)) -1))
          (setq el-files-list
                (append
                 (elisp-files-in-below-directory
                  (car (car current-directory-list)))
                 el-files-list)))))
      (setq current-directory-list (cdr current-directory-list)))
    el-files-list))

(defun start-named-server (name)        ; TODO: cleanup UNTESTED
  "Start a server named 'name' - ensure only one server of that
`NAME' is running"
  (interactive "sServer Name: ")
  (require 'server)
  (let ((server-name name)
        (mk-server-socket-file (expand-file-name name server-socket-dir)))
    (when (file-exists-p mk-server-socket-file)
      (delete-file mk-server-socket-file))
    (server-start)
    (add-hook 'kill-emacs-hook
      `(lambda ()
         (when (file-exists-p ,mk-server-socket-file)
           (delete-file ,mk-server-socket-file))))))

(defun add-hook-once (hook function &optional append local) ; UNTESTED
  "Same as `add-hook', but FUN is only run once.
   Also contrary to `add-hook', this is not idempotent."
  ;; FIXME: need to check if `function' was already added to the hook.
  (let ((code (list 'lambda)))
    (setcdr code `(() (,function) (remove-hook ',hook ',code ',local)))
    (add-hook hook code append local))) ; XEmacs compatible hook management

;;; Three functions using mars/title-as-markdown-title
(defun mars/markdown-header-first ()
  (interactive)
  (mars/title-as-markdown-title 61))    ; (string-to-char "=")

(defun mars/markdown-header-second ()
  (interactive)
  (mars/title-as-markdown-title 45))    ; (string-to-char "-")

(defun mars/underline-with-char (char)  ; UNTESTED (but TESTED w/ mars/title-as-markdown-title)
  (interactive (list (read-from-minibuffer "Char: ")))
  (when (= 0 (length char))
    (error "Need a character"))
  (setq char (aref char 0))
  (mars/title-as-markdown-title char))

;;; create a markdown title and remove trailing space at the end
;;; [hondana@gmx.com: the same on vim/shortkeys -- Vi bindings are
;;; __+ ('+' is shifted equal) for '=' first level header and ___
;;; ('_' is shifted minus) for '-' second level header]
(defun mars/title-as-markdown-title (char)
  (end-of-line)
  (insert " ")                          ; needed to prevent backward-delete-char-hungry not to be too hungry FIXME: replace those two lines
  (backward-delete-char-hungry 1)       ; delete trailing space between the eol and the last word

  (insert "\n"
          (make-string (- (point-at-eol)
                          (point-at-bol))
                       char))
  (forward-line)
  (unless (bolp) (newline))
  (unless (eolp) (newline)))        ; TODO: add trailing lines elimination & new space if needed

;;; delete trailing whitespace backward and forward
(defun backward-delete-char-hungry (arg &optional killp) ; UNTESTED
      "*Delete characters backward in \"hungry\" mode.
    See the documentation of `backward-delete-char-untabify' and `backward-delete-char-untabify-method' for details."
      (interactive "*p\nP")
      (let ((backward-delete-char-untabify-method 'hungry))
        (backward-delete-char-untabify arg killp)))

(defun delete-horizontal-space-forward () ; adapted from `delete-horizontal-space'
      "*Delete all spaces and tabs after point."
      (interactive "*")
      (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun mars/point-in-comment ()              ; UNTESTED
  "Determine if the point is inside a comment. As the first comment character is not a comment, we check the next point (idea by http://www.emacswiki.org/emacs/BackToIndentationOrBeginning).
"
  (interactive)
  (let ((point-type (syntax-ppss-context (syntax-ppss)))
        (next-point-type (save-excursion
                           (syntax-ppss-context (syntax-ppss (+ (point) 1))))))
    (when (or (eq 'comment point-type)
              (eq 'comment next-point-type)) t)))

;;; whack whitespace until the next word
(defun whack-whitespace (&optional arg)
  "Delete all white space from point to the next word.  With prefix ARG delete across newlines as well.  The only danger in this is that you don't have to actually be at the end of a word to make it work.  It skips over to the next whitespace and then whacks it all to the next word."
  (interactive "P")
  (unless (eobp)
    (if arg
        (progn
          (re-search-forward "[ \t\n]+" nil t)
          (replace-match " " nil nil)
          ;; gone to far ?
          (when (eobp)
            (call-interactively #'delete-backward-char 1)))
      (progn
        (re-search-forward "[ \t]+" nil t)
        (replace-match "" nil nil)))))

;;; no line wrap this buffer (for special buffer like MIME-VIEW)
(defun no-line-wrap-this-buffer-internal () ; UNTESTED
  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows nil)
  (setq truncate-lines nil))
(defun no-line-wrap-this-buffer ()      ; UNTESTED
  (lexical-let ((buf (current-buffer)))
    (add-hook-once                      ; defined here
     'post-command-hook
     (lambda ()
       (when (buffer-name buf)          ; avoid bad usage
           (with-current-buffer buf
             (no-line-wrap-this-buffer-internal)))))))

;;; fix amazon URL
(defun fix-amazon-url ()                ; UNTESTED
  "Minimizes the Amazon URL under the point.  You can paste an Amazon
URL out of your browser, put the cursor in it somewhere, and invoke
this method to convert it."
  (interactive)
  (and (search-backward "http://www.amazon.com" (point-at-bol) t)
       (search-forward-regexp
        ".+/\\([A-Z0-9]\\{10\\}\\)/[^[:space:]\"]+" (point-at-eol) t)
       (replace-match
        (format "http://www.amazon.com/o/asin/%s%s"
                (match-string 1)
                (match-string 3)))))

(defun fix-google-search-url ()         ; UNTESTED
  "Minimizes a Google search URL under the point."
  (interactive)
  (and (search-backward-regexp "http://www\\.google\\.[a-z]\\{2,3\\}/search" (point-at-bol) t)
       (search-forward-regexp
        ".+[&?]\\(q=[a-zA-Z0-9%+]+\\)\\(&.+\\)*" (point-at-eol) t)
       (replace-match
        (format "http://www.google.com/search?%s"
                (match-string 1)))))

(defun compile-adjust-variable ()       ; UNTESTED
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (format "gcc -O2 -Wall -o %s %s"
                   (file-name-sans-extension file)
                   file)))))

(defmacro define-hash-region (name hash-type) ; UNTESTED
  `(defun ,name (start end)
     (interactive "r")
     (save-excursion
       (let ((s (,hash-type (buffer-substring start end))))
         (delete-region start end)
         (insert s)))))

;; (define-hash-region sha1-region sha1) ; deprecated: in sha1-el
;; (define-hash-region md5-region md5)

;; From http://www.tbray.org/ongoing/When/200x/2003/09/27/UniEmacs
(defun one-quote () "" (interactive) (insert ?')) ; UNTESTED
(defvar sq-state 'nil "In single-quotes?") ; UNTESTED
(defvar dq-state 'nil "In double quotes?") ; UNTESTED
(defun insert-special (c)               ; UNTESTED
  "Insert special characters, like so:
   s => open/close single quotes
   d => open/close double quotes
   ' => apostrophe
   a => <a href=
   i => <img src=
   & => &amp;
   < => &lt;
   - => mdash
   . => center-dot"
  (interactive "cInsert special (s d ' a i & < - .)")
  (cond
   ((= c ?s)
    (if sq-state
    (progn
      (insert (decode-char 'ucs #x2019))
      (setq sq-state 'nil))
      (insert (decode-char 'ucs #x2018))
      (setq sq-state 't)))
   ((= c ?d)
    (if dq-state
    (progn
      (insert (decode-char 'ucs #x201d))
      (setq dq-state 'nil))
    (insert (decode-char 'ucs #x201c))
    (setq dq-state 't)))
   ((= c ?') (insert (decode-char 'ucs #x2019)))
   ((= c ?a)
    (progn
      (if (> (current-column) 0) (newline-and-indent))
      (insert "<a href=\"\">")
      (backward-char 2)))
   ((= c ?i)
    (progn
      (if (> (current-column) 0) (newline-and-indent))
      (insert "<img src=\"\" alt=\"\" />")
      (backward-char 11)))
   ((= c ?&) (insert "&amp;"))
   ((= c ?<) (insert "&lt;"))
   ((= c ?-) (insert (decode-char 'ucs #x2014)))
   ((= c ?.) (insert (decode-char 'ucs #xb7)))))

(defun fix-and-indent ()                ; UNTESTED
  "Clean up the code"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; scroll one line at a time
(defun scroll-one-line-up (&optional arg) ; UNTESTED
  "Scroll the selected window up (forward in the text) one line (or N lines)."
  (interactive "p")
  (scroll-up (or arg 1)))
(defun scroll-one-line-down (&optional arg) ; UNTESTED
  "Scroll the selected window down (backward in the text) one line (or N)."
  (interactive "p")
  (scroll-down (or arg 1)))

(defun fix-code-region (from to)        ; UNTESTED
  "Indent by 4 spaces the selected code region for blog."
  (interactive
   (list (region-beginning) (region-end)))
  (goto-char from)
  (while (< (point) to)
    (beginning-of-line)
    (indent-to-column 4)
    (forward-line))
  (indent-to-column 4))

;; From http://sami.samhuri.net/2007/6/23/emacs-for-textmate-junkies
(defun wrap-region (left right beg end) ; UNTESTED
  "Wrap the region in arbitrary text, LEFT goes to the left and
RIGHT goes to the right."
  (save-excursion
    (goto-char beg)
    (insert left)
    (goto-char (+ end (length left)))
    (insert right)))

(defun wrap-region-with-tag (tag beg end) ; UNTESTED
  "Wrap the region in the given HTML/XML tag using `wrap-region'. If any
attributes are specified then they are only included in the opening tag."
  (interactive "*sTag (including attributes): \nr")
  (let* ((elems (split-string tag " "))
         (tag-name (car elems))
         (right (format "</%s>" tag-name)))
    (if (= 1 (length elems))
        (wrap-region (format "<%s>" tag-name) right beg end)
      (wrap-region (format "<%s>" tag) right beg end))))

(defun wrap-region-with-tag-or-insert () ; UNTESTED
  (interactive)
  (if (and mark-active transient-mark-mode)
      (call-interactively 'wrap-region-with-tag)
    (insert "<")))

(defun wrap-region-or-insert (left right) ; UNTESTED
  "Wrap the region with `wrap-region' if an active region is
marked, otherwise insert LEFT at point."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (wrap-region left right (region-beginning) (region-end))
    (insert left)))

(defmacro wrap-region-with (left &optional right) ; UNTESTED
  "Returns a function which, when called, will interactively
wrap-region-or-insert using left and right."
  (if right
      `(lambda ()
         (interactive)
         (wrap-region-or-insert ,left ,right))
      `(lambda ()
         (interactive)
         (wrap-region-or-insert ,left ,left))))

;;;======================================================================
;;; From: Jim Janney <jjanney@xmission.xmission.com>
;;; in comp.emacs
;;; show and hide comments in program code.
;; TODO: test it
(defun overlay-comments(beg end attrs)  ; UNTESTED
  (save-excursion
    (goto-char beg)
    (let (state comment-start comment-end overlay)
      (while (nth 4 (setq state
                          (parse-partial-sexp (point) end nil nil nil t)))
        (goto-char (nth 8 state))
        (setq comment-start (point))
        (forward-comment 1)
        (setq comment-end (point))
        (while (= (char-before comment-end) ?\n)
          (setq comment-end (1- comment-end)))
        (setq overlay (make-overlay comment-start comment-end))
        (mapc #'(lambda (attr)
                (overlay-put overlay (car attr) (cdr attr)))
              attrs)))))

(defun hide-comments()                  ; UNTESTED
  (interactive)
  (overlay-comments (point-min)
                    (point-max)
                    '((category . comment) (invisible . comment))))

(defun show-comments()                  ; UNTESTED
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (if (eq (overlay-get ov 'category) 'comment)
        (delete-overlay ov))))

;;; OTHER TIPS
;;;

;; http://homepages.inf.ed.ac.uk/s0243221/emacs/ - delete current line a la vi with `dd'
(defun nuke-line ()
  "Kill an entire line, including the trailing newline character"
  (interactive)
  ;; Store the current column position, so it can later be restored for a more
  ;; natural feel to the deletion
  (let ((previous-column (current-column)))
    ;; Now move to the end of the current line
    (end-of-line)
    ;; Test the length of the line. If it is 0, there is no need for a
    ;; kill-line. All that happens in this case is that the new-line character
    ;; is deleted.
    (if (= (current-column) 0)
        (delete-char 1)
        ;; This is the 'else' clause. The current line being deleted is not zero
        ;; in length. First remove the line by moving to its start and then
        ;; killing, followed by deletion of the newline character, and then
        ;; finally restoration of the column position.
        (progn
          (beginning-of-line)
          (kill-line)
          (delete-char 1)
          (move-to-column previous-column)))))

(defun remove-control-m ()              ; UNTESTED
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward (format "%s$" (char-to-string 13)) (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d ^M removed from buffer." remove-count))))))

(defun toggle-narrow()                  ; UNTESTED
  "Narrow to region, iff region is marked, otherwise widen"
  (interactive)
  (if mark-active
      (narrow-to-region (region-beginning) (region-end))
    (widen)))                           ; may replace C-x n n / C-x n w

;; Copyright (C) 1997, 1998 Thien-Thi Nguyen
(defun another-line ()                  ; UNTESTED
  "Copy line, preserving cursor column, and increment any numbers found.
This should probably be generalized in the future."
  (interactive)
  (let* ((col (current-column))
     (bol (progn (beginning-of-line) (point)))
     (eol (progn (end-of-line) (point)))
     (line (buffer-substring bol eol)))
    (beginning-of-line)
    (while (re-search-forward "[0-9]+" eol 1)
      (let ((num (string-to-number (buffer-substring
                  (match-beginning 0) (match-end 0)))))
    (replace-match (int-to-string (1+ num)))))
    (beginning-of-line)
    (insert line "\n")
    (move-to-column col)))

(defun get-posix-username ()            ; UNTESTED
  "May be use instead of 'USER-LOGIN-NAME if security is needed."
  (let ((me (execvp "whoami")))
    (if (stringp me)
        (replace-regexp-in-string "\n" "" me)
      nil)))

(defun mars/valid-string (clause basename substitute &optional prefix suffix) ; UNTESTED
  "Returns a string if the clause on it is true. Or returns the substitute
which is not affected by suffix optional argument."
  (let ((alternate (if prefix (concat prefix substitute) substitute)))
    (if (stringp basename)
        (let* ((name (if prefix (concat prefix basename)
                       basename))
               (complete-name (if suffix (concat name suffix)
                                name)))
          (if (funcall clause complete-name)
              complete-name
            alternate))
      alternate)))

(defun mars/username-file-if-any (root post default) ; UNTESTED
  (mars/valid-string 'file-exists-p
                     (user-login-name)
                     default
                     root
                     post))

;; terms
(defun eshell/clear ()                  ; UNTESTED
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun default-term ()                  ; UNTESTED
  "Launch the default shell in a *terminal* buffer of a new frame."
  (interactive)
  (let* ((buf (get-buffer "*terminal*"))
         (win (when buf (get-buffer-window buf t))))
    (if win
        (progn
          (when window-system
            (raise-frame (window-frame win)))
          (select-window win))
      (progn
        (when window-system
          (select-frame (make-frame)))
       (term shell-file-name)))))

;; lennart-borgman libraries' loaders
(defun nxhtml-loader ()                 ; UNTESTED
  "load NXHTML library and all lennart-borgman libraries (including Unit Test additions named ERT2)"
  (unless (fboundp 'nxhtml-list-loaded-features)
    (let ((name       "nxhtml")
          (nxhtml-autostart (locate-library "autostart")))
      (when nxhtml-autostart
        (when (eq (string-match name
                                (file-name-nondirectory
                                 (directory-file-name
                                  (file-name-directory nxhtml-autostart)))) 0) ; check "nxhtml" pattern is in the last directory of NXHTML-AUTOSTART path
          (load nxhtml-autostart))))))
(defalias 'ert2-loader 'nxhtml-loader)

;; toggle-transparency
(defun toggle-transparency ()           ; UNTESTED
  (interactive)
  (let ((opa (if (/=  (or (cadr (frame-parameter nil 'alpha)) 100) 100)
                 '(100 100)
               '(97 92))))
    (modify-all-frames-parameters (list (cons 'alpha opa)))))

;; toggle-mode-line
(defun toggle-mode-line ()              ; UNTESTED
  "toggle the modeline on and off"
  (interactive)
  (setq mode-line-format
        (if (eq mode-line-format nil)
            (default-value 'mode-line-format)))
  (redraw-display))

(provide 'defs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defs.el ends here
