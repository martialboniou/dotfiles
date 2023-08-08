;;; vars.el ---
;;
;; Filename: vars.el
;; Description: 
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Wed Feb 23 11:22:37 2011 (+0100)
;; Version: 0.2
;; Last-Updated: Thu Jan 30 14:09:30 2014 (+0100)
;;           By: Martial Boniou
;;     Update #: 244
;; URL: 
;; Keywords: 
;; Compatibility: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: common variables for all configurations
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log: require cl-lib (either from 24.3+ recommended GNU Emacs
;;              or using `el-get' or `melpa' to install compatibility
;;              script)
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

;;; *MOOD*
;;
;; create *vim-now*/*dvorak-now*/*term-now*/*full-ammo-now* to force new 
;; vim/dvorak/term/el-get-sources options
;; remove .emacs.d/data/.launched to reset vim/dvorak/term/el-get-sources options at startup
(eval-when-compile (require 'cl-lib))

(require 'defs)

(defvar *i-am-a-vim-user* t
  "If true, Emacs will be Vimpyrized using `evil-mode'. (ViViVi, the beast.)
Set the boolean *vim-now* to shortcut this variable.")
(defvar *i-am-a-dvorak-typist* t
  "If true, additional Dvorak-friendly keybindings.
Set the boolean *dvorak-now* to shortcut this variable.")
(defvar *i-am-a-terminator* t
  "If true, C-h and C-w will be used as in any Un*x terminal.
Unless `*i-am-a-dvorak-typist*', `CUA' is activated in this
case to support additional cut-paste strategy (ie to replace
C-w as the usual 'cut' key binding).
Set the boolean *term-now* to shortcut this variable.")
(defvar *i-am-a-common-lisp-advocate* t
  "If true, require CL extensions (eg. EIEIO). Highly
recommended to boot CEDET faster.")
(defvar *i-am-an-emacsen-dev* t
  "If true, ELISP helpers will be loaded providing tools
to easily visualize ELISP macro expansions.")
(defvar *i-want-full-ammo* t
  "If true, use a recommended package list to use as `el-get-sources'
and `my-melpa-packages'. Otherwise, `el-get' and `melpa' don't install
any third party extension (you may want this in a minimal/offline
Emacs install). (See `packs-el-get.el' and `packs-melpa.el' files for
more info.)")
(defvar *i-might-be-a-saiki-komon* nil
  "If true, display an organizer window at startup.
A `saiki-komon' is a clan administrator inside gang
organization. (See `gtd' file for usage.)")
(defvar *i-can-do-yubitsume-now* t
  "If true, enable pinky-free helpers as STICKY-CONTROL.
A `yubitsume' is a japanese apologies' ritual which generally
consists in cutting off the portion of one's left little finger
above the top knuckle. In no-window-system mode, most of these
helpers is activated to work on most 70's designed VT where the
Ctrl-Shift combination is unknown.")
(defvar *i-like-shoji* t
  "If true, enable transparency in window-system mode. Shoji
are a japanese window divider consisting of translucent paper
over a frame.")

;;; GLOBAL PATH
;;
;; a `path' is a list of relative or absolute directories
;; a `dir'  is a relative directory
;; a `rep'  is an absolute directory
;;
;; DIRECTORY
(defvar mars/local-root-dir (if (boundp 'user-emacs-directory) user-emacs-directory "~/.emacs.d"))
(defvar mars/temporary-dir (file-name-as-directory "/tmp"))
;; DIRECTORY NAME
(defvar mars/personal-data "data")
;; DIRECTORIES
(defvar mars/local-conf-path (list "lisp" "lisp/init"))
(defvar mars/site-lisp-path (list "vendor")) ; subdirs are loaded in 'load-path too
;;; TIMERS
;;
(defvar emacs-load-start (current-time))
(defvar emacs/breaktime-on-error 3
  "Time (in seconds) of the pause on error.")
(defvar mars/fast-kill t)               ; (setq mars/fast-kill nil) to redo 'loaddefs on quit
;;; MISC
;;
(defvar auto-byte-compile nil
  "If true, automatic byte-compile emacs lisp files on file saving.")
(defvar renew-autoloads-at-startup nil) ; (re-)create autoloads (eg. after a change in `lisp/packs.el') FIXME: find a better process

;;; FIRST TIME
;;
(let ((first-file (joindirs mars/local-root-dir mars/personal-data ".launched")))
  (if (file-exists-p first-file)
      (load-file first-file)
    (progn
      ;; if you changes the `.emacs' defvar by hand, the `.launched' won't change your setting
      (unless (y-or-n-p "Would you like to type in a Vim-like environment? ")
        (setq *i-am-a-vim-user* nil))
      (unless (y-or-n-p "Do you type with a Dvorak keyboard? ")
        (setq *i-am-a-dvorak-typist* nil))
      (unless (y-or-n-p "C-h & C-w for deletion [new cut-paste]? ")
        (setq *i-am-a-terminator* nil))
      (unless (y-or-n-p "Do you want a complete installation of recommended packages? ")
        (setq *i-want-full-ammo* nil))
       (with-temp-file
          first-file
        (progn
          (insert (concat
                   ";; launched\n"
                   (unless *i-am-a-vim-user*
                       "(when (eq *i-am-a-vim-user* t) (setq *i-am-a-vim-user* nil))\n")
                   (unless *i-am-a-dvorak-typist*
                     "(when (eq *i-am-a-dvorak-typist* t) (setq *i-am-a-dvorak-typist* nil))\n")
                   (unless *i-am-a-terminator*
                     "(when (eq *i-am-a-terminator* t) (setq *i-am-a-terminator* nil))\n")
                   (unless *i-want-full-ammo*
                     "(when (eq *i-want-full-ammo* t) (setq *i-want-full-ammo* nil))\n"))))))))
;; force vim/dvorak/term options via special vars
(eval-after-load "defs"
  '(progn
     (mars/force-options
      (*vim-now*       . *i-am-a-vim-user*)
      (*dvorak-now*    . *i-am-a-dvorak-typist*)
      (*term-now*      . *i-am-a-terminator*)
      (*full-ammo-now* . *i-want-full-ammo*))))

;;; MINGW/MSYS COMPATIBILITY
;;
(when (member system-type '(ms-dos windows-nt))
  (defvar mingw-executable-binary-suffixes
    '(".exe" ".com" ".bat" ".cmd" ".btm" ""))
  (defun mingw-executable-find (command)
    "Search for COMMAND in `exec-path' and return the absolute file name.
Return nil if COMMAND is not found anywhere in `exec-path'."
    (let ((list exec-path)
          file)
      (while list
        (setq list
              (if (and (setq file (expand-file-name command (car list)))
                       (let ((suffixes mingw-executable-binary-suffixes)
                             candidate)
                         (while suffixes
                           (setq candidate (concat file (car suffixes)))
                           (if (and (file-exists-p candidate) ; `file-executable-p' is not convenient
                                    (not (file-directory-p candidate)))
                               (setq suffixes nil)
                             (setq suffixes (cdr suffixes))
                             (setq candidate nil)))
                         (setq file candidate)))
                  nil
                (setq file nil)
                (cdr list))))
      file))
  (defalias 'executable-find 'mingw-executable-find)
  (defvar explicit-shell-file-name "bash")
  (setq shell-file-name "bash"))

;;; NEXTSTEP/COCOA COMPATIBILITY
;;
(when (eq system-type 'darwin)
  ;; .MacOSX/environment, launchctl setenv or LSEnvironment option
  ;; are a mess; call the $SHELL instead
  (unless (getenv "TERM_PROGRAM")
    (let* ((path (shell-command-to-string
                 "$SHELL -c \"printf %s \\\"\\\$PATH\\\"\" 2>/dev/null"))
           (new-exec-path (split-string path path-separator)))
      (unless (null new-exec-path)
        (setq exec-path (append new-exec-path exec-path))
        (delete-dups exec-path)
        (setenv "PATH" (combine-and-quote-strings exec-path ":")))))
  ;; meta is meta / alt as meta as always be a nonsense
  (custom-set-variables
   '(ns-command-modifier 'meta)
   '(ns-option-modifier nil)))     ; Option works as compose key too

;;; NOTEBOOK CONTEXT
;; touch ~/.notebook if your computer need low resolution screen setup
(defvar *mars/notebook-context*
    (file-readable-p (expand-file-name ".notebook" "~"))
  "If true, this computer has low resolution. Useful for graphics & font settings.")

;;; DATA PATH
;;
(unless (< emacs-major-version 24)
  (defvar mars/custom-theme (joindirs user-emacs-directory "themes")
    "My personal themes are here."))
(defvar mars/cacert-file-name
  "/etc/ssl/certs/ca-certificates.crt"
  "CA Certifications' file as required by OpenSSL or gnutls. Mac OS X users
should change this variable to point the file with .cer extension as export by
the application 'Keychain Acess.app'.")
(defvar c-include-path nil "Additional include path for C programs.")
(defvar cpp-include-path nil "Additional include path for C++ programs")
(eval-after-load "adapter"              ; IMPORTANT: set those data path when `cl-lib' is installed (retrocompatibility) [required by `cl-reduce' and installed by `el-get' via PACKS-EL-GET in WALKER launched by ADAPTER or installed by another system in ADAPTER like PACKS or PACKS-MELPA or eventually installed by default in the current global site-lisp of any GNU Emacs 24.3+]
  '(let ((data-dir (joindirs mars/local-root-dir mars/personal-data)))
     ;; ensure data directories exist
     (mapc #'(lambda (dirs)
               (let ((newpath (cl-reduce #'joindirs (cons data-dir (listify dirs)))))
                 (unless (file-exists-p newpath)
                   (make-directory newpath t))))
           '(("cache" "semanticdb")
             ("cache" "bookmark")
             ("cache" "newsticker" "images")
             ("cache" "emms")
             ("cache" "eshell")
             ("cache" "image-dired")
             "Notes" "Insert" "BBDB" "elmo"))
     ;; change essential data and cache path to be located in `mars/personal-data'
     (let* ((data-cache (expand-file-name "cache" data-dir))
            (essential-data `((desktop-dir ,data-cache)
                              (desktop-base-file-name "desktop")
                              (desktop-base-lock-name ,(format "%s%s.lock"
                                                               (if (memq system-type
                                                                         '(ms-dos
                                                                           windows-nt
                                                                           cygwin)) "_" ".")
                                                               "desktop")) ; TODO: add a fun in defs named `prefix-hidden-file'
                              ,(unless (< emacs-major-version 24)
                                 `(mars/custom-theme ,(joindirs data-dir "Themes")))
                              (org-directory ,(joindirs data-dir "Notes"))
                              (auto-insert-directory ,(joindirs data-dir "Insert"))
                              (bbdb-file ,(joindirs data-dir "BBDB" (format "%s.bbdb" (user-login-name))))
                              (elmo-msgdb-directory ,(joindirs data-dir "elmo"))
                              (wl-temporary-file-directory ,(joindirs "~" "Downloads"))))
            ;; NOTE: `essential-cached-files' has the form '((VARS NAME-IN-DATA-DIR PREVIOUS-PATH-TO-DELETE) ...)
            (essential-cached-files `((revive-plus:wconf-archive-file "wconf-archive" ,(joindirs user-emacs-directory "wconf-archive"))

                                      (revive-plus:last-wconf-file "last-wconf" ,(joindirs user-emacs-directory "last-wconf"))
                                      (emms-directory "emms" ,(joindirs user-emacs-directory "emms"))
                                      (image-dired-dir "image-dired")
                                      (eshell-directory-name "eshell") ; force it to be a directory
                                      (newsticker-imagecache-dirname ("newsticker" "images"))
                                      (newsticker-cache-filename ("newsticker" "cache"))
                                      (newsticker-groups-filename ("newsticker" "groups"))
                                      (bookmark-default-file ("bookmark" "emacs.bmk"))
                                      (bmkp-bmenu-commands-file ("bookmark" "bmenu-commands.el"))
                                      (bmkp-bmenu-state-file ("bookmark" "bmenu-state.el"))
                                      (anything-c-adaptive-history-file "anything-c-adaptive-history-file")
                                      (ido-save-directory-list-file "ido-last")
                                      (savehist-file "history")
                                      (recentf-save-file "recentf")
                                      (tramp-persistency-file-name "tramp")
                                      (ac-comphist-file "ac-comphist.dat")
                                      (semanticdb-default-save-directory "semanticdb")
                                      (ede-project-placeholder-cache-file "projects.ede")
                                      (ecb-tip-of-the-day-file "ecb-tip-of-day.el")))
            ;; NOTE: `data-as-directory' contains pathname variables that need a path separator (say, '/')
            (data-as-directory '(eshell-directory-name))
            (cachize-fn #'(lambda (file-or-dir &optional obsolete-file-or-dir)
                            ;; generate name of a specific file or directory in cache
                            (let ((full-path (cl-reduce #'joindirs
                                                        (cons data-cache (listify file-or-dir)))))
                              ;; try to copy/purge obsolete file or directory content
                              ;; in a safe way; useful when the emacs' boot doesn't
                              ;; include this file
                              (when (and obsolete-file-or-dir
                                         (file-exists-p obsolete-file-or-dir))
                                (unless (file-exists-p full-path)
                                  (if (file-directory-p obsolete-file-or-dir)
                                      (copy-directory obsolete-file-or-dir
                                                      full-path t t t)
                                    (let ((new-dir (file-name-directory full-path)))
                                      (unless (file-directory-p new-dir)
                                        (make-directory new-dir t))
                                      (copy-file obsolete-file-or-dir full-path))))
                                (if (file-directory-p obsolete-file-or-dir)
                                    (delete-directory obsolete-file-or-dir t)
                                  (delete-file obsolete-file-or-dir)))
                              full-path))))
       ;; - then set path variable
       (mapc #'(lambda (ed)
                 (cl-destructuring-bind (variable path-name) ed
                   (when (and (symbolp variable) (stringp path-name))
                     (set variable path-name))))
             essential-data)
       ;; - and set cache files (and reset default ones if any)
       (mapc #'(lambda (ecf)
                 (cl-destructuring-bind (variable path-name &optional obsolete-path-name) ecf
                   (when (and (symbolp variable) (stringp path-name))
                     (set variable (funcall cachize-fn path-name obsolete-path-name)))))
             essential-cached-files)
       ;; special case: ensure `data-as-directory' files are in directory form
       (mapc #'(lambda (pvar)
                 (when (and (symbolp pvar) (boundp pvar) (stringp (symbol-value pvar)))
                   (set pvar (file-name-as-directory (symbol-value pvar)))))
             data-as-directory)
       ;; add custom theme personal path if any
       (when (and (boundp 'custom-theme-load-path)
                  (file-directory-p mars/custom-theme))
         (add-to-list 'custom-theme-load-path mars/custom-theme))
       (message "vars: variables set and cache directory built."))))

;;; PROGRAM NAMES
;;
(defvar mars/external-browser-name (cond ((eq system-type 'darwin) "open")
                                         ((eq system-type 'windows) "explorer.exe")
                                         ((eq window-system 'x) "xdg-open")
                                         ((executable-find "w3m") "w3m")
                                         (t "lynx"))
  "External Web browser name.")         ; FIXME: UNUSED
(defvar mars/haskell-program-name "ghci"
  "Haskell interpreter fullname.")
(defvar mars/common-lisp-program-name "sbcl"
  "Common Lisp program name.")
(defvar mars/prolog-system 'yap
  "Prolog default system")
(custom-set-variables
 '(tramp-default-method "ssh"))
(if (member system-type '(windows-nt ms-dos))
         (setq ssl-program-name "openssl" ; UNTESTED
               ssl-program-arguments '("s_client" "-host" host "-port" service "-no_ssl2" "-ign_eof")) ; "-verify" "O" "-CApath" "/etc/ssl/certs" "-quiet"
  (progn
    (when (eq system-type 'darwin) (setq mars/cacert-file-name "~/Documents/SSL/Certificates/Certificates.cer"))
    (setq ssl-program-name "gnutls-cli"
          ssl-program-arguments '("-p" service "--x509cafile" mars/cacert-file-name host))))
(defvar w3m-program-name "w3m"
  "The current program name of ye goo' olde W3M.")
(setq w3m-command w3m-program-name)     ; required by `anything-config'
(defvar lintnode-rep (joindirs "~" "Documents" "Code" "javascript" "lintnode")
  "The `flymake-jslint' repository to use node.js with Emacs. Recommended
installation: npm install express connect-form haml underscore .")
(defvar js-comint-program-name "node"
  "The default JavaScript console.")
(defvar factorcode-source-rep (joindirs "~" "Documents" "Code" "factor" "src" "factor")
  "The up-to-date factor source repository. The Emacs environment
named FUEL must be found in the `misc/fuel' subdirectory.")
(defvar mars/quicklisp-slime-rep (expand-file-name ".quicklisp" "~")
 "`slime-helper' directory.")
(defvar spelling-tool-name nil
  "The default program for spell checking. May be set to NIL.")
(defvar mars/texmf-dir (let ((texmf-dir (getenv "TEXMF_DIR")))
                         (cond ((not (or (null texmf-dir) (string= "" texmf-dir))) texmf-dir)
                               ((eq 'darwin system-type) "/usr/local/texlive/texmf-local")
                               ((not (member system-type '(ms-dos windows-nt))) "/usr/share/texmf-texlive") ; Linux, BSD, Cygwin...
                               (t "")))
  "The default location of `TEXMF_DIR' for Auctex installer.")
(defvar mars/notifier (if (eq system-type 'darwin)
                          (if (not (executable-find "terminal-notifier")) 'growl 'terminal-notifier)
                        (if (eq window-system 'x) 'notify-send 'none))
  "The UI notification daemon.")
(eval-after-load "c-eldoc"
  '(setq c-eldoc-includes "-I/usr/local/include/ -I/usr/include -I./ -I../"))

;;; SPECIFICS (<data>/sys/vars-<hostname>.el or <data>/vars-<hostname>.el)
;; DEPRECATED
(let ((sys-rep (joindirs mars/local-root-dir mars/personal-data "sys")))
  (unless (file-exists-p sys-rep)
    (make-directory sys-rep))
  (condition-case nil
      (load-file (joindirs sys-rep
                           (format "vars-%s-%s.el"
                                   (downcase system-name)
                                   (symbol-name system-type))))
    (error nil)))

(provide 'vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vars.el ends here
