;;; packs.el ---
;;
;; Filename: packs.el
;; Description: Maintain/install packages
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 19 12:33:51 2011 (+0100)
;; Version: 0.5-dying-code
;; Last-Updated: Thu Jan 30 14:37:06 2014 (+0100)
;;           By: Martial Boniou
;;     Update #: 541
;; URL: 
;; Keywords: 
;; Compatibility: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: MELPA + EL-GET + VENDOR (UNUSED)
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

;; packs loading is forbidden if not correctly called (from .emacs or configuration file)

(require 'noaccess)
(require 'walker)

;;; POPULATE SITE-LISP
;; - check if required subdirs are present in `mars/site-lisp-path'
;; - install in the first directory of `mars/site-lisp-path' (by default) if not found
;; - add required subdirs to LOAD-PATH if newly installed
;; IMPORTANT: required programs are:
;; - bash, touch;
;; - make, autoconf, rake (need ruby; `rvm' must be installed on Un*k-like);
;; - curl, cvs, svn, git, darcs (best installed with `cabal' coming with haskell-platform);
;; - hg/mercurial, bzr (both best installed with python egg manager `pip' (or `easy_install' via setuptools));
;; - tar, gzip, unzip.
;; Windows users, install mingw/msys/gnuwin and complete installation with ruby/gem, python/setuptools/bzr and haskell/cabal/darcs:
(defvar mars/site-lisp-package-tree nil
  "A package tree to get / install / tag additional packages in `mars/site-lisp-path'")

(setq mars/site-lisp-package-tree '(

                                    ;; (mhc           . ((get . "git clone git://github.com/yoshinari-nomura/mhc.git")
                                    ;;                   (install . "emacs-compile-directory emacs") ; ruby configure.rb; ruby make.rb is OBSOLETE (ftools dependencies)
                                    ;;                   (nosearch . ("icons" "ruby-ext" "samples" "xpm"))))
                                    ;; (rinari            . ((get . "git clone git://github.com/eschulte/rinari.git; cd rinari; git submodule init; git submodule update; cd ..") ; FIXME: no compilation yet!
                                    ;;                       (nosearch . ("doc" "test" "util/jump/test" "util/test"))))
                                    ;; TODO: fetch all the Python/Rope install process
                                    ;; WARNING: `python-mode' is *not* compatible with `wisent-python' in SEMANTIC => use gallina-python.el (provided here)
                                    ;; (python-mode        . ((get . "curl -L http://launchpad.net/python-mode/trunk/5.2.0/+download/python-mode-5.2.0.tgz | tar zx")
                                    ;;                        (install . "emacs-compile-directory .")
                                    ;;                        (nosearch . "website")))
                                    ;; WARNING: python.el is installable but `generate-file-autoloads' function complains about the naming form
                                    ;; (python.el          . ((get . "git clone git://github.com/fgallina/python.el")
                                    ;;                        (install . "emacs-compile-directory")))
                                    ;; (pylookup           . ((get . "git clone git://github.com/tsgates/pylookup.git") ; TODO: Issue 10 to merge for Python 2
                                    ;;                        (install . "emacs-compile-directory")
                                    ;;                        (nosearch . "python-2.7.1-docs-html"))) ; wtf this archive here / need a fork?
                                    ;; (Pymacs             . ((get . "git clone git://github.com/pinard/Pymacs.git")
                                    ;;                        (install . "make; emacs-compile-directory") ; install Pymacs.egg in your python site-packges TODO: may require `eshell' for root install and su/sudo
                                    ;;                        (alert . "You may install PYMACS python part by running:\n\tpip install Pymacs\n\nThen don't forget to install rope and ropemode using pip (or easy_install)")
                                    ;;                        (nosearch . ("build" "contrib" "Pymacs" "tests"))))
                                    ;; (ropemacs           . ((get . "hg clone https://bitbucket.org/agr/ropemacs")
                                    ;;                        (install . "python setup.py install")
                                    ;;                        (nosearch . ".")))
                                    ;; ;; (bbdb               . ((get . "cvs -d \":pserver:anonymous:@bbdb.cvs.sourceforge.net:/cvsroot/bbdb\" checkout bbdb")
                                    ;; ;;                        (install . "autoconf;./configure;cd lisp;make autoloadsc;cd ..; make") ; soon DEPRECATED / IMPORTANT: problem in configure on Windows: `emacs' path with spaces
                                    ;; ;;                        (nosearch . ("autom4te.cache" "bits/bbdb-filters/doc" "html" "tex" "texinfo" "utils"))))
                                    (cedet              . ((get . "bzr checkout bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk cedet")
                                                           (install . "make")
                                                           ;; (nosearch . ("cogre/templates" "cogre/tests" "ede/templates" "semantic/doc" "semantic/tests" "srecode/templates" "testprojects" "www"))
                                                           ;; (cedet . ".") ; in order to use CEDET-UPDATE-AUTOLOADS from previous version to parse EIEIO's DEFCLASS in .loaddefs
                                                           (nosearch . "."))) ; IMPORTANT: cedet-devel-load.el does the job
                                    (ecb                . ((get . "cvs -z3 -d:pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb checkout -P ecb")
                                                           ;; TODO replacement : https://github.com/alexott/ecb/blob/master/Makefile
                                                           (install . "sed 's/\\\(load-path nil)\\\).*\\\(\\\" > ecb-compile-script\\\)/\\1(setq ecb-version-check nil)\\2/' Makefile > Makefile.tmp && mv Makefile.tmp Makefile && sed 's/echo \\\"\\\(load-file \\\\\\\"\\\$\\\(CEDET\\\)\\\/common\\\/cedet.el\\\\\\\"\\\)\\\"' Makefile > Makefile.tmp && Makefile.tmp Makefile && make CEDET=$(dirname `pwd`)/cedet && make autoloads EBATCH=\"emacs -batch -no-site-file -eval \\\"(add-to-list 'load-path \\\\\\\".\\\\\\\")\\\"\"") ; FIXME: assume cedet is in the same directory / windows users should use GNU bash / the first command disables compatibility check for CEDET; TODO: test it on Win32/64
                                                           (nosearch . ("ecb-images" "html"))
                                                           (noauto . ".")))
                                    (vm                 . ((get . "bzr branch lp:vm") ; REQUESTED
                                                           (install . "autoconf; ./configure; make")
                                                           (nosearch . ("autom4te.cache" "info" "pixmaps" "src"))))
                                    ;; stable org-mode
                                    ;; (nxhtml             . ((get . "bzr branch lp:nxhtml")
                                    ;;                        ;; the following version is not fully compatible with 23 due to deprecated face + a bug I found about remove-hooking Viper in `util/mumamo'
                                    ;;                        ;; (get . "curl http://ourcomments.org/Emacs/DL/elisp/nxhtml/zip/nxhtml-2.08-100425.zip > _nxhtml.zip; unzip _nxhtml.zip; rm _nxhtml.zip") ; `unzip' is not pipe-friendly
                                    ;;                        (install . "cd util; sed \"/remove-hook 'text-mode-hook 'viper-mode/d\" mumamo.el > mumamo_tmp.el; mv mumamo_tmp.el mumamo.el; cd ..; emacs-compile-directory;cd nxhtml;emacs-compile-directory -eval \"(add-to-list 'load-path \\\"..\\\")\";cd ../related;emacs-compile-directory -eval \"(add-to-list 'load-path \\\"..\\\")\";cd ../util;emacs-compile-directory -eval \"(add-to-list 'load-path \\\"..\\\")\";")
                                    ;;                        (nosearch . ("alts" "etc" "nxhtml" "related" "tests" "util" ".bzr"))
                                                           ;; (noauto . ".")))
                                    ;; (xwl-elisp          . ((get     . "git clone git://github.com/xwl/xwl-elisp.git")
                                    ;;                        (install . "make byte-compile"))) ; TODO: http://xwl.appspot.com/ (william xu) / include smart-operator
                                    ;; (pp-c-l . ((get . "git clone https://github.com/martialboniou/pp-c-l.git")))
                                    (emacs-revival  . ((get . "git clone https://github.com/martialboniou/emacs-revival.git")
                                                       (install . "make LISPDIR=/Users/mars/.emacs.d/vendor";(format "make LISPDIR=%s" (expand-file-name (car mars/site-lisp-path) mars/local-root-dir))
))))) ; TODO: verify sig on get ?

(setq mars/site-lisp-package-tree nil)

(defun mars/check-command (command &optional commands)
  "Check if command as executable in the `EXEC-PATH'. Throw an error by showing the
faulty COMMAND if COMMANDS is NIL or by listing all non-executable COMMAND and COMMANDS.
"
  (if (executable-find command)
      t
    (let ((error-sentence "Please install %s on your machine"))
      (if commands
          (let (renegades)
            (mapc #'(lambda (exe)
                      (condition-case err
                          (mars/check-command exe)
                        (error (push exe renegades))))
                  (delete command (delete-dups commands)))
            (let ((rng-string (if (null renegades)
                                  ""
                                (concat
                                 (mapconcat 'identity renegades ", ") " and "))))
              (error (format error-sentence (concat rng-string command)))))
        (error (format error-sentence command))))))

(defun mars/fetch-exec-in-command (command-line) ; TESTED
  "Fetch executables in a command. The executables are the list of all binary inside the shell
commands except local binary.
"
  (let ((commands (mapcar #'(lambda (x) (trim-string x))
                          (split-string command-line "[;&|]")))
        non-local-commands)
    (dolist (command commands)
      (when (and (not (= 0 (length command)))
                 (null (string-match "^./" command)))
        (push command non-local-commands)))
    (nreverse
     (delete-dups
      (mapcar #'(lambda (sent)
                  (car (split-string sent)))
              non-local-commands))))) ; shell-command-separator-regexp

(defun mars/execute-commands (sentence &optional additional-commands)
  "Execute sentence as a shell script after checking executables are in EXEC-PATH. Check
`additional-commands' on error.
"
  (mapc #'(lambda (com)
            (mars/check-command com additional-commands))
        (mars/fetch-exec-in-command sentence))
  (shell-command-to-string sentence))

(defun mars/fetch-exec-in-package-tree (package-tree &optional phase-list)
  "Fetch executables in a package tree. PHASE-LIST is the assoc keys to get shell commands in
a well-formed package tree. '(GET INSTALL) is the default PHASE-LIST. The executables are the
list of all binary inside the shell commands except local binary.
"
  (let (execs)
    (mapc
     #'(lambda (x)
         (mapcar #'(lambda (zone)
                     (let ((elt (cdr (assoc zone (cdr x)))))
                       (when (and elt (stringp elt))
                         (let ((bins (mars/fetch-exec-in-command elt)))
                           (unless (null bins)
                             (if execs
                                 (nconc execs bins)
                               (setq execs bins)))))))
                 (or phase-list (list 'get 'install))))
     package-tree)
    (delete-dups execs)))

(defun mars/populate-site-lisp (&optional only-mark)
  "Check if a package exists. If no package found, fetch it, install it
and add it and its subdirs to load-path. If `only-mark' then `renew-autoloads-at-startup'
becomes true when an installation is required. So it will re-create AUTOLOADS later
in `.emacs'. Otherwise AUTOLOADS are generated immediately."
    (when mars/site-lisp-path
      (setq renew-autoloads-at-startup nil)
      (let (broken-packages
            broken-tags
            (site-lisp-path (mapcar #'(lambda (x)
                                        (expand-file-name x mars/local-root-dir))
                                    mars/site-lisp-path))
            executables-in-play)      ; list of binaries used in the current package
        (mapc #'(lambda (x)
                  (let ((path site-lisp-path) (found nil) pending)
                    (while (and (not found) path)
                    (setq pending (pop path))
                    (when (file-directory-p (expand-file-name (symbol-name (car x))
                                                              pending))
                      (setq found t)))
                  (unless found
                   ;; getting & installing
                    (let ((pkg-dir (symbol-name (car x)))
                          (get-method (assoc 'get (cdr x))))
                      (if (not get-method)
                          (error "packs: no method to get the package named %s" pkg-dir)
                        (with-temp-buffer
                          (save-excursion
                            (save-restriction
                              (message "packs: %s installing..." pkg-dir)
                              (cd (car site-lisp-path)) ; install package in the first site-lisp
                              (let ((get-command (cdr get-method)))
                                (when (null executables-in-play)
                                  (setq executables-in-play (mars/fetch-exec-in-package-tree
                                                             mars/site-lisp-package-tree)))
                                (mars/execute-commands get-command executables-in-play)
                                (let ((install-method (assoc 'install (cdr x))))
                                  (when install-method
                                    (condition-case nil
                                        (progn
                                          (cd pkg-dir) ; unable to change directory if broken
                                          (let ((install-command (cdr install-method)))
                                            (mars/execute-commands install-command
                                                                   executables-in-play)))
                                      (error (add-to-list 'broken-packages (car x)))))))))))
                            ;; tagging
                            (let (tag-alerted)
                              (dolist (tag '(nosearch noauto cedet))
                                (let ((tag-method (assoc tag (cdr x))))
                                  (when tag-method
                                    (unless tag-alerted
                                      (setq tag-alerted t)
                                      (message "packs: %s tagging..." (car x)))
                                    (let ((tag-dirs (cdr tag-method)))
                                      (when (stringp tag-dirs)
                                        (setq tag-dirs (list tag-dirs)))
                                      (mapc #'(lambda (dir)
                                                (let* ((dirname (joindirs (car site-lisp-path)
                                                                          pkg-dir dir))
                                                       (file-tag (joindirs dirname
                                                                           (format ".%s"
                                                                                   (symbol-name tag)))))
                                                  (condition-case err
                                                      (progn
                                                        (message "create: %s" file-tag)
                                                        (with-temp-file ; touch file as marker tag
                                                            file-tag
                                                          nil))
                                                    (error
                                                     (add-to-alist tag (format "%s" file-tag)
                                                                   broken-tags)))))
                                            tag-dirs))))))
                            ;; add new directory tree to `load-path'
                            (mars/add-to-load-path (joindirs (car site-lisp-path)
                                                             pkg-dir)))
                            (setq renew-autoloads-at-startup t)
                            (message "packs: %s installed" (car x)))))
              mars/site-lisp-package-tree)
        (when (or broken-packages broken-tags)
          (message "%S" broken-packages )
          (message "%S" broken-tags )
         (with-temp-file (joindirs mars/local-root-dir
                                   mars/personal-data
                                   ".packs-errors.log")
           (insert ";; -*- emacs-lisp: t; no-byte-compile: t -*-\n")
           (mapc #'(lambda (x)
                     (unless (null x)
                       (insert (format "(setq %s %s)\n"
                                       (symbol-name (car x))
                                       (prin1-to-string (cdr x))))))
                 (list (cons 'broken-packages broken-packages)
                       (cons 'broken-tags broken-tags)))))
        (when (and (not only-mark)
                   renew-autoloads-at-startup) ; generate AUTOLOADS
          (let ((mars/loaddefs
                 (joindirs (car site-lisp-path) "loaddefs.el")))
            (load "update-auto-loads")
            (update-autoloads-in-package-area)
            (safe-autoloads-load mars/loaddefs)))
        (setq renew-autoloads-at-startup nil))))

(defun mars/renew-site-lisp ()
  "Renew PATH and AUTOLOADS."
  (interactive)
  (setq renew-autoloads-at-startup t)   ; to force AUTOLOADS' regeneration
                                        ; for files at the root of the first
                                        ; `mars/site-lisp-path' directory
  (mars/populate-site-lisp))
(mars/populate-site-lisp t)             ; populate now!

;;; ELPA
;; nothing

;; FIXME: rebuild `update-autoloads-in-package-area' to create/update loaddefs and `safe-autoloads-load' to load-file it in the case of a different `base' directory.

(provide 'packs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packs.el ends here
