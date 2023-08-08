;;; church-inspired.el --- 
;; 
;; Filename: church-inspired.el
;; Description: Alonzo Church works including early lambda calculus (Lisp) & mathematic logic (Prolog)
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Wed Mar 16 20:38:43 2011 (+0100)
;; Version: 
;; Last-Updated: Fri May 31 17:18:21 2013 (+0200)
;;           By: Martial Boniou
;;     Update #: 27
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: quicklisp slime / redshank / prolog
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Note: for a standalone Common Lisp environment, don't boot SLIME
;;        manually but run it from the QUICKLISP directory with CLBUILD2
;;        script like this:
;; 
;;        $ EMACS="emacs -q -l church-inspired"
;;
;;        Shen is also a Lisp with logic kernel but it is mainly functional
;;        programming with deductive datatypes (see peyton-jones-family)
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

(provide 'one-language-spoken)
(require 'code-preamble)
(unintern 'one-language-spoken obarray)

;;; QUICKLISP SLIME (testing)
(add-lambda-hook 'emacs-startup-hook
  ;; check helper wasn't loaded at startup (using CLBUILD maybe)
  (if (and (boundp 'quicklisp-slime-helper-dist)
           (not (boundp 'mars/quicklisp-slime-rep)))
      (message "quicklisp slime not here (rep-bound: %S)" (boundp 'mars/quicklisp-slime-rep))
    (condition-case err
        (load (expand-file-name "slime-helper.el" mars/quicklisp-slime-rep))
      (error "church-inspired: quicklisp slime helper not loaded: %s" err))
    ;; NOTE: remember REPL may not be the same as the one loaded by another
    ;;       helper launcher like the CLBUILD2 internal slime script
    (eval-after-load "slime-autoloads"
      '(progn
         (when (and (boundp 'mars/common-lisp-program-name)
                    (executable-find mars/common-lisp-program-name))
           (setq inferior-lisp-program mars/common-lisp-program-name))
         (slime-setup '(slime-fancy slime-tramp slime-asdf))))
    (eval-after-load "slime"
      '(progn
         (slime-require :swank-listener-hooks)
         (when (el-get-package-is-installed "ac-slime")
           (mars/add-hooks '(slime-mode-hook slime-repl-mode-hook)
                           #'set-up-slime-ac)
           (eval-after-load "auto-complete"
             '(progn
                (add-to-list 'ac-modes 'slime-repl-mode)
                (add-to-list 'ac-modes 'lisp-mode))))))))

;;; REDSHANK
;; (automatic)

;;; PROLOG
;;
(when (locate-library "prolog-mode")
  (add-to-list 'auto-mode-alist '("\\.pl$"  . prolog-mode)) ; WARNING: Perl (FIXME: find header like prolog.vim?)
  ;; TODO: add .pro, .swi and .P extensions
  (add-to-list 'auto-mode-alist '("\\.yap$" . prolog-mode)))
(when (locate-library "mercury-mode")
  (add-to-list 'auto-mode-alist '("\\.mcy$" . mercury-mode))) ; or `.m' but Obj-C/matlab/Mathematica...
(eval-after-load "prolog"
  '(progn
     (setq prolog-system
           (cond
            ((and (boundp 'mars/prolog-system)
                  (not (null mars/prolog-system))) mars/prolog-system)
            ((executable-find "sicstus") 'sicstus)
            ((executable-find "yap") 'yap)
            ((executable-find "swipl") 'swi)
            (t 'gnu)))))                ; using gprolog by default

(provide 'church-inspired)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; church-inspired.el ends here
