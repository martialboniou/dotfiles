;;; ladybug.el --- 
;; 
;; Filename: ladybug.el
;; Description: 
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 26 00:15:15 2011 (+0100)
;; Version: 
;; Last-Updated: Wed Jun 12 14:09:29 2013 (+0200)
;;           By: Martial Boniou
;;     Update #: 32
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
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

(require 'elisp)
(require 'preamble)

(eval-when-compile (require 'ert))

;;; MARS/AUTO-TEST
(defun mars/auto-test ()
  (interactive)
  (let ((tests-dir (joindirs user-emacs-directory "lisp" "tests")))
     (if (not (file-accessible-directory-p tests-dir))
         (message "ladybug: no tests for this version.")
       (let ((tests-files (mapcar #'file-name-sans-extension
                                  (mapcar #'file-name-nondirectory
                                          (elisp-files-in-below-directory tests-dir)))))
         (if (null tests-files)
             (message "ladybug: no test files to run for this version.")
           (let ((emacs-run-cli (append '(mars/run-emacs nil "-Q" "-L" tests-dir)
                                        (cl-mapcan #'(lambda (a) (list "-l" a)) tests-files))))
             (eval (append emacs-run-cli '("-eval" "(call-interactively #'(lambda () (interactive) (ert-run-tests-interactively t \"*scratch*\")))")))))))))
;
;;; HIGHLIGHT-PARENTHESES
;; OBSOLETE
;; (eval-after-load "highlight-parentheses"
;;   '(progn
;;      (when (assoc 'highlight-parentheses-mode minor-mode-alist)
;;        (setcdr (assoc 'highlight-parentheses-mode minor-mode-alist) '(""))) ; IMPORTANT: don't display name in mode-line
;;      (setq autopair-handle-action-fns
;;            (list 'autopair-default-handle-action
;;                  '(lambda (action pair pos-before)
;;                     (hl-paren-color-update))))))

(provide 'ladybug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ladybug.el ends here
