;;; crypto.el ---
;;
;; Filename: crypto.el
;; Description: Encryption & cracking tool
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Wed Mar  2 16:19:50 2011 (+0100)
;; Version: 
;; Last-Updated: Fri May 31 16:54:42 2013 (+0200)
;;           By: Martial Boniou
;;     Update #: 34
;; URL: 
;; Keywords: 
;; Compatibility: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: ps-ccrypt / EasyPG Assistant / HexviewMode
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

(require 'adapter)

;;; CPT
(when (locate-library "ps-ccrypt")
  (require 'ps-ccrypt))                 ; ccrypt 1.9 needed

;;; GPG
(unless (fboundp 'epa-decrypt-region)
  (require 'epa-file))                     ; GNU Emacs 23+ needed for symmetric enc.
(eval-after-load "epa-file"
  '(progn
     (epa-file-enable)
     (setq epa-file-select-keys nil)
     (eval-after-load "org"
       '(progn
          (require 'org-crypt)
          (org-crypt-use-before-save-magic)
          (setq org-tags-exclude-from-inheritance (quote ("crypt")))))))

;;; HEXVIEW
(when (locate-library "hexview-mode")
  (defun mars/hexedit ()
    (interactive)
    (let ((file (buffer-file-name (current-buffer))))
      (if (or (and file (file-exists-p file))
              (and (eq major-mode 'dired-mode)
                   (setq file (condition-case nil (dired-get-filename) (error nil)))
                   (not (with-temp-buffer (condition-case nil (cd file) (error nil))))))
          (if-bound-call hexview-find-file file)
        (call-interactively 'hexview-find-file)))))

(provide 'crypto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; crypto.el ends here
