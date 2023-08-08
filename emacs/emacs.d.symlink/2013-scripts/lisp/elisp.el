;;; elisp.el ---
;;
;; Filename: elisp.el
;; Description: 
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Wed Feb 23 10:19:49 2011 (+0100)
;; Version: 
;; Last-Updated: Tue Dec 17 14:05:45 2013 (+0100)
;;           By: Martial Boniou
;;     Update #: 45
;; URL: 
;; Keywords: 
;; Compatibility: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Helpers for Emacs Lisp developers
;; Idea+code = Helmut@http://osdir.com/ml/help-gnu-emacs-gnu/2009-09/msg00668.html
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
(require 'etags)

;;; ELISP
;;
(defun elisp-disassemble (function)
  (interactive (list (function-called-at-point)))
  (disassemble function))

(defun elisp-pp (sexp)
  (with-output-to-temp-buffer "*Pp Eval Output*"
    (pp sexp)
    (with-current-buffer standard-output
      (emacs-lisp-mode))))

(defun elisp-macroexpand (form)
  (interactive (list (form-at-point 'sexp)))
  (elisp-pp (macroexpand form)))

(defun elisp-macroexpand-all (form)
  (interactive (list (form-at-point 'sexp)))
  (elisp-pp (macroexpand-all form)))

(defun elisp-push-point-marker ()
  (cond ((featurep 'xemacs)
         (push-tag-mark))
        (t (ring-insert find-tag-marker-ring (point-marker)))))

(defun elisp-pop-found-function ()
  (interactive)
  (cond ((featurep 'xemacs) (pop-tag-mark nil))
        (t (pop-tag-mark))))

(defun elisp-find-definition (name)
  "Jump to the definition of the function (or variable) at point."
  (interactive (list (thing-at-point 'symbol)))
  (cond (name
         (let ((symbol (intern-soft name))
               (search (lambda (fun sym)
                         (let* ((r (save-excursion (funcall fun sym)))
                                (buffer (car r))
                                (point (cdr r)))
                           (cond ((not point)
                                  (error "Found no definition for %s in %s"
                                         name buffer))
                                 (t
                                  (switch-to-buffer buffer)
                                  (goto-char point)
                                  (recenter 1)))))))
           (cond ((fboundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-function-noselect symbol))
                 ((boundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-variable-noselect symbol))
                 (t
                  (message "Symbol not bound: %S" symbol)))))
        (t (message "No symbol at point"))))

(defun elisp-bytecompile-and-load ()
  (interactive)
  (or buffer-file-name
      (error "The buffer must be saved in a file first"))
  (require 'bytecomp)
  ;; Recompile if file or buffer has changed since last compilation.
  (when (and (buffer-modified-p)
             (y-or-n-p (format "save buffer %s first? " (buffer-name))))
    (save-buffer))
  (let ((filename (expand-file-name buffer-file-name)))
    (with-temp-buffer
      (byte-compile-file filename t))))

;;; BONUS
;;
(defun jump-to-form ()
  (interactive)
  (if (featurep 'xemacs)
      (progn
        (forward-char 1)
        (let ((name (symbol-atpt))
              (file (progn (search-forward "\"" nil t 1)(thing-at-point
                                                         'filename))))
          (forward-char 1)
          (help-find-source-or-scroll-up (point))
          (switch-to-buffer (current-buffer))
          (kill-new name)
          (search-forward name)))
    (other-window 1)
    (forward-button 1)
    ;; (find-file (filename-atpt))
    (push-button)))

(defun mars/run-emacs (save-output &rest args)
  "Start `emacs'. Inspired by the function named `emacs' seen
in `util/ourcomments-util' of the `nxhtml' package."
  (recentf-save-list)
  (let* ((out-buf (when save-output
                    (get-buffer-create "call-process emacs output")))
         (buf-arg (or out-buf 0))
         (args-text (mapconcat 'identity (cons "" args) " "))
         ret
         (fin-msg "")
     (emacs-found (locate-file invocation-name (list invocation-directory) exec-suffixes)))
    (when out-buf
      (display-buffer out-buf)
      (setq fin-msg ". Finished.")
      (message "Started 'emacs%s' => %s. Locked until this is finished%s" args-text ret fin-msg)
      (redisplay))
    (setq ret (apply 'call-process emacs-found nil buf-arg nil args))
    (message "Started 'emacs%s' => %s%s" args-text ret fin-msg)
    ret))

(defun mars/boot-emacs (&optional args)
  (interactive "sArguments: ")
  (cl-flet ((check-valid-string (x) (and x (not (eq (string-match x "") 0)))))
    (eval
     `(apply 'mars/run-emacs nil ,(when (check-valid-string args) args)))))

(defun mars/vars-heading ()
  (interactive)
  (insert "(unless (boundp 'mars/local-root-dir) (condition-case nil (load (concat (file-name-directory load-file-name) %s)) (error %s)))\n" "vars" "Unable to get custom variables"))

;;; UNIT TESTS
;;
;; (ert2-loader)
                                        ; ensure to load lennart-borgman
                                        ; tools included in NXHTML library
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (font-lock-add-keywords
               nil
               '(("(\\(\\<ert-deftest\\)\\>\\s *\\(\\sw+\\)?"
                  (1 font-lock-keyword-face nil t)
                  (2 font-lock-function-name-face nil t))))))
(put 'ert-deftest 'lisp-indent-function 'defun)

(provide 'elisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elisp.el ends here
