;;; vt.el ---
;;
;; Filename: vt.el
;; Description: Virtual Terminal
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Wed Feb 23 13:14:51 2011 (+0100)
;; Version: 
;; Last-Updated: Wed Jun 26 12:57:04 2013 (+0200)
;;           By: Martial Boniou
;;     Update #: 25
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

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'adapter)

;;; MULTI-TERM
;;
(eval-after-load "multi-term"
  '(progn
     (defun term-send-quote ()
       (interactive)
       (term-send-raw-string "\C-v"))

     (defun term-send-M-x ()
       (interactive)
       (term-send-raw-string "\ex"))

     (defun term-send-backward-kill-word ()
       (interactive)
       (term-send-raw-string "\C-H"))

     (defun term-switch-to-shell-mode ()
       (interactive)
       (if (equal major-mode 'term-mode)
           (progn
             (shell-mode)
             (set-process-filter  (get-buffer-process (current-buffer)) 'comint-output-filter )
             (local-set-key (kbd "C-j") 'term-switch-to-shell-mode)
             (compilation-shell-minor-mode 1)
             (comint-send-input))
         (progn
           (compilation-shell-minor-mode -1)
           (font-lock-mode -1)
           (set-process-filter  (get-buffer-process (current-buffer)) 'term-emulate-terminal)
           (term-mode)
           (term-char-mode)
           (term-send-raw-string (kbd "C-l")))))

     (dolist
         (bind '(;; ("C-W"     . term-send-forward-word)
                 ;; ("C-w"           . term-send-backward-word)
                 ("C-<backspace>" . term-send-backward-kill-word)
                 ("C-<delete>"    . term-send-forward-kill-word)
                 ("C-k"           . term-send-raw)
                 ("C-y"           . term-send-raw)
                 ("C-c C-z"       . term-stop-subjob)
                 ("C-z"           . term-stop-subjob)
                 ;; work like urxvt tabbed
                 ("<S-down>"      . multi-term)
                 ("<S-left>"      . multi-term-prev)
                 ("<S-right>"     . multi-term-next)
                 ("C-v"           . term-paste)
                 ("C-j"           . term-switch-to-shell-mode)
                 ("C-`"           . (lambda () (interactive) (term-send-raw-string "\C- ")))))
       (add-to-list 'term-bind-key-alist bind))))
;; (setq system-uses-terminfo nil) ; if problem with $TERM

;;; SHELL MODE
;;
(add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

;;; ESHELL MODE
;;
(add-lambda-hook 'eshell-mode-hook
  (setenv "PATH" (mapconcat (lambda (dir) (or dir ".")) exec-path path-separator))
  (when (boundp 'eshell-path-env)
    (setq eshell-path-env (getenv "PATH"))))

;;; SH FILES
;;
(add-to-list 'auto-mode-alist '("\\.[zk]sh$" . sh-mode))

(provide 'vt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vt.el ends here

