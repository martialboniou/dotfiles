;;; peyton-jones-family.el --- 
;; 
;; Filename: peyton-jones-family.el
;; Description: category theory lambda static languages
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Wed Mar 16 20:40:26 2011 (+0100)
;; Version: 
;; Last-Updated: Tue Dec 17 15:05:05 2013 (+0100)
;;           By: Martial Boniou
;;     Update #: 61
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: haskell-mode / shen-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;  Note: Mercury has datatypes but depends on Prolog philosophy
;;         (see church-inspired)
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

;;; HASKELL MODE
(eval-after-load "haskell-mode"
  '(setq haskell-program-name mars/haskell-program-name))

;;; SHEN MODE
(when (locate-library "shen-mode")
  (add-to-list 'auto-mode-alist '("\\.shen\\'" . shen-mode)))
(eval-after-load "shen-mode"
  '(progn
     (require-if-located 'inf-shen)     ; inferior mode
     (eval-after-load "paredit"
       '(progn
          ;; disable paredit-semicolon to type final
          ;; disable paredit-backslash to type comment end tag normally
          (defun shen-paredit-mode-hook ()
            (bind-key paredit-mode-map "\\"
                      #'(lambda ()
                          (interactive)
                          (cond ((eq (preceding-char) 42) (insert "\\"))
                                (t (call-interactively #'paredit-backslash))))))
          (add-hook 'shen-mode-hook #'shen-paredit-mode-hook)
          ;; enable curlies in PAREDIT for type declarations
          (bind-key shen-mode-map "{" #'paredit-open-curly)
          (bind-key shen-mode-map "}" #'paredit-close-curly-and-newline)))))

(provide 'peyton-jones-family)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; peyton-jones-family.el ends here
