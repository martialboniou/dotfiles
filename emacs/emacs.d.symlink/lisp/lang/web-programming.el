;;; web-programming.el --- 
;; 
;; Filename: web-programming.el
;; Description: Web Development
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sun Mar  6 21:14:44 2011 (+0100)
;; Version: 0.3
;; Last-Updated: Fri May 31 17:20:30 2013 (+0200)
;;           By: Martial Boniou
;;     Update #: 60
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: mweb / js / nxhtml (WARNING: unused)
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

;;; web-programming.el --- 
;; 
;; Filename: web-programming.el
;; Description: Web Development
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sun Mar  6 21:14:44 2011 (+0100)
;; Version: 
;; Last-Updated: Wed Oct 26 21:47:50 2011 (+0200)
;;           By: Martial Boniou
;;     Update #: 38
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: mweb / js / nxhtml (WARNING: unused)
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


(provide 'one-language-spoken)
(require 'code-preamble)
(require 'www)
(unintern 'one-language-spoken obarray)

(defvar nxhtml-env nil)

;; MWEB
(unless nxhtml-env
  (when (locate-library "mweb-example-config")
    (load-library "mweb-example-config")))

;;; JS (was ESPRESSO)
(unless (or (> emacs-major-version 23)
            (and (= emacs-major-version 23)
                 (> emacs-minor-version 1)))
  (add-to-list 'auto-mode-alist '("\\.js$"    . espresso-mode))
  (add-to-list 'auto-mode-alist '("\\.json$"  . espresso-mode)))
(add-lambda-hook 'js-mode-hook
  (imenu-add-menubar-index))
;; lintnode -> flymake-jslint
(when (boundp 'lintnode-rep)
  (when (file-accessible-directory-p lintnode-rep)
    (add-to-list 'load-path lintnode-rep)
    (when (locate-library "flymake-jslint")
      (mars/autoload '(("flymake-jslint" lintnode-hook)))
      (add-hook 'js-mode-hook #'lintnode-hook))))

(eval-after-load "flymake-jslint"
  '(progn
     (setq lintnode-location lintnode-rep
           lintnode-node-program (if (and (boundp 'js-comint-program-name)
                                          (file-executable-p js-comint-program-name))
                                     js-comint-program-name
                                   "node")
           lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))))
;; js-comint
(when (el-get-package-is-installed "js-comint")
  (add-lambda-hook 'js-mode-hook
    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
    (local-set-key "\C-cb" 'js-send-buffer)
    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
    (local-set-key "\C-cl" 'js-load-file-and-go))
  (eval-after-load "js-comint"
    '(progn
       (setq inferior-js-program-command (if (and (boundp 'js-comint-program-name)
                                                  (file-executable-p js-comint-program-name))
                                             js-comint-program-name
                                           "node")
             inferior-js-mode-hook (lambda ()
                                     (ansi-color-for-comint-mode-on)
                                     (add-to-list 'comint-preoutput-filter-functions
                                                  (lambda (output)
                                                    (replace-regexp-in-string ".*1G\.\.\..*5G" "..." (replace-regexp-in-string ".*1G.*3G" ">" output)))))))))

;;; PHP
(dolist (php-exts '("\\.php[s34]?\\'"
                    "\\.phtml\\'"
                    "\\.inc\\'"))
  (add-to-list 'auto-mode-alist (cons php-exts 'php-mode)))
;; provided php-mode 1.5.0 or Nxhtml version if enabled
(eval-after-load "php-mode"
  '(progn
     (eval-after-load "flymake"
       '(progn
          (defun flymake-php-init ()
            "Use php to check the syntax of the current file. -- sacha chua"
            (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
                   (local (file-relative-name temp (file-name-directory buffer-file-name))))
              (list "php" (list "-f" local "-l"))))
          (add-to-list 'flymake-err-line-patterns
                       '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))
          (defmacro add-php-flymake-masks (&rest extensions)
            `(progn
               ,@(mapcar (lambda (x)
                           `(add-to-list
                             'flymake-allowed-file-name-masks
                             '(,x flymake-php-init)))
                         extensions)))
          (add-php-flymake-masks "\\.php$" "\\.module$" "\\.install$" "\\.inc$" "\\.engine$")))))

;;; NXHTML - prefer MULTI-MODE-WEB but provide 'ERT2
(when nxhtml-env
  (nxhtml-loader))                      ; defined in `confs/defs'
(eval-after-load "nxhtml-mumamo-mode"
  '(progn
     (setq mumamo-chunk-coloring 'submode-colored)
     (setq nxhtml-skip-welcome t)
     (setq rng-nxml-auto-validate-flag nil)
     (add-to-list 'auto-mode-alist '("\\.\\(ctp\\|xml\\|htm\\|html\\|xslt\\|pt\\|zcm\\|xsl\\|rhtml\\|php\\|inc\\)\\'" . nxhtml-mumamo))
     (defvar hexcolour-keywords
       '(("#[abcdef[:digit:]]\\{6\\}"
          (0 (put-text-property
              (match-beginning 0)
              (match-end 0)
              'face (list :background
                          (match-string-no-properties 0)))))))
     (defun hexcolour-add-to-font-lock ()
       (font-lock-add-keywords nil hexcolour-keywords))
     (add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)))

(provide 'web-programming)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web-programming.el ends here
