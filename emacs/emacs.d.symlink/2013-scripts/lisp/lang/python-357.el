;;; python-357.el --- 
;; 
;; Filename: python-357.el
;; Description: Python
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sun Mar  6 23:42:52 2011 (+0100)
;; Version: 
;; Last-Updated: Tue Dec 17 15:07:35 2013 (+0100)
;;           By: Martial Boniou
;;     Update #: 143
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: fgallina-python & pyflakes / pylookup [ / Pymacs + Rope ]
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
(unintern 'one-language-spoken obarray)

;;; GALLINA-PYTHON - NON GNU VERSION
;; `python-mode' is better than `python' but `wisent-python' from SEMANTIC
;; is hardcoded for `python' => fgallina@github mixed the best of both worlds
(eval-after-load "python"
  '(progn
     ;; ensure python-mode-map exists
     (unless (boundp 'python-mode-map)
       (message "python-357: Warning, you're probably using the GNU version of `python' mode (not recommended).")
       (defvar python-mode-map (copy-keymap py-mode-map)))
     ;; indent
     (custom-set-variables '(indent-tabs-mode nil))
;;     (bind-key python-mode-map "<return>" #'newline-and-indent)
     ;; customize according to installed EGGs' binaries
     ;; inform flymake about pyflakes
      (eval-after-load "flymake"
       '(progn
          (when (executable-find "pyflakes")
            (defun flymake-pyflakes-init ()
              (let* ((temp-file (flymake-init-create-temp-buffer-copy
                                 'flymake-create-temp-inplace))
                     (local-file (file-relative-name temp-file
                                                     (file-name-directory buffer-file-name))))
                (list "pyflakes" (list local-file))))
            (add-to-list 'flymake-allowed-file-name-masks
                         '("\\.py\\'" flymake-pyflakes-init))))) ; this hook-function should be set in `confs/rectify'
     (cl-flet ((py-msg-advise (app) (message (format "python-357: missing application to complete python support:\n\tpip install %s" (prin1-to-string app)))))

       ;; ipython - interactive python toolkit
       ;; (if (executable-find "ipython")
       ;;     ;; (require 'ipython) ; for old python-mode
       ;;     (py-msg-advise 'ipython))
       (eval-after-load "ipython"
         '(eval-after-load "anything-ipython"
            '(use-anything-show-completion #'anything-ipython-complete
                                           '(length initial-pattern))))
       ;; pep8 - style checker
       ;; (unless (executable-find "pep8")
       ;;   (py-msg-advise 'pep8))
       ;; ;; pylint - code static checker
       ;; (unless (executable-find "pylint")
       ;;   (py-msg-advise 'pylint))
       ;; ;; ipdb - ipython-enabled debugger
       ;; (unless (executable-find "ipdb")
       ;;   (py-msg-advise 'ipdb))
       ;; (unless (executable-find "rope")
       ;;   (py-msg-advise 'rope))
       ;; (unless (executable-find "ropemode")
       ;;   (py-msg-advise 'ropemode))
       ;; ;; reimport - full featured reload
       )
     ;; hook
     (add-lambda-hook 'python-mode-hook
       (smart-operator-mode-on)
       ;; PRETTY-LAMBDA set up in `confs/formats'
       ;; AUTOPAIR's enhancements written in `confs/code'
       (setq imenu-create-index-function 'python-imenu-create-index)
       (add-lambda-hook 'local-write-file-hooks
         (save-excursion
           ;; HEADER2 case managed in `confs/formats'
           (delete-trailing-whitespace))))))

;;; PYLOOKUP
;; <C-c h> to display python lookup in python buffer
(eval-after-load "python"
  '(progn
     (bind-key python-mode-map "C-c h" #'pylookup-lookup)))

;; PYMACS + ROPE TODO: eval-after-load'ing correctly

;; TRY
;; (pymacs-load "ropemacs" "rope-")

;; (eval-after-load "python"
;;   '(progn
;;      (pymacs-load "ropemacs" "rope-")
;;      (setq ropemacs-enable-autoimport t)
;;      ;; yasnippet hack
;;      (eval-after-load "yasnippet"
;;        '(progn
;;           (defun prefix-list-elements (list prefix)
;;             (let (value)
;;               (nreverse
;;                (dolist (element list value)
;;                  (setq value (cons (format "%s%s" prefix element) value))))))
;;           (defvar ac-source-rope
;;             '((candidates
;;                . (lambda ()
;;                    (prefix-list-elements (rope-completions) ac-target))))
;;             "Source for Rope")
;;           (defun ac-python-find ()
;;             "Python `ac-find-function'."
;;             (require 'thingatpt)
;;             (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
;;               (if (null symbol)
;;                   (if (string= "." (buffer-substring (- (point) 1) (point)))
;;                       (point) nil) symbol)))
;;           (defun ac-python-candidate ()
;;             "Python `ac-candidates-function'."
;;             (let (candidates)
;;               (dolist (source ac-sources)
;;                 (if (symbolp source)
;;                     (setq source (symbol-value source)))
;;                 (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
;;                        (requires (cdr-safe (assq 'requires source)))
;;                        cand)
;;                   (if (or (null requires)
;;                           (>= (length ac-target) requires))
;;                       (setq cand
;;                             (delq nil
;;                                   (mapcar (lambda (candidate)
;;                                             (propertize candidate 'source source))
;;                                           (funcall (cdr (assq 'candidates source)))))))
;;                   (if (and (> ac-limit 1)
;;                            (> (length cand) ac-limit))
;;                       (setcdr (nthcdr (1- ac-limit) cand) nil))
;;                   (setq candidates (append candidates cand))))
;;               (delete-dups candidates)))
;;           (add-lambda-hook 'python-mode-hook
;;             (auto-complete-mode 1)
;;             (set (make-local-variable 'ac-sources)
;;                  (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
;;             (set (make-local-variable 'ac-find-function) 'ac-python-find)
;;             (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
;;             (set (make-local-variable 'ac-auto-start) nil))
;;           ;;Ryan's python specific tab completion
;;           (defun ryan-python-tab ()
;;             (interactive)
;;             (if (eql (ac-start) 0)
;;                 (indent-for-tab-command)))
;;           (defadvice ac-start (before advice-turn-on-auto-start activate)
;;             (set (make-local-variable 'ac-auto-start) t))
;;           (defadvice ac-cleanup (after advice-turn-off-auto-start activate)
;;             (set (make-local-variable 'ac-auto-start) nil))
;;           (bind-key python-mode-map "\t" #'ryan-python-tab)

;;           )) ))

(provide 'python-357)

;;; MAINTENANCE
;; (executable-find "hg")
;; mkdir /tmp/rope && cd /tmp/rope
;; hg clone http://bitbucket.org/agr/rope
;; hg clone http://bitbucket.org/agr/ropemacs
;; hg clone http://bitbucket.org/agr/ropemode
;; sudo easy_install rope
;; ln -s ../ropemode/ropemode ropemacs/
;; sudo easy_install ropemacs
;; (executable-find "python") to compile Pymacs with 'python setup.py install'
;; in (directory-file-name (locate-library "pymacs"))
;; (executable-find "easy_install") to compile and install rope in a convenient python path
;;+ pyflakes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python-357.el ends here
