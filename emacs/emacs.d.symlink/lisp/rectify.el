;;; rectify.el ---
;;
;; Filename: rectify.el
;; Description: Script correctors & expanders
;; Author: Martial Boniou
;; Maintainer:
;; Created: Sat Feb 19 22:39:36 2011 (+0100)
;; Version:
;; Last-Updated: Thu Dec 19 12:05:13 2013 (+0100)
;;           By: Martial Boniou
;;     Update #: 177
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: Error corrector (flymake) + smart expansion (hippie) +
;;              snippets (yas/jit) + auto-complete
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

(require 'code)

;;; FLYMAKE
;;
(mars/add-hooks '(php-mode-hook         ; TODO: check for slowing down
                  python-mode-hook)
                #'(lambda () (when buffer-file-name (flymake-mode 1))))
(eval-after-load "flymake"
  '(progn
     ;; M-S-h & M-S-l to go to previous & next error resp.
     (add-hook 'find-file-hook #'flymake-find-file-hook)
     ;; flymake cursor
     (condition-case err
         (require 'flymake-cursor)
       (error (message "%s" err)))
     ;; show help
     (defun my-flymake-show-help ()
       (when (get-char-property (point) 'flymake-overlay)
         (let ((help (get-char-property (point) 'help-echo)))
           (when help (message (format "%s" help))))))
     (add-hook 'post-command-hook #'my-flymake-show-help)
     ;; elisp case
     (defun flymake-elisp-init ()
       (unless (string-match "^ " (buffer-name))
         (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                              'flymake-create-temp-inplace))
                (local-file  (file-relative-name
                              temp-file
                              (file-name-directory buffer-file-name))))
           (list
            (expand-file-name invocation-name invocation-directory)
            (list
             "-Q" "--batch" "--eval"
             (prin1-to-string
              '(dolist (file command-line-args-left)
                (with-temp-buffer
                  (insert-file-contents file)
                  (condition-case data
                      (scan-sexps (point-min) (point-max))
                    (scan-error
                     (goto-char(nth 2 data))
                     (princ (format "%s:%s: error: Unmatched bracket or quote\n"
                                    file (line-number-at-pos))))))))
             local-file)))))
     (push '("\\.el\\'" flymake-elisp-init) flymake-allowed-file-name-masks)))

;;; HIPPIE-EXPAND
(eval-after-load "hippie-exp"           ; should be bound to C-p and use
                                        ; snippets* > semantic > dabbrev
                                        ; on a full system (*= yasnippet
                                        ; and your personal expansions).
  '(progn
     (eval-after-load "evil"
       '(setq evil-complete-next-func #'hippie-expand))
     (eval-after-load "vimpulse"        ; DEPRECATED
       '(progn
          (defalias 'vimpulse-abbrev-expand-before 'hippie-expand)
          (defadvice hippie-expand (around minibuffer-case (arg) activate)
            "Add minibuffer case. Inspired by `vimpulse-abbrev-expand-before'."
            (interactive "P")
            (if (minibufferp)           ; no need in Evil b/c evil-complete checks it first
                (minibuffer-complete)
              (progn
                ad-do-it))))) ; in order to use C-p thru vimpulse
     (setq hippie-expand-try-functions-list
           '(try-expand-all-abbrevs
             try-expand-dabbrev
             try-expand-dabbrev-all-buffers
             try-expand-dabbrev-from-kill
             try-complete-lisp-symbol-partially
             try-complete-lisp-symbol
             try-complete-file-name-partially
             try-complete-file-name
             try-expand-line
             try-expand-line-all-buffers
             try-expand-list
             try-expand-list-all-buffers
             try-expand-whole-kill))
     (setq hippie-expand-verbose t)     ; FIXME: for debug only

     ;; paredit
     (eval-after-load "paredit"
       '(progn
          (defadvice he-substitute-string (after he-paredit-fix)
            "Remove extra paren when expanding line in paredit."
            (if (and paredit-mode (equal (substring str -1) ")"))
                (progn (backward-delete-char 1) (forward-char))))))
     ;; additional try functions
     (defun try-special-dabbrev-substring (old)
       "Substring expansion. Useful in writing in Lisp."
       (let ((old-fun (symbol-function 'he-dabbrev-search)))
         (fset 'he-dabbrev-search
               (symbol-function 'special-dabbrev-substring-search))
         (unwind-protect
             (try-expand-dabbrev old)
           (fset 'he-dabbrev-search old-fun))))
     (defun special-dabbrev-substring-search (pattern &optional reverse limit)
       (let ((result ())
             (regpat (cond ((not hippie-expand-dabbrev-as-symbol)
                            (concat (regexp-quote pattern) "\\sw+"))
                           ((eq (char-syntax (aref pattern 0)) ?_)
                            (concat (regexp-quote pattern) "\\(\\sw\\|\\s_\\)+"))
                           (t
                            (concat (regexp-quote pattern)
                                    "\\(\\sw\\|\\s_\\)+")))))
         (while (and (not result)
                     (if reverse
                         (re-search-backward regpat limit t)
                       (re-search-forward regpat limit t)))
           (setq result (buffer-substring-no-properties (save-excursion
                                                          (goto-char (match-beginning 0))
                                                          (skip-syntax-backward "w_")
                                                          (point))
                                                        (match-end 0)))
           (if (he-string-member result he-tried-table t)
               (setq result nil))) ; ignore if bad prefix or already in table
         result))
     (defun calc/try-complete-result (arg)
       "Expands with `calc'. You must be on the end of the line. The line must end with: ' = ' [MicheleBini 2003/11/28]."
       (and
        (not arg) (eolp)
        (save-excursion
          (beginning-of-line)
          (when (and (boundp 'comment-start)
                     comment-start)
            (when (looking-at
                   (concat
                    "[ \n\t]*"
                    (regexp-quote comment-start)))
              (goto-char (match-end 0))
              (when (looking-at "[^\n\t ]+")
                (goto-char (match-end 0)))))
          (looking-at ".* \\(\\([;=]\\) +$\\)"))
        (save-match-data
          (require 'calc nil t))
        (let ((start (match-beginning 0))
              (op (match-string-no-properties 2)))
          (save-excursion
            (goto-char (match-beginning 1))
            (if (re-search-backward (concat "[\n" op "]") start t)
                (goto-char (match-end 0)) (goto-char start))
            (looking-at (concat " *\\(.*[^ ]\\) +" op "\\( +\\)$"))
            (goto-char (match-end 2))
            (let* ((b (match-beginning 2))
                   (e (match-end 2))
                   (a (match-string-no-properties 1))
                   (r (calc-do-calc-eval a nil nil)))
              (when (string-equal a r)
                (let ((b (save-excursion
                           (and (search-backward "\n\n" nil t)
                                (match-end 0))))
                      (p (current-buffer))
                      (pos start)
                      (s nil))
                  (setq r
                        (calc-do-calc-eval
                         (with-temp-buffer
                           (insert a)
                           (goto-char (point-min))
                           (while (re-search-forward
                                   "[^0-9():!^ \t-][^():!^ \t]*" nil t)
                             (setq s (match-string-no-properties 0))
                             (let ((r
                                    (save-match-data
                                      (with-current-buffer p ; (save-excursion)
                                        (goto-char pos)
                                        (and
                                         ;; TODO: support for line
                                         ;; indentation
                                         (re-search-backward
                                          (concat "^" (regexp-quote s)
                                                  " =")
                                          b t)
                                         (progn
                                           (end-of-line)
                                           (search-backward "=" nil t)
                                           (and (looking-at "=\\(.*\\)$")
                                                (match-string-no-properties 1))))))))
                               (if r (replace-match (concat "(" r ")") t t))))
                           (buffer-substring (point-min) (point-max)))
                         nil nil))))
              (and
               r
               (progn
                 (he-init-string b e)
                 (he-substitute-string (concat " " r))
                 t)))))))
     ;; add semantic
     (eval-after-load "senator"
       '(progn
          (setq hippie-expand-try-functions-list
                (cons 'senator-try-expand-semantic
                      hippie-expand-try-functions-list))))
     ;; load additional try functions
     (setq hippie-expand-try-functions-list
           (cons 'try-special-dabbrev-substring
                 (cons 'calc/try-complete-result
                       (delq 'calc/try-complete-with-calc-result
                             hippie-expand-try-functions-list))))
     ;; file-name expansion
     (defun special-expand-file-name-at-point ()
       "Use hippie-expand to expand the file-name."
       (interactive)
       (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name)))
         (call-interactively 'hippie-expand)))
     (bind-key (current-global-map) "C-S-p" #'special-expand-file-name-at-point)))

;;; YASNIPPETS / YAS-JIT
;; bound to 'C-p à la Vim
(if (fboundp 'yas-global-mode)
    (yas-global-mode 1)
  (message "rectify: yasnippet is not installed."))
(eval-after-load "yasnippet"
  '(progn
     (let ((local-yas-dir (joindirs mars/local-root-dir mars/personal-data "Snippets")))
       (when (file-exists-p local-yas-dir)
         (push local-yas-dir yas/snippet-dirs))) ; YAS/SNIPPET-DIRS generated by 'EL-GET
     ;; MuMaMo's tab
     (eval-after-load "nxhtml"
       '(progn
          (setq mumamo-map
                (let ((map (make-sparse-keymap)))
                  (bind-key map "C-M-<prior>" #'mumamo-backward-chunk)
                  (bind-key map "C-M-<next>"  #'mumamo-forward-chunk)
                  (bind-key map "\t"          #'yas/expand)
                  map))
          (mumamo-add-multi-keymap 'mumamo-multi-major-mode mumamo-map)))
     ;; add `yas' world to `hippie-expand'
     (eval-after-load "hippie-exp"
       '(progn
          (setq hippie-expand-try-functions-list
                (cons 'yas/hippie-try-expand hippie-expand-try-functions-list))))
     ;; special case
     ;; - org-mode 7+
     (eval-after-load "org"
       '(progn
          (defun mars/yas-in-org ()
            (bind-key org-mode-map "C-." #'yas/expand)) ; IMPORTANT: if <tab> is ``locked''
          (defun yas/org-very-safe-expand ()
            (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
          (add-lambda-hook 'org-mode-hook
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (bind-key yas/keymap "\t" #'yas/next-field))))
     ;; - ruby
     (eval-after-load "ruby"         ; FIXME: search if ruby is ok (maybe rinari/rhtml here)
       '(progn
          (defun yas/advise-indent-function (function-symbol)
            (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
                     ,(format
                       "Try to expand a snippet before point, then call `%s' as usual"
                       function-symbol)
                     (let ((yas/fallback-behavior nil))
                       (unless (and (interactive-p)
                                    (yas/expand))
                         ad-do-it)))))
          (yas/advise-indent-function 'ruby-indent-line)))))

;;; AUTOCOMPLETE
(eval-after-load "auto-complete-config"
  '(progn
     (let ((data-ac-dir (joindirs mars/local-root-dir mars/personal-data "ac-dict")))
       (unless (file-exists-p data-ac-dir)
         (make-directory data-ac-dir))
       (add-to-list 'ac-dictionary-directories data-ac-dir))
     (eval-after-load "semantic-ia"
       '(progn
          (defun ac-semantic-candidate (prefix)
            (if (memq major-mode
                      '(c-mode c++-mode jde-mode java-mode))
                (mapcar 'semantic-tag-name
                        (ignore-errors
                          (or (semantic-ia-get-completions
                               (semantic-analyze-current-context) (point))
                              (senator-find-tag-for-completion (regexp-quote prefix)))))))
          (defvar ac-source-semantic
            '((candidates . (lambda () (all-completions ac-prefix (ac-semantic-candidate ac-prefix)))))
            "Source for semantic.")
          (setq ac-sources (cons 'ac-source-semantic ac-sources))))))

(provide 'rectify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rectify.el ends here
