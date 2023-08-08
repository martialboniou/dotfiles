;;; code.el ---
;;
;; Filename: code.el
;; Description: Development kit
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Feb 19 11:11:10 2011 (+0100)
;; Version: 
;; Last-Updated: Sun Apr 19 20:48:27 2015 (+0200)
;;           By: Martial Boniou
;;     Update #: 644
;; URL: 
;; Keywords: 
;; Compatibility: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: header2 + auto-insert (skeleton) / hideshow + hideshowvis /
;;              cedet & ecb-autoloads / electric-dot-and-dash /
;;              auto-pair-plus (for non-lisp modes) / paredit (slurp & barf
;;              for lisp modes) + highlight-parentheses / eldoc / comint /
;;              ack-and-a-half / cheat / org-babel / simple-call-tree / `lang'
;;
;;; Ideas: CEDET: https://github.com/alexott/emacs-configs/blob/master/rc/emacs-rc-cedet.el
;;
;;; Notes: cheat needs rubygems' cheat installed
;;         `lang' is the sub-directory for programming languages
;;         bundle packages' configuration and requirements
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
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

(provide 'programming)
(require 'preamble)
(require 'formats)

(eval-when-compile (require-if-located 'ecb)
                   (unless (fboundp #'autopair-mode)
                     (defun autopair-mode (arg) nil)))

(defun require-for-code (package)
  (require-if-located package (format "code: %s required" (symbol-name package))))

;;; LANGUAGES' CONFIGURATION PATH
(defvar lang-rep
  (condition-case nil
      (joindirs (file-name-directory load-file-name) "lang")
    (error (progn (message "code: unable to locate a lang directory") nil)))
  "Programming languages' configuration repository.")

;;; HEADER2 + AUTO-INSERT
(defun mars/create-header2-if-none (&optional update-me) (message "code: header2 is not installed"))
(eval-after-load "header2"
  '(progn
     (add-hook 'write-file-functions 'auto-update-file-header)
     (mars/add-hooks '(c-mode-common-hook emacs-lisp-hook) 'auto-make-header)
     (defun mars/create-header2-if-none (&optional update-me)
       "Creates a header if none. Updates header if UPDATE-ME is T."
       (interactive "P")
       (save-excursion
         (save-restriction
           (narrow-to-region 1 (min header-max (1- (buffer-size))))
           (let ((patterns file-header-update-alist))
             (setq last-command  nil)
             (let (pattern-found stop)
               (while (and (null stop) patterns)
                 (goto-char (point-min))
                 (when (re-search-forward (car (car patterns)) nil t)
                   (goto-char (match-end 0))
                   (if update-me
                       (progn
                         (when (null pattern-found)
                           (setq pattern-found t)
                           (message "Header updated"))
                         (funcall (cdr (car patterns))))
                     (setq stop t)))
                 (setq patterns  (cdr patterns)))
               (unless (or stop pattern-found)
                 (when (y-or-n-p "Would you like to make a file header? ")
                   (widen)
                   (goto-char (point-max))
                   (let ((end (point)))      ; remove extra lines
                     (backward-word 1)
                     (end-of-line)
                     (delete-region (point) end))
                   (newline 2)               ; add two lines to symmetrize
                   (make-header))))))))))
(defalias 'mars/create-header-if-none 'mars/create-header2-if-none)

;;; HIDESHOW + HIDESHOWVIS
;; Note: `vimpulse-compatibility' adds:
;; - `zm' to minimize (hs-hide-all)
;; - `zr' to restore  (hs-show-all)
;; if `*i-am-a-vim-user*'
;;
;; globalize hideshow
(require 'hideshow)
(eval-after-load "hideshow"
  '(progn
     (define-globalized-minor-mode global-hs-mode
       hs-minor-mode turn-on-hs-if-desired
       :initialize 'custom-initialize-delay
       :init-value t
       :group 'hideshow
       :version "23.0")
     (unless (el-get-package-is-installed "hideshowvis")
         (defun hideshowvis-enable () (message "code: hideshowvis is recommended and not installed.")))
     (defun turn-on-hs-if-desired ()
       (when (cond ((eq hs-global-modes t)
                    t)
                   ((eq (car-safe hs-global-modes) 'not)
                    (not (memq major-mode (cdr hs-global-modes))))
                   (t (memq major-mode hs-global-modes)))
         (let (inhibit-quit)
           (unless hs-minor-mode
             (hs-minor-mode 1)
             ;; hideshowvis case - IMPORTANT: not no-window-system friendly
             (when window-system
               (hideshowvis-enable))))))
     (defcustom hs-global-modes nil
       "All modes that need hideshow minor mode activated"
       :type '(choice (const :tag "none" nil)
                      (const :tag "all" t)
                      (set :menu-tag "mode specific" :tag "modes"
                           :value (not)
                           (const :tag "Except" not)
                           (repeat :inline t (symbol :tag "mode"))))
       :group 'hideshow)
     (custom-set-variables
      '(hs-global-modes (quote (c-mode
                                objc-mode
                                c++-mode
                                java-mode
                                perl-mode
                                php-mode
                                js-mode ; = espresso
                                emacs-lisp-mode
                                lisp-mode
                                scheme-mode
                                shen-mode
                                qi-mode
                                clojure-mode
                                ruby-mode    ; rinari ?
                                python-mode))))
     (global-hs-mode t)
     ;; ruby case
     (add-to-list 'hs-special-modes-alist
                  '(ruby-mode
                    "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
                    (lambda (arg) (ruby-end-of-block)) nil))
     ;; hs-hide-all-comments
     (defun hs-hide-all-comments ()
       "Hide all top level blocks, if they are comments, displaying only first line.
Move point to the beginning of the line, and run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'."
       (interactive)
       (hs-life-goes-on
        (save-excursion
          (unless hs-allow-nesting
            (hs-discard-overlays (point-min) (point-max)))
          (goto-char (point-min))
          (let ((spew (make-progress-reporter "Hiding all comment blocks..."
                                              (point-min) (point-max)))
                (re (concat "\\(" hs-c-start-regexp "\\)")))
            (while (re-search-forward re (point-max) t)
              (if (match-beginning 1)
                  ;; found a comment, probably
                  (let ((c-reg (hs-inside-comment-p)))
                    (when (and c-reg (car c-reg))
                      (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                          (hs-hide-block-at-point t c-reg)
                        (goto-char (nth 1 c-reg))))))
              (progress-reporter-update spew (point)))
            (progress-reporter-done spew)))
        (beginning-of-line)
        (run-hooks 'hs-hide-hook)))
     ;; toggle hiding even w/o hideshow
     (defun toggle-hiding (column)
       (interactive "P")
       (if hs-minor-mode
           (if (condition-case nil
                   (hs-toggle-hiding)
                 (error t))
               (hs-show-all))
         (toggle-selective-display column)))
     (defun toggle-selective-display (column)
       (interactive "P")
       (set-selective-display
        (or column
            (unless selective-display
              (1+ (current-column))))))
     (global-set-key (kbd "C-+") 'toggle-hiding)))

;;; CEDET
(eval-after-load "cedet"
  '(progn
     (global-ede-mode 1)                     ; project management
     ;; (semantic-load-enable-minimum-features) ; code helpers
     (if (boundp 'semantic-load-enable-excessive-code-helpers)
         (progn
           (semantic-load-enable-excessive-code-helpers)
           (global-semantic-tag-folding-mode))
       (setq semantic-default-submodes  ; 4 first are recommended
             '(global-semanticdb-minor-mode
               global-semantic-idle-scheduler-mode
               global-semantic-idle-summary-mode
               global-semantic-mru-bookmark-mode
               global-semantic-idle-completions-mode
               global-semantic-decoration-mode
               global-semantic-highlight-func-mode
               global-semantic-stickyfunc-mode)))
     (setq senator-minor-mode-name "SN"
           semantic-imenu-auto-rebuild-directory-indexes nil)
     ;; (global-srecode-minor-mode 1)           ; template insertion menu
     (require-for-code 'semantic-decorate-include)
     ;; smart completions
     (require-for-code 'semantic-ia)
     (setq-mode-local c-mode semanticdb-find-default-throttle
                      '(project unloaded system recursive))
     (setq-mode-local c++-mode semanticdb-find-default-throttle
                      '(project unloaded system recursive))
     (setq-mode-local erlang-mode semanticdb-find-default-throttle
                      '(project unloaded system recursive))
     ;; gcc
     (require-for-code 'semantic-gcc)
     (defun mars/semantic-add-system-include (list symbol)
       (mapc '(lambda (x)
                (when (stringp x)
                  (semantic-add-system-include x symbol)))
             list))
     (unless (null c-include-path)      ; defined in <conf>/vars
       (mars/semantic-add-system-include c-include-path 'c-mode))
     (unless (null cpp-include-path)    ; defined in <conf>/vars
       (mars/semantic-add-system-include c-include-path 'c++-mode))
     (require-for-code 'eassist)
     ;; general bindings [semantic-ia]
     (defun alexott/cedet-hook ()
       (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
       (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
       ;;
       (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
       (local-set-key "\C-c=" 'semantic-decoration-include-visit)

       (local-set-key "\C-cj" 'semantic-ia-fast-jump)
       (local-set-key "\C-cq" 'semantic-ia-show-doc)
       (local-set-key "\C-cs" 'semantic-ia-show-summary)
       (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
     (mars/add-hooks (cons 'c-mode-common-hook
                           (mars/generate-mode-hook-list '(lisp emacs-lisp scheme erlang)))
                     #'alexott/cedet-hook)
     ;; c-mode bindings [eassist]
     (defun alexott/c-mode-cedet-hook ()
       ;; (local-set-key "." 'semantic-complete-self-insert)
       ;; (local-set-key ">" 'semantic-complete-self-insert)
       (local-set-key "\C-ct" 'eassist-switch-h-cpp)
       (local-set-key "\C-xt" 'eassist-switch-h-cpp)
       (local-set-key "\C-ce" 'eassist-list-methods)
       (local-set-key "\C-c\C-r" 'semantic-symref))
     (add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)
     ;; hook & customs
     (add-lambda-hook 'semantic-init-hooks (imenu-add-to-menubar "TAGS")) ; FIXME: ? BUG
     (custom-set-variables
      '(semantic-idle-scheduler-idle-time 3)
      '(semantic-self-insert-show-completion-function (lambda () (semantic-ia-complete-symbol-menu (point))))
;;      '(global-semantic-tag-folding-mode t nil (semantic-util-modes))
)
     ;;     (global-semantic-folding-mode 1) ; FIXME: no more
     (defun mars/Qt ()                  ; TODO: temp var for test => to move to vars-<specific>.el
       (setq qt4-base-dir "/Library/Frameworks/QtCore.framework/Headers"))

     (require-for-code 'semanticdb-global)
     (require-for-code 'semanticdb-ectag)
     ;; GNU global support
     (eval-after-load "semanticdb-global"
       '(progn
          (semanticdb-enable-gnu-global-databases 'c-mode)
          (semanticdb-enable-gnu-global-databases 'c++-mode)
          (eval-after-load "semanticdb-ectag"
            '(progn
               (let ((c-includes '("/opt/local/include" "/usr/local/include" "/usr/brewery/include"))) ; FIXME: go to vars.el
                 (mapc (lambda (x)
                         (semantic-add-system-include x 'c-mode)
                         (semantic-add-system-include x 'c++-mode))
                       c-includes))))))
     ;; ecb or build autoloads via Makefile
     (when (locate-library "ecb-autoloads")
       (condition-case err
           (require 'ecb-autoloads)
         (error (progn
                  (message "code: ecb autoloads error: %s" err)
                  (let ((ecb-dir (locate-library "ecb")))
                    (if ecb-dir
                        (with-temp-buffer
                          (condition-case errare
                              (progn
                                (cd (file-name-directory ecb-dir))
                                (execvp "make" "autoloads")
                                (require 'ecb-autoloads))
                            (error
                             (message "code: make autoloads error: %s" errare))))
                      (message "code: ecb not found"))))))))) ; ecb is out of <site-lisp>/loaddefs

;;; ECB
(defun ecb-activated ()
  (if (boundp 'ecb-activated-window-configuration)
    (not (null ecb-activated-window-configuration))))
(defun ecb-activated-in-this-frame ()
  (and (ecb-activated)
       (eq (selected-frame)
           (when (boundp 'ecb-frame) ecb-frame))))
(eval-after-load "ecb"
  '(progn
     (when (> emacs-major-version 23)
       (setq ecb-version-check nil
             stack-trace-on-error nil)) ; for unstable version
     (setq ecb-tip-of-the-day nil)
     ;; ECB compatible `desktop'
     (eval-after-load "desktop"
      '(progn
         (push '(ecb-minor-mode nil) desktop-minor-mode-table)))))
(defun mars/toggle-ecb ()               ; make a better script w/o 'IF-BOUND-CALL
  (interactive)
  (if (locate-library "ecb")
      (if (ecb-activated)
          (if (ecb-activated-in-this-frame)
              (when (y-or-n-p "Stop ecb? ")
                (if-bound-call ecb-deactivate))
            (when (y-or-n-p "Switch ecb to this current frame? ")
              (let ((frm (selected-frame)))
                (if-bound-call ecb-deactivate)
                (select-frame-set-input-focus frm)
                (if-bound-call ecb-activate))))
        (when (y-or-n-p "Start ecb? ")
          (if-bound-call ecb-activate)))
    (message "code: please install ECB first.")))

;;; TEMPORARY LIST OF LANGAGES WITH LISP PARENS
;; TODO: to 'VARS ?
(defvar lispem-hooks (mars/generate-mode-hook-list
                      '(lisp emacs-lisp lisp-interaction clojure scheme shen qi slime-repl inferior-lisp inferior-qi)))

;;; ELECTRIC-DOT-AND-DASH
;; aka electric-dot-and-slash here
(require-if-located 'electric-dot-and-dash)
(eval-after-load "electric-dot-and-dash"
  '(mars/add-hooks lispem-hooks #'(lambda ()
                                    (local-set-key "." 'electric-dot-and-dash-dot)
                                    (local-set-key "/" 'electric-dot-and-dash-dash))))

;;; AUTO-PAIR-PLUS
;;
(when (el-get-package-is-installed "auto-pair-plus")
  (defvar autopair-hooks (cons 'c-common-hook
                               (mars/generate-mode-hook-list
                                '(python    ; python-mode's autopairs support is extended
                                        ; to work with single and triple quotes
                                  php scala erlang
                                  ruby     ; WARNING: 'RUBY-ELECTRIC must be informed
                                  latex))))
  (mars/add-hooks autopair-hooks #'(lambda ()
                                     (autopair-mode 1))))
(eval-after-load "autopair"
  '(progn
     ;; ruby-electric case
     (eval-after-load "ruby-electric"
       '(progn
          ;; inform 'RUBY-ELECTRIC to ignore AUTOPAIR matching chars
          (defadvice ruby-electric-setup-keymap (around soften-rel-for-autopair nil activate)
            (bind-keys ruby-mode-map
                       " " #'ruby-electric-space
                       "|" #'ruby-electric-bar
                       "RET" #'ruby-electric-return
                       "C-j" #'ruby-electric-return
                       "C-m" #'ruby-electric-return))))
     ;; python case
     (add-lambda-hook 'python-mode-hook
       (push '(?' . ?') (getf autopair-extra-pairs :code))
       (setq autopair-handle-action-fns
             (list #'autopair-default-handle-action
                   #'autopair-python-triple-quote-action)))
     ;; c++ case
     (add-lambda-hook 'c++-mode-hook
       (push ?{ (getf autopair-dont-pair :comment))
       (push '(?< . ?>) (getf autopair-extra-pairs :code)))
     ;; latex case
     (add-lambda-hook 'latex-mode-hook
       (set (make-local-variable 'autopair-handle-action-fns)
            (list #'autopair-default-handle-action
                  #'autopair-latex-mode-paired-delimiter-action)))
     (require-if-located 'auto-pair+)
     (setq autopair-autowrap t)))

;;; PAREDIT + HIGHLIGHT-PARENTHESES
(if (fboundp 'enable-paredit-mode)
    (mars/add-hooks lispem-hooks #'enable-paredit-mode)
  (message "code: paredit is not installed."))
(if (fboundp 'highlight-parentheses-mode)
    (mars/add-hooks lispem-hooks #'(lambda () (highlight-parentheses-mode t)))
  (message "code: highlight-parentheses is not installed."))
(eval-after-load "paredit"
  '(progn
     ;; autopair case (ie deactivate AUTOPAIR when PAREDIT is turned on)
     (eval-after-load "autopair"
       '(progn
          (defadvice paredit-mode (around disable-autopairs-around (arg) activate)
            "Disable autopairs mode if paredit-mode is turned on.
-- tim-c-harper@emacswiki"
            ad-do-it
            (if (null ad-return-value)
                (autopair-mode 1)
              (autopair-mode 0)))))
     ;; viper case
     (eval-after-load "vimpulse"
       ;; NOTE: `PAREDIT-VIPER-COMPAT' should be provided
       (when (functionp 'paredit-viper-compat)
         (add-lambda-hook 'paredit-mode-hook
           (paredit-viper-compat))))

     ;; no-window-system case
     ;; <C-S-*> is unavailable; use <f2>-9 and so on as defined in `shortcuts'
     ;; slime case
     (defun override-slime-repl-bindings-with-paredit ()
       (bind-key slime-repl-mode-map paredit-backward-delete-key nil))
     (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)))

;;; ELDOC
(mars/add-hooks (mars/generate-mode-hook-list
                 '(emacs-lisp lisp-interactive ielm)) #'turn-on-eldoc-mode)

;;; C-ELDOC (an alternative to CEDET for C code)
(when (el-get-package-is-installed "c-eldoc")
  (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode))

;;; ACK-AND-A-HALF
(custom-set-variables ; don't use the project directory but ask
 '(ack-and-a-half-root-directory-function nil))

;;; CHEAT
(eval-after-load "anything"
  '(progn
     (when (executable-find "cheat")    ; gem install cheat
       (defvar anything-c-source-cheat
         '((name . "Cheat Sheets")
           (init . (lambda ()
                     (unless (anything-candidate-buffer)
                       (with-current-buffer (anything-candidate-buffer 'global)
                         (call-process-shell-command
                          "cheat sheets" nil  (current-buffer))
                         (goto-char (point-min))
                         (forward-line 1)
                         (delete-region (point-min) (point))
                         (indent-region (point) (point-max) -2)))))
           (candidates-in-buffer)
           (action . (lambda (entry)
                       (let ((buf (format "*cheat sheet:%s*" entry)))
                         (unless (get-buffer buf)
                           (call-process "cheat" nil (get-buffer-create buf) t entry))
                         (display-buffer buf)
                         (set-window-start (get-buffer-window buf) 1))))))
       (defun anything-cheat ()
         "Preconfigured `anything' for cheat sheets."
         (interactive)
         (anything-other-buffer 'anything-c-source-cheat "*Anything cheat*")))))

;;; ORG BABEL
(eval-after-load "org"
  '(progn
     (org-babel-do-load-languages 'org-babel-load-languages
                                  '((emacs-lisp . t)
                                    (ruby . t)))))

;;; TODO: REPLACE W/ A NEWEST VERSION - VISUALIZE CALL TREE
;; (defvar sct-graphviz-dir
;;   (joindirs mars/local-root-dir mars/personal-data "Temporary" "Graphviz"))

;; (defun mars/simple-call-tree-view ()
;;   (interactive)
;;   (if-bound-call sct-graphviz))
;;(car-safe "/Users/mars/.emacs.d/data/Temporary/Graphviz/code.el.png")

;;; LANGUAGES
(when lang-rep
  (add-to-list 'load-path lang-rep)
  (unless (featurep 'one-language-spoken)
    (require 'pure-object)              ; for smalltalk / factor / io
    (require 'wiki-wiki)                ; for markup languages as mediawiki or markdown
    (require 'church-inspired)          ; for lisp (including scheme) & prolog
    (require 'peyton-jones-family)      ; for ML family and shenlanguage.org
    (require 'web-programming)          ; for web languages (mweb or nxhtml)
    (require 'python-357)))             ; for python 2 / 3

(provide 'code)
(unintern 'programming obarray)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; code.el ends here
