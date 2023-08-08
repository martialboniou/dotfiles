;; -*- auto-byte-compile: nil -*-
(require 'run-tests)
(require 'defs)

;;; TESTS
;;
(ert-deftest mars/generate-mode-hook-list-simple-test ()
  "Tests the composition of a list of hooks from a list of mode prefixes."
  (should (equal (mars/generate-mode-hook-list '(emacs-lisp)) '(emacs-lisp-mode-hook)))
  (should (equal (mars/generate-mode-hook-list '(cc lisp scheme)) '(cc-mode-hook
                                                                    lisp-mode-hook
                                                                    scheme-mode-hook))))

(ert-deftest mars/add-lambda-hook-simple-test ()
  "Tests the generation of LAMBDA from a list of sexps for hooks."
  (should (equal (macroexpand '(add-lambda-hook foo-hook (message "bar") (+ 1 2) (baz quux)))
                 '(add-hook foo-hook (function (lambda nil (message "bar") (+ 1 2) (baz quux)))))))

(defun foo-bar-mode-hook-context (body)
  (unwind-protect
      (progn
        (defvar foo-mode-hook nil)
        (defvar bar-mode-hook nil)
        (funcall body))
    (safe-unintern 'foo-mode-hook)
    (safe-unintern 'bar-mode-hook)))

(ert-deftest mars/add-hooks-simple-test ()
  "Tests the hook bindings."
  (foo-bar-mode-hook-context
   #'(lambda ()
       (mars/add-hooks '(foo-mode-hook bar-mode-hook) #'(lambda () (message "baz")))
       (should (equal (car foo-mode-hook) '(lambda nil (message "baz"))))
       (should (equal (car foo-mode-hook) '(lambda nil (message "baz")))))))

(ert-deftest mars/force-options-simple-test ()
  (unwind-protect
      (progn
        (defvar *old-foo* 'bar)
        (defvar *new-foo* 'foo)
        (should (eq (mars/force-options (*new-foo* . *old-foo*)) 'foo))
        (should (eq *old-foo* *new-foo*)))
    (safe-unintern '*old-foo*)
    (safe-unintern '*new-foo*)))

(ert-deftest add-to-alist-simple-test ()
  (unwind-protect
      (progn
        (defvar foo-alist '())
        (defvar foobar 'baz)
        ;; add a list including 'bar to 'foo key
        (add-to-alist 'foo 'bar foo-alist)
        (should (equal foo-alist '((foo bar))))
        ;; push a function result as symbol to the container at 'foo key
        (add-to-alist 'foo (intern (format "%s" "baz")) foo-alist)
        (should (equal foo-alist '((foo baz bar))))
        ;; add a new container at a new generated key
        (add-to-alist foobar 'quux foo-alist)
        (should (equal foo-alist '((baz quux) (foo baz bar)))))
    (safe-unintern 'foobar)
    (safe-unintern 'foo-alist)))

(ert-deftest joindirs-simple-test ()
  (should (string= (joindirs "~") (expand-file-name "~")))
  (should (string= (joindirs "~" ".emacs.d") (expand-file-name ".emacs.d" "~")))
  (should (string= (joindirs "~" ".emacs.d" "lisp") (expand-file-name "lisp" (expand-file-name
                                                                              ".emacs.d"
                                                                              "~")))))

(defun quux-fundamental-buffer-context (body)
  (unwind-protect
      (progn
        (generate-new-buffer "quux")
        (switch-to-buffer "quux")
        (progn (insert "foo bar baz quux")
               (newline)
               (insert "zork gork bork")
               (newline) (newline))
        (funcall body))
    (safe-kill-buffer "quux")))

(ert-deftest first-line-of-buffer-simple-test ()
  (quux-fundamental-buffer-context
   #'(lambda ()
       (should (string= (first-line-of-buffer) "foo bar baz quux")))))

(ert-deftest count-buffers-simple-test ()
  (unwind-protect
      (let ((nbuf (count-buffers)))
        (generate-new-buffer "quux")
        (should (= (count-buffers) (1+ nbuf)))
        (generate-new-buffer "quuux")
        (should (= (count-buffers) (+ 2 nbuf))))
    (mapc #'(lambda (x)
              (safe-kill-buffer x))
          '("quux" "quuux"))))

(defun foo-dired-ibuffer-context (body)
  (unwind-protect
      (progn
        (defvar count-buffers)
        (set 'dired-use-ls-dired nil)
        (dired (expand-file-name "~"))
        (set 'ibuffer-expert t)
        (trap-messages #'ibuffer)
        (set 'count-buffers (count-buffers))
        (funcall body))
    (safe-unintern 'count-buffers)
    (safe-funcall #'ibuffer-quit)))

(ert-deftest kill-all-dired-buffers-simple-test ()
  (foo-dired-ibuffer-context
   #'(lambda ()
       (trap-messages #'kill-all-dired-buffers)
       (should (=  (1- count-buffers) (count-buffers))))))

(defun vsort-window-list (&optional frame)
  (unless frame
    (selected-frame))
  (let ((w (frame-first-window frame))
        l
        top)
  (while (and w (not (eq w top)))
    (unless top (set 'top w))
    (push w l)
    (setq w (next-window w)))
  (reverse l)))

(defun window-buffer-name ()
  (mapcar #'(lambda (w)
              (and (windowp w)
                   (buffer-name (window-buffer w))))
          (vsort-window-list (selected-frame))))

(defun generate-foo-list (&optional num symbol-flag)
  "Generates a list of *foo*. NUM defines the size and
SYMBOL-FLAG whether it's a list of strings or symbols."
  (unless (and (integerp num)
               (> num 0))
    (setq num 1))
  (let (list)
    (while (> num 0)
      (add-to-list
       'list
       (cond ((= num 1) (if symbol-flag 'foo "foo"))
             ((= num 2) (if symbol-flag 'bar "bar"))
             ((= num 3) (if symbol-flag 'baz "baz"))
             (t  (let ((quux (format "q%sux" (make-string (- num 3) ?u))))
                   (if symbol-flag (intern quux) quux)))))
      (setq num (1- num)))
    list))

(defun windows-context (num body)
  "create NUM split windows, executes the BODY lambda and destroy buffers
by restoring single window frame."
  (let ((foos (reverse (generate-foo-list num))))
    (unwind-protect
        (progn
          (mapc #'(lambda (x)
                    (generate-new-buffer x))
                foos)
          (switch-to-buffer (car foos))
          (delete-other-windows)
          (mapc #'(lambda (x)
                    (if (fboundp #'split-window-above-each-other)
                        (split-window-above-each-other)
                      (split-window-vertically))
                    (switch-to-buffer x))
                (cdr foos))               ; hsplitted windows contains FOO, BAR, BAZ,..
          (funcall body))
      (mapc #'(lambda (x) (safe-kill-buffer x))
            foos)
      (delete-other-windows))))

(defun window-context (body)
  (windows-context 1 body))

(ert-deftest transpose-buffers-complete-test ()
  (windows-context
   3                                    ; 3 panes FOO / BAR / BAZ (focused on FOO)
   #'(lambda ()
       (transpose-buffers 1)
       (should (equal (window-buffer-name) '("bar" "foo" "baz"))) ; invert windows
       (should (eq (intern
                    (buffer-name
                     (window-buffer
                      (selected-window)))) 'foo)) ; stay in the same buffer
       (transpose-buffers 1)
       (should (equal (window-buffer-name) '("bar" "baz" "foo"))) ; invert windows
       (should (eq (intern
                    (buffer-name
                     (window-buffer
                      (selected-window)))) 'foo)) ; stay in the same buffer
       (select-window (previous-window))
       (transpose-buffers -1)
       (should (equal (window-buffer-name) '("baz" "bar" "foo"))) ; revert windows from 'FOO
       (should (eq (intern
                    (buffer-name
                     (window-buffer
                      (selected-window)))) 'baz))))) ; gone to the previous window

(ert-deftest listify-simple-test ()
  (should (equal (listify 'var) '(var)))
  (should (equal (listify 1) '(1)))
  (should (equal (listify '(var)) '(var))))

(ert-deftest swap-windows-simple-test ()
  (windows-context
   2                                    ; 2 panes FOO / BAR (focused on FOO)
   #'(lambda ()
       (swap-windows)
       (should (equal (window-buffer-name) '("bar" "foo")))
       (should (eq (intern
                    (buffer-name
                     (window-buffer
                      (selected-window)))) 'bar)))))

(ert-deftest the-the-simple-test ()
  (window-context
   #'(lambda ()
       (insert "My tailor is rich. Your flowers are are beautiful.")
       (newline)
       (insert "Grasp all, lose all.")
       (newline)
       (insert "He who's afraid of leaves, Must not come into a a wood")
       (goto-char (point-min))
       (trap-messages #'the-the)
       (should (= (point) 40))
       (trap-messages #'the-the)
       (should (= (point) 122))
       (trap-messages #'the-the)
       (should (= (point) (point-max))) ; eof
       (trap-messages #'the-the)
       (should (= (point) (point-max)))))) ; don't move

(ert-deftest trim-string ()
  (should (string= (trim-string "  that's all folks!\n \t ") "that's all folks!"))
  (should (string= (trim-string "\n  \n\t\n\t  that's all folks!\n \t ") "that's all folks!"))
  (should (string= (trim-string "that's all folks!") "that's all folks!"))
  (should (string= (trim-string "  that\t's all \nfolks!\n \t ") "that\t's all \nfolks!")))

(ert-deftest elisp-files-in-below-directory-posix-test ()
  (let ((rdm (expand-file-name "vendor/README" (file-name-directory (symbol-file 'windows-context)))))
    (if (file-exists-p rdm)
        (let ((vendor-dir (file-name-directory rdm)))
          (should (equal
                   (sort (elisp-files-in-below-directory vendor-dir)
                         #'string<)
                   (mapcar
                    #'(lambda (file)
                        (expand-file-name file vendor-dir))
                    '("file0.el" "pack1/file1.el" "pack1/third-party/file2.el"
                      "pack2/file3.el" "pack2/file4.el" "pack2/file5.el")))))
      (message "You must run this test with a Bourne shell script from the root directory to test."))))

(defun mars/markdown-text-zone-context (longline test)
  "If LONGLINE is T, add trailing whitespace to the title."
  (insert "My Title From Hell")
  (when (eq longline 'longline)
    (insert "      \t\t      "))
  (newline)
  (insert "My story began...")
  (funcall test))

(defun mars/markdown-test-template (buffer-or-name type &optional char)
  #'(lambda ()
      (with-current-buffer buffer-or-name
                (goto-char (+ (random 10) (point-min))) ; random position on the first line
                ;; make header
                (unless char
                  (set 'char ?*))
                (cond ((eq type 'first) (mars/markdown-header-first))
                      ((eq type 'second) (mars/markdown-header-second))
                      (t (mars/title-as-markdown-title char)))
                (mars/markdown-header-first) ; make title
                (should (string= (buffer-substring (point) (point-at-eol))
                                 "My story began...")) ; check we're on the first paragraph
                (forward-line -2)       ; jump the blank line
                (should (string= (buffer-substring (point-at-bol) (point-at-eol))
                                 (make-string 18
                                              (cond
                                               ((eq type 'first) ?=)
                                               ((eq type 'second) ?-)
                                               (t char))))) ; check the underlining
                (forward-line -1)       ; go back to the first line
                (should (string= (buffer-substring (point-at-bol) (point-at-eol))
                                 "My Title From Hell"))))) ; check trailing space if any

(ert-deftest mars/markdown-header-complete-test ()
  (window-context
   #'(lambda ()
       (let ((buf (car (generate-foo-list))))
         (mapc #'(lambda (longline-case)   ; check consistent title & title w/ trailing spaces
                   (with-current-buffer buf
                     (mars/markdown-text-zone-context
                      longline-case
                      #'(lambda ()
                          (mars/markdown-test-template
                           buf 'first)
                          (mars/markdown-test-template
                           buf 'second)
                          (mars/markdown-test-template
                           buf 'otherwise ?~)))))
               '(normal longline))))))

;; TODO: test backward-delete-char-hungry ASAP

(ert-deftest delete-horizontal-space-forward-simple-test ()
  (window-context
   #'(lambda ()
       (insert " \t Delete all spaces and tabs after point. \t     \t  \nA new line just for fun!  \t  ")
       (point-min)
       (goto-char (point-at-eol))
       (goto-char (re-search-backward "\\."))
       (goto-char (1+ (point)))
       (delete-horizontal-space-forward)
       (should (string=
                (buffer-substring (point-min) (point-at-eol))
                " \t Delete all spaces and tabs after point."))
       (forward-line 1)
       (goto-char (point-at-bol))
       (should (string=
                (buffer-substring (point) (point-at-eol))
                "A new line just for fun!  \t  ")))))

(defun whack-whitespace-template (test)
  (window-context
   #'(lambda ()
       (insert "Delete all white \t \n  spaces.")
       (goto-char (point-min))
       (let ((changed (safe-word-search-forward "white"))
             (buf (car (generate-foo-list))))
         (when changed
           (funcall test))))))

(ert-deftest whack-whitespace-cr-case-test ()
  (whack-whitespace-template
   #'(lambda ()
       (whack-whitespace t)       ; normal case
       (should (string=
                (buffer-substring (point-min) (point-max))
                "Delete all white spaces.")) ; normal case
       (goto-char (point-max))
       (should (string=
                (buffer-substring (point-min) (point-max))
                "Delete all white spaces.")) ; eob case 1
       (insert "\n\n\n")
       (goto-char (point-min))
       (goto-char (re-search-forward "\\."))
       (whack-whitespace t)
       (should (string=
                (buffer-substring (point-min) (point-max))
                "Delete all white spaces."))))) ; eob case 2

(ert-deftest whack-whitespace-flat-case-test ()
  (whack-whitespace-template
   #'(lambda ()
       (whack-whitespace)
       (should (string=
                (buffer-substring (point-min) (point))
                "Delete all white")))))

(ert-deftest nuke-line-simple-test ()
  (window-context
   #'(lambda ()
       (insert "This is my line 1.") (newline)
       (insert "This is my line 2.") (newline)
       (insert "This is my line 3.")
       (goto-char (point-min))
       (forward-line 1)                 ; second line to be removed
       (let ((pos (+ (point) (random 10))))
         (goto-char pos)
         (nuke-line)
         (should (string=
                  (buffer-substring (point-min) (point-max))
                  "This is my line 1.\nThis is my line 3."))
         ;; check we're on the same column/line
         (should (= (point) pos))))))

(provide 'test-defs)
