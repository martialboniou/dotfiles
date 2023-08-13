;;; Simple Emacs Early Init -*- no-byte-compile: t; -*-

;; Emacs version = 29.1
(setq original/gc-cons-threshold gc-cons-threshold
      gc-cons-threshold most-positive-fixnum
      read-process-output-max (* 1024 1024)
      load-prefer-newer t
      auto-compile-display-buffer nil
      auto-compile-mode-line-counter t
      original/file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

;;; PERFORMANCE

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold original/gc-cons-threshold)
   (setq file-name-handler-alist original/file-name-handler-alist)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))

;;; LOAD-PATH & AUTO-COMPILATION OF COMPILED FILES

(let* ((extensions-repository-name "lisp")
       (auto-compile--library 'auto-compile)
       (auto-compile--name (symbol-name auto-compile--library)))
  (dolist (sub-directory-name `(,extensions-repository-name ,auto-compile--name))
    (add-to-list 'load-path (expand-file-name sub-directory-name
                                              user-emacs-directory)))
  (when (require auto-compile--library nil 'noerror)
    ;; run `make` to ensure compilation of auto-compile, init & lisp directory
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

(provide 'early-init)
