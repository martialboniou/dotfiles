;;; Simple Emacs Early Init -*- lexical-binding: t; -*-

;; Emacs version = 29.1
(defvar auto-compile-display-buffer nil)
(defvar auto-compile-mode-line-counter t)
(defvar use-package-always-ensure nil) ; IMPORTANT: don't touch
(defvar use-package-expand-minimally t)
(defvar straight-use-package-by-default t)
(defvar straight-vc-git-default-clone-depth '(1 single-branch))
(defvar use-package-verbose nil)
(defvar bootstrap-version)

(declare-function straight-use-package "straight" (p))

(setq read-process-output-max (* 1024 1024)
      package-enable-at-startup nil ; IMPORTANT: Packaging in Batch mode required
      load-prefer-newer t)

;;; PERFORMANCE

(unless noninteractive
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (let ((original/gc-cons-threshold gc-cons-threshold)
        (original/file-name-handler-alist file-name-handler-alist))
    (setq gc-cons-threshold most-positive-fixnum
          file-name-handler-alist nil)
    (run-with-idle-timer
      5 nil
      (lambda ()
        (setq gc-cons-threshold original/gc-cons-threshold)
        (setq file-name-handler-alist original/file-name-handler-alist)
        (makunbound 'gc-cons-threshold-original)
        (makunbound 'file-name-handler-alist-original)
        (message "gc-cons-threshold and file-name-handler-alist restored")))))

;;; UTILITIES

(defmacro hondana/use (package &rest params)
  `(eval-and-compile
     (use-package ,package ,@params)))

;;; BOOTSTRAP STRAIGHT TO INSTALL PACKAGES FROM SOURCES

(let ((bootstrap-file (expand-file-name
                        "straight/repos/straight.el/bootstrap.el"
                        user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
   (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
  
;;; LOAD-PATH & AUTO-COMPILATION FOR COMPILED FILES

;; Auto-compile
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "auto-compile" user-emacs-directory))
  (require 'auto-compile))
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; Extensions (ie `lisp' directory)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(provide 'early-init)
