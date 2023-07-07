;; require this file in any configuration file to load `.emacs' in another mode

(provide 'booting)

(eval-when-compile (require 'cl-lib))

(require 'appearance)

(require 'packs)

;; (when *i-am-a-common-lisp-advocate*
;;   (unless (fboundp 'eieio-defclass)     ; `cedet' if no CLOS
;;     (safe-load-cedet)))

(require 'adapter)

(require 'behave)

(when custom-file
  (load custom-file 'noerror))
(defadvice custom-buffer-create (before my-advice-custom-buffer-create activate)
  "Exit the current Customize buffer before creating a new one, unless there are modified widgets."
  (if (eq major-mode 'Custom-mode)
      (let ((custom-buffer-done-kill t)
            (custom-buffer-modified nil))
        (mapc (lambda (widget)
                (and (not custom-buffer-modified)
                     (eq (widget-get widget :custom-state) 'modified)
                     (setq custom-buffer-modified t)))
              custom-options)
        (if (not custom-buffer-modified)
            (Custom-buffer-done)))))

(require 'formats)                      ; emacs as an universal typewriter
(require 'crypto)                       ; emacs as a secret agent
(require 'window-manager)               ; emacs as a window manager
(require 'file-manager)                 ; emacs as a file manager
(require 'shortcuts)                    ; emacs as a key commander

(when *i-am-an-emacsen-dev*
  (require 'ladybug))                   ; emacs as an elisp developer tool

;; bytecompile via the locally defined `auto-byte-compile' boolean
(put 'auto-byte-compile 'safe-local-variable #'booleanp)
;; --> on save for any file in emacs-lisp-mode having `auto-byte-compile'
;; sets to true in:
;; 1- local variables defined in the heading/ending comments
;; 2- `.dir-locals.el' file located at the root directory of the file
(add-hook 'emacs-lisp-mode-hook #'auto-byte-compile-save-hook)
;; --> on kill for all initial and configuration files
(add-lambda-hook 'kill-emacs-hook
  (let ((byte-compile-warnings '(not nresolved free-vars callargs redefine obsolete noruntime cl-functions interactive-only)))
    (byte-compile-new-files-in-directories-maybe
     (mapcar #'(lambda (dir)
                 (expand-file-name dir
                                   mars/local-root-dir))
             mars/local-conf-path))
    (when user-init-file
      (byte-compile-new-files-maybe user-init-file))))

;; fast kill emacs or not but confirm anyway
(defadvice update-autoloads-in-package-area (around mars/fast-kill-version activate)
  (unless mars/fast-kill
    (progn
      ad-do-it)))
(setq confirm-kill-emacs 'y-or-n-p)

;; confirm deleting frame iff multiple windows or buffer property in `confirm-frame-action-buffer-alist'
(defadvice delete-frame (around confirm-delete-frame
                (&optional frame force) activate)
  (if (< (length (frame-list)) 2)
      (kill-emacs)
    (let ((windows (window-list frame)))
      (if (> (length windows) 1)
          (if (y-or-n-p (format "Multiple windows in this frame detected. Close anyway? ")) (progn ad-do-it) nil)
        ;; one window case
        (let ((pending confirm-frame-action-buffer-alist)
              (buf (window-buffer (car windows)))
              found)
          (while (and (null found) pending)
            (let ((property (pop pending)))
              (when (member (with-current-buffer
                                buf
                              (symbol-value (car property))) (cdr property))
                (setq found t))))
          (if (and found (not (y-or-n-p (format "Are you sure you want to delete this frame? "))))
              nil
            ad-do-it))))))

;; reload emacs
(defun reload-emacs ()          ; TODO: check it prolly might not work
  (let ((memo (featurep 'emacs-normal-startup)))
    (unless memo
      (provide 'emacs-normal-startup))
    (load user-init-file)
    (unless memo
      (unintern 'emacs-normal-startup obarray))))

;; load time (NOTE: ADAPTER should install `cl-lib' if not found)
;; #'EMACS-INIT-TIME doesn't count time in non-`init.el' startup
(defvar emacs-startup-time nil
  "Display the Emacs startup time. Says if the Emacs startup time function has not been displayed if NIL.")
(defun emacs-startup-time ()
  (if (null emacs-startup-time) "no startup time" (format "%d seconds" emacs-startup-time)))
(defun emacs-startup-time-display ()
  (interactive)
  (let ((not-displayed-yet nil))
    (when (null emacs-startup-time)
      (setq not-displayed-yet t)
      (setq emacs-startup-time (cl-destructuring-bind (hi lo ms &optional ps) (current-time)
                                 (- (+ hi lo) (+ (first emacs-load-start)
                                                 (second emacs-load-start))))))
    (let ((load-time-msg (format "%s loaded in %s%s"
                                 (cond
                                  ((featurep 'emacs-normal-startup) "Emacs")
                                  ((featurep 'peyton-jones-family) "Emacs for functional programming")
                                  ((featurep 'church-inspired) "Emacs for Lisp or Prolog")
                                  ((featurep 'python-357) "Emacs for Python")
                                  ((featurep 'pure-object) "Emacs for object-oriented programming")
                                  ((featurep 'wiki-wiki) "Emacs for Markup languages")
                                  ((featurep 'mail) "MUA-Emacs")
                                  ((featurep 'gtd) "GTD-Emacs")
                                  (t "Emacs in minimal setup"))
                                 (emacs-startup-time)
                                 (if (null window-system) (format " in %s" (shell-command-to-string  (format "ps -p %s | tail -1 | awk '{print $2}'" (emacs-pid)))) ""))))
      (if not-displayed-yet
          (display-external-pop-up "Emacs startup" load-time-msg)
        (message load-time-msg)))))
(add-hook 'emacs-startup-hook #'emacs-startup-time-display)

(put 'narrow-to-region 'disabled nil)

(provide 'kernel)
(unintern 'booting obarray)
