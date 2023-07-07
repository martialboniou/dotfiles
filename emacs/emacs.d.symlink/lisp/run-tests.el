;; unit test library
;; tested on GNU Emacs 24.3
(require 'ert)
(require 'cl-lib)

;; settings
(set 'message-log-max nil)              ; no *Messages* > faster
(progn                                  ; set the frame size  wide enough to test windows
                                        ; manipulation
  (set-frame-size (selected-frame) 48 48)
  (add-to-list 'default-frame-alist '(height . 48) '(width . 48)))

;; utilities
(defun trap-messages (body)
  "Don't display messages."
   (require 'cl-lib)
   (cl-flet ((message (format-string &rest args) nil))
     (funcall body)))
(defun safe-funcall (function)
  (condition-case nil
      (funcall function)
    (error nil)))
(defun safe-word-search-forward (word &optional test-name)
  (condition-case err
      (progn
        (word-search-forward word)
        t)
    (error (progn
             (message
              "%s: bad formed sentence - %s not tested"
              err (or test-name
                      "a function were "))))))
(defun safe-kill-buffer (buffer-name)
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name)))
(defun safe-unintern (symbol)
  (when (boundp symbol)
    (unintern symbol obarray)))

(provide 'run-tests)
