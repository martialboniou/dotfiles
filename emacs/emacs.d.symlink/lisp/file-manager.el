;;; file-manager.el ---
;;
;; Filename: file-manager.el
;; Description: file management
;; Author: Martial Boniou
;; Maintainer: Martial Boniou
;; Created: Sat Feb 19 11:17:32 2011 (+0100)
;; Version: 0.8.3
;; Last-Updated: Tue Dec 17 14:57:40 2013 (+0100)
;;           By: Martial Boniou
;;     Update #: 97
;; URL: 
;; Keywords: 
;; Compatibility: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: dired / dired-details / dired-extension / dired+ /
;;              wdired / wdired-extension / dired-x / sunrise-commander
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log: was box.el before
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

(require 'adapter)

;;; DIRED / DIRED-DETAILS / DIRED-EXTENSION
;; dired-x [guessing shell commands + dired commands for `non-dired' buffers]
(add-lambda-hook 'dired-load-hook
  (setq dired-dwim-target t)            ; easier copies in dir mode
  (require 'dired-x)
  (bind-key dired-mode-map "C-S-r" #'dired-rar-add-files))
;; dired-details(+) [show/hide] & dired+ [colors + bonus] & wdired(-extension) [editable]
(eval-after-load "dired"
  '(progn
     (setq dired-listing-switches "-alh"
           auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
                                 auto-mode-alist))
     (defun dired-apply-function (function) ; mapped on '<C-d> d'
       "Run FUNCTION on marked files."
       (interactive "aApply on marked files: ")
       (mapc function (dired-get-marked-files)))
     (defun dired-do-command (command)  ; mapped on '<C-d> <C-d>'
       "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved. -- matt curtis (with enhancements by <hondana@gmx.com>"
       (interactive "CRun on marked files M-x ")
       (let ((keep (y-or-n-p "Keep files in unsaved buffers? ")))
         (save-window-excursion
           (mapc #'(lambda (filename)
                     (find-file filename)
                     (call-interactively command)
                     (unless keep
                       (save-buffer)
                       (kill-buffer)))
                 (dired-get-marked-files)))))
       (define-prefix-command 'dired-do-map)
       (bind-key dired-mode-map "C-d" 'dired-do-map)
       (bind-keys dired-do-map
                  "C-d" #'dired-do-command
                  "d"   #'dired-apply-function)
       ;; (require 'dired-aux)               ; attributes and goodies (autoloaded)
       (unless (el-get-package-is-installed "dired-details")
         (message "file-manager: dired-details is not installed."))
       (eval-after-load "dired-details"
         '(progn
            (dired-details-install)            ; show/hide (type ")" to show)
            ;; (when (locate-library "dired-details+") (require 'dired-details+))       ; no need in this setting
            (setq dired-details-hidden-string "")))
       (when (el-get-package-is-installed "dired-plus")
         (require 'dired+)) ; colors + bonus
       (when (locate-library "wdired")      ; should be in Emacs 24
         (require 'wdired)                 ; editable (type `r' to rename [default was `e'])
         (when (locate-library "wdired-extension")
           (require 'wdired-extension)))      ; rect-mark + wdired-format-filename
       ;; (when (boundp 'viper-emacs-state-mode-list)
       ;;   ;; the following line manages the viper/wdired clash
       ;;   ;; (add-to-list 'viper-emacs-state-mode-list 'dired-mode)
       ;;   ;; the following line forces the not-recommended vi nagivation over dired commands
       ;;   (setq mars/dired-vi-purist-map (make-sparse-keymap))
       ;;   (viper-modify-major-mode 'dired-mode 'emacs-state mars/dired-vi-purist-map)
       ;;   (bind-keys mars/dired-vi-purist-map
       ;;              "k" #'viper-previous-line
       ;;              "l" #'viper-forward-char)
))

;;; WDIRED
(eval-after-load "wdired"
  '(progn
     (bind-key dired-mode-map "r" #'wdired-change-to-wdired-mode)
     (bind-key wdired-mode-map "<return>" #'wdired-finish-edit)
     (setq wdired-allow-to-change-permissions t) ; allow -rwxrwxrwx changes
     (bind-key dired-mode-map
               "<menu-bar> <immediate> <wdired-change-to-wdired-mode>"
               '("Edit File Names" . wdired-change-to-wdired-mode))
     (eval-after-load "viper"
       '(progn
          (defadvice wdired-change-to-wdired-mode (after viper activate)
            (unless (eq viper-current-state 'emacs-state)
              (viper-change-state 'vi-state)))
          (defadvice wdired-finish-edit (after viper activate)
            (unless (eq viper-current-state 'emacs-state)
              (viper-change-state-to-vi)) ; back to normal state
            (viper-modify-major-mode    ; back to dired map
             'dired-mode 'vi-state dired-mode-map))))))

;;; DIRED-X
(eval-after-load "dired-x"
  '(progn
     (unless (fboundp 'dired-omit-toggle)
       (defalias 'dired-omit-files 'dired-omit-mode))
     (setq dired-omit-files-p t
           dired-omit-files (concat dired-omit-files "\\|^\\..+$")
           dired-guess-shell-alist-user '(("\\.image" "squeak"))))) ; FIXME: pharo

;;; SUNRISE-COMMANDER
(when (el-get-package-is-installed "sunrise-commander")
  (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode)))
(eval-after-load 'sunrise-commander
  '(progn
     (defun dired-details-hide()
       "Make an invisible, evaporable overlay for each file-line's details in this dired buffer."
       (interactive)
       (unless (memq major-mode '(dired-mode vc-dired-mode sr-mode))
         (error "dired-details-hide can only be called in dired mode"))
       (when dired-details-debug
         (let ((b (get-buffer-create "dired-details-debug")))
           (append-to-buffer b (point) (point-max))))
       (save-excursion
         (save-restriction
           (widen)
           (mapc '(lambda (dir-and-pos)
                    (let ((cached-overlays (assoc (car dir-and-pos)
                                                  dired-details-internal-overlay-list)))
                      (if cached-overlays
                          (dired-details-frob-overlays t)
                        (let ((cache (list (car dir-and-pos)))
                              (subdir-start (cdr dir-and-pos))
                              (subdir-end (1- (dired-get-subdir-max dir-and-pos))))
                          (goto-char subdir-start)
                          (dired-goto-next-file)
                          (while (< (point) subdir-end)
                            (dired-details-make-current-line-overlay cache)
                            (dired-next-line 1))
                          (setq dired-details-internal-overlay-list
                                (cons cache dired-details-internal-overlay-list))))))
                 dired-subdir-alist)))
       (setq dired-details-state 'hidden))))

(provide 'file-manager)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file-manager.el ends here
