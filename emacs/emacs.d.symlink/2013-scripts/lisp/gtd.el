;;; gtd.el ---
;;
;; Filename: gtd.el
;; Description: Organize Life & Ideas with Emacs
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Tue Feb 22 11:31:42 2011 (+0100)
;; Version: 0.3
;; Last-Updated: Thu Dec 19 14:05:24 2013 (+0100)
;;           By: Martial Boniou
;;     Update #: 135
;; URL: 
;; Keywords: 
;; Compatibility:  ORG 8.0 pre or more
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: OrgMode
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

(require 'crypto)
(require 'preamble)

;;; ORG MODE
;; (from 'Using Org Mode as a Day Planner' by John Wiegley)

(require 'org-loaddefs)
(require 'org-protocol)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(bind-key mode-specific-map "a" #'org-agenda)

;; common setup
(eval-after-load "org"
  '(progn
     ;; auto archiving setup: 'ARCHIVE-DONE-TASKS needed to clean up
     (eval-after-load "org-wiegley-ext" ; TODO: upgrade for 2013 org
       '(setq org-my-archive-expiry-days 45))
     (add-to-list 'safe-local-variable-values '(after-save-hook archive-done-tasks)) ; add: -*- after-save-hook (archive-done-tasks) -*- in your Org files to archive automatically (ie. all DONE|DEFERRED|CANCELLED tasks go to the archive file if date exceeds 45 days)
     (if (fboundp 'org-wiegley-ext)
         (require 'org-wiegley-ext)
       (defun archive-done-tasks () (message "gtd: org-wiegley-ext is not installed.")))
     (when *i-might-be-a-saiki-komon*
       (defun display-organizer-at-startup ()
         (call-interactively #'mars/two-days-calendar)))
     (setq org-log-done 'time
           org-todo-keywords '((sequence "TODO(t)" "STARTED(s@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "APPT(a@!)" "|" "DONE(d!)" "DEFERRED(f)" "CANCELLED(c@)")))
     (add-lambda-hook 'org-mode-hook
       (setq truncate-lines nil)) ; turn on soft wrapping mode for org mode
     ;; original C-n/C-p behavior kept
     (add-lambda-hook 'org-agenda-mode-hook
       (bind-keys '(org-agenda-mode-map org-agenda-keymap)
                  "C-n" #'org-agenda-next-line
                  "C-p" #'org-agenda-previous-line))))

;; viper compatibility setup
(eval-after-load "viper"
  '(if (boundp 'viper-version)
       (bind-key viper-vi-global-user-map "C-c /" #'org-sparse-tree)))

;; agenda & remember setup
;; (add-hook 'remember-mode-hook #'org-remember-apply-template)
(custom-set-variables
 '(org-default-notes-file (expand-file-name "Notes.org"
                                            org-directory))
 '(org-diary-agenda-file (expand-file-name "Diary.org"
                                           org-directory))
 '(org-agenda-files (mapcar #'(lambda (item)
                                (expand-file-name (format "%s.org" item)
                                                  org-directory))
                            '("Todo")))
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-agenda-custom-commands
   (quote (("d" todo "DELEGATED" nil)
           ("c" todo "DONE|DEFERRED|CANCELLED" nil)
           ("w" todo "WAITING" nil)
           ("W" agenda "" ((org-agenda-ndays 21)))
           ("A" agenda ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
             (org-agenda-ndays 1)
             (org-agenda-overriding-header "Today's Priority #A tasks: ")))
           ("u" alltodo ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote scheluded) (quote deadline)
                                          (quote regexp) "<[^\n]+>")))
             (org-agenda-overriding-header "Unscheduled TODO entries: ")))))))

;; capture setup
(eval-after-load "org-capture"
  '(progn
     (setq org-capture-templates
           '(("l" "Web Link" entry
              (file+headline org-default-notes-file "Web Links")
              "* %a\nAdded: %U\nComments: %i%?")
             ("b" "Simple Bookmark" entry
              (file+headline org-default-notes-file "Simple Bookmarks")
              "* %a\n\n%i%?")
             ("t" "Todo" entry
              (file+headline (car org-agenda-files) "Tasks")
              "* TODO  %?\n  %u")
             ("n" "Notes" entry
              (file+headline org-default-notes-file "Notes")
              "* %u %?")
             ("s" "Top Secret" entry
              (file+headline (expand-file-name "Iris.gpg"
                                               org-directory) "Notes")
              "* %u %?")))
     ;; DELETE-FRAME w/o alert in * Remember * frame case
     (defun current-frame-is-remember ()
       "Answers true if the frame is named * Remember *."
       (equal "* Remember *" (frame-parameter nil 'name)))
     (defun current-frame-is-not-remember () (not (current-frame-is-remember)))
     ;; http://metajack.im/2008/12/30/gtd-capture-with-emacs-orgmode
     (defadvice org-capture-finalize (after delete-remember-frame (&optional stay-with-capture) activate)
       "Advise org-capture-finalize to close the frame if it is the remember frame"
       (when (current-frame-is-remember) (delete-frame)))
     (defadvice org-capture-kill (after delete-remember-frame activate)
       "Advise org-capture-kill to close the frame if it is the remember frame"
       (when (current-frame-is-remember) (delete-frame)))
     ;; single window FIXME: in remember-frame only!!
     (add-hook 'org-capture-mode-hook #'delete-other-windows)
     ;; 1- capture link/bookmark with global capture (C-c c or org-protocol external bookmarklet)
     (global-set-key (kbd "C-c c") 'org-capture)
     ;; javascript:location.href='org-protocol:/capture:/l/'+encodeURIComponent(location.href)+'/'+encodeURIComponent(document.title)+'/'+encodeURIComponent(window.getSelection())
     (setq org-capture-templates-contexts '(("l" (current-frame-is-not-remember))
                                            ("b" (current-frame-is-not-remember))))
     ;; 2- capture notes with `make-remember-frame' ([f8-f7] see 'SHORTCUTS)
     ;; 3- capture todos in either way
     (add-to-list 'org-capture-templates-contexts
                  '("n" (current-frame-is-remember)))
     (add-to-list 'org-capture-templates-contexts
                  '("s" (current-frame-is-remember)))))

;; additional interactive for agenda & remember
(defun make-remember-frame ()
  "Create a new frame and run `org-capture' for TODOs only."
  (interactive)
  (make-frame '((name . "* Remember *") (width . 80) (height . 10)))
  (select-frame-by-name "* Remember *")
  (org-capture))

(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p 'any)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer))
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer))))
      (call-interactively #'org-agenda-list))))

(defun mars/today-calendar ()
  "Open `org-agenda' on today."
  (interactive)
  (org-agenda-list nil nil 'day))

(defun mars/two-days-calendar ()
  "Open `org-agenda' on today & tomorrow."
  (interactive)
  (org-agenda-list nil nil 2))

(eval-after-load "org-agenda"
  '(progn
     (defun mars/unscheduled-tasks ()
       "Open `org-agenda' on unscheduled tasks."
       (interactive)
       (let ((org-agenda-skip-function (lambda () (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "<[^\n]+>")))
             (org-agenda-overriding-header "Unscheduled TODO entries: "))
         (org-todo-list t)))))

(provide 'gtd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gtd.el ends here
