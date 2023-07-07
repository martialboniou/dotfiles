;; -*- no-byte-compile: t -*-
;;; wl ---
;;
;; Filename: wl
;; Description:
;; Author: Martial Boniou
;; Maintainer:
;; Created: Mon Jan 14 14:31:57 2008
;; Version:
;; Last-Updated: Fri Jun 21 18:23:22 2013 (+0200)
;;           By: Martial Boniou
;;     Update #: 330
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   Cannot open load file: wl-init.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; (setq wl-summary-auto-refile-skip-marks ("n" "N"))
;; martial-2008-08-25 new messages stay in shared IMAP server
;; but (after the next sync refresh) when they are still unread
;; they are refiled if needed (+seaside, +squeak, ... subfolders);
;; expiration takes place 2 months later for non-refiled read
;; messages (new/unread ones stay in place forever)
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;  2010-04-22: bbdb field notes is a bit a mess; a new field 'friend'
;;              mu-cite introduced
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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

(eval-when-compile ;; (require 'filladapt)
                   (require 'wl)
                   (require 'wl-spam)
                   (require 'elmo-imap4)
                   (require 'elmo-pop3)
                   (require 'elmo-nntp)
                   (require 'elmo-archive)
                   (require 'mime-edit))
(declare-function filladapt-mode "filladapt")
(declare-function wl-summary-refile "wl-summary")

;;; UTF-8 support
(setq default-mime-charset-for-write 'utf-8)

;;; SMTP + NNTP
;; - GMX - FIXME: set local for contextual *me* envelope-from => use wl-draft-config-alist to set variables
(setq wl-smtp-posting-server "smtp.yourserver.com"
      wl-smtp-posting-user "me@yourserver.com"
      wl-smtp-posting-port 587
      wl-smtp-authenticate-type "plain"
      wl-smtp-connection-type 'starttls)

(setq wl-folder-check-async t)

;;; SEMI
(setq mime-setup-enable-inline-html nil
      mime-edit-split-message nil)
;; (setq mime-edit-message-default-max-lines 1000)

;;; Private Info
;; name & domains
(setq wl-local-domain "localhost"
      wl-message-id-domain "localhost"
      wl-from "Me <me@yourserver.com>"
      wl-user-mail-address-list
      (list (wl-address-header-extract-address wl-from)
            "foo@yourserver.com"
            "bar@yourserver.com"
            "zorg@otherserver.com"))

;; default IMAP4
(setq elmo-imap4-default-server "imap.yoursecondserver.com"
      elmo-imap4-default-user "zorg@yoursecondserver.com"
      elmo-imap4-default-authenticate-type 'clear ; raw
      elmo-imap4-default-stream-type 'ssl  ; direct
      elmo-imap4-default-port '993
      elmo-imap4-use-modified-utf7 t)

;; POP case
;; (setq elmo-pop3-default-server "localhost")
;; (setq wl-draft-send-mail-function 'wl-draft-send-mail-with-pop-before-smtp)

;;; Basic Settings
(setq wl-default-folder "%inbox"
      ;; wl-draft-folder "%[Google Mail]/Drafts" ; GMail IMAP
      wl-draft-folder "+drafts"
      wl-trash-folder "%[Google Mail]/Bin"
      wl-spam-folder "%[Google Mail]/Spam"
      wl-queue-folder ".queue"
      wl-default-spec "+"
      wl-draft-use-cache t
;;      wl-fcc "+outbox"
      wl-fcc "%[Google Mail]/Sent Mail"
      wl-fcc-force-as-read t
      wl-auto-uncheck-folder-list '("[$+].*")
      wl-auto-flush-queue nil
      wl-interactive-exit t
      wl-interactive-send t
      wl-thread-insert-opened t
      wl-stay-folder-window nil ; t for 3 panes
      wl-folder-window-width 35)
;; IMPORTANT: NEED HACK to point-min/point-max choice after sync...

;;; Messages' hack
(if (string-match "XEmacs\\|Lucid" emacs-version)
    '(progn
      (setq wl-message-truncate-lines nil)
      (add-hook 'mime-view-mode-hook 'no-line-wrap-this-buffer)) ; see confs/defs.el
    (setq wl-message-truncate-lines t))  ; normally truncate in emacs
(setq wl-draft-truncate-lines nil
      wl-draft-use-frame t
      wl-summary-indent-length-limit nil
      wl-summary-width nil
      wl-summary-divide-thread-when-subject-changed t ; divide thread by change of subject
      wl-thread-indent-level 2 ; change format of thread view
      wl-thread-have-younger-brother-str "+"
      wl-thread-youngest-child-str       "+"
      wl-thread-vertical-str             "|"
      wl-thread-horizontal-str           "-"
      wl-thread-space-str                " ")
;; (setq wl-auto-select-first t) ; display first message automatically
;; (setq wl-auto-select-next t) ; goto next folder when exit from summary
(setq wl-auto-select-next 'skip-no-unread) ; skip folder if there is no unread message
(setq wl-summary-move-order 'unread) ; jump to unread message in 'N' or 'P'

;; update-current-summaries
(defun mars/wl-update-current-summaries ()
  (let ((buffers (wl-collect-summary)))
    (while buffers
      (with-current-buffer (car buffers)
        (save-excursion
          (wl-summary-sync-update)))
      (setq buffers (cdr buffers)))))

;;; Biff
(defun mars/wl-desktop-notify ()
  (if (functionp 'display-external-pop-up)
      (let ((mars/multimedia-directory (if (eq system-type 'darwin)
                                           "/opt/local"
                                         "/usr")))
        (display-external-pop-up "Wanderlust"
                                 "You got mail"
                                 (concat mars/multimedia-directory "/share/icons/gnome/32x32/status/appointment-soon.png")
                                 (concat mars/multimedia-directory "/share/sounds/purple/receive.wav")))))
(defun mars/wl-update ()
  (mars/wl-update-current-summaries)
  (mars/wl-desktop-notify))
(defun mars/wl-biff ()
  "Set up biff on some folders"
  (setq wl-biff-notify-hook nil       ; IMPORTANT: 'wl-biff-check-folder-list reset otherwise
        ;;     wl-biff-check-folder-list
        ;;       (mapcar
        ;;        (lambda (item)
        ;;           (elmo-folder-name-internal
        ;;            (wl-folder-get-elmo-folder item)))
        ;;         (wl-folder-get-entity-list wl-folder-entity))
        wl-biff-check-folder-list '("%inbox"
                                    "%Inbox:\"martial@gmx.com\"@imap.gmx.com"
                                    "%Spam:\"martial@gmx.com\"@imap.gmx.com")
        wl-biff-use-idle-timer t
        wl-biff-check-interval 80)
  (add-hook 'wl-biff-notify-hook '(lambda () (mars/wl-update)))
  (wl-biff-start))

;; general hook
;; (add-hook 'wl-hook '(lambda () (mars/wl-biff)))
(mars/wl-biff)

;; folder hook
(add-hook 'wl-folder-mode-hook
          '(lambda () (hl-line-mode t)))
;; summary hook
(add-hook 'wl-summary-exec-hook
          '(lambda () (wl-summary-sync-update)))
(add-hook 'wl-summary-mode-hook
          '(lambda () (hl-line-mode t)))
;; message hook
(add-hook 'wl-message-buffer-created-hook
          '(lambda () (setq truncate-lines nil)))

;;; Network
;; cache setting.
;; (messages in localdir, localnews, maildir are not cached.)
(setq elmo-archive-use-cache nil
      elmo-archive-treat-file t
      elmo-nntp-use-cache t
      elmo-imap4-use-cache t
      elmo-pop3-use-cache t)
;; Enable disconnected operation in IMAP folder.
(setq elmo-enable-disconnected-operation t)
;; Store draft message in queue folder if message is sent in unplugged status.
(setq wl-draft-enable-queuing t)
;; when plug status is changed from unplugged to plugged,
;; queued message is flushed automatically.
(setq wl-auto-flush-queue t)

;;; Special Settings
;;;;;;;;;;;;;;;;;;;;

;; ML message displays ML name and ML sequence number in subject.
(setq wl-summary-line-format "%n%T%P %M/%D(%W)%h:%m %t%[%17(%c %f%) %] %#%~%s")

;; Set summary line format according to folder name.
(setq wl-folder-summary-line-format-alist
      '(("^%inbox\\.emacs\\.wl$" .
        "%-5l%T%P%M/%D %h:%m %-4S %[ %17f %] %t%C%s")
       ("^%" . "%T%P%M/%D %h:%m %-4S %[ %17f %] %t%C%s")
       ("^+" . "%n%T%P%M/%D %h:%m %-4S %[ %17f %] %t%C%s")))

;; Message Display Settings
(setq wl-message-ignored-field-list
            '(".*Received:" ".*Path:" ".*Id:" "^References:"
                  "^Replied:" "^Errors-To:"
                      "^Lines:" "^Sender:" ".*Host:" "^Xref:"
                          "^Content-Type:" "^Precedence:"
                              "^Status:" "^X-VM-.*:"))
(setq wl-message-visible-field-list '("^Message-Id:"))
;;(setq wl-message-auto-reassemble-message/partial t)

;; Re-fill messages that arrive poorly formatted (martial: by Ron Isaacson)
;; Simply type 'l' in summary to refill the top part (*message* keeps
;; its non-truncated shape) 'C-u l' refill the whole message including
;; quoted sections

(defun wl-summary-fill-message (all)
  (interactive "P")
  (if (and wl-message-buffer (get-buffer-window wl-message-buffer))
      (progn
        (wl-summary-toggle-disp-msg 'on)
        (with-current-buffer
          wl-message-buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (while (or (looking-at "^\\[[1-9]") (looking-at "^$"))
            (forward-line 1))
          (let ((buffer-read-only nil)
                (start (point))
                (end (if all
                         (point-max)
                       (if (or (re-search-forward "^[^>\n]* wrote:[ \n]+" nil
                                                  t)
                               (re-search-forward "^>>>>>" nil t)
                               (re-search-forward "^ *>.*\n *>" nil t))
                           (match-beginning 0)
                         (point-max)))))
            (save-restriction
              (narrow-to-region start end)
              (filladapt-mode 1)
              (fill-region (point-min) (point-max)))))
        (message "Message re-filled"))
    (message "No message to re-fill")))

(define-key wl-summary-mode-map "l" 'wl-summary-fill-message)

;;; Refiling
(setq wl-summary-auto-refile-skip-marks (list "n" "N")) ; unread messages are refiled
(setq elmo-msgdb-extra-fields
      '("x-ml-name"
        "x-mailing-list"
        "x-loop"
        "reply-to"
        "sender"
        "subject"
        "mailing-list"
        "List-Post"
        "X-BeenThere"
        "newsgroups"))
(setq wl-refile-rule-alist
      '(("From"
         ("\\[@ebay\\.\\]" . "+troc")
         ("\\[@paypal\\.\\]*" . "+troc"))
        ("To"
         ("tmux-users@lists.sourceforge.net" . "+tmux")
         ("cheol666@gmx\\.ch" . "+private"))
        ;; Seaside / Squeak / Smalltalk
        ("Reply-To"
         ("smalltalk-fr@googlegroups\\.com" . "+smalltalk-fr") ; since 2010
         ("sbe@iam\\.unibe\\.ch" . "+squeak-book-news")
         ("seaside@lists\\.squeakfoundation\\.org" . "+seaside")
         ("smallwiki@iam\\.unibe\\.ch" . "+seaside")
         ("squeak-fr@lists\\.squeakfoundation\\.org" . "+squeak-fr")
         ("squeak-dev@lists\\.squeakfoundation\\.org" . "+squeak-dev")
         ("beginners@lists\\.squeakfoundation\\.org" . "+squeak-newcomers")
         ;; Emacs
         ("carbon-emacs@googlegroups\\.com" . "+emacsen")
         ;; Clojure
         ("clojure@googlegroups\\.com" . "+clj")
         ;; Qi (Common Lisp based functional deductive language)
         ("Qilang@googlegroups\\.com" . "+Qi")
         ;; Factor (stack-based programming environment)
         ("factor-talk@lists\\.sourceforge\\.net" .  "+factor")
         ;; Squeak By Example (old)
         ("sbe-discussion@iam\\.unibe\\.be" . "+squeak-book-news"))
        ("X-ML-Name"
         ;; Wanderlust
         ("^Wanderlust" . "+wl"))
        ("Cc"
         ;; tmux
         ("tmux-users@lists.sourceforge.net" . "+tmux"))
        ("Subject"
         ;; Erlyweb / Yaws / Erlang
         ("\\[Erlyaws-list\\]" . "+erl")
         ;; Squeak News
         ("\\[sbe-discussion\\]" . "+squeak-book-news"))
        ("From"
         ("mailman-owner" . "+trash")) ; mailman bulletins (squeakfoundation, seaside...) go to trash
        ("List-Post"
         ("Qilang@googlegroups\\.com" . "+Qi")
         ("erlyaws-list@lists\\.sourceforge\\.net" . "+erl")
         ("sbe-discussion@iam\\.unibe\\.ch" . "+squeak-book-news"))
        ("X-BeenThere"
         ("erlyaws-list@lists\\.sourceforge\\.net" . "+erl")
         ("sbe-discussion@iam\\.unibe\\.ch" . "+squeak-book-news"))))

;; Marks to skip auto-refile (default is "N" "U" "!").
;; nil means all message is auto-refiled.
;; (setq wl-summary-auto-refile-skip-marks nil)

;; Auto refile this (with o) ::: addon from: http://www.kakura.jp/pg

(define-key wl-summary-mode-map "o" 'wl-summary-auto-refile-this-mail)
(defun wl-summary-auto-refile-this-mail () (interactive)
  (let* ((number (wl-summary-message-number))
         (dst
          (wl-folder-get-realname
           (wl-refile-guess (elmo-message-entity wl-summary-buffer-elmo-folder number)
                            wl-auto-refile-guess-functions))))
    (unless dst
      (setq dst (wl-summary-get-refile-destination 'refile number)))
    (wl-summary-refile number dst)
    (wl-summary-next)))

;; Spam filtering
;; (setq elmo-spam-scheme 'bogofilter)
;; (require 'wl-spam)

;; use ads' syntax highlight
(require 'wl-highlight-ad)
(set-face-foreground 'wl-highlight-ad-face "gray70")
(setq wl-highlight-ad-regexp-user-alist
      '(
        ;; keep <info@kakura.jp> rules for japanese/us ads
        ("@.*nikkeibp.co.jp" ("^─PR─+$"             . "^─+$") default)
        ("@egroups.co.jp"    ("^-.*~-->$"             . "^-+~->$"))
        ("@mainichi.co.jp"   ("^-+【ＡＤ】-+$"
                              . "\n.*\n.*\n.*\n.*\n.*\n-+\n.*\n.*\n.*\n.*\n.*\n-+\\|\n.*\n.*\n.*\n.*\n.*\n-+"))
        ("@pc.mycom.co.jp"   ("^―+\\[PR\\]―+$"      . "^―+$"))
        ("mag2"              ("^-【.*まぐ.*】-+$"     . "^-+$")
         ("^_@_+-PR-_+$"          . "^_+$") default)
        ("@hotmail.com"      ("^_+\n.*\\(MSN\\|マイクロソフト\\)" . "^$") continue)
        ("@yahoo.co"         ("^_+\nDo You Yahoo!\\?" . "^$") continue)
        (".*"                ("^\\[snip!\\]$"         . "")   default)))

;; Expiration + archiving
(setq wl-expire-alist
      '(("^\\+trash$"   (date 30) remove)
        ;; delete
        ("^\\+tmp$"     (date 15) trash)
        ;; re-file to wl-trash-folder
        ("^\\+outbox$"  (number 300) "$outbox;lha")
        ;; re-file to the specific folder
        ("^\\+ml/tmp$"  nil)
        ;; do not expire
        ("^\\+ml/wl$"   (number 500 510) wl-expire-archive-number1 t)
        ;; archive by message number (retaining numbers)
        ("^\\+ml/.*"    (number 300 310) wl-expire-archive-number2 t)
        ;; archive by a fixed number (retaining numbers)
        ("^\\+diary$"   (date 30) wl-expire-archive-date)
        ;; archive by year and month (numbers discarded)
        ("^\\%inbox$"   (date 30) wl-expire-archive-date)
        ))
(add-hook 'wl-summary-prepared-hook 'wl-summary-expire)

;; Signature with mu-cite (loaded in confs/mail.el) + reply/forward top format
(eval-after-load "mu-cite"
  '(progn
     (defvar *simple-reply/mu-cite* nil
       "Choose your mu-cite-top-format")
     (add-hook 'mail-citation-hook 'mu-cite-original)
     (setq signature-insert-at-eof t
           signature-delete-blank-lines-at-eof t
           signature-file-name "~/.signature.martial")
     (setq signature-file-alist
           '((("From" . "Me@") . "~/.signature.me")
             (("From" . "ohohoh@") . "~/.signature.santaclaus")
             (("From" . ".") . "~/.signature")))
     (add-hook
      'wl-init-hook
      '(lambda ()
         ;; Add support for (signature . "filename")
         (unless (assq 'signature wl-draft-config-sub-func-alist)
           (wl-append wl-draft-config-sub-func-alist
                      '((signature . wl-draft-config-sub-signature))))
         (defun mime-edit-insert-signature (&optional arg)
           "Redefine to insert a signature file directly, not as a tag."
           (interactive "P")
           (insert-signature arg))
         ;; Keep track of recently used Email addresses
         ;; (recent-addresses-mode 1)
         ))

     (defun wl-draft-config-sub-signature (content)
       "Insert the signature at the end of the MIME message."
       (let ((signature-insert-at-eof nil)
             (signature-file-name content))
         (goto-char (mime-edit-content-end))
         (insert-signature)))
     ;; insert prefix at all
     ;; (setq mu-cite-cited-prefix-regexp "^\x00\xff\x00$")
     ;; reply/forward mu-cite
     (if *simple-reply/mu-cite*
         (setq mu-cite-top-format '("On " date "," from " spake thus:\n"))
       (setq mu-cite-top-format '(mars/cite-label)))
     (add-hook 'mu-cite-instantiation-hook 'mars/mu-cite-set-methods)
     ;; (setq mu-cite-prefix-format '("> "))
     (defun mars/get-cite-label (from dt subject
                                      &optional ml-nm ml-cnt ml-cnt2 ml-cnt3)
       (let ((locales/fr (y-or-n-p "Français? "))
             (ml-prefix "[")
             (ml-sep ":")
             (ml-suffix "]")
             from-wrote)
         (if (= (length from) 0)
             (setq from (if locales/fr
                            "anonyme"
                            "anonymous")))
         (let ((locales/wrote (if locales/fr
                                  "a écrit ceci"
                                "spake thus"))
               (locales/date-prefix (if locales/fr
                                        "Le"
                                      "On")))
         (setq from-wrote (concat (mars/simplify-882from from)
                                  " "
                                  locales/wrote
                                  ":\n\n"))
         (if (not (string-match
                   "^\\([[(]\\)\\([a-zA-Z0-9._-]+\\)\\([
:,]\\)\\([0-9]+\\)\\([])]\\)" subject))
             ()
           (setq ml-prefix (mars/match-string 1 subject)
                 ml-nm     (mars/match-string 2 subject)
                 ml-sep    (mars/match-string 3 subject)
                 ml-cnt    (mars/match-string 4 subject)
                 ml-suffix (mars/match-string 5 subject)))
         (if (= (length ml-cnt) 0)
             (setq ml-cnt ml-cnt2))
         (if (= (length ml-cnt) 0)
             (setq ml-cnt ml-cnt3))
         (if (= (length dt) 0)
             from-wrote
           (if (or (= (length ml-nm) 0) (= (length ml-cnt) 0))
               (concat locales/date-prefix
                       " " (mars/simplify-822date dt 'at-time (not 'iso-ymd))
                       ",\n" from-wrote)
             (concat locales/date-prefix
                     " " (mars/simplify-822date dt nil (not 'iso-ymd))
                     ", " ml-prefix ml-nm ml-sep ml-cnt ml-suffix
                     ",\n" from-wrote))))))
     (defun mars/mu-cite-set-methods ()
       (setq mars/cite-methods-alist
             (cons
              (cons 'mars/cite-label
                    (function
                     (lambda ()
                       (mars/get-cite-label
                        (mu-cite-get-field-value "From")
                        (mu-cite-get-field-value "Date")
                        (mu-cite-get-field-value "Subject")
                        (mu-cite-get-value 'ml-name)
                        (mu-cite-get-value 'ml-count)))))
              mu-cite-methods-alist)))
     (setq mars/time-mon-alist
           '(("Jan" 1 "January" "Jan.")
             ("Feb" 2 "February" "Feb.")
             ("Mar" 3 "March" "Mar.")
             ("Apr" 4 "April" "Apr.")
             ("May" 5 "May" "May")
             ("Jun" 6 "June" "June")
             ("Jul" 7 "July" "July")
             ("Aug" 8 "August" "Aug.")
             ("Sep" 9 "September" "Sep.")
             ("Oct" 10 "October" "Oct.")
             ("Nov" 11 "November" "Nov.")
             ("Dec" 12 "December" "Dec.")))
     (setq mars/locales/fr/time-mon-alist
           '(("Jan" 1 "Janvier" "Jan.")
             ("Feb" 2 "Février" "Fév.")
             ("Mar" 3 "Mars" "Mar.")
             ("Apr" 4 "Avril" "Avr.")
             ("May" 5 "Mai" "Mai")
             ("Jun" 6 "Juin" "Juin")
             ("Jul" 7 "Juillet" "Juil.")
             ("Aug" 8 "Août" "Août")
             ("Sep" 9 "Septembre" "Sep.")
             ("Oct" 10 "Octobre" "Oct.")
             ("Nov" 11 "Novembre" "Nov.")
             ("Dec" 12 "Décembre" "Déc.")))
     (defun mars/time-mon-to-int (str)
       (or (nth 1 (assoc (capitalize str) mars/time-mon-alist)) 0))
     (defun mars/time-mon-to-month (str)
       (or (nth 2 (assoc (capitalize str) mars/time-mon-alist)) str))
     (defun mars/time-mon-to-mon4 (str)
       (or (nth 3 (assoc (capitalize str) mars/time-mon-alist)) str))
     (defun mars/simplify-822date (str &optional at-time iso-ymd)
       "Simplify `Date' in RFC 822 message.
Note: the simplified date is not conformed to RFC 822."
       (if (not (string-match "\\([0-9]+\\)[ \t]+\\([a-zA-Z]+\\)[ \t]+\\([0-9]+\\)[
\t]+\\([0-9]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?\\([
\t]+\\([-+0-9a-zA-Z]+\\)\\)?" str))
           str
         (let (day mon year month ymd hour min ampm tmzn)
           (setq day (string-to-int (mars/match-string 1 str)))
           (setq mon (mars/match-string 2 str))
           (setq year (string-to-int (mars/match-string 3 str)))
           (if (< year 50)
               (setq year (+ year 2000))
             (if (< year 1000)
                 (setq year (+ year 1900))))
           (if iso-ymd
               (setq ymd (format "%04d-%02d-%02d" year (mars/time-mon-to-int mon) day))
             (setq ymd (concat (mars/time-mon-to-month mon) " "
                               (int-to-string day) ", "
                               (int-to-string year))))
           (if (not at-time)
               ymd
             (setq hour (string-to-int (mars/match-string 4 str)))
             (setq min (mars/match-string 5 str))
             (if (match-beginning 9)
                 (setq tmzn (concat " " (mars/match-string 9 str))))
             (if (< hour 12)
                 (setq ampm "AM")
               (setq ampm "PM")
               (setq hour (- hour 12)))
             (if (= hour 0)
                 (setq hour 12))
             (concat ymd " at " (int-to-string hour) ":" min ampm tmzn)))))
     (defun mars/simplify-822from (str)
       "Simplify `From' in RFC 822 message."
       (let (name addr)
         (if (string-match "\\([^@<> [EMAIL PROTECTED]@<> \n\t]+\\)" str)
             (setq addr (mars/trim-spc (mars/match-string 1 str) 'simplify)))
         (if (string-match "\\(.*\\)<[^@<> [EMAIL PROTECTED]@<> \n\t]+>" str)
             (progn
               (setq name (mars/match-string 1 str))
               ;; remove non-ascii comment
               (if (string-match "([^)]*[^\000-\177][^)]*)" name)
                   (setq name (replace-match "" nil t name)))
               (setq name (mars/trim-spc name 'simplify))))
         (if (and (or (= (length name) 0) (string-match "[^\000-\177]" name))
                  (string-match
                   "<?[^@<> [EMAIL PROTECTED]@<> \n\t]+>?[ \n\t]*(\\([^)]+\\))" str))
             (setq name (mars/trim-spc (mars/match-string 1 str) 'simplify)))
         (if (and (or (= (length name) 0) (string-match "[^\000-\177]" name))
                  (string-match
                   "(\\([^)]+\\))[ \n\t]*<[^@<> [EMAIL PROTECTED]@<> \n\t]+>" str))
             (setq name (mars/trim-spc (mars/match-string 1 str) 'simplify)))
         (if (or (= (length name) 0) (string-match "[^\000-\177]" name))
             addr
           (mars/trim-spc (concat name " <" addr ">") 'simplify))))
     (defun mars/trim-spc (str &optional simplify)
       "Trimming white spaces."
       (if (string-match "^[ \n\t]+" str)
           (setq str (replace-match "" nil t str)))
       (if (string-match "[ \n\t]+$" str)
           (setq str (replace-match "" nil t str)))
       (if (not simplify)
           ()
         (while (string-match "[\n\t]+" str)
           (setq str (replace-match " " nil t str)))
         (while (string-match "  +" str)
           (setq str (replace-match " " nil t str))))
       str)
     (if (fboundp 'match-string-no-properties)
         (defalias 'mars/match-string 'match-string-no-properties)
       (defalias 'mars/match-string 'match-string))
     ))

;; Minimal Header Display
(setq wl-message-visible-field-list
      '("Date:" "^From:" "^To:" "^Delivered-To:" "^Subject:" "^Reply-[tT]o:" "^Cc:" "^User-Agent:"))
(setq wl-message-ignored-field-list '(".*"))

;; Some IMAP servers (like Dovecot) interpret the call to
;; `elmo-folder-exists-p' as a client query that should unmark new
;; mails and leave them as simply Unread (instead of Recent).
(defun mars/elmo-folder-exists-p (folder) t)
(defadvice wl-biff-check-folders (around my-disable-exists-test activate)
  "Disable `elmo-folder-exists-p' and make it return t."
  (let ((real-elmo-folder-exists-p
     (symbol-function 'elmo-folder-exists-p)))
    (fset 'elmo-folder-exists-p (symbol-function 'mars/elmo-folder-exists-p))
    ad-do-it
    (fset 'elmo-folder-exists-p real-elmo-folder-exists-p))) ; from http://www.huoc.org/hacks/dotemacs/wlrc.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wl ends here
