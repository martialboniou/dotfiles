;;; media.el ---
;;
;; Filename: media.el
;; Description: 
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Jan 19 20:16:06 2008
;; Version: 0.3
;; Last-Updated: Sat Apr 25 18:32:51 2015 (+0200)
;;           By: Martial Boniou
;;     Update #: 130
;; URL: 
;; Keywords: 
;; Compatibility: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: image-mode + eimp + iimage + mpg123 + emms 3
;;
;;              was emms-init.el initially
;;              2010-03: use all in emms to get the browser
;;                       and display covers
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log: switch from emms2 to emms3 (cvs branches through darcs)
;;
;;  TODO: split `loaddefs' in `loaddefs' + `cedet-loaddefs' autoloads
;;        in order not to load CEDET when media is loaded as standalone
;;        (b/c no need extra autoloads and CEDET in this case)
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
;; Floor, Boston, MA 02110-1301, USA.;;; emms-init.el --- Initialize emms

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'www)
(require 'preamble)

;;; IMAGE-MODE
;;
(add-to-list 'auto-mode-alist '("\\.\\(bmp\\|BMP\\)$" . image-mode))
(eval-after-load "image-mode"
  '(progn
     (auto-image-file-mode 1)

     (defun my-image-next-by-number ()
       (interactive)
       (let ((file-name (buffer-file-name))
             base num suffix
             num-width fmt)
         (unless (string-match
                  "^\\(.*[^0-9-]\\)?\\(?:[0-9]+-\\)?\\([0-9]+\\)\\(\\.[^.]+\\)?$"
                  file-name)
           (error "Improper file name"))
         (setq base (match-string 1 file-name))
         (setq num (match-string 2 file-name))
         (setq suffix (match-string 3 file-name))
         (setq num-width (length num))
         (setq fmt (format "%%s%%0%dd%%s" num-width))
         (setq num (1+ (string-to-number num)))
         (setq file-name (format fmt base num suffix))
         (unless (file-exists-p file-name)
           (setq fmt (format "%%s%%0%dd-*%%s" num-width))
           (setq file-name (format fmt base num suffix))
           (setq file-name (file-expand-wildcards file-name))
           (if file-name
               (setq file-name (car file-name))
             (error "No more files")))
         (find-alternate-file file-name)))

     (defun my-image-scroll-up-or-next-by-number ()
       (interactive)
       (let* ((image (image-get-display-property))
              (edges (window-inside-edges))
              (win-height (- (nth 3 edges) (nth 1 edges)))
              (img-height (ceiling (cdr (image-size image)))))
         (if (< (+ win-height (window-vscroll nil t))
                img-height)
             (image-scroll-up)
           (my-image-next-by-number))))

     (bind-key image-mode-map "SPC"
               #'my-image-scroll-up-or-next-by-number)))

;;; EIMP
;;
(when (executable-find "mogrify")
  (add-hook 'image-mode-hook 'eimp-mode))

;;; IIMAGE
;; TODO: test this
(mapc #'(lambda (x)
          (add-hook x 'turn-on-iimage-mode))
      '(Info-mode-hook texinfo-mode-hook wikipedia-mode)) ; info/wiki case
(eval-after-load "org"             ; org-mode case
  '(progn
     (require 'iimage)
     (add-to-list 'iimage-mode-image-regex-alist
                  (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                                "\\)\\]")  1))
     (defun org-toggle-iimage-in-org ()
       "Display images in your org file."
       (interactive)
       (if (face-underline-p 'org-link)
           (set-face-underline-p 'org-link nil)
         (set-face-underline-p 'org-link t))
       (iimage-mode))))


;;; MPG123
;;
;; (autoload 'mpg123 "mpg123" "A Front-end to mpg123/ogg123" t)

;;; EMMS 3
;;
(when (locate-library "emms")
  (let* ((emms-path (file-name-directory (locate-library "emms")))
         (emms-bin-path (joindirs (file-name-directory (directory-file-name emms-path)) "src")))
    (if-bound-call emms-devel)      ; CHECK: emms-start-browse on <F2>
    (eval-after-load "emms"
      '(progn
         (display-time)
         ;; (setq default-file-name-coding-system 'utf-8) ; maybe not correct
         (emms-default-players)

         ;; fix processes for `emms-stream-info'
         (eval-after-load "emms-stream-info"
           '(progn
              ;; - mplayer need cache or "Icy Info" field may not be reached
              (defun emms-stream-info-mplayer-backend (url)
                "Backend command for running mplayer on URL. NEED CACHE"
                (condition-case excep
                    (call-process "mplayer" nil t nil
                                  "-endpos" "0" "-vo" "null" "-ao" "null"
                                  url)
                  (file-error
                   (error "Could not find the mplayer backend binary"))))
              ;; - vlc requires vlc://quit to properly end
              (defun emms-stream-info-vlc-backend (url)
                "Backend command for running VLC on URL."
                (condition-case excep
                    (call-process "vlc" nil t nil
                                  "-vvv" "--intf" "dummy" "--stop-time" "1" "--noaudio"
                                  url "vlc://quit")
                  (file-error
                   (error "Could not find the VLC backend binary"))))))

         ;; use the script by Fang Lungang to create cover_small/cover_med

         ;; emms: customization
         ;; (add-to-list 'emms-info-functions 'emms-info-mpd)
         ;; (setq emms-player-list 'emms-player-mpd)

         ;; need compiled libtag-based binary using: make emms-print-metadata
         (setq exec-path (append (list emms-bin-path) exec-path))
         (require-if-located 'emms-info-libtag)
         (eval-after-load "emms-info-libtag"
           '(setq emms-info-functions '(emms-info-libtag)))
         ;; (setq emms-player-list 'emms-player-mpd)

         ;; Custom variables

         (setq emms-info-asynchronously t)
         (setq emms-mode-line-mode-line-function
               (lambda nil
                 (let ((track (emms-playlist-current-selected-track)))
                   (let ((title (emms-track-get track 'info-title)))
                     (let ((name (emms-track-get track 'name)))
                       (if (not (null title))
                           (format emms-mode-line-format title)
                         (if (not (null (string-match "^url: " (emms-track-simple-description track))))
                             (format emms-mode-line-format "Internet Radio")
                           (setq name2 (replace-regexp-in-string ".*\/" "" name))
                           (format emms-mode-line-format name2))))))))
         (emms-mode-line-disable)
         (emms-mode-line-enable)
         (setq emms-track-description-function
               (lambda (track)
                 (let ((artist (emms-track-get track 'info-artist))
                       (album  (emms-track-get track 'info-album))
                       (number (emms-track-get track 'info-tracknumber))
                       (title  (emms-track-get track 'info-title)))
                   (if (and artist album title)
                       (if number
                           (format "%s: %s - [%03d] %s" artist album (string-to-int number) title)
                         (format "%s: %s - %s" artist album title))
                     (emms-track-simple-description track)))))
         ;; (set-face-attribute 'emms-playlist-track-face    nil :font "DejaVu Sans-10")
         ;; (set-face-attribute 'emms-playlist-selected-face nil :background "White" :foreground "Firebrick")
         ;; Initialization
         (define-emms-simple-player mplayer '(file url)
           "\\.[mM][pP][23]$" "mplayer")

         (define-emms-simple-player mplayer-ogg '(file)
           (regexp-opt '(".ogg" ".OGG" ".FLAC" ".flac" )) "mplayer")

         (define-emms-simple-player mplayer-playlist '(streamlist)
           "http://" "mplayer" "-playlist")

         (define-emms-simple-player mplayer-list '(file url)
           (regexp-opt '(".m3u" ".pls")) "mplayer" "-playlist")

         (define-emms-simple-player mplayer-video '(file url)
           (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv"
                         ".wma" ".mov" ".avi" ".divx" ".ogm" ".asf"
                         ".mkv" "http://")) "mplayer")

         (setq emms-player-list '(emms-player-mplayer-mp3
                                  emms-player-mplayer-ogg
                                  emms-player-mplayer-playlist
                                  emms-player-mplayer-video
                                  emms-player-mplayer-list
                                  ))

         ;; When asked for emms-play-directory,
         ;; always start from this one
         (setq emms-source-file-default-directory (joindirs "~" "Music")
               emms-stream-default-list '(       ; Metal Rules
                                          ("Metal On: The Thrasher"
                                           "http://94.23.244.89:8006" 1 url)
                                          ("2Late2Heal"
                                           "http://listen.radionomy.com/2late2healradio-001" 1 url)
                                          ("ThrashZoneRadio"
                                           "http://listen.radionomy.com/thrash-zone-radio" 1 url)
                                          ("PowerMetal.cl"
                                           "http://listen.radionomy.com/powermetalcl" 1 url)
                                          ("Metal On: The Brutal"
                                           "http://94.23.244.89:8004" 1 url)
                                          ("Metal On: The Heavy"
                                           "http://94.23.244.89:8002" 1 url)
                                          ("H4XED"
                                           "http://sc-01.h4xed.us:7080" 1 url)
                                          ("DarkSoul 7 Extreme Metal Radio"
                                           "http://www.darksoul7.com:8000" 1 url)))
         ;; Type M-x emms-add-all to add all music in your ~/Music directory.

         (defun mars/emms-any-streams ()
           (interactive)
           (if (boundp 'anything-c-source-emms-streams)
               (anything-emms)
             (emms-streams)))

         (defun mars/safe-emms-playlists ()
           (let ((buf (emms-playlist-buffer-list)))
             (and buf
                  (plain-buffer-list buf))))

         (defun mars/safe-emms-start-stop ()
           (interactive)
           (condition-case nil
               (if (mars/safe-emms-playlists)
                   (emms-pause)                ; try pause/resume
                 (error "no playlist"))
             (error
              (progn
                ;; remove the buffer created by 'EMMS-PAUSE
                (mapc
                 #'(lambda (elt) (kill-buffer elt))
                 (mars/safe-emms-playlists))
                (require 'emms-history)
                (emms-history-load)            ; try to restore history
                ;; fetch the last living playlist saved in history
                (let ((last-loaded-emms-playlist
                       (car (mars/safe-emms-playlists))))
                  (if (null last-loaded-emms-playlist)
                      (emms)                   ; emms if nothing in history
                    (if (null emms-player-playing-p) ; if stopped or paused
                        (save-current-buffer
                          (set-buffer last-loaded-emms-playlist)
                          (condition-case nil
                              ;; try a smart start of the current or next track
                              (emms-playlist-mode-play-smart)
                            (error
                             (cl-flet ((info-emms-unable-to-play
                                        (which-track)
                                        (message "Information: [mars] emms: unable to play the %s track in the last playlist saved in history" which-track)))
                               (info-emms-unable-to-play "next")
                               (emms-playlist-mode-first)
                               (condition-case nil
                                   ;; try a smart start of the very first track
                                   (emms-playlist-mode-play-smart)
                                 (error
                                  (info-emms-unable-to-play "first")
                                  (emms))))))) ; else open a default playlist to load
                      (when emms-player-paused-p
                        (condition-case nil
                            (emms-pause)
                          (error (emms)))))))))))))))

(provide 'media)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; media.el ends here
