;;; window-manager.el ---
;;
;; Filename: window-manager.el
;; Description: Buffers & Windows Management
;; Author: Martial Boniou
;; Maintainer: Martial Boniou
;; Created: Sat Feb 19 11:17:32 2011 (+0100)
;; Version: 0.8.1
;; Last-Updated: Fri May 31 17:07:44 2013 (+0200)
;;           By: Martial Boniou
;;     Update #: 83
;; URL: 
;; Keywords: 
;; Compatibility: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary: everything to split / tile / cycle / archive windows /
;;              autosave current window configuration / no windowing
;;              mouse support
;;  Note: need handmade tiling and buffer-move autoloads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log: was merged with box.el (previous name of `file-manager.el'
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

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'noaccess)
(require 'adapter)

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; revive-plus
;; see. `user-init-file' because window configuration
;;       persistence has no sense when Emacs boots partially


;;; ROTATE-WINDOWS + TOGGLE-SINGLE-WINDOW
(defun rotate-windows-helper(x d)
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x))) (rotate-windows-helper (cdr x) d)))

(defun mars/rotate-windows (&optional direction selected stack)
  "Rotate windows. direction is 'DOWN by default ('UP othewise). Recommended with tiling effect."
  (let ((list (or stack (window-list nil 0))))
    (unless (null (cdr list))
      (let ((dir  (if (not (or (equal direction 'up)
                               (equal direction 'down)))
                      'down
                    direction)))
        (if (equal dir 'down)
            (progn
              (rotate-windows-helper list (window-buffer (car list)))
              (when selected
                (select-window (car (last list)))))
          (progn
            (rotate-windows-helper (reverse list) (window-buffer (car (last list))))
            (when selected
              (select-window (cadr list)))))))))

;;; WINDMOVE
(eval-after-load "windmove"
  '(progn
     (defun windmove-do-window-select (dir &optional arg window)
       "Switch to another frame if null `other-window'."
       (let ((other-window (windmove-find-other-window dir arg window)))
         (cond ((null other-window)
                (select-frame-set-input-focus
                 (if (or (eq dir 'up) (eq dir 'left))
                     (previous-frame)
                   (next-frame))))
               ((and (window-minibuffer-p other-window)
                     (not (minibuffer-window-active-p other-window)))
                (message "Minibuffer is inactive"))
               (t (select-window other-window)))))))

;;; NEW-BALANCE-WINDOWS
;; Ehud Karni's BALANCE-WINDOWS
(defun balance-windows (&optional horizontally)
  "Make all visible windows on the current frame the same size (approximately).
If optional prefix arg is not given, \"same size\" is same height.
When prefix arg is given,  \"same size\" is same width."
  (interactive "P")
  (let* (count size w cmjr resize
               (edge (if horizontally 0 1))  ;; Minor field to sort by 0=LEFT, 1=TOP
               (mjr (- 1 edge))              ;; Major field to sort
               (far (+ 2 edge))              ;; far edge (right/bottom) - for current
               size
               (windows nil)                 ;; list of windows
               (ix 0)
               nwin                          ;; number of windows
               (curw (selected-window))      ;; selected window (to return to)
               )
    ;; Build and sort list of all windows on frame
    (save-window-excursion
      (walk-windows (function (lambda (w)
                                (let ((ltrb (window-edges w)))
                                  (setq windows (cons (list
                                                       (nth mjr  ltrb)
                                                       (nth edge ltrb)
                                                       (nth far  ltrb)
                                                       w) windows)))))
                    'nomini)
      (setq windows (sort windows (lambda (e1 e2)
                                    (if (< (nth 0 e1) (nth 0 e2))
                                        t
                                      (if (= (nth 0 e1) (nth 0 e2))
                                          (if (< (nth 1 e1) (nth 1 e2))
                                              t)))))))
    (setq nwin (length windows))
    ;; add 1 extra entry (for while check)
    (setq windows (append windows '((-1 -1 -1 nil))))

    (while (< ix nwin)                      ; walk on all (sorted) windows
      (setq count ix)                     ; count the windows in 1 column (or row)
      (setq cmjr (car (nth ix windows)))  ; column / raw identification
      (while (= cmjr (car (nth ix windows)))  ; same column / row
        (setq ix (1+ ix)))              ; next window
      (setq count (- ix count))
      (if (/= count 1)                    ; do only if more than one window in this column/row
          (let ((gix (- ix count)))
            (setq size (- (nth far (window-edges (nth 3 (nth (1- ix)
                                                             windows))))
                          (nth edge (window-edges (nth 3 (nth (- ix count)
                                                              windows))))))
            (setq size (/ (+ size count -1) count)) ; average window size
            ;; (message "Size=%d" size)
            (while (< gix ix)
              (setq w (nth 3 (nth gix windows)))
              (setq resize (- size (- (nth far (window-edges w))
                                      (nth edge (window-edges w)))))
              ;; (message "Window=%s  resize=%d" w resize)
                                        ; don't resize by 1 character/line
              (if (or (> resize 1)
                      (< resize -1))
                  (progn

                    ;; (sit-for 2)

                    (select-window w)       ; window to work on
                    (enlarge-window resize horizontally) ; FIXME: 'preserve)
                    ;; (sit-for 2)
                    ))
              (setq gix (1+ gix))))))

    ;; (message "")
    (select-window curw)))

;;; NO WINDOWING MOUSE SUPPORT
;; enable mouse in xterm, RXvts and iTerm2.app
(unless window-system
  (xterm-mouse-mode t)
  ;; mouse wheel support
  (when (locate-library "mwheel-term")
   (require 'mwheel-term)))

(provide 'window-manager)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; window-manager.el ends here
