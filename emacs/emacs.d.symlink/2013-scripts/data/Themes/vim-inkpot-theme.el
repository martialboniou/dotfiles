;;; vim-inkpot-theme.el --- 
;; 
;; Filename: vim-inkpot-theme.el
;; Description: Inkpot colorscheme
;; Author: Ciaran McCreesh
;; Maintainer: Martial Boniou
;; Created: Tue Dec  3 17:43:12 2013 (+0100)
;; Version: 0.1
;; Package-Requires: ()
;; Last-Updated: Tue Dec  3 18:03:39 2013 (+0100)
;;           By: Martial Boniou
;;     Update #: 2
;; URL: https://github.com/martialboniou/Dots/blob/master/emacs/emacs.d.symlink/data/Themes/vim-inkpot-theme.el
;; Doc URL: https://github.com/ciaranm/inkpot
;; Keywords: 
;; Compatibility: Emacs 24
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: TODO: a version in 256 colors
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

(deftheme vim-inkpot
  "A color theme based on inkpot Vim colorscheme")

(custom-theme-set-faces
 'vim-inkpot
 '(default ((t (:foreground "#cfbfad" :background "#1e1e27")))) ; +Normal
 '(border ((t (:foreground "#2e2e37"))))                        ; +Cursor guifg
 '(cursor ((t (:foreground "#8b8bff"))))                        ; +Cursor guibg
 '(region ((t (:foreground "#eeeeee" :background "#4e4e8f"))))  ; +Visual
 '(highlight ((t (:background "#2e2e37"))))                     ; +CursorLine
 '(hl-line   ((t (:background "#2e2e37"))))                     ; +CursorLine
 '(fringe ((t (:foreground "#8b8bcd" :background "#2e2e2e"))))  ; +LineNr
 '(paren-face ((t (:foreground "#c080d0"))))                    ; +Special
 '(show-paren-match-face ((t (:foreground "#cfbfad" :background "#4e4e8f" :bold nil)))) ; +MatchParen
 '(show-paren-mismatch-face
   ((((class color) (min-colors 4096)) (:foreground "#ffffff" :background "#6e2e2e" :bold t))
    (((class color) (min-colors 256)) (:foreground "#5fd7af" :background "#0087df" :bold t)))) ; +Error
 '(isearch ((t (:bold t :foreground "#303030" :background "#ad7b57")))) ; +Search
 '(modeline ((t (:bold t :foreground "#b9b9b9" :background "#3e3e5e")))) ; +StatusLine
 '(modeline-inactive ((t (:foreground "#708090" :background "#3e3e5e")))) ; +StatusLineNC | User2
 '(modeline-buffer-id ((t (:bold t :foreground "#b9b9b9" :background "#3e3e5e")))) ; +StatusLine
 '(minibuffer-prompt ((t (:bold t :foreground "#7070a0")))) ; +User2 guifg
 '(ediff-current-diff-A ((((class color) (min-colors 16))
                          (:foreground "#ffffcd" :background "#306b8f"))
                         (((class color)) (:foreground "white" :background "blue3"))
                         (t (:inverse-video t)))) ; +DiffChange
 '(ediff-fine-diff-A ((((class color) (min-colors 16))
                       (:foreground "#ffffcd" :background "#4a2a4a"))
                      (((class color)) (:foreground "white" :background "green4"))
                      (t (:inverse-video t)))) ; +DiffText
 '(font-lock-builtin-face ((t (:foreground "#c080d0")))) ; +Special
 '(font-lock-comment-face ((t (:foreground "#cd8b00" :italic nil)))) ; +Comment
 '(font-lock-constant-face ((t (:foreground "#ffcd8b")))) ; +Constant
 '(font-lock-doc-face ((t (:foreground "#cd8b00" :italic nil)))) ; +Comment
 '(font-lock-function-name-face ((t (:foreground "#cfbfad" :bold t)))) ; +Normal guifg (bold)
 '(font-lock-keyword-face ((t (:foreground "#808bed")))) ; +Statement
 '(font-lock-preprocessor-face ((t (:foreground "#409090")))) ; +PreProc
 '(font-lock-number-face ((t (:foreground "#f0ad6d")))) ; +Number
 '(font-lock-reference-face ((t (:bold t :foreground "#808bed")))) ; +Statement
 '(font-lock-string-face ((t (:foreground "#ffcd8b" :background "#404040" :italic nil)))) ; +String
 '(font-lock-type-face ((t (:foreground "#ff8bff")))) ; +Type
 '(font-lock-variable-name-face ((t (:foreground "#ff8bff")))) ; +Identifier
 '(font-lock-warning-face
   ((((class color) (min-colors 4096)) (:foreground "#ffffff" :background "#6e2e2e" :bold t))
    (((class color) (min-colors 256)) (:foreground "#5fd7af" :background "#0087df" :bold t))))) ; +Error

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'vim-inkpot)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vim-inkpot-theme.el ends here
