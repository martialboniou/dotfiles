;;; vim-molokai-theme.el --- 
;; 
;; Filename: vim-molokai-theme.el
;; Description: Molokai theme based on Molokai theme on TextMate then on Vim
;; Author: Bin Huang
;; Maintainer: Martial Boniou
;; Created: Tue Dec  3 17:32:06 2013 (+0100)
;; Version: 0.1
;; Package-Requires: ()
;; Last-Updated: Tue Dec  3 18:02:14 2013 (+0100)
;;           By: Martial Boniou
;;     Update #: 3
;; URL: https://github.com/martialboniou/Dots/blob/master/emacs/emacs.d.symlink/data/Themes/vim-molokai-theme.el
;; Doc URL: https://github.com/hbin/molokai-theme/blob/master/molokai-theme.el
;; Keywords: 
;; Compatibility: Emacs 24
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
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

(deftheme vim-molokai
  "A color theme based on TextMate molokai theme")

(let ((class '((class color) (min-colors 89)))
      ;; molokai palette
      (molokai-white          "#ffffff")
      (molokai-fg             "#f8f8f0")
      (molokai-red            "#ff0000")
      (molokai-pink           "#f92672")
      (molokai-orange+5       "#ef5939")
      (molokai-orange         "#fd971f")
      (molokai-yellow         "#ffff00")
      (molokai-darkgoldenrod  "#e6db74")
      (molokai-wheat          "#c4be89")
      (molokai-olive          "#808000")
      (molokai-chartreuse     "#a6e22e")
      (molokai-lime           "#00ff00")
      (molokai-green          "#008000")
      (molokai-darkwine       "#1e0010")
      (molokai-maroon         "#800000")
      (molokai-wine           "#960050")
      (molokai-teal           "#008080")
      (molokai-aqua           "#00ffff")
      (molokai-blue           "#66d9ef")
      (molokai-slateblue      "#7070f0")
      (molokai-purple         "#ae81ff")
      (molokai-palevioletred  "#d33682")
      (molokai-grey-2         "#bcbcbc")
      (molokai-grey-1         "#8f8f8f")
      (molokai-grey           "#808080")
      (molokai-grey+2         "#403d3d")
      (molokai-grey+3         "#4c4745")
      (molokai-grey+5         "#232526")
      (molokai-bg             "#1b1d1e")
      (molokai-grey+10        "#080808")
      (molokai-dark           "#000000")
      (molokai-base01         "#465457")
      (molokai-base02         "#455354")
      (molokai-base03         "#293739")
      (molokai-dodgerblue     "#13354a"))
  (custom-theme-set-faces
   'vim-molokai
   ;; base
   `(default ((t (:background ,molokai-bg :foreground ,molokai-fg))))
   `(cursor ((t (:background ,molokai-fg :foreground ,molokai-bg))))
   `(fringe ((t (:foreground ,molokai-base02 :background ,molokai-bg))))
   `(highlight ((t (:background ,molokai-grey))))
   `(region ((t (:background  ,molokai-grey+2 :inverse-video t))))
   `(warning ((t (:foreground ,molokai-palevioletred :weight bold))))
   ;; font lock
   `(font-lock-builtin-face ((t (:foreground ,molokai-chartreuse))))
   `(font-lock-comment-face ((t (:foreground ,molokai-base01))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,molokai-base01))))
   `(font-lock-constant-face ((t (:foreground ,molokai-purple))))
   `(font-lock-doc-string-face ((t (:foreground ,molokai-darkgoldenrod))))
   `(font-lock-function-name-face ((t (:foreground ,molokai-chartreuse))))
   `(font-lock-keyword-face ((t (:foreground ,molokai-pink :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,molokai-wine))))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,molokai-darkgoldenrod))))
   `(font-lock-type-face ((t (:foreground ,molokai-blue :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,molokai-orange))))
   `(font-lock-warning-face ((t (:foreground ,molokai-palevioletred :weight bold))))
   ;; bonus faces = number & notify & todo
   `(font-lock-number-face ((t (:inherit font-lock-constant-face))))
   `(font-lock-negation-char-face ((t (:inherit font-lock-number-face))))
   `(font-lock-notify-face ((t (:inherit font-lock-warning-face))))
   `(font-lock-todo-face ((t (:foreground ,molokai-base02 :italic t :slant italic))))
   ;; mode line
   `(mode-line ((t (:foreground ,molokai-fg :background ,molokai-base03 :box nil))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-inactive ((t (:foreground ,molokai-fg :background ,molokai-base02 :box nil))))
   ;; search
   `(isearch ((t (:foreground ,molokai-dark :background ,molokai-wheat :weight bold))))
   `(isearch-lazy-highlight-face ((t (:foreground ,molokai-base02 :background ,molokai-fg))))
   `(isearch-fail ((t (:foreground ,molokai-wine :background ,molokai-darkwine))))
   ;; linum-mode
   `(linum ((t (:foreground ,molokai-grey-2 :background ,molokai-grey+5))))
   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,molokai-grey+5)) (t :weight bold)))
   `(hl-line ((,class (:background ,molokai-grey+5)) (t :weight bold)))
   ;; ido-mode
   ;; flycheck
   ;; paren-face
   `(paren-face ((t (:inherit font-lock-builtin-face ))))
   ;; show-paren
   `(show-paren-match-face
     ((t (:foreground ,molokai-dark :background ,molokai-orange :bold t)))) ; +MatchParen
   `(show-paren-match-face
     ((t (:foreground ,molokai-pink :background ,molokai-grey+5 :bold t)))) ; +ErrorMsg
   ;; auto-complete
   `(ac-candidate-face ((t (:foreground ,molokai-blue :background ,molokai-dark)))) ; Pmenu
   `(ac-selection-face ((t (:foreground ,molokai-fg :background ,molokai-grey)))) ; PmenuSel
   ;; ediff
   `(ediff-current-diff-A ((t (:foreground ,molokai-grey-1 :background ,molokai-grey+3)))) ; +DiffChange
   `(ediff-fine-diff-A ((t (:background ,molokai-grey+3 :italic t :bold t)))) ; +DiffText
   ;; rainbow-delimiters
   ;; highlight-symbols
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'vim-molokai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vim-molokai-theme.el ends here
