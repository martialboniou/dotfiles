;;; vim-wombat256mod-theme.el --- 
;; 
;; Filename: vim-wombat256mod-theme.el
;; Description: Wombat colorscheme 256 mod (fork from 2010/07/23 version)
;; Author: David Liang + Bespalov & Nielsen (dengmao <at> gmail <dot> com)
;; Maintainer: Martial Boniou
;; Created: Mon Dec  2 11:40:54 2013 (+0100)
;; Version: 0.1
;; Package-Requires: ()
;; Last-Updated: Mon Dec  2 17:44:06 2013 (+0100)
;;           By: Martial Boniou
;;     Update #: 70
;; URL: https://github.com/martialboniou/Dots/blob/master/emacs/emacs.d.symlink/data/Themes/vim-wombat256mod-theme.el
;; Doc URL: http://www.vim.org/scripts/script.php?script_id=2465
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

(deftheme vim-wombat256mod
  "A color theme based on the Vim colorscheme wombat256mod-2.0mod by David Liang (http://www.vim.org/scripts/script.php?script_id=2465)")

(let* ((full-color-class '((class color) (min-colors 4096)))
       (mini-color-class '((class color) (min-colors 256)))
        ;; -wombat256mod default color class palette
       (wombat-common-statusline-fg   "#ffffd7")
       (wombat-common-statusline-bg   "#444444")
       (wombat-common-linenr-bg       "#080808")
       (wombat-common-pmenusel-fg     wombat-common-linenr-bg)
       (wombat-common-statuslinenc-bg wombat-common-statusline-bg)
       (wombat-common-warningmsg      "#ff5f55")
       (wombat-common-pmenu-fg        wombat-common-statusline-fg)
       (wombat-common-pmenu-bg        wombat-common-statusline-bg)
       ;; -wombat256mod full palette
       (wombat-normal-fg              "#e3e0d7")
       (wombat-normal-bg              "#242424")
       (wombat-cursor-line            "#32322f")
       (wombat-cursor                 "#eae788")
       (wombat-visual-fg              "#c3c6ca")
       (wombat-visual-bg              "#554d4b")
       (wombat-linenr-fg              "#857b6f")
       (wombat-special-fg             "#eadead")
       (wombat-comment-fg             "#9c998e")
       (wombat-function-fg            "#cae982")
       (wombat-constant-fg            "#e5786d")
       (wombat-preproc-fg             wombat-constant-fg)
       (wombat-identifier-fg          "#cae982")
       (wombat-keyword-fg             "#88b8f6")
       (wombat-statement-fg           wombat-keyword-fg)
       (wombat-string-fg              "#95e454")
       (wombat-type-fg                "#d4d987")
       (wombat-number-fg              "#e5786d")
       (wombat-matchparen-fg          wombat-cursor)
       (wombat-matchparen-bg          "#857b6f")
       (wombat-errormsg-fg            "#ff2026")
       (wombat-errormsg-bg            "#3a3a3a")
       (wombat-statuslinenc-fg        wombat-common-linenr-bg)
       (wombat-todo                   wombat-matchparen-bg)
       (wombat-pmenusel-bg            wombat-function-fg)
       (wombat-diffchange             "#382a37")
       (wombat-difftext-bg            "#73186e")
       ;; -wombat256mod 256 colors palette
       (wombat2-normal-fg             "#d0d0d0")
       (wombat2-normal-bg             "#1c1c1c")
       (wombat2-cursor-line           "#303030")
       (wombat2-cursor                "#ffff87")
       (wombat2-visual-fg             "#c6c6c6")
       (wombat2-visual-bg             wombat2-cursor-line)
       (wombat2-linenr-fg             "#626262")
       (wombat2-special-fg            "#ffffaf")
       (wombat2-comment-fg            "#949494")
       (wombat2-function-fg           "#d7ff87")
       (wombat2-constant-fg           "#d7875f")
       (wombat2-preproc-fg            wombat2-constant-fg)
       (wombat2-identifier-fg         "#d7ff87")
       (wombat2-keyword-fg            "#87afff")
       (wombat2-statement-fg          wombat2-keyword-fg)
       (wombat2-string-fg             "#87d75f")
       (wombat2-type-fg               "#d7d787")
       (wombat2-number-fg             "#d7875f")
       (wombat2-matchparen-fg         wombat2-cursor)
       (wombat2-matchparen-bg         "#87875f")
       (wombat2-errormsg-fg           "#ff0000")
       (wombat2-errormsg-bg           wombat2-cursor-line)
       (wombat2-statuslinenc-fg       "#626262")
       (wombat2-todo                  wombat2-matchparen-bg)
       (wombat2-pmenusel-bg           wombat2-function-fg)
       (wombat2-diffchange            "#3a3a3a")
       (wombat2-difftext-bg           "#5f005f"))
  (custom-theme-set-faces
   'vim-wombat256mod
   `(default ((,full-color-class (:foreground ,wombat-normal-fg :background ,wombat-normal-bg))
              (,mini-color-class (:foreground ,wombat2-normal-fg :background ,wombat2-normal-bg))))
   `(border ((,full-color-class (:background ,wombat-cursor-line))
             (,mini-color-class (:background ,wombat2-cursor-line))))
   `(cursor ((,full-color-class (:foreground ,wombat-normal-bg :background ,wombat-cursor))
             (,mini-color-class (:foreground ,wombat2-normal-bg :background ,wombat2-cursor))))
   `(region ((,full-color-class (:foreground ,wombat-visual-fg :background ,wombat-visual-bg))
             (,mini-color-class (:foreground ,wombat2-visual-fg :background ,wombat2-visual-bg))))
   `(highlight ((,full-color-class (:background ,wombat-cursor-line))
                (,mini-color-class (:background ,wombat2-cursor-line))))
   `(hl-line ((,full-color-class (:background ,wombat-cursor-line))
              (,mini-color-class (:background ,wombat2-cursor-line))))
   `(fringe ((,full-color-class (:foreground ,wombat-linenr-fg :background ,wombat-common-linenr-bg))
             (,mini-color-class (:foreground ,wombat2-linenr-fg :background ,wombat-common-linenr-bg))))
   `(paren-face ((,full-color-class (:foreground ,wombat-special-fg))
                 (,mini-color-class (:foreground ,wombat2-special-fg))))
   `(show-paren-match-face
     ((,full-color-class (:foreground ,wombat-matchparen-fg :background ,wombat-matchparen-bg :bold t))
      (,mini-color-class (:foreground ,wombat2-matchparen-fg :background ,wombat2-matchparen-bg :bold t))))
   `(show-paren-mismatch-face
     ((,full-color-class (:foreground ,wombat-errormsg-fg :background ,wombat-errormsg-bg :bold t))
      (,mini-color-class (:foreground ,wombat2-errormsg-fg :background ,wombat2-errormsg-bg :bold t))))
   `(isearch ((t (:inverse-video t :bold nil :underline nil)))) ; +Search | (inverse-video)
   `(isearch-lazy-highlight-face ((t (:inherit font-lock-comment-face :inverse-video t))))
   `(modeline
     ((t (:italic t :bold nil :foreground ,wombat-common-statusline-fg :background ,wombat-common-statusline-bg))))
   `(modeline-inactive
     ((,full-color-class (:italic nil :bold nil :foreground ,wombat-statuslinenc-fg :background ,wombat-common-statuslinenc-bg))
      (,mini-color-class  (:italic nil :bold nil :foreground ,wombat2-statuslinenc-fg :background ,wombat-common-statuslinenc-bg))))
   `(modeline-buffer-id
     ((t (:italic t :slant italic :bold nil :foreground ,wombat-common-statusline-fg :background ,wombat-common-statusline-bg))))
   `(minibuffer-prompt
     ((t (:bold t :foreground ,wombat-common-statusline-fg))))
   `(font-lock-builtin-face
     ((,full-color-class (:foreground ,wombat-special-fg))
      (,mini-color-class (:foreground ,wombat2-special-fg))))
   `(font-lock-comment-face
     ((,full-color-class (:foreground ,wombat-comment-fg :italic t :slant italic))
      (,mini-color-class (:foreground ,wombat2-comment-fg :italic t :slant italic))))
   `(font-lock-comment-delimiter-face
     ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face
     ((,full-color-class (:foreground ,wombat-constant-fg))
      (,mini-color-class (:foreground ,wombat2-constant-fg))))
   `(font-lock-preprocessor-face
     ((,full-color-class (:foreground ,wombat-preproc-fg))
      (,mini-color-class (:foreground ,wombat2-preproc-fg))))
   `(font-lock-function-name-face
     ((,full-color-class (:foreground ,wombat-function-fg))
      (,mini-color-class (:foreground ,wombat2-function-fg))))
   `(font-lock-variable-name-face
     ((,full-color-class (:foreground ,wombat-identifier-fg))
      (,mini-color-class (:foreground ,wombat2-identifier-fg))))
   `(font-lock-keyword-face
     ((,full-color-class (:foreground ,wombat-keyword-fg))
      (,mini-color-class (:foreground ,wombat2-keyword-fg))))
   `(font-lock-reference-face
     ((,full-color-class (:foreground ,wombat-statement-fg))
      (,mini-color-class (:foreground ,wombat2-statement-fg))))
   `(font-lock-string-face
     ((,full-color-class (:italic t :slant italic :background nil :foreground ,wombat-string-fg))
      (,mini-color-class  (:italic t :slant italic :background nil :foreground ,wombat2-string-fg))))
   `(font-lock-doc-face
     ((t (:inherit font-lock-string-face))))
   `(font-lock-type-face
     ((,full-color-class (:foreground ,wombat-type-fg))
      (,mini-color-class (:foreground ,wombat2-type-fg))))
   `(font-lock-warning-face               ; as Error
     ((,full-color-class (:foreground ,wombat-errormsg-fg :background ,wombat-errormsg-bg :bold t))
      (,mini-color-class (:foreground ,wombat2-errormsg-fg :background ,wombat2-errormsg-bg :bold t)))) ; ErrorMsg
   ;; bonus faces = number & notify & todo
   `(font-lock-number-face
     ((,full-color-class (:foreground ,wombat-number-fg))
      (,mini-color-class (:foreground ,wombat2-number-fg))))
   `(font-lock-negation-char-face
     ((t (:inherit font-lock-number-face))))
   `(font-lock-notify-face                        ; as Warning
     ((t (:foreground ,wombat-common-warningmsg))))
   `(font-lock-todo-face            ; as Todo
     ((,full-color-class (:foreground ,wombat-todo :italic t :slant italic))
      (,mini-color-class (:foreground ,wombat2-todo :italic t :slant italic))))
   ;; auto-complete
   `(ac-candidate-face
     ((t (:foreground ,wombat-common-pmenu-fg :background ,wombat-common-pmenu-bg))))
   `(ac-selection-face
     ((,full-color-class (:foreground ,wombat-common-pmenusel-fg :background ,wombat-pmenusel-bg))
      (,mini-color-class (:foreground ,wombat-common-pmenusel-fg :background ,wombat2-pmenusel-bg))))
   ;; ediff faces
   `(ediff-current-diff-A ((,full-color-class
                            (:background ,wombat-diffchange))
                           (,mini-color-class
                            (:background ,wombat2-diffchange))
                           (t (:inverse-video t))))
   `(ediff-fine-diff-A ((,full-color-class
                         (:background ,wombat-difftext-bg))
                        (,mini-color-class
                         (:background ,wombat2-difftext-bg))
                        (t (:inverse-video t))))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'vim-wombat256mod)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vim-wombat256mod-theme.el ends here
