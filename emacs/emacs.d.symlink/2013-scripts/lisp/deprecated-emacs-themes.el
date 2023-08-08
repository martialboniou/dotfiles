;; -*- auto-byte-compile: nil -*-
;;; deprecated-emacs-themes.el --- 
;; 
;; Filename: deprecated-emacs-themes.el
;; Description: themes for pre-Emacs 24
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Tue Dec  3 16:31:56 2013 (+0100)
;; Version: 0.1
;; Package-Requires: (vim-everywhere)
;; Last-Updated: Thu Dec 19 13:27:57 2013 (+0100)
;;           By: Martial Boniou
;;     Update #: 24
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
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

(require 'noaccess)                     ; cl-lib provided by `preamble.el'
(unless (< emacs-major-version 24) (message "deprecated-emacs-themes: you should not use this deprecated theme installer."))
(require 'color-theme)

;; may be used by `VIM-EVERYWHERE'
;; molokai theme (https://github.com/hbin/molokai-theme/blob/master/molokai-theme.el)
(defun custom-vim-colorscheme-molokai ()
  (interactive)
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
    (color-theme-install
     `(vim-molokai
       ;; base
       ((foreground-color . ,molokai-fg)
        (background-color . ,molokai-bg)
        (background-mode . dark))         ; color orientation
       (default ((t (:background ,molokai-bg :foreground ,molokai-fg))))
       (cursor ((t (:background ,molokai-fg :foreground ,molokai-bg))))
       (fringe ((t (:foreground ,molokai-base02 :background ,molokai-bg))))
       (highlight ((t (:background ,molokai-grey))))
       (region ((t (:background  ,molokai-grey+2))
                (t :inverse-video t)))
       (warning ((t (:foreground ,molokai-palevioletred :weight bold))))
       ;; font lock
       (font-lock-builtin-face ((t (:foreground ,molokai-chartreuse))))
       (font-lock-comment-face ((t (:foreground ,molokai-base01))))
       (font-lock-comment-delimiter-face ((t (:foreground ,molokai-base01))))
       (font-lock-constant-face ((t (:foreground ,molokai-purple))))
       (font-lock-doc-string-face ((t (:foreground ,molokai-darkgoldenrod))))
       (font-lock-function-name-face ((t (:foreground ,molokai-chartreuse))))
       (font-lock-keyword-face ((t (:foreground ,molokai-pink :weight bold))))
       (font-lock-negation-char-face ((t (:foreground ,molokai-wine))))
       (font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
       (font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
       (font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
       (font-lock-string-face ((t (:foreground ,molokai-darkgoldenrod))))
       (font-lock-type-face ((t (:foreground ,molokai-blue :weight bold))))
       (font-lock-variable-name-face ((t (:foreground ,molokai-orange))))
       (font-lock-warning-face ((t (:foreground ,molokai-palevioletred :weight bold))))
       ;; bonus faces = number & notify & todo
       (font-lock-number-face ((t (:inherit font-lock-constant-face))))
       (font-lock-negation-char-face ((t (:inherit font-lock-number-face))))
       (font-lock-notify-face ((t (:inherit font-lock-warning-face))))
       (font-lock-todo-face ((t (:foreground ,molokai-base02 :italic t :slant italic))))
       ;; mode line
       (mode-line ((t (:foreground ,molokai-fg
                                   :background ,molokai-base03
                                   :box nil))))
       (mode-line-buffer-id ((t (:weight bold))))
       (mode-line-inactive ((t (:foreground ,molokai-fg
                                            :background ,molokai-base02
                                            :box nil))))
       ;; search
       (isearch ((t (:foreground ,molokai-dark :background ,molokai-wheat :weight bold))))
       (isearch-lazy-highlight-face ((t (:foreground ,molokai-base02 :background ,molokai-fg))))
       (isearch-fail ((t (:foreground ,molokai-wine :background ,molokai-darkwine))))
       ;; linum-mode
       (linum ((t (:foreground ,molokai-grey-2 :background ,molokai-grey+5))))
       ;; hl-line-mode
       (hl-line-face ((,class (:background ,molokai-grey+5)) (t :weight bold)))
       (hl-line ((,class (:background ,molokai-grey+5)) (t :weight bold)))
       ;; ido-mode
       ;; flycheck
       ;; paren-face
       (paren-face ((t (:inherit font-lock-builtin-face ))))
       ;; show-paren
       (show-paren-match-face
        ((t (:foreground ,molokai-dark :background ,molokai-orange :bold t)))) ; +MatchParen
       (show-paren-match-face
        ((t (:foreground ,molokai-pink :background ,molokai-grey+5 :bold t)))) ; +ErrorMsg
       ;; auto-complete
       (ac-candidate-face ((t (:foreground ,molokai-blue :background ,molokai-dark)))) ; Pmenu
       (ac-selection-face ((t (:foreground ,molokai-fg :background ,molokai-grey)))) ; PmenuSel
       ;; ediff
       (ediff-current-diff-A ((t (:foreground ,molokai-grey-1 :background ,molokai-grey+3)))) ; +DiffChange
       (ediff-fine-diff-A ((t (:background ,molokai-grey+3 :italic t :bold t)))) ; +DiffText
       ;; rainbow-delimiters
       ;; highlight-symbols
       ))))

;; wombat colorscheme 256 mod (2010/07/23 version) Liang/Bespalov/Nielsen (dengmao@gmail.com)
(defun custom-vim-colorscheme-wombat256mod ()
  (interactive)
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
    (color-theme-install
     `(vim-wombat256mod
       ((foreground-color . ,wombat-normal-fg)
        (background-color . ,wombat-normal-bg)
        (background-mode . dark))       ; color orientation
       (default ((,full-color-class (:foreground ,wombat-normal-fg :background ,wombat-normal-bg))
                 (,mini-color-class (:foreground ,wombat2-normal-fg :background ,wombat2-normal-bg))))
       (border ((,full-color-class (:background ,wombat-cursor-line))
                (,mini-color-class (:background ,wombat2-cursor-line))))
       (cursor ((,full-color-class (:foreground ,wombat-normal-bg :background ,wombat-cursor))
                (,mini-color-class (:foreground ,wombat2-normal-bg :background ,wombat2-cursor))))
       (region ((,full-color-class (:foreground ,wombat-visual-fg :background ,wombat-visual-bg))
                (,mini-color-class (:foreground ,wombat2-visual-fg :background ,wombat2-visual-bg))))
       (highlight ((,full-color-class (:background ,wombat-cursor-line))
                   (,mini-color-class (:background ,wombat2-cursor-line))))
       (hl-line ((,full-color-class (:background ,wombat-cursor-line))
                 (,mini-color-class (:background ,wombat2-cursor-line))))
       (fringe ((,full-color-class (:foreground ,wombat-linenr-fg :background ,wombat-common-linenr-bg))
                (,mini-color-class (:foreground ,wombat2-linenr-fg :background ,wombat-common-linenr-bg))))
       (paren-face ((,full-color-class (:foreground ,wombat-special-fg))
                    (,mini-color-class (:foreground ,wombat2-special-fg))))
       (show-paren-match-face
        ((,full-color-class (:foreground ,wombat-matchparen-fg :background ,wombat-matchparen-bg :bold t))
         (,mini-color-class (:foreground ,wombat2-matchparen-fg :background ,wombat2-matchparen-bg :bold t))))
       (show-paren-mismatch-face
        ((,full-color-class (:foreground ,wombat-errormsg-fg :background ,wombat-errormsg-bg :bold t))
         (,mini-color-class (:foreground ,wombat2-errormsg-fg :background ,wombat2-errormsg-bg :bold t))))
       (isearch ((t (:inverse-video t :bold nil :underline nil)))) ; +Search | (inverse-video)
       (isearch-lazy-highlight-face ((t (:inherit font-lock-comment-face :inverse-video t))))
       (modeline
        ((t (:italic t :bold nil :foreground ,wombat-common-statusline-fg :background ,wombat-common-statusline-bg))))
       (modeline-inactive
        ((,full-color-class (:italic nil :bold nil :foreground ,wombat-statuslinenc-fg :background ,wombat-common-statuslinenc-bg))
         (,mini-color-class  (:italic nil :bold nil :foreground ,wombat2-statuslinenc-fg :background ,wombat-common-statuslinenc-bg))))
       (modeline-buffer-id
        ((t (:italic t :slant italic :bold nil :foreground ,wombat-common-statusline-fg :background ,wombat-common-statusline-bg))))
       (minibuffer-prompt
        ((t (:bold t :foreground ,wombat-common-statusline-fg))))
       (font-lock-builtin-face
        ((,full-color-class (:foreground ,wombat-special-fg))
         (,mini-color-class (:foreground ,wombat2-special-fg))))
       (font-lock-comment-face
        ((,full-color-class (:foreground ,wombat-comment-fg :italic t :slant italic))
         (,mini-color-class (:foreground ,wombat2-comment-fg :italic t :slant italic))))
       (font-lock-comment-delimiter-face
        ((t (:inherit font-lock-comment-face))))
       (font-lock-constant-face
        ((,full-color-class (:foreground ,wombat-constant-fg))
         (,mini-color-class (:foreground ,wombat2-constant-fg))))
       (font-lock-preprocessor-face
        ((,full-color-class (:foreground ,wombat-preproc-fg))
         (,mini-color-class (:foreground ,wombat2-preproc-fg))))
       (font-lock-function-name-face
        ((,full-color-class (:foreground ,wombat-function-fg))
         (,mini-color-class (:foreground ,wombat2-function-fg))))
       (font-lock-variable-name-face
        ((,full-color-class (:foreground ,wombat-identifier-fg))
         (,mini-color-class (:foreground ,wombat2-identifier-fg))))
       (font-lock-keyword-face
        ((,full-color-class (:foreground ,wombat-keyword-fg))
         (,mini-color-class (:foreground ,wombat2-keyword-fg))))
       (font-lock-reference-face
        ((,full-color-class (:foreground ,wombat-statement-fg))
         (,mini-color-class (:foreground ,wombat2-statement-fg))))
       (font-lock-string-face
        ((,full-color-class (:italic t :slant italic :background nil :foreground ,wombat-string-fg))
         (,mini-color-class  (:italic t :slant italic :background nil :foreground ,wombat2-string-fg))))
       (font-lock-doc-face
        ((t (:inherit font-lock-string-face))))
       (font-lock-type-face
        ((,full-color-class (:foreground ,wombat-type-fg))
         (,mini-color-class (:foreground ,wombat2-type-fg))))
       (font-lock-warning-face               ; as Error
        ((,full-color-class (:foreground ,wombat-errormsg-fg :background ,wombat-errormsg-bg :bold t))
         (,mini-color-class (:foreground ,wombat2-errormsg-fg :background ,wombat2-errormsg-bg :bold t)))) ; ErrorMsg
                                        ; bonus faces = number & notify & todo
       (font-lock-number-face
        ((,full-color-class (:foreground ,wombat-number-fg))
         (,mini-color-class (:foreground ,wombat2-number-fg))))
       (font-lock-negation-char-face
        ((t (:inherit font-lock-number-face))))
       (font-lock-notify-face                        ; as Warning
        ((t (:foreground ,wombat-common-warningmsg))))
       (font-lock-todo-face            ; as Todo
        ((,full-color-class (:foreground ,wombat-todo :italic t :slant italic))
         (,mini-color-class (:foreground ,wombat2-todo :italic t :slant italic))))
                                        ; auto-complete
       (ac-candidate-face
        ((t (:foreground ,wombat-common-pmenu-fg :background ,wombat-common-pmenu-bg))))
       (ac-selection-face
        ((,full-color-class (:foreground ,wombat-common-pmenusel-fg :background ,wombat-pmenusel-bg))
         (,mini-color-class (:foreground ,wombat-common-pmenusel-fg :background ,wombat2-pmenusel-bg))))
                                        ; ediff faces
       (ediff-current-diff-A ((,full-color-class
                               (:background ,wombat-diffchange))
                              (,mini-color-class
                               (:background ,wombat2-diffchange))
                              (t (:inverse-video t))))
       (ediff-fine-diff-A ((,full-color-class
                            (:background ,wombat-difftext-bg))
                           (,mini-color-class
                            (:background ,wombat2-difftext-bg))
                           (t (:inverse-video t))))))))

;; inkpot colorscheme from `github.com/ciaranm/inkpot' TODO: a version in 256 colors
(defun custom-vim-colorscheme-inkpot ()
  (interactive)
  (color-theme-install
   `(vim-inkpot
     ((foreground-color . "#cfbfad")  ; +Normal guifg
      (background-color . "#1e1e27")  ; +Normal guibg
      (border-color . "#2e2e37")      ; +CursorLine
      (cursor-color . "#8b8bff")      ; +Cursor guibg
      (background-mode . dark))       ; color orientation
     (region ((t (:foreground "#eeeeee" :background "#4e4e8f")))) ; +Visual
     (highlight ((t (:background "#2e2e37")))) ; +CursorLine
     (hl-line   ((t (:background "#2e2e37")))) ; +CursorLine
     (fringe ((t (:foreground "#8b8bcd" :background "#2e2e2e")))) ; +LineNr
     (paren-face ((t (:foreground "#c080d0")))) ; +Special
     (show-paren-match-face ((t (:foreground "#cfbfad" :background "#4e4e8f" :bold nil)))) ; +MatchParen
     (show-paren-mismatch-face
      ((((class color) (min-colors 4096)) (:foreground "#ffffff" :background "#6e2e2e" :bold t))
       (((class color) (min-colors 256)) (:foreground "#5fd7af" :background "#0087df" :bold t)))) ; +Error
     (isearch ((t (:bold t :foreground "#303030" :background "#ad7b57")))) ; +Search
     (modeline ((t (:bold t :foreground "#b9b9b9" :background "#3e3e5e")))) ; +StatusLine
     (modeline-inactive ((t (:foreground "#708090" :background "#3e3e5e")))) ; +StatusLineNC | User2
     (modeline-buffer-id ((t (:bold t :foreground "#b9b9b9" :background "#3e3e5e")))) ; +StatusLine
     (minibuffer-prompt ((t (:bold t :foreground "#7070a0")))) ; +User2 guifg
     (ediff-current-diff-A ((((class color) (min-colors 16))
                             (:foreground "#ffffcd" :background "#306b8f"))
                            (((class color)) (:foreground "white" :background "blue3"))
                            (t (:inverse-video t)))) ; +DiffChange
     (ediff-fine-diff-A ((((class color) (min-colors 16))
                          (:foreground "#ffffcd" :background "#4a2a4a"))
                         (((class color)) (:foreground "white" :background "green4"))
                         (t (:inverse-video t)))) ; +DiffText
     (font-lock-builtin-face ((t (:foreground "#c080d0")))) ; +Special
     (font-lock-comment-face ((t (:foreground "#cd8b00" :italic nil)))) ; +Comment
     (font-lock-constant-face ((t (:foreground "#ffcd8b")))) ; +Constant
     (font-lock-doc-face ((t (:foreground "#cd8b00" :italic nil)))) ; +Comment
     (font-lock-function-name-face ((t (:foreground "#cfbfad" :bold t)))) ; +Normal guifg (bold)
     (font-lock-keyword-face ((t (:foreground "#808bed")))) ; +Statement
     (font-lock-preprocessor-face ((t (:foreground "#409090")))) ; +PreProc
     (font-lock-number-face ((t (:foreground "#f0ad6d")))) ; +Number
     (font-lock-reference-face ((t (:bold t :foreground "#808bed")))) ; +Statement
     (font-lock-string-face ((t (:foreground "#ffcd8b" :background "#404040" :italic nil)))) ; +String
     (font-lock-type-face ((t (:foreground "#ff8bff")))) ; +Type
     (font-lock-variable-name-face ((t (:foreground "#ff8bff")))) ; +Identifier
     (font-lock-warning-face
      ((((class color) (min-colors 4096)) (:foreground "#ffffff" :background "#6e2e2e" :bold t))
       (((class color) (min-colors 256)) (:foreground "#5fd7af" :background "#0087df" :bold t))))))) ; +Error

(provide 'deprecated-emacs-themes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; deprecated-emacs-themes.el ends here
