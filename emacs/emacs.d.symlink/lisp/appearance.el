(require 'defs)
(require 'vars)             ; enable 'SAFE-BUILD-CUSTOM-FILE

;;; APPEARANCE
;;
;; - disable eyecandies
(if (and (featurep 'ns) (eq window-system 'ns))
    (disable-eyecandies tool-bar-mode scroll-bar-mode) ; OS X reclaims its menus
  (disable-eyecandies menu-bar-mode tool-bar-mode scroll-bar-mode))
;; - basic customization
(custom-set-variables
 '(inihibit-startup-message t)
 '(custom-file (safe-build-custom-file "Customize"))
 '(line-number-mode t)
 '(column-number-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(resize-mini-windows 'grow-only)   ; resize windows/frame if needed
 '(max-mini-window-height 0.1)
 '(focus-follows-mouse nil)
 '(mac-allow-anti-aliasing t))
(mouse-avoidance-mode 'cat-and-mouse)
;; - translucent emacs window
(when (and window-system
           (> emacs-major-version 23))
  (toggle-transparency))

(eval-after-load "adapter"
  '(progn
     ;; PRETTY
     ;; pretty control-l
     (when (locate-library "pp-c-l")
       (require 'pp-c-l)
       (pretty-control-l-mode 1))

     ;; ALIAS-MINOR-MODES TODO: merge to appearance.el
     ;; shorten indicator in mode-line
     (when (fboundp 'alias-minor-modes)
       (alias-minor-modes '(undo-tree UT abbrev Ab autopair AP paredit PE flymake FM mouse-embrace ME)))

     ;; PRETTY-LAMBDA
     ;; replace LAMBDA -> λ
     (eval-after-load "adapter"
       '(when (el-get-package-is-installed "pretty-lambdada")
          (pretty-lambda-for-modes)
          (setq pretty-lambda-auto-modes '(lisp-mode
                                           scheme-mode
                                           emacs-lisp-mode
                                           python-mode))))))

;;; FANCY
;;
(unless (member system-type '(windows-nt ms-dos))
                    ;(set-face-attribute 'default :height 120)
  (set-face-attribute 'default nil
              :height (if *mars/notebook-context* 100 140)
              :family "DejaVu Sans Mono")) ; TODO: force DejaVu_Sans_Mono font install on OSX
(if window-system
    (setq hl-paren-colors
      '("orange1"
        "yellow1"
        "greenyellow"
        "green1"
        "springgreen1"
        "cyan1"
        "slateblue1"
        "magenta1"
        "purple"))
  (setq hl-paren-colors                 ; TODO: yellow1 not found in term / investigate
    '("orange1"
      "yellow"
      "greenyellow"
      "green1"
      "springgreen1"
      "cyan1"
      "slateblue1"
      "magenta1"
      "purple")))                   ; draw rainbows in LISPEM

(eval-after-load "behave"
  '(progn
     ;; change mode-line color by evil state in 'VIM-EVERYWHERE
     ;; normal case -> no Vim-like behavior -> color-theme & mic-paren
     (unless *i-am-a-vim-user*          ; 'VIM-EVERYWHERE loads its own colorscheme and parens' support
       (require-if-located 'mic-paren)  ; faces for (mis)matching parentheses
       (when (< emacs-major-version 24)
         (eval-after-load "color-theme" '(color-theme-ld-dark)))
       (eval-after-load "mic-paren"   '(paren-activate)))
     ;; colors out of theme
     (defvar mars/custom-set-faces
       '(custom-set-faces
         '(action-lock-face ((((class color)) (:background "black" :foreground "DeepSkyBlue" :overline "yellow"))))
         ;; default paren-face for matching
         '(paren-face-match ((t (:inherit show-paren-match-face))))
         '(paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
         '(paren-face-no-match ((((class color) (background dark)) (:inherit paren-face :background "IndianRed4"))
                                (((class color) (background light))(:inherit paren-face :background "IndianRedD1"))))
         ;; Evil using inheritance
         ;; viper using inheritance (prefer Evil)
         '(viper-minibuffer-emacs ((((class color)) (:inherit font-lock-warning-face))))
         '(viper-minibuffer-insert ((((class color)) (:inherit default))))
         '(viper-minibuffer-vi ((((class color)) (:inherit font-lock-builtin-face))))
         '(viper-replace-overlay ((t nil)))
         '(viper-search ((((class color)) (:inherit isearch))))
         ;; linum as fringe (like LineNr in Vim)
         '(linum ((((class color)) (:inherit fringe))))
         ;; dark background friend ido colors
         '(ido-only-match ((((class color) (background dark)) (:foreground "#00cd00"))  ; green3
                           (((class color) (background light)) (:foreground "#228b22")))) ; ForestGreen
         '(ido-subdir ((((class color) (background dark)) (:foreground "#cd0000"))
                       (((class color) (background light)) (:foreground "#8b2222"))))
         ;; anything using inheritance
         '(anything-M-x-key-face ((t (:inherit font-lock-reference-face :underline t))))
         '(anything-dir-heading ((t (:inherit font-lock-doc-face :weight bold))))
         '(anything-dir-priv ((t (:inherit font-lock-doc-face :weight bold))))
         '(anything-dired-symlink-face ((t (:inherit font-lock-string-face :slant italic))))
         '(anything-file-name ((t (:inherit default))))
         '(anything-grep-file ((t (:inherit font-lock-builtin-face :underline t))))
         '(anything-grep-lineno ((t (:inherit font-lock-type-face))))
         '(anything-header ((t (:inherit font-lock-warning-face))))
         '(anything-isearch-match ((t (:inherit isearch))))
         '(anything-match ((t (:inherit match :inverse-video t))))
         '(anything-overlay-line-face ((t (:inherit font-lock-warning-face :foreground "default"))))
         ;; obvious trailing whitespace
         ;; '(trailing-whitespace ((((class color) (background dark))
         ;;                         (:background "gray90" :strike-through "red1"))
         ;;                        (((class color) (background light))
         ;;                         (:background "gray10" :strike-through "red1"))))
         ;; no more flashy background in MuMaMo
         '(mumamo-background-chunk-major ((((class color) (min-colors 88)
                                            (background dark)) (:background "gray10"))
                                          (((class color) (min-colors 88)
                                            (background light)) (:background "gray90"))))
         '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88)
                                               (background dark)) (:background "gray20"))
                                             (((class color) (min-colors 88)
                                               (background light)) (:background "gray80"))))
         ;; no more pinkish flymake
         '(flymake-errline ((((class color)) (:inherit font-lock-warning-face))))
         '(flymake-warnline ((((class color)) (:inherit font-lock-warning-face :inverse-video t))))
         ;; clearer dired
         '(diredp-dir-priv ((t (:inherit font-lock-doc-face :weight bold))))
         '(diredp-file-name ((t (:inherit default))))
         ;; TODO: clearer org-mode
         ;; '(org-date ((((color class)) (:inherit font-lock-number-face))))
         ;; '(org-agenda ((((color class)) (:inherit font-lock-keyword-face))))
         ;; greyish ediff (NOTE: color-theme may override this)
         '(ediff-even-diff-Ancestor ((((class color) (min-colors 16) (background dark))   (:background "SlateGray4" :foreground "SlateGray1"))
                                     (((class color) (min-colors 16) (background light)) (:background "SlateGray1" :foreground "SlateGray4"))))
         '(ediff-odd-diff-A ((((class color) (min-colors 16) (background dark)) (:background "gray40" :foreground "white"))
                             (((class color) (min-colors 16) (background light)) (:background "gray60" :foreground "black"))))
         '(ediff-odd-diff-B ((((class color) (min-colors 16) (background dark)) (:background "gray40" :foreground "white"))
                             (((class color) (min-colors 16) (background light)) (:background "gray60" :foreground "black"))))
         '(ediff-odd-diff-C ((((class color) (min-colors 16) (background dark)) (:background "gray40" :foreground "white"))
                             (((class color) (min-colors 16) (background light)) (:background "gray60" :foreground "black"))))
         '(ediff-even-diff-A ((((class color) (min-colors 16) (background dark)) (:background "gray20" :foreground "white"))
                              (((class color) (min-colors 16) (background light)) (:background "gray80" :foreground "black"))))
         '(ediff-even-diff-B ((((class color) (min-colors 16) (background dark)) (:background "gray20" :foreground "white"))
                              (((class color) (min-colors 16) (background light)) (:background "gray80" :foreground "black"))))
         '(ediff-even-diff-C ((((class color) (min-colors 16) (background dark)) (:background "gray20" :foreground "white"))
                              (((class color) (min-colors 16) (background light)) (:background "gray80" :foreground "black"))))
         ;; readable default faces for emms
         '(emms-playlist-selected-face ((((class color)) (:inherit font-lock-keyword-face))))
         '(emms-playlist-track-face ((((class color)) (:inherit default))))
         ;; readable default faces for wanderlust AKA wl
                                        ;'(wl-highlight-action-argument-face ((((class color) (background dark)) (:foreground "orange" :slant italic))))
         '(wl-highlight-action-argument-face ((((class color)) (:inherit font-lock-builtin-face :italic t :slant italic))))
         '(wl-highlight-folder-closed-face ((((class color) (background dark)) (:foreground "red"))))
         '(wl-highlight-folder-few-face ((((class color) (background dark)) (:foreground "gold" :slant italic :weight bold))))
         '(wl-highlight-folder-many-face ((((class color) (background dark)) (:foreground "#ddee99" :slant italic :weight bold))))
         '(wl-highlight-folder-unknown-face ((((class color) (background dark)) (:foreground "#14ff45"))))
         '(wl-highlight-folder-unread-face ((((class color) (background dark)) (:foreground "#3486f5"))))
         '(wl-highlight-folder-zero-face ((((class color) (background dark)) (:foreground "white" :weight bold))))
         '(wl-highlight-header-separator-face ((((class color)) (:background "DarkRed" :foreground "Black"))))
         '(wl-highlight-demo-face ((((class color)) (:inherit default))))
         '(wl-highlight-logo-face ((((class color) (background dark)) (:background "#000000" :foreground "SkyBlue"))))
         '(wl-highlight-message-citation-header ((((class color) (background dark)) (:foreground "Yellow" :slant italic))))
         '(wl-highlight-message-header-contents ((t (:foreground "DarkOrange" :weight normal))))
         '(wl-highlight-message-headers ((t (:foreground "grey" :weight bold))))
         '(wl-highlight-message-important-header-contents ((t (:foreground "Green" :weight bold))))
         '(wl-highlight-message-important-header-contents2 ((t (:foreground "GreenYellow" :weight bold))))
         '(wl-highlight-message-signature ((((class color) (background dark)) (:foreground "#7878ee" :slant italic))))
         '(wl-highlight-message-unimportant-header-contents ((t (:foreground "#999" :weight normal))))
         '(wl-highlight-summary-answered-face ((((class color) (background dark)) (:foreground "yellow" :slant italic))))
         '(wl-highlight-summary-deleted-face ((((class color) (background dark)) (:background "DarkRed" :foreground "black" :slant italic :weight bold))))
         '(wl-highlight-summary-disposed-face ((((class color) (background dark)) (:background "orange" :foreground "black" :slant italic :weight bold))))
         '(wl-highlight-summary-forwarded-face ((((class color) (background dark)) (:foreground "orange" :overline t :underline t :slant italic))))
         '(wl-highlight-summary-high-unread-face ((t (:foreground "#3486f5" :weight bold))))
         '(wl-highlight-summary-killed-face ((((class color) (background dark)) (:foreground "DarkRed" :overline t :underline t))))
         '(wl-highlight-summary-new-face ((t (:foreground "#3486f5" :weight bold))))
         '(wl-highlight-summary-prefetch-face ((((class color) (background dark)) (:background "DeepSkyBlue" :foreground "black" :slant italic :weight bold))))
         '(wl-highlight-summary-refiled-face ((((class color) (background dark)) (:background "grey10" :foreground "orange" :slant italic :weight bold))))
         '(wl-highlight-summary-resend-face ((((class color) (background dark)) (:background "orange3" :foreground "black" :slant italic :weight bold))))
         '(wl-highlight-summary-target-face ((((class color)) (:foreground "HotPink1"))))
         '(wl-highlight-summary-thread-top-face ((((class color) (background dark)) (:foreground "GreenYellow"))))
         '(wl-highlight-summary-unread-face ((((class color) (background dark)) (:foreground "#eee"))))
         '(wl-highlight-summary-normal-face ((((class color) (background dark)) (:foreground "PaleGreen"))))
         '(wl-highlight-summary-low-read-face ((((class color) (background dark)) (:inherit wl-highlight-summary-normal-face :slant italic))))
         '(wl-highlight-summary-high-read-face ((((class color) (background dark)) (:inherit wl-highlight-summary-normal-face :weight bold))))
         ;; readable default faces for newsticker
         ;; '(newsticker-enclosure-face ((t (:background "orange" :foreground "black" :weight bold))))
         '(newsticker-treeview-selection-face ((((class color)) (:inherit newsticker-treeview-new-face :inverse-video t))))
         ;; howm needs more sober default colors
         '(howm-menu-list-face ((t (:foreground "green"))))
         '(howm-mode-keyword-face ((((class color)) (:foreground "green"))))
         '(howm-mode-ref-face ((((class color) (background dark)) (:foreground "sandy brown"))))
         '(howm-mode-title-face ((((class color)) (:foreground "grey"))))
         '(howm-reminder-normal-face ((((class color)) (:foreground "khaki"))))
         '(howm-reminder-tomorrow-face ((((class color)) (:background "Cyan" :foreground "black"))))
         '(howm-view-hilit-face ((((class color)) (:foreground "yellow"))))
         '(howm-view-name-face ((((class color)) (:foreground "purple")))))
       "Personal colors for special programs.")
     (eval-after-load (if (< emacs-major-version 24) "color-theme" "custom") mars/custom-set-faces)))

(provide 'appearance)
