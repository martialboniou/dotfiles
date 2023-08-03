(local rose-pine
       {1 :rose-pine/neovim
        :priority 1000
        :lazy false
        :opts {:disable_background true :disable_float_background true}
        :config (λ [_ opts]
                  (#($.setup opts) (require :rose-pine))
                  (vim.cmd.colorscheme :rose-pine-moon))})

(local rainbow-delimiters
       {1 :HiPhish/rainbow-delimiters.nvim
        :event [:BufReadPost :BufNewFile]
        :dependencies [:nvim-treesitter/nvim-treesitter]
        :config (λ []
                  (local rainbow (require :rainbow-delimiters))
                  (set vim.g.rainbow_delimiters
                       {:blacklist [:c :cpp :h :hpp :m]
                        ;; shuffle colors
                        :highlight [:RainbowDelimiterGreen
                                    :RainbowDelimiterOrange
                                    :RainbowDelimiterCyan
                                    :RainbowDelimiterYellow
                                    :RainbowDelimiterBlue
                                    :RainbowDelimiterRed
                                    :RainbowDelimiterViolet]
                        :query {"" :rainbow-delimiters :latex :rainbow-blocks}
                        :strategy {"" (. rainbow.strategy :global)
                                   :commonlisp (. rainbow.strategy :local)}}))})

;; fnlfmt: skip
(λ tokyonight-custom-colors [colors]
  ;; Tomorrow Night palette based on:
  ;; - the https://doc.rust-lang.org/book css theme
  ;; - adapted from https://github.com/jmblog/color-themes-for-highlightjs
  ;; - originally created by https://github.com/chriskempson/tomorrow-theme
  ;; #c5c8c6 ; tomorrow foreground
  ;; #1d1f21 ; tomorrow background
  ;; *selection: #373b41 (unused in book)
  ;; *line: #282a2e (unused in book)
  ;; *window: #4d5057 (unused in book)
  ;; #969896 ; tomorrow comment (comment)
  ;; #cc6666 ; tomorrow red (variable, attribute, tag, regexp, ruby-constant, xml-tag-title, xml-doctype, xml-pi, html-doctype, css-id, css-class, css-pseudo)
  ;; #de935f ; tomorrow orange (params, constant, number, preprocessor, pragma, built-in, literal)
  ;; #f0c674 ; tomorrow yellow (ruby-class-title, css-rule-attribute)
  ;; #b5bd68 ; tomorrow green (string, value, inheritance, header, name, ruby-symbol, xml-cdata)
  ;; #8abeb7 ; tomorrow aqua (title, css-hexcolor)
  ;; #81a2be ; tomorrow blue (python-decoration+title, ruby-function-title+title-keyword)
  ;; #b294bb ; tomorrow purple (hljs-keyword, hljs-function)
  ;; #718c00 ; addition (only used in book)
  ;; #c82829 ; deletion (only used in book)
  ;; WORK IN PROGRESS
  (set colors.bg_dark "#1d1f21") ; tomorrow background
  (set colors.bg "#1d1f21")      ; tomorrow background
  (set colors.fg "#c5c8c6")      ; tomorrow foreground
  (set colors.comment "#565f89") ; tomorrow comment
  ;;;
  (set colors.blue1 "#c5c8c6")   ; tomorrow foreground (identifier)
  (set colors.blue6 "#8abeb7")   ; tomorrow aqua (lsp regexp)
  (set colors.magenta "#b294bb") ; tomorrow purple (keyword fn)
  (set colors.purple "#b294bb")  ; tomorrow purple (keyword let)
  ;;;
  (set colors.green "#b5bd68")   ; tomorrow green (string)
  (set colors.orange "#de935f")
  (set colors.yellow "#f0c674")
  (set colors.red "#cc6666"))

(λ tokyonight-custom-highlights [highlights colors]
  ;; b/c magenta = purple now
  (set highlights.rainbowcol6 {:fg colors.magenta2}))

(local _tkn {1 :folke/tokyonight.nvim
             :priority 1000
             :lazy false
             :opts {:on_colors #(tokyonight-custom-colors)
                    :on_highlights #(tokyonight-custom-highlights)
                    :style :night
                    :styles {:comments {} :keywords {}}}
             :config (λ [_ opts]
                       (let [tokyonight (require :tokyonight)]
                         (tokyonight.setup opts)
                         (vim.cmd.coloscheme :tokyonight)
                         (vim.api.nvim_set_hl 0
                                              "@lsp.typemod.function.declaration.rust"
                                              {:link "@string.regex"})
                         (vim.api.nvim_set_hl 0
                                              "@lsp.typemod.enum.declaration.rust"
                                              {:link "@string.regex"})
                         (vim.api.nvim_set_hl 0 "@lsp.type.function.rust"
                                              {:link :Normal})))})

(local _kat {1 :katawful/kat.nvim
             :version :3.1
             :priority 1000
             :lazy false
             :config #(vim.cmd.coloscheme :kat.nvim)})

[rose-pine rainbow-delimiters]
