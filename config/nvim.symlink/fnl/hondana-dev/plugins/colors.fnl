(import-macros {: tc} :hondana-dev.macros)
;; less colors hype
;; 0 = last theme (b/c permute)

(tc type number)
(local chosen-theme-idx 0)

(tc type "Theme[]")
(local themes
       [{:foreground "#adb6b4" :n_hues 2 :desc :blue->orange-yellow}
        {:foreground "#efefee" :n_hues 2 :desc :pink->cyan}
        {:foreground "#aabcbb" :n_hues 3 :desc :cyan->green->pink}])

(tc type number)
(local chosen-theme (-> themes
                        (#(. $ (-> $
                                   (length)
                                   (#(-> chosen-theme-idx
                                         (- 1)
                                         (% $)))
                                   (+ 1))))))

(tc type "fun(self:LazyPlugin, opts:table)")
(fn config [_ opts]
  (let [{: setup} (require :mini.hues)]
    (setup opts)
    ;; trigger an event to proc `hondana-dev.linenumbers`'s autocmd
    ;; as mini.hues is NOT a colorscheme
    (vim.cmd "doau ColorScheme")))

(tc type LazySpec)
(local mini-hues {1 :echasnovski/mini.hues
                  :priority 1000
                  :lazy false
                  :opts {:saturation :high
                         :background "#181818"
                         ;; :accent :bg ; is not multi-pane friendly (IMO, too neutral!)
                         :accent :blue
                         :foreground (. chosen-theme :foreground)
                         :n_hues (. chosen-theme :n_hues)}
                  : config})

(tc type LazySpec)
(local rainbow-delimiters
       {1 :HiPhish/rainbow-delimiters.nvim
        :event [:BufReadPost :BufNewFile]
        :dependencies [:nvim-treesitter/nvim-treesitter]
        :config (λ []
                  (local {: strategy} (require :rainbow-delimiters))
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
                        :strategy {"" (. strategy :global)
                                   :commonlisp (. strategy :local)}}))})

(tc type LazySpec)
(local icons {1 :nvim-tree/nvim-web-devicons
              :opts {:override_by_extension {:fnl {:icon "🌱"
                                                   :color "#428850"
                                                   :name :fnl}}}})

(tc type LazySpec)
(local P (-> mini-hues (#[$ rainbow-delimiters icons])))

(tc class Theme field foreground string field n_hues number field desc string)

P
