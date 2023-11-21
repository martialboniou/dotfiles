;; less colors hype
(import-macros {: cal!} :hondana-dev/macros)

(local chosen-theme-idx 0) ;; 0 = last theme (b/c permute)
(local themes
       [{:foreground "#adb6b4" :n_hues 2 :desc :blue->orange-yellow}
        {:foreground "#efefee" :n_hues 2 :desc :pink->cyan}
        {:foreground "#aabcbb" :n_hues 3 :desc :cyan->green->pink}])

(local chosen-theme
       (-> themes
           (#(. $ (-> $ (length) (#(-> chosen-theme-idx (- 1) (% $))) (+ 1))))))

(local mini-hues {1 :echasnovski/mini.hues
                  :version false
                  :priority 1000
                  :lazy false
                  :opts {:saturation :high
                         :background "#181818"
                         :accent :bg
                         :foreground (. chosen-theme :foreground)
                         :n_hues (. chosen-theme :n_hues)}})

;; previous colorscheme
(local _rose-pine
       {1 :rose-pine/neovim
        :priority 1000
        :lazy false
        :opts {:disable_background true :disable_float_background true}
        :config (λ [_ opts]
                  (cal! :rose-pine :setup opts)
                  (vim.cmd.colorscheme :rose-pine-moon)
                  ;; I don't like the dull default color for the punctuation
                  (vim.api.nvim_set_hl 0 "@punctuation" {:fg :White})
                  (vim.api.nvim_set_hl 0 :Operator {:fg :Pink}))})

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

(local icons {1 :nvim-tree/nvim-web-devicons
              :opts {:override_by_extension {:fnl {:icon "🌱"
                                                   :color "#428850"
                                                   :name :fnl}}}})

(-> mini-hues (#[$ rainbow-delimiters icons]))
