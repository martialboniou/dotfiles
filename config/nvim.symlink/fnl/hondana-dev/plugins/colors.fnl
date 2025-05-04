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
  (let [{: setup} (require :mini.hues)
        max-colors 8
        ;; set rainbow-delimiters to {} if you want it be auto-generated
        rainbow-delimiters {:RainbowDelimiterBlue {:foreground 53233}
                            :RainbowDelimiterCyan {:foreground 55466}
                            :RainbowDelimiterGreen {:foreground 10537290}
                            :RainbowDelimiterOrange {:foreground 16748401}
                            :RainbowDelimiterRed {:foreground 16746950}
                            :RainbowDelimiterViolet {:foreground 13540095}
                            :RainbowDelimiterYellow {:foreground 15510028}}]
    ;; this block should not be executed
    (when (and (< opts.n_hues max-colors) (not (next rainbow-delimiters)))
      (local {: plugins :n_hues hues} opts)
      ;; make 8 hues for rainbow-delimiters
      (set opts.n_hues max-colors)
      (when (not opts.plugins)
        (set opts.plugins {}))
      (set opts.plugins.default false)
      (set (. opts.plugins "HiPhish/rainbow-delimiters.nvim") true)
      (setup opts)
      (var rainbow-colors (vim.tbl_keys rainbow-delimiters))
      (when (not (next rainbow-colors))
        (set rainbow-colors
             [:RainbowDelimiterBlue
              :RainbowDelimiterCyan
              :RainbowDelimiterGreen
              :RainbowDelimiterOrange
              :RainbowDelimiterRed
              :RainbowDelimiterViolet
              :RainbowDelimiterYellow]))
      (each [_ k (ipairs rainbow-colors)]
        (set (. rainbow-delimiters k) (vim.api.nvim_get_hl_by_name k true)))
      (set opts.n_hues hues)
      (set opts.plugins plugins))
    (setup opts)
    ;; a more colorful rainbow-delimiters
    (each [k v (pairs rainbow-delimiters)]
      (vim.api.nvim_set_hl 0 k v))
    ;; transparency
    (vim.api.nvim_set_hl 0 :Normal {:bg :NONE})
    ;; trigger an event to proc `hondana-dev.linenumbers`'s autocmd
    ;; as mini.hues is NOT a colorscheme (eqv to `:doau ColorScheme`)
    (vim.api.nvim_exec_autocmds :ColorScheme {})))

(tc type LazySpec)
(local mini-hues {1 :echasnovski/mini.hues
                  :priority 1000
                  :lazy false
                  :opts {:saturation :high
                         :background "#181818"
                         ;; :accent :bg ; is not multi-pane friendly (IMO, too neutral!)
                         ;; check `hondana-dev.status`
                         :accent :bg
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
