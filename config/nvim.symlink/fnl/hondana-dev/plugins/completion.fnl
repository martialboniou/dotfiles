(import-macros {: tc} :hondana-dev.macros)
;;; 2025-05-16 blink-cmp
;;  inspired by https://github.com/barraIhsan/dotfiles (GNU GPL v3.0)

;; F = utility functions at the end of this module
(local F {:get-web-devicon #(values false "")})

;;; SETUP FUNCTIONS
(fn text [ctx]
  "lspkind text setup: https://cmp.saghen.dev/recipes.html#nvim-web-devicons-lspkind"
  (var icon ctx.kind_icon)
  (if (vim.tbl_contains [:Path] ctx.source_name)
      ;; get the first value
      (let [dev-icon (F.get-web-devicon ctx.label)]
        (when dev-icon (set icon dev-icon)))
      (set icon
           (-> :lspkind (require) (#($.symbolic ctx.kind {:mode :symbol})))))
  (.. icon ctx.icon_gap))

(fn highlight [ctx]
  "lspkind highlight setup: https://cmp.saghen.dev/recipes.html#nvim-web-devicons-lspkind"
  (var hl ctx.kind_hl)
  (when (vim.tbl_contains [:Path] ctx.source_name)
    (let [(dev-icon dev-hl) (F.get-web-devicon ctx.label)]
      (when dev-icon (set hl dev-hl))))
  hl)

(λ opts []
  {:snippets {:preset :luasnip}
   :sources {:default [:lsp :snippets :lazydev :buffer :path]
             ;; INFO: ensure hondana-dev.plugins.languages has lazydev
             :providers {:lazydev {:name :LazyDev
                                   :module :lazydev.integrations.blink}}}
   :signature {:enabled true :window {:show_documentation true}}
   :completion {:list {:selection {:preselect true
                                   ;; don't insert before accept + use `ghost text`
                                   :auto_insert false}}
                ;; WARN: I prefer to disable the auto brackets; it's stupid in Lisp dialects
                ;; or when aliasing functions with no call, also Haskell...
                :accept {:auto_brackets {:enabled false}}
                :ghost_text {:enabled true :show_with_menu false}
                :documentation {:auto_show true}
                :menu {:auto_show false
                       :draw {:components {:kind_icon {: text : highlight}}}}}
   :keymap {;; use the default settings (REMINDER: `<C-Space>` to init)
            :preset :default
            ;; NOTE: <C-n>/<C-p> = show too (like <C-space>)
            :<C-n> [:show :select_next :fallback_to_mappings]
            :<C-p> [:show :select_prev :fallback_to_mappings]
            ;; NOTE: <C-f>/<C-b> = jump forward/backward (<C-b> CANNOT be used in tmux; I chose F5/`fn a` as a tmux prefix)
            :<C-f> [:snippet_forward :fallback]
            :<C-b> [:snippet_backward :fallback]
            ;; <Tab>/<S-Tab> works for these snippet keybindings too
            ;; NOTE: <C-u>/<C-d> = scroll (assigned to <C-b>/<C-f> by default)
            :<C-u> [:scroll_documentation_up :fallback]
            :<C-d> [:scroll_documentation_down :fallback]}})

(tc return "false|string")
(λ build []
  (and (= 1 (vim.fn.executable :cargo)) "cargo build --release"))

;; TODO: cmp-emoji & cmp-conjure eqv.

(λ luasnip-config []
  (let [{: lazy_load} (require :luasnip.loaders.from_vscode)
        {: config} (require :luasnip)
        autosnip #(config.set_config {:enable_autosnippets $})]
    (autosnip true)
    (lazy_load {:paths (-> :config (vim.fn.stdpath)
                           (vim.fs.joinpath :snippets :vscode))})))

;;; PLUGINS
(tc type LazySpec)
(local P {1 :saghen/blink.cmp
          :event :InsertEnter
          : build
          : opts
          :dependencies [;; pictograms
                         :onsails/lspkind.nvim
                         ;; snippets
                         {1 :L3MON4D3/LuaSnip
                          :version "v2.*"
                          :build "make install_jsregexp"
                          :dependencies [:rafamadriz/friendly-snippets]
                          :config luasnip-config}]})

;;; UTILITY FUNCTIONS
(fn F.get-web-devicon [label]
  (let [{: get_icon} (require :nvim-web-devicons)]
    ;; returns a multi-value
    (get_icon label)))

P
