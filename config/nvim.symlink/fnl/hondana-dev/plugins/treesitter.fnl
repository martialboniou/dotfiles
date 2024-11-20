(import-macros {: tc} :hondana-dev.macros)

(tc type "string[]")
(local preferred-languages [:typescript
                            :tsx
                            :javascript
                            :json
                            :c
                            :awk
                            :lua
                            :fennel
                            :vim
                            :vimdoc
                            :rust
                            :markdown
                            :markdown_inline
                            :html
                            :php
                            :yaml
                            :ocaml
                            :ocaml_interface
                            :scheme
                            :zig
                            :go])

(tc type "table<string, string>")
(local selection_modes {})
;;; unorthodox keys so invert k&v
(each [v k (pairs {:v "@parameter.outer"
                   :V "@function.outer"
                   :<C-v> "@class.outer"})]
  (set (. selection_modes k) v))

(tc type LazySpec)
(local P ;;
       [{1 :nvim-treesitter/nvim-treesitter
         :dependencies [{1 :folke/ts-comments.nvim
                         ;; fnlfmt works better with `;;` than `;` as Fennel Lisp comment
                         :opts {:lang {:fennel ";; %s"}}
                         :event :VeryLazy
                         :enabled (-> :nvim-0.10.0 (vim.fn.has) (= 1))}]
         :build ":TSUpdate"
         :event [:BufReadPost :BufNewFile]
         :cmd [:TSUpdateSync]
         :config (λ [_ opts]
                   (-> :nvim-treesitter.configs
                       (require)
                       (#($.setup opts))))
         :opts {:ensure_installed preferred-languages
                :matchup {;; special vim-matchup (check hondana-dev.plugins.operators)
                          :enable true
                          :disable []}
                :sync_install false
                :auto_install false
                :rainbow {:enable true :extended_mode true}
                :playground {:enable true}
                :highlight {:enable true
                            :additional_vim_regex_highlighting false}
                :incremental_selection {:enable true}
                :indent {:enable true
                         ;; FIXME: indent twice in c (ai + nvim_treesitter#indent())
                         :disable [:c]}
                :textobjects {:select {:enable true
                                       :lookahead true
                                       :keymaps {:af "@function.outer"
                                                 :ac "@class.outer"
                                                 :if "@function.inner"
                                                 :ic "@class.inner"
                                                 :as {:query "@scope"
                                                      :query_group :locals}}
                                       : selection_modes}
                              :move {:enable true
                                     :set_jumps true
                                     ;; NOTE: neither [t nor ]t b/c todo-comments
                                     ;;       check hondana-dev.plugins.quickfix
                                     :goto_next_start {"]m" "@function.outer"
                                                       "]]" "@class.outer"}
                                     :goto_next_end {"]M" "@function.outer"
                                                     "][" "@class.outer"}
                                     :goto_previous_start {"[m" "@function.outer"
                                                           "[[" "@class.outer"}
                                     :goto_previous_end {"[M" "@function.outer"
                                                         "[]" "@class.outer"}}}}}])

P
