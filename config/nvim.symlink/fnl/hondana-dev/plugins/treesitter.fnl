(import-macros {: tc} :hondana-dev.macros)

(tc type "string[]")

;; fnlfmt: skip
(local preferred-languages [:typescript      :tsx             :javascript :json
                            :c               :awk             :lua        :fennel
                            :vim             :vimdoc          :rust       :markdown
                            :markdown_inline :html            :php        :yaml
                            :ocaml           :ocaml_interface :scheme     :zig
                            :go])

(tc type "table<string, string>")
(local selection_modes {})
;;; unorthodox keys so invert k&v
(each [v k (pairs {:v "@parameter.outer"
                   :V "@function.outer"
                   :<C-v> "@class.outer"})]
  (set (. selection_modes k) v))

(tc type "fun(self:LazyPlugin, opts:table)|true")
(local text-object-config ;;
       #(let [configs (require :nvim-treesitter.configs)]
          ;; from LazyVim nvim-treesitter-textobjects default setup
          ;; when in diff mode, use vim text objects c & C instead
          (tc type "table<string, fun(...)>")
          (local move (require :nvim-treesitter.textobjects.move))
          (each [name fun (pairs move)]
            (when (-> :goto (name:find) (= 1))
              (set (. move name)
                   (fn [q ...]
                     (when vim.wo.diff
                       (tc type "table<string, string>")
                       (local config
                              (. (configs.get_module :textobjects.move) name))
                       (each [key query (pairs (or config {}))]
                         (when (= q query)
                           (when (key:find "[%]%[][cC]")
                             (->> key (.. "normal! ") (vim.cmd))
                             (lua :return)))))
                     ;;
                     ;; :return (ie propagate the original `fun` from `move` otherwise)
                     (fun q ...)))))))

(tc type LazySpec)
(local P ;;
       [{1 :nvim-treesitter/nvim-treesitter
         :version false
         :dependencies [{1 :folke/ts-comments.nvim
                         ;; fnlfmt works better with `;;` than `;` as Fennel Lisp comment
                         :opts {:lang {:fennel ";; %s"}}
                         :event :VeryLazy
                         :enabled (-> :nvim-0.10.0 (vim.fn.has) (= 1))}
                        {1 :nvim-treesitter/nvim-treesitter-textobjects
                         :enabled true
                         :config text-object-config}]
         :build ":TSUpdate"
         :event [:BufReadPost :BufNewFile]
         :cmd [:TSUpdateSync :TSUpdate :TSInstall]
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
                                       ;; default select keys
                                       ;; (eg. vif => visual selection of the inner function)
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
                                     ;;
                                     ;; 2024-11-25: new settings
                                     :goto_next_start {"]f" "@function.outer"
                                                       "]c" "@class.outer"
                                                       "]a" "@parameter.inner"}
                                     :goto_next_end {"]F" "@function.outer"
                                                     "]C" "@class.outer"
                                                     "]A" "@parameter.inner"}
                                     :goto_previous_start {"[f" "@function.outer"
                                                           "[c" "@class.outer"
                                                           "[a" "@parameter.inner"}
                                     :goto_previous_end {"[F" "@function.outer"
                                                         "[C" "@class.outer"
                                                         "[A" "@parameter.inner"}}}}}])

P
