(import-macros {: tc} :hondana-dev.macros)

;; F = utility or setup functions at the end of this module
(local F {})

(tc type "string[]")

;; fnlfmt: skip
(local preferred-languages [:typescript      :tsx             :javascript :json
                            :c               :awk             :lua        :fennel
                            :vim             :vimdoc          :rust       :markdown
                            :markdown_inline :html            :php        :yaml
                            :ocaml           :ocaml_interface :scheme     :zig
                            :go])

;;; KEYS
(tc type "table<string, string>")
;; Usage: `vim` => visual selection of the inner method/function
(local keymaps ;;
       {;; classes
        :ac "@class.outer"
        :ic "@class.inner"
        ;; INFO: idea from Josean Martinez
        ;;
        ;; BEWARE: methods/functions (m, not f)
        :am "@function.outer"
        :im "@function.inner"
        ;; function calls
        :af "@call.outer"
        :if "@call.inner"
        ;; args
        :aa "@parameter.outer"
        :ia "@parameter.inner"
        ;; ifs
        :ai "@conditional.outer"
        :ii "@conditional.inner"
        ;; loops
        :al "@loop.outer"
        :il "@loop.inner"
        ;; =s (technically :=)
        :a= "@assignment.outer"
        :i= "@assignment.outer"
        ;; lhs/rhs of =s
        :l= "@assignment.lhs"
        :r= "@assignment.rhs"
        ;; scopes
        :as {:query "@scope" :query_group :locals}})

(tc type "table<string, string>")
;; NOTE: use a recent `fnlfmt`, otherwise `"@<any>"` can be rewritten as an illegal
;;       form `:@<any>`
(local selection_modes ;;
       {"@parameter.outer" :v "@function.outer" :V "@class.outer" :<C-v>})

;;; PLUGINS & SETUP
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
                         :config F.textobjects-config}]
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
                                       : keymaps
                                       : selection_modes}
                              :move {:enable true
                                     :set_jumps true
                                     ;; NOTE: neither [t nor ]t b/c todo-comments
                                     ;;       check hondana-dev.plugins.quickfix
                                     ;;
                                     ;; 2024-11-25: new settings
                                     ;; TODO: add more?
                                     :goto_next_start {"]m" "@function.outer"
                                                       "]c" "@class.outer"
                                                       "]a" "@parameter.inner"}
                                     :goto_next_end {"]M" "@function.outer"
                                                     "]C" "@class.outer"
                                                     "]A" "@parameter.inner"}
                                     :goto_previous_start {"[m" "@function.outer"
                                                           "[c" "@class.outer"
                                                           "[a" "@parameter.inner"}
                                     :goto_previous_end {"[M" "@function.outer"
                                                         "[C" "@class.outer"
                                                         "[A" "@parameter.inner"}}}}}])

;;; CONFIG FOR NVIM-TREESITTER-TEXTOBJECTS

(tc type "fun(self:LazyPlugin, opts:table)|true")
(fn F.textobjects-config []
  "When in diff mode, use vim text objects c & C instead"
  (let [configs (require :nvim-treesitter.configs)]
    (tc type "table<string, fun(...)>")
    (local move (require :nvim-treesitter.textobjects.move))
    (each [name fun (pairs move)]
      (when (-> :goto (name:find) (= 1))
        (set (. move name) ;;
             (fn [q ...]
               (when vim.wo.diff
                 (tc type "table<string, string>")
                 (local config (. (configs.get_module :textobjects.move) name))
                 (each [key query (pairs (or config {}))]
                   (when (= q query)
                     (when (key:find "[%]%[][cC]")
                       (->> key (.. "normal! ") (vim.cmd))
                       (lua :return)))))
               ;;
               ;; :return (ie propagate the original `fun` from `move` otherwise)
               (fun q ...)))))))

P
