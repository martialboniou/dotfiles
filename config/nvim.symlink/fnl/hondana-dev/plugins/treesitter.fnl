(import-macros {: tc} :hondana-dev.macros)

;; F = utility or setup functions at the end of this module
(local F {})

(tc type "string[]")

;; fnlfmt: skip
(local preferred-languages [:typescript :tsx      :javascript :json
                            :c          :awk      :lua        :fennel
                            :rust       :yaml     :markdown   :markdown_inline
                            :roc        :haskell  :ocaml      :ocaml_interface
                            :zig        :go       :elixir     :eex
                            :heex
                            ;; these next ones may have queries used by hondana-dev.plugins.colors
                            :commonlisp :latex])

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
        :r= "@assignment.rhs"})

;; KEYS.scopes
;; WARN: nvim-treesitter uses LuaCATS, not nvim-treesitter-textobjects
;;       string|table must replace string
(tc diagnostic "disable-next-line:assign-type-mismatch")
(set keymaps.as {:query "@scope" :query_group :locals})

(tc type "table<string, string>")
;; NOTE: use a recent `fnlfmt`, otherwise `"@<any>"` can be rewritten as an illegal
;;       form `:@<any>`
(local selection_modes ;;
       {"@parameter.outer" :v "@function.outer" :V "@class.outer" :<C-v>})

;;; CONFIG FUNCTIONS FOR NVIM-TREESITTER(-TEXTOBJECTS)

(tc type "fun(self:LazyPlugin, opts:table):nil|true")
(fn F.textobjects-config []
  "When in diff mode, use vim text objects c & C instead"
  (let [{: get_module : setup} (require :nvim-treesitter.configs)]
    (tc type "table<string, fun(...)>")
    (local move (require :nvim-treesitter.textobjects.move))
    (each [name fun (pairs move)]
      (when (-> :goto (name:find) (= 1))
        (set (. move name) ;;
             (fn [q ...]
               (when vim.wo.diff
                 (tc type "table<string, string>")
                 (local config (. (get_module :textobjects.move) name))
                 (each [key query (pairs (or config {}))]
                   (when (= q query)
                     (when (key:find "[%]%[][cC]")
                       (->> key (.. "normal! ") (vim.cmd))
                       (lua :return)))))
               ;;
               ;; :return (ie propagate the original `fun` from `move` otherwise)
               (fun q ...)))))))

(tc param _ "string?" "language")
(tc param buf integer "buffer number")
(tc return boolean|nil)
(fn F.disable [_ buf]
  (let [;; 200 KB max
        max_filesize (* 200 1024)
        (ok stats) (pcall vim.uv.fs_stat (vim.api.nvim_buf_get_name buf))]
    (and ok stats (> stats.size max_filesize))))

(tc type "fun(self:LazyPlugin, opts:table):nil|true")
(fn F.config [_ opts]
  (-> :nvim-treesitter.configs
      (require)
      (#($.setup opts))
      ;; HACK: temporary hack utils when nvim is 0.11
      (when (-> "nvim-0.11" (vim.fn.has) (= 1))
        (let [new-setup-commands #(each [command-name def (pairs $2)]
                                    (let [f-args (or def.f_args "<f-args>")
                                          {: flatten} (require :nvim-treesitter.compat)
                                          call-fn (-> "lua require'nvim-treesitter.%s'.commands.%s['run<bang>'](%s)"
                                                      (string.format $1
                                                                     command-name
                                                                     f-args))
                                          parts (flatten ["command!"
                                                          "-bar"
                                                          (or def.args [])
                                                          command-name
                                                          call-fn])]
                                      (-> parts (table.concat " ")
                                          (vim.api.nvim_command))))
              utils (require :nvim-treesitter.utils)]
          (set utils.setup_commands new-setup-commands))))
  (when (-> :koka (vim.fn.executable) (= 1))
    ;; additional parser for koka
    (let [{:get_parser_configs get} (require :nvim-treesitter.parsers)
          parser-config (get)]
      (set parser-config.koka {:install_info {:url "https://github.com/mtoohey31/tree-sitter-koka"
                                              :branch :main
                                              :files ["src/parser.c"
                                                      "src/scanner.c"]}
                               :filetype :koka}))))

;;; PLUGINS & SETUP
(tc type LazySpec)
(local P ;;
       {1 :nvim-treesitter/nvim-treesitter
        :version false
        :dependencies [{1 :folke/ts-comments.nvim
                        ;; fnlfmt works better with `;;` than `;` as Fennel Lisp comment
                        :opts {:lang {:fennel ";; %s"}}
                        :event :VeryLazy
                        :enabled true}
                       {1 :nvim-treesitter/nvim-treesitter-textobjects
                        :config F.textobjects-config}]
        :build ":TSUpdate"
        :cmd [:TSUpdateSync :TSUpdate :TSInstall]
        :config F.config
        :opts {:ensure_installed preferred-languages
               :matchup {;; special vim-matchup (check hondana-dev.plugins.operators)
                         :enable true
                         :disable []}
               :sync_install false
               :auto_install true
               :rainbow {:enable true :extended_mode true}
               :playground {:enable true}
               :highlight {:enable true
                           :disable F.disable
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
                                                        "[A" "@parameter.inner"}}}}})

P
