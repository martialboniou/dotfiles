(import-macros {: tc} :hondana-dev.macros)
(import-macros {: g!} :hibiscus.vim)

(fn init []
  "conjure init"
  ;; (g! "conjure#filetype#fennel" :conjure.client.fennel.stdio)
  (g! "conjure#mapping#doc_word" :<localleader>K)
  (g! "conjure#debug" false))

(fn config [_ opts]
  "iron core setup"
  (let [{: setup} (require :iron.core)
        ;; NOTE: ensure a table exists at the `config` node too
        opts (if opts.config opts (vim.tbl_extend :force opts {:config {}}))]
    (-> opts (. :config) (type) (not= :table)
        (and (vim.notify "iron repl: the config node of the setup must have a table"
                         vim.log.levels.ERROR)))
    (setup opts)))

(local repl_definition ;;
       {;; TODO: roc
        ;; :roc {:command ["roc" "repl"]}
        :typescript {:command [:npx :tsx] :open ".editor\n" :close "\04"}})

(tc type LazySpec)
(local P [{1 :Vigemus/iron.nvim
           :opts {:config ;;
                  {: repl_definition}
                  :keymaps {:send_motion :<localleader>sc
                            :visual_send :<localleader>sc
                            :send_file :<localleader>sf
                            :send_line :<localleader>sl
                            :send_paragraph :<localleader>sp
                            :send_until_cursor :<localleader>su
                            :send_mark :<localleader>sm
                            :mark_motion :<localleader>mc
                            :mark_visual :<localleader>mc
                            :remove_mark :<localleader>md
                            :cr :<localleader>s<cr>
                            :interrupt :<localleader>s<localleader>
                            :exit :<localleader>sq
                            :clear :<localleader>cl}}
           : config
           ;; :IronRepl :IronRestart :IronFocus :IronHide
           :cmd [:IronRepl]}
          ;; for now, conjure is used in cmp (check plugins/completions)
          {1 :Olical/conjure
           :ft [:fennel :scheme :rust :clojure]
           : init
           :dependencies [{1 :PaterJason/cmp-conjure
                           :dependencies :hrsh7th/nvim-cmp}]}])

P
