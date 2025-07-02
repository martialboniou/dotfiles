(import-macros {: tc} :hondana-dev.macros)
(import-macros {: make-lazykeys!} :hondana-dev.macros.vim)

(local {:nvim_create_autocmd au
        :nvim_create_augroup create-augroup
        :nvim_get_current_bug current-buf} vim.api)

(local augroup #(create-augroup $ {:clear true}))

(local init #(let [group (augroup :Hondana_GitFugitive)
                   pattern :fugitive
                   callback #(let [options {:buffer (current-buf) :remap false}
                                   leader-set #(vim.keymap.set :n
                                                               (.. :<leader> $1)
                                                               $2 options)]
                               (leader-set :p #(vim.cmd.Git :push))
                               (leader-set :n :P
                                           #(vim.cmd.Git [:pull :--rebase]))
                               ;; NOTE: It allows me to easily set the
                               ;; branch i am pushing and any tracking 
                               ;; needed if i did not set the branch up 
                               ;; correctly
                               (leader-set :t ":Git push -u origin "))]
               (au :FileType {: callback : group : pattern})))

(local keys (make-lazykeys! [[:gs
                              #(let [(ok _) (pcall vim.cmd.Git)]
                                 (when (not ok)
                                   (vim.notify "This file does not belong to a Git repository"
                                               vim.log.levels.WARN)))
                              "Open fuGITive status"]]))

(tc type "LazySpec[]")
(local P ;;
       [{1 :tpope/vim-fugitive :cmd :Git : init : keys}
        ;; use :Gitsigns toggle_current_line_blame sometimes
        {1 :lewis6991/gitsigns.nvim :cmd :Gitsigns}])

P
