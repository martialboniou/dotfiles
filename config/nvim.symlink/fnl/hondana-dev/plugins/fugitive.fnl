(import-macros {: tc} :hondana-dev.macros)
(import-macros {: make-lazykeys!} :hondana-dev.macros.vim)

(tc type LazySpec)
(local P ;;
       {1 :tpope/vim-fugitive
        :cmd :Git
        :init #(let [group (vim.api.nvim_create_augroup :ThePrimeagen_Fugitive
                                                        {})
                     pattern :fugitive
                     callback #(let [bufnr (vim.api.nvim_get_current_buf)
                                     options {:buffer bufnr :remap false}]
                                 (vim.keymap.set :n :<leader>p
                                                 #(vim.cmd.Git :push) options)
                                 ;; rebase always
                                 (vim.keymap.set :n :<leader>P
                                                 #(vim.cmd.Git [:pull
                                                                :--rebase])
                                                 options)
                                 ;; NOTE: It allows me to easily set the
                                 ;; branch i am pushing and any tracking 
                                 ;; needed if i did not set the branch up 
                                 ;; correctly
                                 (vim.keymap.set :n :<leader>t
                                                 ":Git push -u origin " options))]
                 (vim.api.nvim_create_autocmd :FileType
                                              {: callback : group : pattern}))
        :keys (make-lazykeys! [[:gs
                                #(let [(ok _) (pcall vim.cmd.Git)]
                                   (when (not ok)
                                     (vim.notify "This file does not belong to a Git repository"
                                                 vim.log.levels.WARN)))
                                "Open fuGITive status"]])})

P
