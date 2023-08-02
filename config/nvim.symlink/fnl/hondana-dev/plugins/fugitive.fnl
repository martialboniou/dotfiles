{1 :tpope/vim-fugitive
 :cmd :Git
 :config (λ []
           (local The-primeagen-Fugitive
                  (vim.api.nvim_create_augroup :ThePrimeagen_Fugitive {}))
           (local autocmd vim.api.nvim_create_autocmd)
           (autocmd :BufWinEnter
                    {:callback (λ []
                                 (when (= vim.bo.ft :fugitive)
                                   (let [bufnr (vim.api.nvim_get_current_buf)
                                         options {:buffer bufnr :remap false}]
                                     (vim.keymap.set :n :<leader>p
                                                     (fn [] (vim.cmd.Git :push))
                                                     options)
                                     ; rebase always
                                     (vim.keymap.set :n :<leader>P
                                                     (fn []
                                                       (vim.cmd.Git [:pull
                                                                     :--rebase]))
                                                     options)
                                     ;; NOTE: It allows me to easily set the
                                     ;; branch i am pushing and any tracking 
                                     ;; needed if i did not set the branch up 
                                     ;; correctly
                                     (vim.keymap.set :n :<leader>t
                                                     ":Git push -u origin "
                                                     options))))
                     :group The-primeagen-Fugitive
                     :pattern "*"}))
 :keys [{1 :<leader>gs 2 vim.cmd.Git :desc "Open fuGITive status"}]}
