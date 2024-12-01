{1 :mfussenegger/nvim-lint
 :event [:BufReadPre :BufNewFile]
 :config #(let [lint (require :lint)
                {:nvim_create_autocmd au :nvim_create_augroup augroup} vim.api
                group (augroup :Hondana_Lint {:clear true})
                callback #(when (vim.opt_local.modifiable:get) (lint.try_lint))]
            (set lint.linters_by_ft {:markdown []})
            (au [:BufEnter :BufWritePost :InsertLeave] {: group : callback}))}
