;; no more grayish comments (not loaded by default unless `boot.roles` contains
;; a commentator tag
(local {:nvim_create_autocmd au & api} vim.api)
(local augrp #(api.nvim_create_augroup $ {:clear true}))

(let [group (augrp :Hondana_Commentator_HiComments)
      callback #(vim.cmd "hi Comment guifg=#fabd2f")]
  (au [:ColorScheme] {: callback : group}))
