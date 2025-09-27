(local {:nvim_create_autocmd au
        :nvim_create_augroup augroup
        :nvim_win_set_cursor set-cursor} vim.api)

;; prevent cursor from moving after yank (useful in visual mode)
;; source: https://github.com/danielt812/nvim-config/blob/main/lua/config/autocmds.lua
(let [group (augroup :Hondana_YankPosition {:clear true})
      desc "Prevent cursor moving after yank"
      callback #(when (and (= vim.v.event.operator :y) vim.b.cursor_pre_yank)
                  (set-cursor 0 vim.b.cursor_pre_yank)
                  (set vim.b.cursor_pre_yank nil))]
  (au :TextYankPost {: group : callback : desc}))
