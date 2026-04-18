(import-macros {: tc} :hondana-dev.macros)
(import-macros {: make-lazykeys!} :hondana-dev.macros.vim)

(tc type "fun(self:LazyPlugin, opts:table)")
(fn vim-test-config []
  (vim.cmd "function! BufferTermStrategy(cmd)\nexe 'te ' . a:cmd\nendfunction
                         let g:test#custom_strategies = {'bufferterm': function('BufferTermStrategry')}
                         let g:test#strategy = 'bufferterm'
                         "))

(tc type boolean)
(local silent true)

(tc type LazySpec)
(local P [{1 :nvim-neotest/neotest :dependencies :nvim-neotest/nvim-nio}
          {1 :vim-test/vim-test
           ;; <leader> keybindings
           :keys (make-lazykeys! [[:Tf
                                   "<Cmd>TestFile<CR>"
                                   {: silent :desc "Run this file"}]
                                  [:Tn
                                   "<Cmd>TestNearest<CR>"
                                   {: silent :desc "Run nearest file"}]
                                  [:Tl
                                   "<Cmd>TestLast<CR>"
                                   {: silent :desc "Run last test"}]])
           :config vim-test-config}])

P
