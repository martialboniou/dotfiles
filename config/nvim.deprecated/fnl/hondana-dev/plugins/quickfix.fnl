(import-macros {: tc} :hondana-dev.macros)
(import-macros {: make-lazykeys!} :hondana-dev.macros.vim)
;; NOTE: todo-comments.nvim will colorize your todo comments (here as an example) 

(tc type LazySpec)
(local P ;;
       [{1 :folke/trouble.nvim
         :opts {}
         :cmd :Trouble
         :keys (make-lazykeys! [[:xx
                                 "<Cmd>Trouble diagnostics toggle<CR>"
                                 "Diagnostics (Trouble)"]
                                [:xX
                                 "<Cmd>Trouble diagnostics toggle filter.buf=0<CR>"
                                 "Buffer Diagnostics (Trouble)"]
                                ;; TODO: check whether the following ones conflict with LSP code actions
                                [:cs
                                 "<Cmd>Trouble symbols toggle focus=false<CR>"
                                 "Symbols (Trouble)"]
                                [:cl
                                 "<Cmd>Trouble lsp toggle focus=false win.position=right<CR>"
                                 "LSP Definitions/References/... (Trouble)"]
                                [:xL
                                 "<Cmd>Trouble loclist toggle<CR>"
                                 "Location List (Trouble)"]
                                [:xQ
                                 "<Cmd>Trouble quickfix toggle<CR>"
                                 "Quickfix List (Trouble)"]])}
        {1 :folke/todo-comments.nvim
         :dependencies [:nvim-lua/plenary.nvim]
         :event :VeryLazy
         :opts {}
         :init #(let [map #(vim.keymap.set $...)
                      {: jump_next : jump_prev} (require :todo-comments)]
                  (map :n "]t" jump_next {:desc "Next todo comment"})
                  (map :n "[t" jump_prev {:desc "Previous todo comment"})
                  nil)}])

P
