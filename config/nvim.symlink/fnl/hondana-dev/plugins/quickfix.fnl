;; NOTE: todo-comments.nvim will colorize your todo comments (here as an example) 
[{1 :folke/trouble.nvim
  :opts {}
  :cmd :Trouble
  :keys [{1 :<leader>xx
          2 "<Cmd>Trouble diagnostics toggle<CR>"
          :desc "Diagnostics (Trouble)"}
         {1 :<leader>xX
          2 "<Cmd>Trouble diagnostics toggle filter.buf=0<CR>"
          :desc "Buffer Diagnostics (Trouble)"}
         ;; TODO: check whether the following ones conflict with LSP code actions
         {1 :<leader>cs
          2 "<Cmd>Trouble symbols toggle focus=false<CR>"
          :desc "Symbols (Trouble)"}
         {1 :<leader>cl
          2 "<Cmd>Trouble lsp toggle focus=false win.position=right<CR>"
          :desc "LSP Definitions / references / ... (Trouble)"}
         {1 :<leader>xL
          2 "<Cmd>Trouble loclist toggle<CR>"
          :desc "Location List (Trouble)"}
         {1 :<leader>xQ
          2 "<Cmd>Trouble quickfix toggle<CR>"
          :desc "Quickfix List (Trouble)"}]}
 {1 :folke/todo-comments.nvim
  :dependencies [:nvim-lua/plenary.nvim]
  :event :VeryLazy
  :opts {}
  :init #(let [map #(vim.keymap.set $...)
               {: jump_next : jump_prev} (require :todo-comments)]
           (map :n "]t" jump_next {:desc "Next todo comment"})
           (map :n "[t" jump_prev {:desc "Previous todo comment"})
           nil)}]
