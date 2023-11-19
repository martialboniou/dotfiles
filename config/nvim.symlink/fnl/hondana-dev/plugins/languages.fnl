(macro make-gopher-keys! [...]
  (icollect [_ t# (ipairs [...])]
    `{1 ,(.. :<leader>gt (string.sub t# 1 1))
      2 #(vim.cmd ,(.. "GoTagAdd " t#))
      :desc ,(.. "Add " (string.upper t#) " struct tags")}))

[;;; GO LANG
 {1 :dreamsofcode-io/nvim-dap-go
  :ft :go
  :keys [{1 :<leader>dgt
          2 #(#($.debug_test) (require :dap-go))
          :desc "Debug go test"}
         {1 :<leader>dgl
          2 #(#($.debug_last) (require :dap-go))
          :desc "Debug last go test"}]
  :dependencies [:mfussenegger/nvim-dap]}
 {1 :olexsmir/gopher.nvim
  :ft :go
  :dependencies [:nvim-lua/plenary.nvim :nvim-treesitter/nvim-treesitter]
  :keys (make-gopher-keys! :json :yaml)
  :build #(vim.cmd " silent! GoInstallDeps ")}]
