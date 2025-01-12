(import-macros {: tc : funcall!} :hondana-dev.macros)
(import-macros {: make-lazykeys!} :hondana-dev.macros.vim)

(macro make-gopher-keys! [...]
  (icollect [_ t# (ipairs [...])]
    `{1 ,(.. :<leader>gt (string.sub t# 1 1))
      2 #(vim.cmd ,(.. "GoTagAdd " t#))
      :desc ,(.. "Add " (string.upper t#) " struct tags")}))

(tc type LazySpec)
(local P ;; 
       [;;; HASKELL
        {1 :mrcjkb/haskell-tools.nvim :version "^4" :lazy false}
        ;;; GOLANG
        {1 :dreamsofcode-io/nvim-dap-go
         :ft :go
         :keys (make-lazykeys! [[:dgt
                                 #(funcall! :dap-go :debug_test)
                                 "Debug go test"]
                                [:dgl
                                 #(funcall! :dap-go :debug_last)
                                 "Debug last go test"]])
         :dependencies [:mfussenegger/nvim-dap]}
        {1 :olexsmir/gopher.nvim
         :ft :go
         :dependencies [:nvim-lua/plenary.nvim
                        :nvim-treesitter/nvim-treesitter]
         :keys (make-gopher-keys! :json :yaml)
         :build #(vim.cmd " silent! GoInstallDeps ")}])

P
