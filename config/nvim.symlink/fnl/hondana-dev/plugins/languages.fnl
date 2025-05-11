(import-macros {: tc : funcall!} :hondana-dev.macros)
(import-macros {: make-lazykeys!} :hondana-dev.macros.vim)

(macro make-gopher-keys! [...]
  (icollect [_ t# (ipairs [...])]
    `{1 ,(.. :<leader>gt (string.sub t# 1 1))
      2 #(vim.cmd ,(.. "GoTagAdd " t#))
      :desc ,(.. "Add " (string.upper t#) " struct tags")}))

;; default keys/build = gopher (for golang)
(local (keys build)
       (values (make-gopher-keys! :json :yaml)
               #(vim.cmd " silent! GoInstallDeps ")))

(fn nvim-dap-go-keys []
  (make-lazykeys! [[:dgt #(funcall! :dap-go :debug_test) "Debug go test"]
                   [:dgl #(funcall! :dap-go :debug_last) "Debug last go test"]]))

(tc type "LazySpec[]")
(local P ;; 
       [;;; HASKELL
        {1 :mrcjkb/haskell-tools.nvim :version "^4" :lazy false}
        ;;; GOLANG
        {1 :dreamsofcode-io/nvim-dap-go
         :ft :go
         :keys nvim-dap-go-keys
         :dependencies [:mfussenegger/nvim-dap]}
        {1 :olexsmir/gopher.nvim
         :ft :go
         :dependencies [:nvim-lua/plenary.nvim
                        :nvim-treesitter/nvim-treesitter]
         : keys
         : build}])

;;; (optional) UNISON
(-> :hondana-dev.utils.globals (require) (. :ucm)
    (#(when $
        (local {: joinpath} vim.fs)
        (local complete-unison-vim-path #(joinpath $ :editor-support :vim))
        ;;
        (tc param plugin LazyPlugin param fun string)

        (fn unison-core [plugin fun]
          (let [{fun as-is} (require :lazy.core.loader)]
            (as-is (joinpath plugin.dir ""))
            (-> plugin (. :dir) (complete-unison-vim-path) (as-is))))

        (local (config init)
               (values #(do
                          (vim.opt.rtp:append (complete-unison-vim-path $.dir))
                          (unison-core $ :packadd))
                       #(unison-core $ :ftdetect)))
        (table.insert P {1 :unisonweb/unison :branch :trunk : config : init}))))

P
