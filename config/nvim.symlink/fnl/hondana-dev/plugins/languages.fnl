(import-macros {: tc : funcall!} :hondana-dev.macros)
(import-macros {: make-lazykeys!} :hondana-dev.macros.vim)

(macro make-gopher-keys! [...]
  (icollect [_ t# (ipairs [...])]
    `{1 ,(.. :<leader>gt (string.sub t# 1 1))
      2 #(vim.cmd ,(.. "GoTagAdd " t#))
      :desc ,(.. "Add " (string.upper t#) " struct tags")}))

(local {: roles} (require :hondana-dev.utils.globals))

;; default keys/build = gopher (for golang)
(local (keys build)
       (values (make-gopher-keys! :json :yaml)
               #(vim.cmd " silent! GoInstallDeps ")))

(fn nvim-dap-go-keys []
  (make-lazykeys! [[:dgt #(funcall! :dap-go :debug_test) "Debug go test"]
                   [:dgl #(funcall! :dap-go :debug_last) "Debug last go test"]]))

(tc type "LazySpec[]")
(local P ;; 
       [;;; LUA IN THE NEOVIM CONTEXT (MANDATORY IN THIS SETUP!)
        {1 :folke/lazydev.nvim
         :ft :lua
         :opts {:library [{:path "${3rd}/luv/library" :words ["vim%.uv"]}]}}
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

;;; HASKELL
;; NOTE: v6 uses `vim.lsp.config['haskell-tools']` to avoid conflicts with
;; `nvim-lspconfig` I still use for mappings & optional LSP settings (even if
;; `vim.lsp.enable()` can be called anywhere)
(when (roles:check :haskell-cultist)
  (table.insert P {1 :mrcjkb/haskell-tools.nvim
                   :version "^6"
                   ;; this plugin is already lazy
                   :lazy false}))

;; RUST
(when (roles:check :rustacean)
  (table.insert P {1 :mrcjkb/rustaceanvim
                   :version "^6"
                   ;; this plugin is already lazy
                   :lazy false}))

;;; (optional) UNISON
(-> :hondana-dev.utils.globals (require) (. :ucm)
    (#(when $
        (local {: joinpath} vim.fs)
        (local complete-unison-vim-path #(joinpath $ :editor-support :vim))
        ;;
        (tc param plugin LazyPlugin param fun string)

        (fn unison-core [plugin _fun]
          ;; INFO: `fun` was seen as unused by fennel-ls; solution: an underscore prefix
          (let [{_fun as-is} (require :lazy.core.loader)]
            (as-is (joinpath plugin.dir ""))
            (-> plugin (. :dir) (complete-unison-vim-path) (as-is))))

        (local (config init)
               (values #(do
                          (vim.opt.rtp:append (complete-unison-vim-path $.dir))
                          (unison-core $ :packadd))
                       #(unison-core $ :ftdetect)))
        (table.insert P {1 :unisonweb/unison :branch :trunk : config : init}))))

P
