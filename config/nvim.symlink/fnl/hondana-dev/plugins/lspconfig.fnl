;;; LSP config
;;; 2025-05-16 - neovim v0.11+ - mason v2.0
(import-macros {: tc} :hondana-dev.macros)

;;; SETUP FUNCTIONS
(fn callback [ev]
  (local opts {:buffer ev.buf :silent true})
  (local keyset (fn [desc ...]
                  (local o opts)
                  (set o.desc desc)
                  (local args [...])
                  (local tbl [])
                  (for [i 1 (length args)]
                    (table.insert tbl (. args i)))
                  (table.insert tbl 6)
                  (-> tbl (unpack) (print))))
  ;; keybindings
  (keyset "Show LSP references" :n :gR "<Cmd>Telescope lsp_references<CR>")
  (keyset "Go to declaration" :n :gD vim.lsp.buf.declaration)
  (keyset "Show LSP definitions" :n :gd "<Cmd>Telescope lsp_definitions<CR>")
  ;; or gI
  (keyset "Show LSP implementations" :n :gi
          "<Cmd>Telescope lsp_implementations<CR>")
  (keyset "Show LSP type definitions" :n :gt
          "<Cmd>Telescope lsp_type_definitions<CR>")
  (keyset "Show available code actions" [:n :v] :<leader>ca
          vim.lsp.buf.code_action)
  ;; nn = ergo
  (let [rename-shortcuts [:rn :nn]]
    (for [i 1 (length rename-shortcuts)]
      (keyset "Smart rename" :n (.. :<leader> (. rename-shortcuts i))
              vim.lsp.buf.rename)))
  (keyset "Show buffer diagnostics" :n :<leader>D
          "<Cmd>Telescope diagnostics bufnr=0<CR>")
  (keyset "Show line diagnostics" :n :<leader>d vim.diagnostic.open_float)
  (keyset "Go to previous diagnostic" :n "[d" vim.diagnostic.goto_prev)
  (keyset "Go to next diagnostic" :n "]d" vim.diagnostic.goto_next)
  (keyset "Show documentation for what is under cursor" :n :K vim.lsp.buf.hover)
  ;; rs = restart LSP
  (keyset "Restart LSP" :n :<leader>rs "<Cmd>LspRestart<CR>"))

(λ config []
  (vim.api.nvim_create_autocmd :LspAttach {: callback})
  ;; enable autocompletion
  (let [capabilities (-> :blink.cmp (require) (#($.get_lsp_capabilities)))]
    (set capabilities.textDocument.completion.completionItem.snippetSupport
         true))
  ;; enable inlay hint
  (vim.lsp.inlay_hint.enable true [0])
  ;; naive setup
  ;; TODO: restore most of the pre-0.11 settings
  (local c #(vim.lsp.config $1 {:settings $2}))
  ;; no wrap line
  (c :html {:html {:format {:wrapLineLength 0}}})
  ;; validate using schema & pull from schemastore
  (c :jsonls
     {:json {:validate {:enable true}
             :schemas (-> :schemastore (require) (#($.json.schemas)))}})
  ;; global vim & others
  (c :lua_ls {:Lua {:diagnostics {:globals [:vim :mp]}}}))

;;; PLUGINS
(tc type LazySpec)
(local P {1 :neovim/nvim-lspconfig
          :event [:BufReadPre :BufNewFile]
          :dependencies [{1 :mason-org/mason.nvim
                          :opts {}
                          :cmd [:Mason
                                :MasonUpdate
                                :MasonLog
                                :MasonInstall
                                :MasonUninstall]}
                         :b0o/schemastore.nvim]
          : config})

P
