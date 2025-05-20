;;; LSP config
;;; 2025-05-20 - neovim v0.11+ - mason v2.0
(import-macros {: tc} :hondana-dev.macros)

;;; SETUP FUNCTIONS
;;
(fn callback [ev]
  (local opts {:buffer ev.buf :silent true})
  (local builtin (require :telescope.builtin))
  (local keyset (fn [desc ...]
                  (let [o opts
                        args [...]
                        tbl []]
                    (when (and desc (not= "" desc))
                      (set o.desc desc))
                    (for [i 1 (length args)]
                      (table.insert tbl (. args i)))
                    (table.insert tbl o)
                    (-> tbl (unpack) (vim.keymap.set)))))
  ;; keybindings
  (keyset "Show LSP definitions" :n :gd builtin.lsp_definitions)
  (keyset "Go to declaration" :n :gD vim.lsp.buf.declaration)
  ;; replaces vim.lsp.buf.implementations (gri since 0.11)
  (keyset "Show LSP implementations" :n :gI builtin.lsp_implementations)
  (let [open-float-diagnostic-shortcuts [:g/ :<leader>vdd]]
    (for [i 1 (length open-float-diagnostic-shortcuts)]
      (keyset "Show line diagnostics" :n (. open-float-diagnostic-shortcuts i)
              vim.diagnostic.open_float)))
  ;; (keyset "Show documentation for what is under cursor" :n :K vim.lsp.buf.hover)
  ;; replaces vim.lsp.buf.workspace_symbol
  (keyset "Show LSP dynamic workspace symbols" :n :<leader>vws
          builtin.lsp_dynamic_workspace_symbols)
  ;; grn is available since 0.11; <leader>nn do the same
  (keyset "" :n :<leader>nn vim.lsp.buf.rename)
  (keyset "Show buffer diagnostics" :n :<leader>D
          #(builtin.diagnostics {:bufnr 0}))
  ;; [d & ]d works as the default 0.11 mappings
  ;; (keyset "Go to previous diagnostic" :n "[d" vim.diagnostic.goto_prev)
  ;; (keyset "Go to next diagnostic" :n "]d" vim.diagnostic.goto_next)
  (keyset "Show LSP type definitions" :n :<leader>vtd
          builtin.lsp_type_definitions)
  ;; gra is available since 0.11
  (keyset "Show available code actions" [:n :v] :<leader>ca
          vim.lsp.buf.code_action)
  ;; WARN: `<leader>vrr`/`<leader>rr` has been removed
  (keyset "Show LSP references" :n :gR builtin.lsp_references)
  ;; grr is available since 0.11
  ;; (keyset "Show LSP references" :n :grr builtin.lsp_references)
  ;; gO = vim.lsp.buf.document_symbol since 0.11
  ;; (keyset "Show LSP document symbol" :n :gO builtin.lsp_document_symbols)
  ;; insert mode = <C-s> is the default mapping since 0.11 (it was <C-h> before)
  ;; (keyset "Show LSP signature help" :i :<C-s> vim.lsp.buf.signature_help)
  ;; <leader>rs = restart LSP
  (keyset "Restart LSP" :n :<leader>rs "<Cmd>LspRestart<CR>"))

(λ config []
  ;; can use vim.lsp.config :* now?
  (vim.api.nvim_create_autocmd :LspAttach {: callback}))

;; WARN: blink knows lsp so no need to enable autocompletion
;; (let [capabilities (-> :blink.cmp (require) (#($.get_lsp_capabilities)))]
;;   (set capabilities.textDocument.completion.completionItem.snippetSupport
;;        true))
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
