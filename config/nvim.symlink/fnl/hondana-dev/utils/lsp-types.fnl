(import-macros {: tc} :hondana-dev.macros)

;; from vim.lsp
(tc class vim.lsp.ClientConfig field name string additional slot for name)
(tc class vim.lsp.Client field stop "fun(force?: boolean)")
