(import-macros {: tc} :hondana-dev.macros)

;; from vim.lsp
(tc class vim.lsp.ClientConfig)
(tc class vim.lsp.Client field stop "fun(force?: boolean)")

(tc class "LspClientConfig: vim.lsp.ClientConfig" ;;
    field name string additional slot for name)
