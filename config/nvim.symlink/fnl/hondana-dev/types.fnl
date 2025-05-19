(import-macros {: tc} :hondana-dev.macros)

(tc alias bool_fun "fun(): boolean")
(tc alias string_iterator ;;
    "| fun(_: any, v: string): nil" ;;
    "| fun(_: any, v: string): string, string")

(tc alias matcher ;;
    "| fun(startpath: string, func: fun(path: string): boolean): string" ;;
    "| fun(startpath: string, func: fun(path: string): boolean): nil")

(tc class "LspClientConfig: vim.lsp.ClientConfig")
(tc field name string additional slot for name)

(tc class NumberOpts)
(tc field rnu? boolean)
(tc field nu? boolean)

;; roles
(tc alias Role "'developer'|'rustacean'|'haskell-cultist'|'shenturion'")
