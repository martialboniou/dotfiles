(import-macros {: tc} :hondana-dev.macros)

;;; SERVERS
;;

;; set the list of your LSP language servers here
(local servers [:lua_ls :html :jsonls :pyright])

;; optional-servers = list of non-mason language servers, the optional second value is a
;; binary to test (the server name is not always the language server binary name)
(local optional-servers [;; NOTE: Mason/LuaRocks might have an outdated version of fennel-ls
                         ;; you will need a `flsproject.fnl` file at the root
                         ;; use `~/.config/nvim/fnl/build-flsproject.sh`
                         [:fennel_ls :fennel-ls]
                         ;; zig & zls must be of the same milestone so avoid the Mason version
                         ;; if you need an older version or an unstable one
                         :zls
                         ;; clangd can be managed by Mason
                         :clangd
                         ;; DON'T UNCOMMENT: haskell-tools.nvim v6 replaces [:hls :haskell-language-server-wrapper]
                         :gopls
                         :ocamllsp])

;;; ENABLE LSP (check setup in `fnl/after/lsp`)
;;
;; NOTE: the following code could have been put at the end of `config()` in `hondana-dev.plugins.lspconfig`
(tc return boolean)
(fn executable? [binary-name]
  (-> binary-name (vim.fn.executable) (= 1)))

(tc param ctx Role)
(tc param content string)
(tc param tbl "string[]")
(fn unless-role-insert [ctx content tbl]
  (let [{: roles} (require :hondana-dev.utils.globals)]
    (when (not (roles:check ctx))
      (table.insert tbl content))))

;; filter some unorthodox LSPs/environment (check `hondana-dev.plugins.languages`)
(unless-role-insert :haskell-cultist :hls servers)
(unless-role-insert :rustacean :rust servers)
;;
(vim.lsp.enable servers)
;; TODO: restore most of the pre-0.11 settings
;; additional settings/activation of non-mason/optional servers
(for [i 1 (length optional-servers)]
  (local os (case (. optional-servers i)
              [a b] {:server a :binary b}
              a {:server a :binary a}))
  (when (executable? os.binary) (vim.lsp.enable os.server)))

;;; COSMETICS
;;
;; WARN: enable inlay hint
;; (vim.lsp.inlay_hint.enable [0])
