(import-macros {: tc} :hondana-dev.macros)

;;; SERVERS
;;
;; set the list of your LSP language servers to enable here
(tc type "string[]")
(local servers [:lua_ls
                :awk_ls
                :html
                :jsonls
                :ts_ls
                :tailwindcss
                :cssls
                :pyright
                :marksman
                :elixir_ls])

;; unison case: check https://github.com/unisonweb/unison
;; NOTE: the UCM listener existence is known at comptime, no need to check for an executable
(when (-> :hondana-dev.utils.globals (require) (. :ucm))
  (table.insert servers :unison))

;; optional-servers = list of non-mason language servers, the optional second value is a
;; binary to test (the server name is not always the language server binary name)
(tc alias ServerBinary "string|[string, string]")
(tc type "ServerBinary[]")
(local optional-servers
       [;; NOTE: Mason/LuaRocks might have an outdated version of fennel-ls
        ;; you will need a `flsproject.fnl` file at the root
        ;; use `~/.config/nvim/fnl/build-flsproject.sh`
        [:fennel_ls :fennel-ls]
        ;; zig & zls must be of the same milestone so avoid the Mason version
        ;; if you need an older version or an unstable one
        :zls
        :koka
        [:roc_ls :roc_language_server]
        ;; clangd can be managed by Mason
        :clangd
        ;; DON'T UNCOMMENT: haskell-tools.nvim v6 replaces [:hls :haskell-language-server-wrapper]
        :gopls
        :ocamllsp
        :nixd
        ;; options for `elm`: `init`, `make`, `reactor`...
        [:elm_ls :elm-language-server]])

;;; ENABLE LSP (check setup in `fnl/after/lsp`)
;;
;; NOTE: the following code could have been put at the end of `config()` in `hondana-dev.plugins.lspconfig`
(tc binary_name "string")
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
;;
;; TODO: restore most of the pre-0.11 settings
;; additional settings/activation of non-mason/optional servers
(for [i 1 (length optional-servers)]
  (local os (case (. optional-servers i)
              [a b] {:server a :binary b}
              a {:server a :binary a}
              _ (error :unreachable)))
  (tc cast os "{binary: string, server: string}")
  (when (executable? os.binary)
    (vim.lsp.enable os.server)))

;;; ATTACH KEYBINDINGS/MAPPINGS
;;
(fn on_attach [_client buffer]
  (local builtin (require :telescope.builtin))
  (local keyset (fn [desc ...]
                  (let [opts {: buffer :silent true}
                        args [...]
                        tbl []]
                    (when (and desc (not= "" desc))
                      (set opts.desc desc))
                    (for [i 1 (length args)]
                      (table.insert tbl (. args i)))
                    (table.insert tbl opts)
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
  (keyset "Restart LSP" :n :<leader>rs "<Cmd>LspRestart<CR>")
  ;; <leader>ih = toggle inlay hint
  (keyset "Toggle LSP inlay hints" :n :<leader>ih
          #(vim.lsp.inlay_hint.enable (not vim.lsp.inlay_hint.is_enabled))))

(vim.lsp.config "*" {: on_attach})

;;; COSMETICS
;;
;; WARN: enable inlay hint
;; (vim.lsp.inlay_hint.enable [0])
