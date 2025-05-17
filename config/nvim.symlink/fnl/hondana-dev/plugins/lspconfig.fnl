;;; LSP config
;;; 2025-05-16 - neovim v0.11+ - mason v2.0
(import-macros {: tc} :hondana-dev.macros)

(tc return boolean)
(fn executable? [binary-name]
  (-> binary-name (vim.fn.executable) (= 1)))

;;; SETUP FUNCTIONS
(fn callback [ev]
  (local opts {:buffer ev.buf :silent true})
  (local builtin (require :telescope.builtin))
  (local keyset (fn [desc ...]
                  (let [o opts
                        args [...]
                        tbl []]
                    (set o.desc desc)
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
  (keyset "Show documentation for what is under cursor" :n :K vim.lsp.buf.hover)
  ;; replaces vim.lsp.buf.workspace_symbol
  (keyset "Show LSP dynamic workspace symbols" :n :<leader>vws
          builtin.lsp_dynamic_workspace_symbols)
  ;; nn = ergo
  (let [rename-shortcuts [:rn :nn]]
    (for [i 1 (length rename-shortcuts)]
      (keyset "Smart rename" :n (.. :<leader> (. rename-shortcuts i))
              vim.lsp.buf.rename)))
  (keyset "Show buffer diagnostics" :n :<leader>D
          #(builtin.diagnostics {:bufnr 0}))
  ;; [d & ]d works as the default 0.11 mappings
  (keyset "Go to previous diagnostic" :n "[d" vim.diagnostic.goto_prev)
  (keyset "Go to next diagnostic" :n "]d" vim.diagnostic.goto_next)
  ;; WARN: `<leader>vtd` has been removed
  (keyset "Show LSP type definitions" :n :gt builtin.lsp_type_definitions)
  ;; gra is available since 0.11
  (keyset "Show available code actions" [:n :v] :<leader>ca
          vim.lsp.buf.code_action)
  ;; WARN: `<leader>vrr`/`<leader>rr` has been removed
  (keyset "Show LSP references" :n :gR builtin.lsp_references)
  ;; grr is available since 0.11
  (keyset "Show LSP references" :n :grr builtin.lsp_references)
  ;; g0 = vim.lsp.buf.document_symbol since 0.11
  (keyset "Show LSP document symbol" :n :g0 builtin.lsp_document_symbols)
  ;; rs = restart LSP
  (keyset "Restart LSP" :n :<leader>rs "<Cmd>LspRestart<CR>")
  ;; insert mode = <C-s> is the default mapping since 0.11 (it was <C-h> before)
  (keyset "Show LSP signature help" :i :<C-s> vim.lsp.buf.signature_help))

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
  (c :lua_ls {:Lua {:diagnostics {:globals [:vim :mp]}}})
  ;; fennel_ls
  ;; NOTE: I recommend to install fennel-ls manually (Mason/LuaRocks might have an outdated version)
  ;; you will need a `flsproject.fnl` file at the root: use `~/.config/nvim/fnl/build-flsproject.sh`
  (when (executable? :fennel-ls)
    ;; TIP: change root project with `:lcd` if needed
    (vim.lsp.config :fennel_ls ;; search in the vicinity instead of visiting
                    ;; the ancestors with root_pattern from nvim-lspconfig
                    ;; WARN: the Fennel code must have a `fnl` directory root with a `flsproject.fnl`
                    ;; {:root_dir #(-> [:fnl]
                    ;;                 (vim.fs.find {:upward true
                    ;;                               :type :directory
                    ;;                               :path (vim.fn.getcwd)})
                    ;;                 (. 1))}
                    {})
    (vim.lsp.enable :fennel_ls))
  ;;
  ;; other enabled
  (local enabled [:lua_ls])
  (vim.lsp.enable enabled))

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
