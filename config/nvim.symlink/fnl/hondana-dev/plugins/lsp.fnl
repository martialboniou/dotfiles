;;; LSP setup (LSP Zero powered)
;;; table structure by: https://github.com/MuhametSmaili/nvim/blob/main/lua/smaili/plugins/lsp/init.lua
;;; 2024-09-28
(local zero-setup-preferred-preset :recommended)

(local lsp-custom-keymaps
       {:n {:<leader>f #(vim.lsp.buf.format)
            :gd #(vim.lsp.buf.definition)
            :K #(vim.lsp.buf.hover)
            :<leader>vws #(vim.lsp.buf.workspace_symbol)
            :<leader>vd #(vim.diagnostic.open_float)
            "[d" #(vim.diagnostic.goto_prev)
            "]d" #(vim.diagnostic.goto_next)
            :<leader>vca #(vim.lsp.buf.code_action)
            :<leader>vrr #(vim.lsp.buf.references)
            :<leader>vrn #(vim.lsp.buf.rename)
            ;;; ! for ergonomics: <leader> + ca = vca, rr = vrr, nn, vrn
            :<leader>ca #(vim.lsp.buf.code_action)
            :<leader>rr #(vim.lsp.buf.references)
            :<leader>nn #(vim.lsp.buf.rename)}
        :i {:<C-h> #(vim.lsp.buf.signature_help)}})

;; I use the Mason clangd but you can use another one; remove _remove-me_
(local llvm-local-binary-path :/opt/homebrew/opt/llvm/bin_remove-me_)
;; change to true if you want the clangd's overthought semantics!
(local allow-clangd-semantics (-> "shitty colors" (type) (not= :string)))

;; WARN: this will be replaced by mason-lspconfig in version 3
(local preferred-language-servers [;; ASAP -> :ts-ls
                                   :rust_analyzer
                                   :clangd
                                   :html
                                   :lua_ls
                                   :jsonls
                                   :tailwindcss
                                   :dockerls
                                   :docker_compose_language_service
                                   :astro
                                   :marksman
                                   :vimls
                                   :cssls
                                   :ocamllsp
                                   :gopls
                                   ;; :zls DON'T USE MASON HERE (see below)
                                   :fennel_language_server
                                   ;; fennel_ls needs too much work especially
                                   ;; without macro-path's multiple root-uri
                                   ;; -> just here for testing
                                   ;; :fennel_ls
                                   ])

{1 :neovim/nvim-lspconfig
 :event :BufReadPost
 :dependencies [;; LSP Zero
                {1 :VonHeikemen/lsp-zero.nvim :branch :v2.x :lazy true}
                ;; {1 :simrat39/rust-tools.nvim}
                {1 :williamboman/mason.nvim
                 ;; check hondana-dev.plugins.null-ls for the ensure_installed packages
                 :opts {}
                 :cmd :Mason
                 :run ":MasonUpdate"}
                :williamboman/mason-lspconfig.nvim
                ;; see Autocompletion
                :hrsh7th/nvim-cmp
                ;; optional/highlight same word (LSP support)
                :rrethy/vim-illuminate
                {;; optional/fancy navbar with LSP (+ other tools)
                 1 :glepnir/lspsaga.nvim
                 :dependencies [:nvim-tree/nvim-web-devicons
                                :nvim-treesitter/nvim-treesitter]
                 :event :LspAttach
                 :opts {:code_action {:show_server_name true
                                      :extend_gitsigns false}
                        :lightbulb {:enable false}
                        :diagnostic {:on_insert false :on_insert_follow false}
                        :rename {:in_select false}}}]
 :opts {:diagnostics {:update_in_insert false :virtual_text true}
        :autoformat true
        :zero-setup {:preset zero-setup-preferred-preset
                     :preferences {:suggest_lsp_servers false}
                     :servers preferred-language-servers
                     :sign-icons {:error "✘"
                                  :warn "▲"
                                  :hint "⚑"
                                  :info "»"}
                     :sign-chars {:error :E :warn :W :hint :H :info :I}}}
 :config (λ [_ opts]
           ;; reduce boilerplate code with LSP Zero
           (local lsp-zero (require :lsp-zero))
           (local lsp-zero-setup opts.zero-setup)
           (lsp-zero.nvim_workspace)
           (lsp-zero.preset lsp-zero-setup.preset)
           (lsp-zero.ensure_installed lsp-zero-setup.servers)
           (lsp-zero.set_preferences lsp-zero-setup.preferences)
           ;; change the following to lsp-zero-setup.sign-chars
           (lsp-zero.set_sign_icons lsp-zero-setup.sign-icons)
           (vim.diagnostic.config opts.diagnostics)
           (lsp-zero.on_attach (λ [_ buffer]
                                 (local options {: buffer :remap false})
                                 (each [mode map (pairs lsp-custom-keymaps)]
                                   (each [key fun (pairs map)]
                                     (vim.keymap.set mode key fun options)))))
           (local lspconfig (require :lspconfig))
           ;; check if there's a clangd in your llvm-local-binary-path
           (let [local-clangd (.. llvm-local-binary-path :/clangd) ; unused now
                 capabilities (vim.lsp.protocol.make_client_capabilities)
                 on_attach #(do
                              ;; disable formattings (see hondana-dev.plugins.null-ls)
                              (set $1.server_capabilities.documentFormattingProvider
                                   false)
                              (set $1.server_capabilities.documentRangeFormattingProvider
                                   false)
                              ;; disable semantics if not allowed
                              (or allow-clangd-semantics
                                  (set $1.server_capabilities.semanticTokensProvider
                                       nil)))]
             ;; IMPORTANT: null-ls will do the clang-format with extra args
             (set capabilities.offsetEncoding [:utf-16])
             (lspconfig.clangd.setup {:cmd [(if (-> local-clangd
                                                    (vim.fn.executable)
                                                    (= 1))
                                                local-clangd
                                                :clangd)]
                                      : on_attach
                                      : capabilities}))
           ;; Lua
           (lspconfig.lua_ls.setup (lsp-zero.nvim_lua_ls))
           (tset (require :lspconfig.configs) :fennel_language_server
                 {:default_config {:cmd [:fennel-language-server]
                                   :filetypes [:fennel]
                                   :single_file_support true
                                   :root_dir (lspconfig.util.root_pattern :fnl)
                                   :settings {:fennel {:workspace {:library (vim.api.nvim_list_runtime_paths)}
                                                       ;; I added Löve here (it won't hurt)
                                                       :diagnostics {:globals [:vim
                                                                               :love]}}}}})
           (lspconfig.fennel_language_server.setup {})
           ;; INSTALL https://sr.ht/~xerool/fennel-ls if you use fennel_ls (not recommended)
           ;; NOTE: I need the zls that fits zig's version
           (when (-> :zls (vim.fn.executable) (= 1))
             (tset (require :lspconfig.configs) :zls
                   {:default_config {:cmd [:zls]
                                     :filetypes [:zig]
                                     :root_dir (lspconfig.util.root_pattern :build.zig
                                                                            :*.zig
                                                                            :.git)}})
             (lspconfig.zls.setup {}))
           (lsp-zero.setup))}
