;;; LSP setup (LSP Zero powered)
;;; table structure by: https://github.com/MuhametSmaili/nvim/blob/main/lua/smaili/plugins/lsp/init.lua
;;; 2023-09-03

(local zero-setup-preferred-preset :recommended)

(local lsp-custom-keymaps
       {:n {:<leader>f #(vim.lsp.buf.format)
            :gd #(vim.lsp.buf.definition)
            :K #(vim.lsp.buf.hover)
            :<leader>vws #(vim.lsp.buf.workspace_symbol)
            :<leader>vd #(vim.diagnostic.open_float)
            "[d" #(vim.diagnostic.goto_next)
            "]d" #(vim.diagnostic.goto_prev)
            :<leader>vca #(vim.lsp.buf.code_action)
            :<leader>vrr #(vim.lsp.buf.references)
            :<leader>vrn #(vim.lsp.buf.rename)
            ;;; ! for ergonomics: <leader> + ca = vca, rr = vrr, nn, vrn
            :<leader>ca #(vim.lsp.buf.code_action)
            :<leader>rr #(vim.lsp.buf.references)
            :<leader>nn #(vim.lsp.buf.rename)}
        :i {:<C-h> #(vim.lsp.buf.signature_help)}})

;; NOTE: stylua is ready to use but still unused here
(local mason-lspconfig-preferred-install [:rust_analyzer :ocaml-lsp])

;; NOTE: ocaml-lsp is the same as ocaml-lsp-server via opam

;; will be replaced by mason-lspconfig in version 3
(local preferred-language-servers
       [:tsserver
        :rust_analyzer
        :html
        :lua_ls
        :jsonls
        :tailwindcss
        :dockerls
        :docker_compose_language_service
        :astro
        :vimls
        :cssls
        :ocamllsp
        :zls
        ;; fennel_ls needs too much work especially
        ;; without macro-path's multiple root-uri
        :fennel_language_server])

{1 :neovim/nvim-lspconfig
 :event :BufReadPost
 :dependencies [;; LSP Zero
                {1 :VonHeikemen/lsp-zero.nvim :branch :v2.x :lazy true}
                ;; {1 :simrat39/rust-tools.nvim}
                {1 :williamboman/mason.nvim
                 :opts {}
                 :cmd :Mason
                 :run ":MasonUpdate"}
                {1 :williamboman/mason-lspconfig.nvim
                 :opts (λ [_ opts]
                         (when (= (type opts.ensure_installed) :table)
                           (vim.list_extend opts.ensure_installed
                                            mason-lspconfig-preferred-install)))}
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
           (lsp-zero.on_attach (λ [_ bufnr]
                                 (local options {:buffer bufnr :remap false})
                                 (each [mode map (pairs lsp-custom-keymaps)]
                                   (each [key fun (pairs map)]
                                     (vim.keymap.set mode key fun options)))))
           (local lspconfig (require :lspconfig))
           ;; Lua
           (lspconfig.lua_ls.setup (lsp-zero.nvim_lua_ls))
           (tset (require :lspconfig.configs) :fennel_language_server
                 {:default_config {:cmd [:fennel-language-server]
                                   :filetypes [:fennel]
                                   :single_file_support true
                                   :root_dir (lspconfig.util.root_pattern :fnl)
                                   :settings {:fennel {:workspace {:library (vim.api.nvim_list_runtime_paths)}
                                                       :diagnostics {:globals [:vim]}}}}})
           (lspconfig.fennel_language_server.setup {})
           ;; INSTALL https://sr.ht/~xerool/fennel-ls if you use fennel_ls (not recommended)
           (if (-> :fennel-ls (vim.fn.executable) (= 1))
               (do
                 ;; (tset (require :lspconfig.configs) :fennel_ls
                 ;;       {:default_config {:cmd [:fennel-ls]
                 ;;                         :filetypes [:fennel]
                 ;;                         :single_file_support true
                 ;;                         :root_dir (λ [startpath]
                 ;;                                     (let [path ((lspconfig.util.root_pattern :fnl) startpath)]
                 ;;                                       (local root
                 ;;                                              (table.concat [path
                 ;;                                                             :fnl]
                 ;;                                                            "/"))
                 ;;                                       (when (lspconfig.util.path.exists root)
                 ;;                                         root)))
                 ;;                         :settings {:fennel {:workspace {:library (vim.api.nvim_list_runtime_paths)}
                 ;;                                             :diagnostics {:globals [:vim]}}
                 ;;                                    ;                                    :fennel-ls {:macro-path "~/.local/share/nvim/lazy/hibiscus\.nvim/fnl/?.fnl;./?.fnl;"}
                 ;;                                    }}})
                 ;; (lspconfig.fennel_ls.setup {})
                 ))
           (lsp-zero.setup))}
