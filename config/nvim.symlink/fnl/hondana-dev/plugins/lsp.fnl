;;; LSP setup (LSP Zero powered)
;;; table structure by: https://github.com/MuhametSmaili/nvim/blob/main/lua/smaili/plugins/lsp/init.lua
;;; 2024-10-23
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
(local preferred-language-servers
       [;; TODO: restore :ts-ls
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
        ; :fennel_language_server
        ;; fennel_ls needs too much work especially
        ;; without macro-path's multiple root-uri '
        ;; -> just here for testing
        :fennel_ls])

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
                ;; optional/highlight same word -> LSP support
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
           (local {:util {: root_pattern}
                   : clangd
                   : lua_ls
                   : fennel_ls
                   : fennel_language_server
                   : zls} (require :lspconfig))
           ;; check if there's a clangd in your llvm-local-binary-path
           (let [local-clangd (.. llvm-local-binary-path :/clangd) ; unused now
                 capabilities (vim.lsp.protocol.make_client_capabilities)
                 on_attach #(do
                              ;; disable formattings (see hondana-dev.plugins.null-ls)
                              (set $.server_capabilities.documentFormattingProvider
                                   false)
                              (set $.server_capabilities.documentRangeFormattingProvider
                                   false)
                              ;; disable semantics if not allowed
                              (or allow-clangd-semantics
                                  (set $.server_capabilities.semanticTokensProvider
                                       nil)))]
             ;; NOTE: null-ls will do the clang-format with extra args
             (set capabilities.offsetEncoding [:utf-16])
             (clangd.setup {:cmd [(if (-> local-clangd
                                          (vim.fn.executable)
                                          (= 1))
                                      local-clangd
                                      :clangd)]
                            : on_attach
                            : capabilities}))
           ;; Lua
           ;; TODO: try folke/lazydev.nvim and get rid of that overheat at workspace.library!
           (lua_ls.setup {:on_init (fn [client]
                                     (when client.workspace_folders
                                       (let [path (. client.workspace_folders 1
                                                     :name)
                                             checkfile (fn [...]
                                                         (vim.uv.fs_stat ...))
                                             json (.. path :/.luarc.json)]
                                         (when (or (checkfile json)
                                                   (checkfile (.. json :c)))
                                           (lua :return))))
                                     (set client.config.settings.Lua
                                          (vim.tbl_deep_extend :force
                                                               client.config.settings.Lua
                                                               {:runtime {:version :LuaJIT}
                                                                :workspace {:checkThirdParty false
                                                                            :library (vim.api.nvim_list_runtime_paths)}})))
                          :settings {:Lua {:diagnostics {:globals [:vim :love]}}}})
           (fennel_ls.setup (let [fennel-path "./?.fnl;./?/init.fnl;src/?.fnl;src/?/init.fnl"
                                  macro-path "./?.fnl;./?/init-macros.fnl;./?/init.fnl;src/?.fnl;src/?/init-macros.fnl;src/?/init.fnl"]
                              ;; TODO: make an utility function from that
                              (let [tangerine-path (-> :data
                                                       (vim.fn.stdpath)
                                                       (.. :/lazy/tangerine.nvim/fnl))
                                    hibiscus-macro-path (-> :data
                                                            (vim.fn.stdpath)
                                                            (.. :/lazy/hibiscus.nvim/fnl))
                                    user-macro-path (-> :config
                                                        (vim.fn.stdpath)
                                                        (.. :/fnl))
                                    fennel-path (if (-> tangerine-path
                                                        (vim.fn.isdirectory)
                                                        (= 1))
                                                    (.. ";" tangerine-path
                                                        "/?.fnl;" tangerine-path
                                                        :/?/init.fnl)
                                                    fennel-path)
                                    macro-path (if (-> hibiscus-macro-path
                                                       (vim.fn.isdirectory)
                                                       (= 1))
                                                   (.. ";" hibiscus-macro-path
                                                       "/?.fnl;"
                                                       hibiscus-macro-path
                                                       "/?/init-macros.fnl;"
                                                       hibiscus-macro-path
                                                       :/?/init.fnl)
                                                   macro-path)
                                    macro-path (if (-> user-macro-path
                                                       (vim.fn.isdirectory)
                                                       (= 1))
                                                   (.. ";" user-macro-path
                                                       "/?.fnl;" user-macro-path
                                                       "/?/init-macros.fnl;"
                                                       user-macro-path
                                                       :/?/init.fnl)
                                                   macro-path)]
                                {:settings {;;  :root_dir #(. (vim.fs.find [:fnl ] {:upward true :type :directory :path $}) 1)
                                            :fennel-ls {: fennel-path
                                                        : macro-path
                                                        :version :lua51
                                                        :extra-globals :vim}}})))
           ;; (fennel_language_server.setup {:cmd [:fennel-language-server]
           ;;                                :filetypes [:fennel]
           ;;                                :single_file_support true
           ;;                                :root_dir (root_pattern :fnl)
           ;;                                :settings {:fennel {:workspace {:library (vim.api.nvim_list_runtime_paths)}
           ;;                                                    ;; I added Löve here (it won't hurt)
           ;;                                                    :diagnostics {:globals [:vim
           ;;                                                                            :love]}}}})
           ;; NOTE: I need the zls that fits zig's version
           (when (-> :zls (vim.fn.executable) (= 1))
             (zls.setup {:cmd [:zls]
                         :filetypes [:zig]
                         :root_dir (root_pattern :build.zig :.git)}))
           (lsp-zero.setup))}
