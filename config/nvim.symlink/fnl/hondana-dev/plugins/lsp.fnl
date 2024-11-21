;;; LSP setup (LSP Zero powered)
;;; table structure by: https://github.com/MuhametSmaili/nvim/blob/main/lua/smaili/plugins/lsp/init.lua
;;; 2024-11-04
(import-macros {: tc} :hondana-dev.macros)
(tc type string)
(local zero-setup-preferred-preset :recommended)

(tc type "table<string, table<string, fun(): nil>>")
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
(tc type string)
(local llvm-local-binary-path :/opt/homebrew/opt/llvm/bin_remove-me_)
;; change to true if you want the clangd's overthought semantics!
(tc type boolean)
(local allow-clangd-semantics (-> "shitty colors" (type) (not= :string)))

;; WARN: this will be replaced by mason-lspconfig in version 3
(tc type "string[]")
(local preferred-language-servers [;; TODO: restore :ts-ls
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
                                   ;; :zls & :fennel_ls: DON'T USE MASON HERE (see below)
                                   ;; fennel_ls is hard to setup but it looks promising
                                   ])

(tc param names "string[]" return "string[]")
(λ lazy-get-plugin-paths [names]
  {:author :uga-rosa-at-zenn-dev}
  (let [{: plugins} (require :lazy.core.config)
        paths []]
    (each [_ name (ipairs names)]
      (let [plugin (. plugins name)]
        (if plugin
            (table.insert paths (.. plugin.dir :/lua))
            (vim.notify (.. "Invalid plugin name: " name)))))
    paths))

(tc param plugins "string[]" return "string[]")
(λ library [plugins]
  (let [paths (lazy-get-plugin-paths plugins)
        make-libraries #(icollect [_ l (ipairs [$...])]
                          (.. "${3rd}/" l :/library))]
    (each [_ path (ipairs [(.. vim.env.VIMRUNTIME :/lua)
                           (unpack (make-libraries :luv :busted :luassert))])]
      (table.insert paths path))
    paths))

(tc type LazySpec)
(local P ;;
       {1 :neovim/nvim-lspconfig
        ;; doesn't start on the BufNewFile event so load it with the command `:LspStart`
        :event :BufReadPost
        :cmd :LspStart
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
                               :diagnostic {:on_insert false
                                            :on_insert_follow false}
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
                                            (vim.keymap.set mode key fun
                                                            options)))))
                  (local {:util {: root_pattern}
                          : clangd
                          : lua_ls
                          : fennel_ls
                          : zls} (require :lspconfig))
                  ;; check if there's a clangd in your llvm-local-binary-path
                  (let [local-clangd (.. llvm-local-binary-path :/clangd)
                        ; unused now
                        capabilities (vim.lsp.protocol.make_client_capabilities)
                        on_attach #(do
                                     ;; disable formattings (see hondana-dev.plugins.null-ls)
                                     ;; FIXME: broken with the newest LSP?
                                     (set $.server_capabilities.documentFormattingProvider
                                          false)
                                     (set $.server_capabilities.documentRangeFormattingProvider
                                          false)
                                     ;; disable semantics if not allowed
                                     (when (not allow-clangd-semantics)
                                       (set $.server_capabilities.semanticTokensProvider
                                            false)))]
                    ;; NOTE: null-ls will do the clang-format with extra args
                    (set capabilities.offsetEncoding [:utf-16])
                    (set capabilities.general.positionEncodings [:utf-16])
                    (clangd.setup {:cmd [(if (-> local-clangd
                                                 (vim.fn.executable)
                                                 (= 1))
                                             local-clangd
                                             :clangd)]
                                   : on_attach
                                   : capabilities}))
                  ;; NOTE: I need the zls that fits zig's version
                  (when (-> :zls (vim.fn.executable) (= 1))
                    (zls.setup {:cmd [:zls]
                                :filetypes [:zig]
                                :root_dir (root_pattern :build.zig :.git)}))
                  ;; TODO: try folke/lazydev.nvim for a smoother setup
                  (lua_ls.setup {:on_init (fn [client]
                                            (when client.workspace_folders
                                              (let [path (. client.workspace_folders
                                                            1 :name)
                                                    checkfile (fn [...]
                                                                (vim.uv.fs_stat ...))
                                                    json (.. path :/.luarc.json)]
                                                (when (or (checkfile json)
                                                          (checkfile (.. json
                                                                         :c)))
                                                  (lua :return))))
                                            ;; don't put a `.luarc.json` in $HOME
                                            (set client.config.settings.Lua
                                                 (vim.tbl_deep_extend :force
                                                                      client.config.settings.Lua
                                                                      {:runtime {:version :LuaJIT}
                                                                       :diagnostics {:unusedLocalExclude ["_*"]
                                                                                     :disable [:unused-vararg
                                                                                               :deprecated]
                                                                                     :globals [:vim
                                                                                               :love]}
                                                                       :workspace {:checkThirdParty :false
                                                                                   ;; set the lazy plugins you need
                                                                                   ;; avoid `(vim.api.nvim_list_runtime_paths)`
                                                                                   :library (library [:lazy.nvim
                                                                                                      :harpoon])}})))
                                 :settings {:Lua {}}})
                  ;; NOTE: I recommend to install fennel-ls manually (Mason/LuaRocks might have an outdated version)
                  ;; you will need a `flsproject.fnl` file at the root: use `~/.config/nvim/fnl/build-flsproject.sh`
                  (when (-> :fennel-ls (vim.fn.executable) (= 1))
                    ;; NOTE: change root project with `:lcd` if needed
                    (fennel_ls.setup {:root_dir ;; search in the vicinity instead of visiting
                                      ;; the ancestors with root_pattern from nvim-lspconfig
                                      ;; WARN: the Fennel code must have a `fnl` directory root with a `flsproject.fnl`
                                      #(-> [:fnl]
                                                     (vim.fs.find {:upward true
                                                                   :type :directory
                                                                   :path (vim.fn.getcwd)})
                                                     (. 1))}))
                  (lsp-zero.setup))})

P
