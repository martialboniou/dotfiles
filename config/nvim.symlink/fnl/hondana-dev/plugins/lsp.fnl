;;; LSP setup
;;; table structure by: https://github.com/MuhametSmaili/nvim/blob/main/lua/smaili/plugins/lsp/init.lua
;;; 2024-12-24
(import-macros {: tc : funcall!} :hondana-dev.macros)

(macro set-value! [x ...]
  "returns a sequence of the rest of args as keys and x value"
  (let [o []]
    (each [_ e (ipairs [...])]
      (set (. o (tostring e)) x))
    o))

;; F = utility functions at the end of this module
(local F {})

(macro custom-keys [maps]
  (local o [])
  (each [mode key-cons (pairs maps)]
    (each [key pair (pairs key-cons)]
      ;; only the buffer is not known at comptime
      (table.insert o `(fn [buf#]
                         (vim.keymap.set ,mode ,key ,(. pair 2)
                                         {:remap false
                                          :desc ,(. pair 1)
                                          :buffer buf#})))))
  o)

;;; at comptime:
;; @alias keymapsOptions "table<string, string>"
;; @alias keymapFun "fun(): nil
;; @alias descFunCons "{[1]: string, [2]: keymapFun}"
;; @alias keymapSetFun "fun(integer?)" accepts a buffer as param
;; @cast custom-keys "fun(table<string, table<string, descFunCons>>): keymapSetFun[]"
(tc alias keymapSetFun "fun(integer?)")
(tc return "table<integer, keymapSetFun>")
;; NOTE: function instead of a local variable to ensure the last annotation is well-set
(fn keymap-set-fns []
  (custom-keys {:n {;; REMINDER (don't unquote)
                    ;; :<leader>f ["Format buffer (Conform)"
                    ;;             (. (require :conform) :format
                    ;;                {:async true :lsp_format :fallback})]
                    ;; replaces vim.lsp.buf.definition
                    :gd ["LSP Go to definition (via Telescope)"
                         #(funcall! :telescope.builtin :lsp_definitions)]
                    :gD ["LSP Go to declaration" vim.lsp.buf.declaration]
                    :gI ["LSP Go to implementation (via Telescope)"
                         #(funcall! :telescope.builtin :lsp_implementations)]
                    :K ["LSP Hover" vim.lsp.buf.hover]
                    ;; replaces vim.lsp.buf.workspace_symbol
                    :<leader>vws ["LSP View workspace symbols (via Telescope)"
                                  #(funcall! :telescope.builtin
                                             :lsp_dynamic_workspace_symbols)]
                    :<leader>vdd ["View diagnostic" vim.diagnostic.open_float]
                    "[d" ["Go to previous diagnostic" vim.diagnostic.goto_prev]
                    "]d" ["Go to next diagnostic" vim.diagnostic.goto_next]
                    :<leader>vca ["LSP Code actions" vim.lsp.buf.code_action]
                    ;; replaces vim.lsp.buf.references
                    :<leader>vrr ["LSP References (via Telescope)"
                                  #(funcall! :telescope.builtin :lsp_references)]
                    :<leader>vrn ["LSP Rename" vim.lsp.buf.rename]
                    ;; replace via.lsp.buf.type_definition
                    :<leader>vtd ["LSP View Type Definition (via Telescope)"
                                  #(funcall! :telescope.builtin
                                             :lsp_type_definitions)]
                    ;; replace vim.lsp.buf.document_symbol
                    :<leader>vds ["LSP View Document Symbol (via Telescope)"
                                  #(funcall! :telescope.builtin
                                             :lsp_document_symbols)]
                    ;;; ! for ergonomics: <leader> + ca = vca, rr = vrr, nn, vrn
                    :<leader>ca ["LSP Code actions" vim.lsp.buf.code_action]
                    ;; NOTE: `<leader>r` is the starting key for `hondana-dev.plugins.refactoring`
                    :<leader>rr ["LSP References (via Telescope)"
                                 #(funcall! :telescope.builtin :lsp_references)]
                    :<leader>dd ["View diagnostic" vim.diagnostic.open_float]
                    :<leader>nn ["LSP Rename" vim.lsp.buf.rename]}
                :i {:<C-h> ["LSP Signature help" vim.lsp.buf.signature_help]}}))

;; I use the Mason clangd but you can use another one; remove _remove-me_
(tc type string)
(local llvm-local-binary-path :/opt/homebrew/opt/llvm/bin_remove-me_)

;;; SERVERS FOR MASON-LSPCONFIG
(tc type "table<string,table>")
(local servers {;;; LSP SERVERS
                :ts_ls {}
                :clangd {:cmd [(let [local-clangd (.. llvm-local-binary-path
                                                      :/clangd)]
                                 (if (-> local-clangd (vim.fn.executable) (= 1))
                                     local-clangd
                                     :clangd))]
                         ;; required because `conform` will do the job (was required for `null_ls` too)
                         ;; otherwise: `warning: multiple different client offset_encodings detected...`
                         :capabilities {:offsetEncoding [:utf-16]
                                        :general.positionEncodings [:utf-16]}}
                :lua_ls {;; TODO: try folke/lazydev.nvim for a smoother setup
                         :settings {:Lua {}}
                         :on_init ;;
                         (fn [client]
                                    ;; WARN: don't put a `.luarc.json` in $HOME
                                    (when client.workspace_folders
                                      (let [path (. client.workspace_folders 1
                                                    :name)
                                            checkfile (fn [...]
                                                        (vim.uv.fs_stat ...))
                                            json (.. path :/.luarc.json)]
                                        (when (or (checkfile json)
                                                  (checkfile (.. json :c)))
                                          (lua :return))))
                                    ;;
                                    (let [library (F.library [:lazy.nvim
                                                              :nvim-treesitter
                                                              ;; :ts-comments.nvim
                                                              :plenary.nvim
                                                              :nvim-lspconfig
                                                              ;; :nvim-nio
                                                              ;; :nvim-dap
                                                              ;; :lspsaga.nvim
                                                              :mason.nvim
                                                              :mason-lspconfig.nvim
                                                              :conform.nvim
                                                              :nvim-lint
                                                              ;; :nvim-cmp
                                                              ;; :refactoring.nvim
                                                              ;; :which-key.nvim
                                                              :harpoon
                                                              :mini.files
                                                              ;; :rainbow-delimiters.nvim
                                                              ;; :todo-comments.nvim
                                                              ;; :zk-vim
                                                              ])
                                          ;; only set the libraries you need for diagnostics
                                          ;; to avoid `(vim.api.nvim_list_runtime_paths)`
                                          settings ;; additional settings for Lua
                                          {:runtime {:version :LuaJIT}
                                           :diagnostics {:unusedLocalExclude ["_*"]
                                                         :disable [:unused-vararg
                                                                   :deprecated]
                                                         :globals [:vim :love]}
                                           :workspace {:checkThirdParty :false
                                                       : library}}]
                                      (set client.config.settings.Lua
                                           (vim.tbl_deep_extend :force
                                                                client.config.settings.Lua
                                                                settings))))}
                :rust_analyzer {}
                :html {}
                :jsonls {}
                ;; FIXME: error 6 in markdown (may be in other files)
                :tailwindcss {;; missing default settings leading to an error
                              :settings {:tailwindCSS {:hovers true
                                                       :suggestions true
                                                       :codeActions true}}
                              :init_options {:userLanguages (set-value! :html-eex
                                                                        elixir
                                                                        eelixir
                                                                        heex)}}
                :dockerls {}
                :docker_compose_language_service {}
                :astro {}
                :marksman {}
                :vimls {}
                :cssls {}
                :ocamllsp {}
                :gopls {}
                :elixirls {}
                ;; :zls & :fennel_ls: DON'T USE MASON HERE (see below)
                ;; fennel_ls is hard to setup but it looks promising
                })

;;; MASON'S FULL INSTALL LIST
(tc type "string[]")
(local ensure_installed (or (vim.tbl_keys servers) []))
(vim.list_extend ensure_installed [:stylua
                                   :jq
                                   ;; taplo = toml toolkit used by some zk functions (see hondana-dev.utils)
                                   :taplo
                                   :ocamlformat
                                   :clang-format
                                   :awk-language-server
                                   :markdownlint-cli2
                                   :markdown-toc
                                   :cmakelint
                                   :gofumpt
                                   :goimports-reviser
                                   :golines])

;;; SETUP FOR LSPCONFIG & MASON
(tc type "fun(self:LazyPlugin, opts:table): nil")
(fn config [_ opts]
  ;; additional settings for diagnostic
  (vim.diagnostic.config opts.diagnostics)
  (var capabilities (vim.lsp.protocol.make_client_capabilities))
  ;; WARN: set this first
  (local {:default_capabilities defaults} (require :cmp_nvim_lsp))
  (set capabilities (vim.tbl_deep_extend :force capabilities (defaults)))
  ;;
  ;; 1/4 step: Mason & installs
  ;;
  (let [mason (require :mason)
        lspconfig (require :lspconfig)
        {: setup} (require :mason-tool-installer)
        mason-lspconfig (require :mason-lspconfig)]
    (mason.setup)
    (setup {: ensure_installed})
    ;;
    ;; 2/4 step: add manually-installed servers (non Mason ones; check `servers`)
    ;;
    ;; * Roc *
    ;; NOTE: I disable the syntax highlight from LSP (use Tree-sitter only)
    (when (-> :roc_language_server (vim.fn.executable) (= 1))
      (let [on_init #(set $.server_capabilities.semanticTokensProvider nil)]
        (lspconfig.roc_ls.setup {: on_init : capabilities})))
    ;;
    ;; * Fennel *
    ;; NOTE: I recommend to install fennel-ls manually (Mason/LuaRocks might have an outdated version)
    ;; you will need a `flsproject.fnl` file at the root: use `~/.config/nvim/fnl/build-flsproject.sh`
    (when (-> :fennel-ls (vim.fn.executable) (= 1))
      ;; TIP: change root project with `:lcd` if needed
      (lspconfig.fennel_ls.setup {: capabilities
                                  :root_dir ;; search in the vicinity instead of visiting
                                  ;; the ancestors with root_pattern from nvim-lspconfig
                                  ;; WARN: the Fennel code must have a `fnl` directory root with a `flsproject.fnl`
                                  #(-> [:fnl]
                                                 (vim.fs.find {:upward true
                                                               :type :directory
                                                               :path (vim.fn.getcwd)})
                                                 (. 1))}))
    ;;
    ;; * Zig *
    ;; NOTE: I need a zls that fits the zig's version
    (when (-> :zls (vim.fn.executable) (= 1))
      ;; no autosave b/c slow
      (set vim.g.zig_fmt_autosave 0)
      (lspconfig.zls.setup {: capabilities
                            :cmd [:zls]
                            :filetypes [:zig]
                            :root_dir (lspconfig.util.root_pattern :build.zig)}))
    ;;
    ;; 3/4 step: lspconfig via Mason; the handlers add capabilities for each servers
    ;;
    (local handlers {1 (fn [server-name]
                         (let [server (or (. servers server-name) {})]
                           (set server.capabilities
                                (vim.tbl_deep_extend :force {} capabilities
                                                     (or server.capabilities {})))
                           (-> lspconfig (. server-name) (. :setup)
                               (#($ server)))))})
    (tc diagnostic "disable-next-line: missing-fields")
    (mason-lspconfig.setup {: handlers}))
  ;;
  ;; 4/4 step: LSPAttach's callback to append the keybindings
  ;;
  (let [callback (fn [event]
                   (let [fs (keymap-set-fns)
                         tally (length fs)]
                     (for [i 1 tally]
                       (let [f (. fs i)]
                         (f event.buf)))))]
    (vim.api.nvim_create_autocmd :LspAttach {:desc "LSP actions" : callback})))

;;; PLUGINS
(tc type LazySpec)
(local P ;;
       [{1 :williamboman/mason.nvim :cmd [:Mason] :config true}
        ;; the mason/mason-lspconfig setup is also done by the local function
        ;; `config` above
        :williamboman/mason-lspconfig.nvim
        :WhoIsSethDaniel/mason-tool-installer.nvim
        {1 :neovim/nvim-lspconfig
         ;; doesn't start on the BufNewFile event so load it with the command `:LspStart`
         :event :BufReadPost
         :cmd :LspStart
         :dependencies [;; telescope.builtin will be used to integrate LSP functions to Telescope
                        :nvim-telescope/telescope.nvim
                        ;; see `hondana-dev.plugins.completion`
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
         : config}])

;;; UTILITY FUNCTIONS
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
(λ F.library [plugins]
  (let [paths (lazy-get-plugin-paths plugins)
        make-libraries #(icollect [_ l (ipairs [$...])]
                          (.. "${3rd}/" l :/library))]
    (local addons (make-libraries :luv :busted :luassert))
    ;; next line optional since 0.10 but vim.lsp.LspConfig unknown for some reason
    (local addons [(.. vim.env.VIMRUNTIME :/lua) (unpack addons)])
    ;; HACK: luvit (unsure if good strat)
    (let [{:options {: root}} (require :lazy.core.config)
          luvit-path (-> root (#[$ :luvit-meta :library]) (table.concat "/"))]
      (when (vim.uv.fs_stat luvit-path)
        (table.insert paths luvit-path)))
    ;; 3rd addons
    (each [_ path (ipairs addons)]
      (table.insert paths path))
    paths))

P
