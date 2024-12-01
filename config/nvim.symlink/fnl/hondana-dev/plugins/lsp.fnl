;;; LSP setup
;;; table structure by: https://github.com/MuhametSmaili/nvim/blob/main/lua/smaili/plugins/lsp/init.lua
;;; 2024-11-28
(import-macros {: tc : funcall!} :hondana-dev.macros)

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
  (custom-keys {:n {:<leader>f ["LSP Format" vim.lsp.buf.format]
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
                    :<leader>vd ["View diagnostic" vim.diagnostic.open_float]
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
                    :<leader>nn ["LSP Rename" vim.lsp.buf.rename]}
                :i {:<C-h> ["LSP Signature help" vim.lsp.buf.signature_help]}}))

;; I use the Mason clangd but you can use another one; remove _remove-me_
(tc type string)
(local llvm-local-binary-path :/opt/homebrew/opt/llvm/bin_remove-me_)
;; change to true if you want the clangd's overthought semantics!
(tc type boolean)
(local allow-clangd-semantics (-> "shitty colors" (type) (not= :string)))

;;; SERVERS FOR MASON-LSPCONFIG
(tc type "string[]")
(local ensure_installed ;; don't change the const name (check PLUGINS section)
       [:ts_ls
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

;;; SETUP FOR LSPCONFIG
(tc type "fun(self:LazyPlugin, opts:table): nil")
(fn config [_ opts]
  ;; additional settings for diagnostic
  (vim.diagnostic.config opts.diagnostics)
  (local {:util {: root_pattern} : clangd : lua_ls : fennel_ls : zls}
         (require :lspconfig))
  (var {:util {:default_config {: capabilities}}} (require :lspconfig))
  ;; WARN: set this first
  (let [{:default_capabilities defaults} (require :cmp_nvim_lsp)]
    (set capabilities (vim.tbl_deep_extend :force capabilities (defaults))))
  (let [callback (fn [event]
                   (let [fs (keymap-set-fns)]
                     (for [i 1 (length fs)]
                       (let [f (. fs i)]
                         (f event.buf)))))]
    (vim.api.nvim_create_autocmd :LspAttach {:desc "LSP actions" : callback}))
  ;;
  ;; * Clang setup *
  ;; check if there's a clangd in your llvm-local-binary-path
  (let [local-clangd (.. llvm-local-binary-path :/clangd) ; unused now
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
                       (set $.server_capabilities.semanticTokensProvider false)))]
    ;; NOTE: null-ls will do the clang-format with extra args
    (lua "---@diagnostic disable-next-line")
    (set capabilities.offsetEncoding [:utf-16])
    (set capabilities.general.positionEncodings [:utf-16])
    (clangd.setup {:cmd [(if (-> local-clangd
                                 (vim.fn.executable)
                                 (= 1))
                             local-clangd
                             :clangd)]
                   : on_attach
                   : capabilities}))
  ;;
  ;; * Zig setup *
  ;; NOTE: I need the zls that fits zig's version
  (when (-> :zls (vim.fn.executable) (= 1))
    (zls.setup {:cmd [:zls]
                :filetypes [:zig]
                :root_dir (root_pattern :build.zig :.git)}))
  ;;
  ;; * Lua setup *
  ;; TODO: try folke/lazydev.nvim for a smoother setup
  (let [on_init ;;
        (fn [client]
          ;; WARN: don't put a `.luarc.json` in $HOME
          (when client.workspace_folders
            (let [path (. client.workspace_folders 1 :name)
                  checkfile (fn [...]
                              (vim.uv.fs_stat ...))
                  json (.. path :/.luarc.json)]
              (when (or (checkfile json) (checkfile (.. json :c)))
                (lua :return))))
          (let [;; only set the libraries you need for diagnostics
                ;; to avoid `(vim.api.nvim_list_runtime_paths)`
                library (F.library [:lazy.nvim
                                    :nvim-treesitter
                                    ;; :ts-comments.nvim
                                    :plenary.nvim
                                    :nvim-lspconfig
                                    ;; :nvim-nio
                                    ;; :nvim-dap
                                    ;; :lspsaga.nvim
                                    ;; :null-ls.nvim
                                    ;; :mason-null-ls.nvim
                                    ;; :nvim-cmp
                                    ;; :refactoring.nvim
                                    ;; :which-key.nvim
                                    :harpoon
                                    :mini.files
                                    ;; :rainbow-delimiters.nvim
                                    ;; :todo-comments.nvim
                                    ;; :zk-vim
                                    ])
                settings ;; additional settings for Lua
                {:runtime {:version :LuaJIT}
                 :diagnostics {:unusedLocalExclude ["_*"]
                               :disable [:unused-vararg :deprecated]
                               :globals [:vim :love]}
                 :workspace {:checkThirdParty :false : library}}]
            (set client.config.settings.Lua
                 (vim.tbl_deep_extend :force client.config.settings.Lua
                                      settings))))]
    (lua_ls.setup {: on_init :settings {:Lua {}}}))
  ;;
  ;; * Fennel setup *
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
                                     (. 1))})))

;;; PLUGINS (incl. MASON-LSPCONFIG SETUP)
(tc type LazySpec)
(local P ;;
       {1 :neovim/nvim-lspconfig
        ;; doesn't start on the BufNewFile event so load it with the command `:LspStart`
        :event :BufReadPost
        :cmd :LspStart
        :dependencies [;; telescope.builtin will be used to integrate LSP functions to Telescope
                       :nvim-telescope/telescope.nvim
                       {1 :williamboman/mason.nvim
                        ;; check hondana-dev.plugins.null-ls about the other Mason packages
                        :opts {}
                        :cmd :Mason
                        :run ":MasonUpdate"}
                       {1 :williamboman/mason-lspconfig.nvim
                        :config #(let [{:setup mason-setup} (require :mason)
                                       {: setup} (require :mason-lspconfig)]
                                   (mason-setup)
                                   (setup {: ensure_installed}))}
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
        : config})

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
    (each [_ path (ipairs [(.. vim.env.VIMRUNTIME :/lua)
                           (unpack (make-libraries :luv :busted :luassert))])]
      (table.insert paths path))
    paths))

P
