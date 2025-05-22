;;; Mason ensure installed
;;; 2025-05-20 - neovim v0.11+ - mason v2.0
(import-macros {: tc} :hondana-dev.macros)

;;; NOTE: mason.nvim loaded in `lspconfig.fnl`

;;; PLUGINS
(tc type "LazySpec[]")
(local P [{1 :mason-org/mason-lspconfig.nvim
           :opts {:ensure_installed [:html
                                     :cssls
                                     :ts_ls
                                     :eslint
                                     :jsonls
                                     :yamlls
                                     :tailwindcss
                                     :astro
                                     :lua_ls
                                     :html
                                     :graphql
                                     :emmet_language_server
                                     :marksman
                                     :pyright
                                     :bashls]}}
          {1 :WhoIsSethDaniel/mason-tool-installer.nvim
           :dependencies [{1 :mason-org/mason.nvim
                           :opts {}
                           :cmd [:Mason
                                 :MasonUpdate
                                 :MasonLog
                                 :MasonInstall
                                 :MasonUninstall]}
                          :b0o/schemastore.nvim]
           :opts {:ensure_installed [:stylua
                                     :prettierd
                                     :isort
                                     :black
                                     :pylint
                                     :shfmt]}}])

;; NOTE: Alpine Linux may require a `apk add gcompat` + `:MasonInstall` with a specific target
;; (`--target=linux_arm64_gnu`, `--target=linux_x64_gnu`) for some plugins (`lua_ls`, `stylua`...)

P
