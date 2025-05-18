;;; Mason ensure installed
;;; 2025-05-17 - neovim v0.11+ - mason v2.0
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
                                     :pyright
                                     :bashls]}}
          {1 :WhoIsSethDaniel/mason-tool-installer.nvim
           :dependencies [:mason-org/mason.nvim]
           :opts {:ensure_installed [:stylua
                                     :prettierd
                                     :isort
                                     :black
                                     :pylint
                                     :shfmt]}}])

;; NOTE: Alpine Linux may require a `apk add gcompat` + `:MasonInstall` with a specific target
;; (`--target=linux_arm64_gnu`, `--target=linux_x64_gnu`) for some plugins (`lua_ls`, `stylua`...)

P
