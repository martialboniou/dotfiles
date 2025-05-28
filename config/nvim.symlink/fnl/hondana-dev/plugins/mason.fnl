;;; Mason ensure installed
;;; 2025-05-28 - neovim v0.11+ - mason v2.0
;;
;;  INSTALL YOUR LANGUAGE SERVERS/LINTERS/FORMATTERS WITH `:MasonEnsureInstalled`
;;  (This change makes the nvim-lspconfig dependent plugins such as mason-lspconfig &
;;  mason-tool-installer superfluous)
;;
(import-macros {: tc} :hondana-dev.macros)

(local {:nvim_create_user_command uc} vim.api)

(local M {})

(fn mason-ensure-installed []
  (local registry (require :mason-registry))
  (registry.refresh)
  (local filtered-servers
         (icollect [_ server (ipairs M.servers)]
           (let [(ok pkg) (pcall registry.get_package server)]
             (if ok
                 (when (and (not (pkg:is_installed)) (not (pkg:is_installing)))
                   server)
                 (vim.notify (.. server " not found") vim.log.levels.WARN)))))
  (when (. filtered-servers 1)
    (let [{:MasonInstall install} (require :mason.api.command)]
      (install filtered-servers))))

(set M.servers [;; LSP
                :html-lsp
                :css-lsp
                :typescript-language-server
                :eslint-lsp
                :json-lsp
                :yaml-language-server
                :tailwindcss-language-server
                :astro-language-server
                :lua-language-server
                :html-lsp
                :graphql-language-service-cli
                :emmet-language-server
                :marksman
                :pyright
                :bash-language-server
                :awk-language-server
                ;; OTHERS (linters, formatters...)
                :stylua
                :jq
                ;; taplo = toml toolkit used by some zk functions (see hondana-dev.utils)
                :taplo
                :prettierd
                :isort
                :black
                :pylint
                :shfmt
                :markdownlint-cli2
                :markdown-toc
                :cmakelint])

(fn config [_ opts]
  (local {: setup} (require :mason))
  (setup opts)
  (uc :MasonEnsureInstalled mason-ensure-installed {}))

;;; PLUGINS
(tc type "LazySpec[]")
(local P [{1 :mason-org/mason.nvim
           ;;  :opts {}
           : config
           :cmd [:Mason
                 :MasonEnsureInstalled
                 :MasonUpdate
                 :MasonLog
                 :MasonInstall
                 :MasonUninstall]}
          ;; used by `json-lsp` (check `fnl/after/lsp/jsonls.fnl`)
          :b0o/schemastore.nvim])

;; NOTE: Alpine Linux may require a `apk add gcompat` + `:MasonInstall` with a specific target
;; (`--target=linux_arm64_gnu`, `--target=linux_x64_gnu`) for some plugins (`lua_ls`, `stylua`...)

P
