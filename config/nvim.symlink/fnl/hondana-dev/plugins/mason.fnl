;;; Mason ensure installed
;;; 2025-05-29 - neovim v0.11+ - mason v2.0
;;
;;  INSTALL YOUR LANGUAGE SERVERS/LINTERS/FORMATTERS WITH `:MasonEnsureInstalled`
;;  (This change makes the nvim-lspconfig dependent plugins such as mason-lspconfig &
;;  mason-tool-installer superfluous)
;;
(import-macros {: tc} :hondana-dev.macros)

(local apps [;; LSP
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
             ;; (deprecated) :pyright ;; replaced by pyright-extended
             ;; pyright-extended requires ruff as linter & yapf as formatter
             :ruff
             :yapf
             :pyright-extended
             :bash-language-server
             :awk-language-server
             :vim-language-server
             ;;
             ;; OTHERS (linters, formatters...)
             :stylua
             :jq
             ;; taplo = toml toolkit used by some zk functions (see `hondana-dev.utils.zk`)
             :taplo
             :prettierd
             ;; isort/black/pylint: replaced by `@replit/pyright-extend`
             ;; :isort
             ;; :black
             ;; :pylint
             :shfmt
             :markdownlint-cli2
             :markdown-toc
             ;; if you use go, `golines`, `gofumpt` & `goimports-reviser` must
             ;; be installed (see `hondana-dev.plugins.lint`)
             :cmakelint])

(local {:api {:nvim_create_user_command uc} :fn {: stdpath} :fs {: joinpath}}
       vim)

(fn mason-ensure-installed []
  "Install the recommended servers. Rebuild a target if Linux Alpine."
  (local registry (require :mason-registry))
  (registry.refresh)
  (local filtered-servers
         (icollect [_ server (ipairs apps)]
           (let [(ok pkg) (pcall registry.get_package server)]
             (if ok
                 (when (and (not (pkg:is_installed)) (not (pkg:is_installing)))
                   server)
                 (vim.notify (.. server " not found") vim.log.levels.WARN)))))
  (if (. filtered-servers 1)
      (let [C (require :mason.api.command)
            G (require :hondana-dev.utils.globals)
            install #(C.MasonInstall filtered-servers $)]
        (if (and G.posix (= :Linux _G.jit.os))
            (let [U (require :hondana-dev.utils.fns)]
              (fn on-result [data]
                (local opts (if (data:match "%S*Alpine%s")
                                ;; ensure Linux Alpine as a known target
                                {:target (table.concat [:linux
                                                        _G.jit.arch
                                                        :gnu]
                                                       "_")}
                                {}))
                (install opts))

              (fn on-error []
                (vim.notify "Something went wrong in :MasonEnsureInstalled! Contact the code maintainer."
                            vim.log.levels.ERROR))

              (U.spawn-pipe [:uname :-v] on-result on-error))
            ;; :else
            (install)))
      (vim.notify "No new servers to install." :vim.log.levels.INFO)))

(fn config [_ opts]
  (local {: setup} (require :mason))
  (setup opts)
  (uc :MasonEnsureInstalled mason-ensure-installed {}))

(local opts {:registries ["github:mason-org/mason-registry"
                          ;; append my own local registry here
                          (-> :config (stdpath) (joinpath :mason-registry)
                              (#(.. "file:" $)))]})

;;; PLUGINS
(tc type "LazySpec[]")
(local P [{1 :mason-org/mason.nvim
           : opts
           : config
           :cmd [:Mason
                 :MasonEnsureInstalled
                 :MasonUpdate
                 :MasonLog
                 :MasonInstall
                 :MasonUninstall
                 :MasonUninstallAll]}
          ;; used by `json-lsp` (check `fnl/after/lsp/jsonls.fnl`)
          :b0o/schemastore.nvim])

;; NOTE: Alpine Linux may require a `apk add gcompat`
;; TIP FOR ALPINE:
;; - `apk add g++ libstdc++ cmake unzip gzip wget curl gettext-dev readline-dev`
;; - `apk add clangd clang-extra-tools clang llvm-dev dotnet8-sdk`
;; - `apk add luarocks luajit lua5.1-dev tree-sitter yq`
;; - `npm i -g node-gyp tree-sitter-cli jsonlint`
;; - `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`
;; MY EXPERIENCE WITH ALPINE AND MASON:
;; - Mason packages that can be installed WITHOUT THE TARGET (use
;; `:MasonInstall`):
;;   - `jq`
;;   - `taplo`
;;   - `shfmt`
;;   - `marksman`
;;   - `awk-language-server`
;; - install some recommended apps by hand if Mason doesn't work for them:
;;   - `goimports-reviser`
;;     - type: formatter/import sorter
;;     - Alpine Linux 3.22
;;       - version 3.9.0
;;       - requirements: `go >= 1.24`
;;       - install: `go install -v github.com/incu6us/goimports-reviser@latest`
;;     - Alpine Linux 3.21
;;       - version: 3.8.2
;;       - requirements: `go >= 1.23`
;;       - install: `go install -v github.com/incu6us/goimports-reviser/v3@v3.8.2`
;;     - INFO: `golines` can be installed from Mason even on Alpine Linux 3.20
;;     - INFO: `gofumpt` can be installed from Mason on Alpine Linux 3.21
;;   - `lua-language-server` (the Mason version crashes on Alpine Linux 3.22)
;;     - type: LSP
;;     - requirements: `luamake`
;;       - `luamake`
;;         - requirements: `apk add samurai linux-headers libunwind-dev binutils-dev`
;;         - build: `compile/build.sh`
;;         - install: `compile/install.sh` (switch the shebang to `bash`)
;;         - NOTE: remove the `luamake` alias added in rc file when no need
;;     - build: `./make.sh`
;;     - install: `LS="$HOME/.local/bin/lua-language-server" echo "#!/bin/bash\nexec \"${LUA_LS_PATH}\" \"$@\"" > "${B}" && chmod 750 "${B}"`
;;   - older apps known to have been fixed:
;;     - `marksman` (no issue with Mason on Alpine Linux 3.22)
;;       - type: LSP
;;       - requirements: `dotnet workload update`
;;       - install: `make install`
;;       - NOTE: the binary available for linux works too (tested on Alpine Linux
;;       3.21 for Aarch64)

P
