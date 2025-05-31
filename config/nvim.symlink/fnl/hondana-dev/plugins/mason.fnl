;;; Mason ensure installed
;;; 2025-05-29 - neovim v0.11+ - mason v2.0
;;
;;  INSTALL YOUR LANGUAGE SERVERS/LINTERS/FORMATTERS WITH `:MasonEnsureInstalled`
;;  (This change makes the nvim-lspconfig dependent plugins such as mason-lspconfig &
;;  mason-tool-installer superfluous)
;;
(import-macros {: tc} :hondana-dev.macros)

(local servers [;; LSP
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
                :vim-language-server
                ;;
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

(local {:api {:nvim_create_user_command uc} : uv} vim)

(fn mason-ensure-installed []
  "Install the recommended servers. Rebuild a target if Linux Alpine."
  (local registry (require :mason-registry))
  (registry.refresh)
  (local filtered-servers
         (icollect [_ server (ipairs servers)]
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
                 :MasonUninstall
                 :MasonUninstallAll]}
          ;; used by `json-lsp` (check `fnl/after/lsp/jsonls.fnl`)
          :b0o/schemastore.nvim])

;; NOTE: Alpine Linux may require a `apk add gcompat`
;; TIP FOR ALPINE:
;; - `apk add g++ libstdc++ cmake unzip gzip wget curl gettext-dev readline-dev`
;; - `apk add clangd clang-extra-tools clang dotnet8-sdk`
;; - `apk add luarocks luajit lua5.1-dev tree-sitter`
;; - `npm i -g node-gyp tree-sitter-cli jsonlint`
;; - `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`
;; - `luamake`
;;   - requirements: `apk add samurai linux-headers libunwind-dev binutils-dev`
;;   - build: `compile/build.sh`
;;   - install: `compile/install.sh` (switch the shebang to `bash`)
;;   - NOTE: remove the `luamake` alias added in rc file when no need
;; - `lua-language-server`
;;   - requirements: `luamake`
;;   - build: `./make.sh`
;;   - install: `LS="$HOME/.local/bin/lua-language-server" echo "#!/bin/bash\nexec \"${LUA_LS_PATH}\" \"$@\"" > "${B}" && chmod 750 "${B}"`
;; - `marksman`
;;   - requirements: `dotnet workload update`
;;   - install: `make install`
;;   - NOTE: the binary available for linux works too (tested on Alpine 3.20 for Aarch64)

P
