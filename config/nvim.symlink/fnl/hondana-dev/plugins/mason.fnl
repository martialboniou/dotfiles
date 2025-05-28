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
        (when (and G.posix (= :Linux _G.jit.os))
          (var handle nil)
          (tc cast handle uv.uv_handle_t)
          (local new uv.new_pipe)
          (local stdio [nil (new) (new)])
          (local [cmd & args] ["uname" "-v"])
          (local options {: args : stdio})
          (local on-exit
                 #(do
                    (for [i 2 3]
                      (let [p (. stdio i)]
                        (when p (uv.read_stop p) (uv.close p))))
                    (uv.close handle)
                    (when (not= 0 $)
                      (vim.schedule #(vim.notify "Something went wrong in :MasonEnsureInstalled! Contact the code maintainer."
                                                 vim.log.levels.ERROR)))))
          (set handle (uv.spawn cmd options on-exit))

          (fn on-read [_ data]
            (when data
              (local opts (if (data:match "%S*Alpine%s")
                              ;; ensure Linux Alpine as a known target
                              {:target (table.concat ["linux"
                                                      _G.jit.arch
                                                      "gnu"]
                                                     "_")}
                              {}))
              (vim.schedule #(install opts))))

          (for [i 2 3]
            (let [p (. stdio i)]
              (when p
                (uv.read_start p on-read))))
          (lua :return))
        (install))
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

;; NOTE: Alpine Linux may require a `apk add gcompat` + `:MasonInstall` with a specific target
;; (`--target=linux_arm64_gnu`, `--target=linux_x64_gnu`) for some plugins (`lua_ls`, `stylua`...)

P
