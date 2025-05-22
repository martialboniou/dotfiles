;; https://clangd.llvm.org/extensions.html#switch-between-sourceheader
(local name :clangd)

(local {:nvim_buf_create_user_command uc} vim.api)

(fn switch-source-header [bufnr]
  (local method-name :textDocument/switchSourceHeader)
  (local client (-> {: bufnr : name} vim.lsp.get_clients (. 1)))
  (when client
    (local params (vim.lsp.util.make_text_document_params bufnr))
    (client.request method-name params
                    (fn [err result]
                      (when err (error (tostring err)))
                      (if result (vim.uri_to_fname result)
                          (vim.notify "corresponding file cannot be determined")))
                    bufnr)
    (lua :return))
  ;; no client
  (-> "method %s is not supported by any servers active on the current buffer"
      (: :format method-name)
      (vim.notify vim.log.levels.ERROR)))

(fn on_attach []
  (uc 0 :LspClangdSwitchSourceHeader #(switch-source-header 0)
      {:desc "Switch between source/header"}))

{:filetypes [:c :cpp :objc :objcpp :cuda :proto]
 :cmd [name :--background-index]
 :root_markers [:.clangd
                :.clang-format
                :.clang-tidy
                :compile_commands.json
                :compile_flags.txt
                :configure.ac
                :.git]
 :capabilities {:textDocument {:completion {:editsNearCursor true}}
                :offsetEncoding [:utf-8 :utf-16]}
 : on_attach}
