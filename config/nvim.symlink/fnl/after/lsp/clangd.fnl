;; https://clangd.llvm.org/extensions.html#switch-between-sourceheader
(import-macros {: tc} :hondana-dev.macros)

(tc type string)
(local name :clangd)

(local {: notify : api : cmd :fn {: strchars} :log {: levels} :lsp {: util}}
       vim)

(local {:nvim_buf_create_user_command uc} api)

(tc param bufnr integer)
(tc return vim.lsp.Client? client rpc object)
(fn get-client [bufnr]
  (-> {: bufnr : name} (vim.lsp.get_clients) (. 1)))

(tc param bufnr integer)
(fn switch-source-header [bufnr]
  (local method-name :textDocument/switchSourceHeader)
  (local client (get-client bufnr))
  (if client
      (let [params (util.make_text_document_params bufnr)]
        (client:request method-name params
                        (fn [err res]
                          (when err (error (tostring err)))
                          (if res (cmd.edit (vim.uri_to_fname res))
                              (notify "corresponding file cannot be determined")))
                        bufnr))
      ;; no client
      (-> "method %s is not supported by any servers active on the current buffer"
          (: :format method-name)
          (notify levels.ERROR))))

(tc param node "string?" param name0 string)
(tc return "string?")
(fn format-node [node name]
  (-?>> node
        (: "%s: %s" :format name)))

(tc param result string)
(tc param node_name string)
(tc param _3fname? string)
(fn format-result [result node-name ?name]
  (format-node (?. result 1 node-name) (or ?name node-name)))

(fn symbol-info []
  (local bufnr (api.nvim_get_current_buf))
  (local client (get-client bufnr))
  (if (and client (client:supports_method :textDocument/symbolInfo))
      (let [win (api.nvim_get_current_win)
            params (util.make_position_params win client.offset_encoding)]
        (client:request :textDocument/symbolInfo params
                        (fn [err res]
                          (when (and (not err) (not= 0 (length res)))
                            (local content [(format-result res :name)])
                            (local context
                                   (format-result res :containerName :container))
                            (when context
                              (table.insert content context))
                            (local content-size
                                   (icollect [_ n (ipairs content)]
                                     (strchars n)))
                            (util.open_floating_preview content ""
                                                        {:height (length content)
                                                         :width (-> content-size
                                                                    (unpack)
                                                                    (math.max))
                                                         :focusable false
                                                         :focus false
                                                         :border :single
                                                         :title "Symbol Info"})))
                        bufnr))
      (notify "Clangd client not found" levels.ERROR)))

(fn on_attach []
  (uc 0 :LspClangdSwitchSourceHeader #(switch-source-header 0)
      {:desc "Switch between source/header"})
  (uc 0 :LspClangdShowSymbolInfo symbol-info {:desc "Show symbol info"}))

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
