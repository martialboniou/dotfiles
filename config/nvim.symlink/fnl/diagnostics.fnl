(import-macros {: tc} :hondana-dev.macros)
;; some ideas:
;; - https://github.com/artemave/workspace-diagnostics.nvim

(when (-> _G.vim (not))
  (error "you cannot execute this without nvim "))

(tc path string?)
(local path (-?> "git rev-parse --show-toplevel" (_G.vim.fn.systemlist)
                 (. 1)))

(when (not path)
  (error "you must be in a git project; darcs not yet supported"))

(tc path "nil|string[]")

(local files (-?> path
                  (#[:fd
                     "'\\.lua$'"
                     :--no-ignore
                     :--exclude
                     :fennel.lua
                     :--exclude
                     :.git
                     path])
                  (table.concat " ")
                  (_G.vim.fn.system)
                  (_G.vim.fn.split "\n")))

;; (local files (-?> path
;;                   (#["git ls-files --full-name" $])
;;                   (table.concat " ")
;;                   (_G.vim.fn.system)
;;                   (_G.vim.fn.split "\n")))

(when (not files)
  (print "nothing to process")
  (lua "os.exit()"))

(local loaded-clients [])

(fn get-files []
  (let [readables (vim.tbl_filter #(-> $ (vim.fn.filereadable) (= 1)) files)]
    (vim.tbl_map #(-> $ (vim.fn.fnamemodify ":p")) readables)))

(tc param client vim.lsp.Client)
(fn _populate [client]
  (when (not (_G.vim.tbl_contains loaded-clients client.id))
    (table.insert loaded-clients client.id)
    (when (and (vim.tbl_get client.server_capabilities :textDocumentSync
                            :openClose)
               (vim.tbl_get client.config :capabilities :textDocument
                            :publicDiagnostics))
      (if (vim.tbl_get client.config :filetypes)
          (each [_ file (ipairs (get-files files))]
            (-> #(let [uri (_G.vim.uri_from_fname file)
                       text (-> file (_G.vim.fn.readfile "\n") (_G.vim.fn.join))]
                   (client.notify :textDocument/didOpen
                                  {:textDocument {: uri
                                                  : text
                                                  :version 0
                                                  :languageId :lua}})))
            (_G.vim.defer_fn 0)))
      ;; :else
      (let [msg (-> client.name
                    (#(.. ["[test-diagnostics]"] $
                          " is skipped: please define `config.filetypes` when setting up the client")))]
        (-> msg (#[$ :WarningMsg]) (#[$ true []]) (vim.api.nvim_echo))))))

(each [_ file (ipairs (get-files files))]
  (vim.cmd (.. "e " file)))

;; (vim.cmd "runtime! filetype.lua")

(let [cmd (-?> :config
               (vim.fn.stdpath)
               (.. :/init.lua)
               (#[:source $])
               (table.concat " "))]
  (if cmd
      (do
        (vim.cmd cmd)
        (vim.cmd "Trouble diagnostics"))
      ;; :else
      (error "no init.lua (required!)")))
