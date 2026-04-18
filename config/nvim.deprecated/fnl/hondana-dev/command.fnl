(local {:nvim_create_user_command uc
        :nvim_buf_create_user_command buc
        :nvim_create_autocmd au
        :nvim_create_augroup augroup} vim.api)

(local opts {})

;;; LEGACY FUNCTIONS

;; cycle :cnext and others (used by `<C-j>`/`<C-k>`; check `hondana-dev.remap`)
;; TODO: luaify this!
(let [commands ["command! Cnext try | cnext | catch | cfirst | catch | endtry"
                "cabbrev cnext Cnext"
                "command! Cprev try | cprev | catch | clast | catch | endtry"
                "cabbrev cprev Cprev"
                "command! Lnext try | lnext | catch | lfirst | catch | endtry"
                "cabbrev lnext Lnext"
                "command! Lprev try | lprev | catch | llast | catch | endtry"
                "cabbrev lprev Lprev"]]
  (for [i 1 (length commands)]
    (-> commands (. i) (vim.cmd))))

;;; UTILITY FUNCTIONS

;; print a C #include guard at current the cursor position
(λ include-guard-scheme []
  (when (= (vim.fn.expand "%p") "")
    (error "Empty filename (save file and try again)"))
  ;; vimscript's toupper() is unicode (might not be a problem here: "a-z\.")
  (let [t #(-> $ (vim.fn.expand) (vim.fn.toupper))
        ext (t "%:t:e")
        guard (.. (t "%:t:r") "_" (if (= "" ext) "" (.. ext "_")))]
    (each [_ cmd (ipairs [(.. :O "#ifndef " guard)
                          (.. :o "#define " guard)
                          :o
                          (.. :o "#endif // " guard)
                          :k])]
      (vim.cmd.normal cmd))))

;; toggle the executability of the current file
(λ toggle-exec []
  (let [{: toggle-executable} (require :hondana-dev.utils)
        (ok res) (pcall toggle-executable)
        {:INFO info :ERROR err} vim.log.levels]
    (-> ok
        (#(if $ (values "Success:" info)
              (values "Error: toggle-executable in hondana-dev.remap:" err)))
        (#(let [(msg level) $...]
            (vim.notify (.. msg " " res) level))))))

;;; COMMANDS

;; toggle the current file as executable
(uc :ToggleExec toggle-exec opts)

;; imprint the current filename at the cursor position
(uc :ImprintFilename ":put =expand('%:t')" opts)

;; fake LspInfo when no plugin `neovim/nvim-lspconfig`
(uc :LspInfo (fn [args]
               (-?> {:bufnr 0} (vim.lsp.get_clients)
                    (#(if (not= 0 (length $)) $
                          (vim.notify "No servers" vim.log.levels.INFO)))
                    (#(if args.bang
                          (vim.inspect $)
                          (let [msg (if (> (length $) 1)
                                        {:pre "First active"
                                         :post " (check LspInfo! for more details)"}
                                        {:pre "Active" :post ""})]
                            (-> (. $ 1 :name)
                                (#(.. msg.pre
                                      " language server in this buffer: " $
                                      msg.post)))))) (print)))
    {:desc "Get the current buffer clients" :bang true})

;; toggle a checkbox (eg. for a list in a buffer; `gt` key in a markdown/org buffer)
;; => check hondana-dev.plugins.operators

;; imprint a C #include guard at the current cursor position
(let [group (augroup "Hondana_C_IncludeGuard" {:clear true})
      callback #(buc 0 :ImprintCHeader include-guard-scheme opts)
      pattern [:cpp :h :c]]
  (au :FileType {: group : callback : pattern}))

;; delete line comments (first version)
(let [group (augroup "Hondana_DeleteLineComments" {:clear true})
      delete-line-comments #(buc 0 :DeleteLineComments
                                 (.. "g,^\\s*" (or $ :#) ",d") opts)]
  ;; useful to get rid of the extra documentation inside the build.zig* files
  ;; built by a `zig init` command
  (au :FileType {: group
                 :callback #(delete-line-comments "//")
                 :pattern [:zig :rust]})
  ;; NOTE: make a generic Tree-sitter version of this (especially for blocks as
  ;; in C, OCaml... even if these shortcuts can be useful for larger files
  ;; where TS might not work)
  (au :FileType
      {: group
       :callback #(delete-line-comments)
       :pattern [:roc :nix :zsh :bash :sh :python :yaml]})
  (au :FileType {: group
                 :callback #(delete-line-comments "--")
                 :pattern [:unison :haskell :lua :elm]})
  (au :FileType {: group
                 :callback #(delete-line-comments ";")
                 :pattern [:lisp :fennel :scheme :clojure]})
  (au :FileType {: group
                 :callback #(delete-line-comments "\\\\\\\\")
                 :pattern [:shen]}))
