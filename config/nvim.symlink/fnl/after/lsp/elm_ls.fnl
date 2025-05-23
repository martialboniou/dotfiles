{:filetypes [:elm]
 :cmd [:elm-language-server]
 :root_dir (fn [bufnr on-dir]
             (local {: api : fs} vim)
             (case [(api.nvim_buf_get_option 0 :filetype)
                    (api.nvim_buf_get_name bufnr)]
               (where [f n]
                      (or (= f :elm) (and (= f :json) (n:match "elm%.json$"))))
               (on-dir (fs.root n :elm.json))
               _ (on-dir nil)))
 :init_options {;; :off | :warning | :error
                :elmReviewDiagnostics :off
                :skipInstallPackageConfirmation false
                :diasbleElmDiagnostics false
                :onlyUpdateDiagnosticsOnSave false}
 :capabilities {:offsetEncoding [:utf-8 :utf-16]}}
