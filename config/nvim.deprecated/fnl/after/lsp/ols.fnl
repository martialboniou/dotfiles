;; language server for odin
;; this project is still in early development
(local root_dir (let [{: root-dir} (require :hondana-dev.utils.fns)]
                  (root-dir :ols.json :.git "*.odin")))

{:filetypes [:odin] :cmd [:ols] : root_dir}
