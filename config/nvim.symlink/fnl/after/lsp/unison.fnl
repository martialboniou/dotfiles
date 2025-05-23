;; a friendly programming language from the future: https://www.unison-lang.org
{:filetype [:unison]
 :cmd [:nc :localhost (or (os.getenv :UNISON_LSP_PORT) :5757)]
 :root_dir (fn [bufnr on-dir]
             (-?> :hondana-dev.utils.root-pattern (require) (. :root-pattern)
                  (#(($ "*.u") (vim.api.nvim_buf_get_name bufnr))) (on-dir)))
 :settings {}}
