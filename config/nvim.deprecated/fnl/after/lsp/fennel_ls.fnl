;; since 0.11, need to import this recipe provided by the removed plugin `nvim-lspconfig`
{:filetypes [:fennel]
 :cmd [:fennel-ls]
 :root_dir (fn [bufnr on-dir]
             (local {: uv : fs : api} vim)

             (fn has-fls-project-cfg [path]
               (-?> path (fs.joinpath :flsproject.fnl) (uv.fs_stat) (. :type)
                    (= :file)))

             (-> bufnr (api.nvim_buf_get_name) (fs.parents) (vim.iter)
                 (: :find has-fls-project-cfg) (or (fs.root 0 :.git)) (on-dir)))
 :settings {}}
