{:filetypes [:elixir :eelixir :heex :surface]
 ;; can be added with mason
 :cmd [:elixir-ls]
 ;; 'root_dir' is chosen like this: if two or more directories containing
 ;; `mix.exs` were found when searching directories upward, the second one
 ;; (higher up) is chosen, with the assumption that it is the root of an
 ;; umbrella app. Otherwise the directory containing the single mix.exs that was
 ;; found is chosen.
 :root_dir (fn [bufnr on-dir]
             (local {: api : fs} vim)
             (local [child-or-root-path maybe-umbrella-path]
                    (->> bufnr (api.nvim_buf_get_name)
                         (#{:upward true :path $ :limit 2}) (fs.find [:mix.exs])))
             (-> maybe-umbrella-path (or child-or-root-path) (fs.dirname)
                 (on-dir)))}
