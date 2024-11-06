(import-macros {: tc} :hondana-dev.macros)
(local M {})

(tc param n_path string return :nil|string)
(λ M.notebook-root [n-path]
  (let [{: root-pattern} (require :hondana-dev.utils.root-pattern)]
    ((root-pattern :.zk) n-path)))

;; required command: taplo (installed via Mason for example)
(λ M.get-notebook-global-path []
  "extract `notebook.dir` from the `~/.config/zk/config.toml"
  (let [config-file (-> "~/.config/zk/config.toml"
                        (vim.fn.expand))
        output (-> [:taplo :get :-f "\"%s\"" :notebook.dir]
                   (table.concat " ")
                   (string.format config-file)
                   (vim.fn.system))
        dir (vim.fn.expand (output:match "(.+)\n"))]
    (when (-> dir (vim.fn.isdirectory) (= 1))
      ;; check if the global notebook is initialized
      (M.notebook-root dir))))

(tc param ?bufnr number return string)
(fn M.resolve-notebook-path [?bufnr]
  "check buffer, cwd or ~/.config/zk/config.toml:"
  (var path (vim.api.nvim_buf_get_name (or ?bufnr 0)))
  (let [cwd (vim.fn.getcwd 0)]
    (when (= "" path) (set path cwd))
    (when (not (M.notebook-root path))
      (if (M.notebook-root cwd)
          (set path cwd)
          (let [notebook-dir (M.get-notebook-global-path)]
            (when notebook-dir
              (set path notebook-dir)))))
    path))

M
