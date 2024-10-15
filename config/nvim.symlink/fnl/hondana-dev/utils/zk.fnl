(local Lsp (require :hondana-dev.utils.lsp))
(local root-pattern (require :hondana-dev.utils.root-pattern))

(local M {})

;; make one instance from the generic hondana-dev.utils.Lsp class
;; TODO: remove name-root; here for debugging
(set M.lsp (let [name-root :zk
                 name (.. name-root :-mini)
                 cmd [name-root :lsp]]
             (Lsp:new name {: name : cmd})))

(fn M._lsp_buf_auto_add [bufnr]
  (when (and (not= :nofile (vim.api.nvim_buf_get_option bufnr :buftype))
             (M.notebook-root (vim.api.nvim_buf_get_name bufnr)))
    (M.lsp:buf_add bufnr)))

(fn M.notebook-root [n-path]
  ((root-pattern.root-pattern :.zk) n-path))

;; required command: taplo (installed via Mason for example)
(fn M.get-notebook-global-path []
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
