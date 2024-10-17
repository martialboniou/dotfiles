;; Zettelkasten is a note taking tool
;; - notes are written in markdown
;; - a notebook has a `.zk` directory at the root (check `zk init`)
;; - a global notebook has been defined in `~/.config/zk/config.toml` (change at will)
(macro make-lazy-zk-keys []
  (let [keys [[:<leader>zn "<Cmd>ZkNew {title = vim.fn.input('Title: ')}<CR>"]
              [:<leader>zo "<Cmd>ZkNotes {sort = { 'modified' }}<CR>"]
              [:<leader>zt :<Cmd>ZkTags<CR>]
              [:<leader>zf
               "<Cmd>ZkNotes {sort = { 'modified' }, match = { vim.fn.input('Search: ') } }<CR>"]
              {1 :<leader>zf 2 ":'<,'>ZkMatch<CR>" :mode :v}]]
    (each [_ key (ipairs keys)]
      (each [k v (pairs {:noremap true :silent false})]
        (tset key k v)))
    keys))

(fn make-zk-keys-for-notebook []
  "Add the key mappings only for markdown files in a zk notebook"
  (let [U (require :zk.util)]
    ;; ensure there's a .zk at the root
    (when (-> "%:p"
              (vim.fn.expand)
              (U.notebook_root)
              (not= nil))
      (let [map #(vim.api.nvim_buf_set_keymap 0 $...)
            opts {:noremap true :silent false}]
        ;; open link under the caret
        (map :n :<CR> "<Cmd>lua vim.lsp.buf.definition()<CR>" opts)
        ;; overrides `<leader>zn` + znt/znc for visual ZkNewFromContentSelection
        (map :n :<leader>zn
             "<Cmd>ZkNew { dir = vim.fn.expand('%:p:h'), title = vim.fn.input('Title: ') }<CR>"
             opts)
        (map :v :<leader>znt
             ":'<,'>ZkNewFromTitleSelection { dir = vim.fn.expand('%:p:h') }<CR>"
             opts)
        (map :v :<leader>znc
             ":'<,'>ZkNewFromTitleSelection { dir = vim.fn.expand('%:p:h'), title = vim.fn.input('Title: ') }<CR>"
             opts)
        ;;  open notes (back)linked by the current buffer
        (map :n :<leader>zb :<Cmd>ZkBacklinks<CR> opts)
        ;;  alternate solution using LSP
        ;;  (map :n  :<leader>zb "<Cmd>lua vim.lsp.buf.references()<CR>" opts)
        (map :n :<leader>zl :<Cmd>ZkLinks<CR> opts)
        ;;  preview a linked note
        (map :n :K "<Cmd>lua vim.lsp.buf.hover()<CR>" opts)
        ;;  special code action for visual (<leader>ca is used for LSP's marksman)
        (map :v :<leader>za ":'<,'>lua vim.lsp.buf.range_code_action()<CR>"
             opts)))))

(let [keys (make-lazy-zk-keys)]
  {1 :zk-org/zk-nvim
   :event :VeryLazy
   : keys
   :opts {:picker :telescope}
   :config #(let [zk (require :zk)] (zk.setup $2))
   :init #(let [zk (require :zk)
                commands (require :zk.commands)
                group (vim.api.nvim_create_augroup :Hondana_AfterFtpluginMarkdown
                                                   {})
                ;; callback make-zk-keys-for-notebook
                callback make-zk-keys-for-notebook
                pattern :markdown]
            ;; replace ZkNotes to aim for your global notebook via `~/.config/zk/config.toml`
            (commands.del :ZkNotes)
            (commands.add :ZkNotes
                          #(let [rp (require :hondana-dev.utils.root-pattern)
                                 notebook? (rp.find-project-root (vim.fn.getcwd 0)
                                                                 :.zk)
                                 path (when (not notebook?)
                                        (let [utils (require :hondana-dev.utils.zk)]
                                          (utils.get-notebook-global-path)))]
                             (zk.edit (if (not path) $
                                          ;; open your global notebook if no other choices
                                          (vim.tbl_extend :force
                                                          {:notebook_path path}
                                                          (or $ {})))
                                      {:title "Zk Notes"})))
            ;; specific keys for after/plugin/markdown
            (vim.api.nvim_create_autocmd :FileType
                                         {: callback : group : pattern}))})

;;; examples of telescope usage
;; :Telescope zk notes createdAfter=3\ days\ ago
;; :Telescope zk tags created=today