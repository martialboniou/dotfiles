;; zk +  https://github.com/zk-org/zk-nvim/blob/main/lua/zk/util.lua
(local utils-zk :hondana-dev.utils.zk)
(local zk (require utils-zk))

;; API + zk
(local M {})

;; starter
(fn M.setup []
  "setup with auto attach"
  (let [filetypes [:markdown]
        trigger (if (not filetypes) "BufReadPost *"
                    (.. "FileType " (table.concat filetypes ",")))]
    (zk._lsp_buf_auto_add 0)
    (-> ["autocmd %s lua require'" :utils-zk "'._lsp_buf_auto_add(0)"]
        (table.concat)
        (string.format trigger)
        (vim.api.nvim_command))))

;; https://github.com/zk-org/zk-nvim/blob/main/lua/zk/api.lua
(fn execute-command [cmd cb ?options ?path]
  (zk.lsp:start)
  (let [
        ;; an empty table would be sent as an empty list, which causes ar error on the server
        options (if (and ?options (vim.tbl_isempty ?options)) nil ?options)
        client (zk.lsp:client)
        bufnr 0
        command (.. :zk. cmd)
        arguments [(or ?path (zk.resolve-notebook-path bufnr)) options]]
    (client.request :workspace/executeCommand {: command : arguments} cb bufnr)))

(fn M.list [cb ?options ?path]
  (execute-command :list cb ?options ?path))

;; if not in a Zk notebook (ie in an orphan markdown file), open the global
;; notebook according to the link match using your ~/.config/zk/config.toml
;; setup
;; NOTE: the link must be between at least one level of square brackets inside
;;       any markdown file (eg. [link])
(fn find-project-root [startpath root-subdirectory]
  (each [dir (vim.fs.parents startpath)]
    (when (->> root-subdirectory
               (vim.fs.joinpath dir)
               (vim.fn.isdirectory)
               (= 1))
      (lua "return dir"))))

;; zk.pickers.telescope = ui if mentionned
(local ui {})

(fn ui.create-note-entry-maker [_]
  #(let [title (or $.title $.path)]
     {:value $ :path $.absPath :display title :ordinal title}))

(fn ui.make-note-previewer []
  (let [previewers (require :telescope.previewers)
        config (require :telescope.config)
        conf config.values
        define_preview (fn [self entry]
                         (let [value entry.value
                               bufname (or value.title value.path)
                               winid self.state.winid]
                           (conf.buffer_previewer_maker value.absPath
                                                        self.state.bufnr
                                                        {: bufname : winid})))]
    (previewers.new_buffer_previewer {: define_preview})))

(fn ui.show-note-picker [cb notes options]
  (let [pickers (require :telescope.pickers)
        finders (require :telescope.finders)
        actions (require :telescope.actions)
        action-state (require :telescope.actions.state)
        action-utils (require :telescope.actions.utils)
        conf (. (require :telescope.config) :values)
        action-fn (fn [prompt-bufnr]
                    (λ []
                      ;; $ = prompt-bufnr later
                      (let [v (. (action-state.get_selected_entry) :value)]
                        (if options.multi-select
                            (do
                              (var selection [])
                              (action-utils.map_selections prompt-bufnr
                                                           (fn [entry _]
                                                             (table.insert selection
                                                                           entry.value)))
                              (when (vim.tbl_isempty selection)
                                (set selection [v]))
                              (actions.close prompt-bufnr)
                              (cb selection))
                            (do
                              (actions.close prompt-bufnr)
                              (cb v))))))
        attach_mappings ;; @field attach_mappings function
        (fn [prompt-bufnr]
          (actions.select_default:replace (action-fn prompt-bufnr))
          true)
        options (vim.tbl_extend :force {:title "Zk Notes"} (or options {}))
        ;; telescope_options, finders.new_table, sorter, previewer, attach_mappings 
        picker (pickers.new {:prompt_title options.title}
                            {:finder (finders.new_table {:results notes
                                                         :entry_maker (ui.create-note-entry-maker options)})
                             :sorter (conf.file_sorter options)
                             :previewer (ui.make-note-previewer)
                             : attach_mappings})]
    (picker:find)))

;; (fn M.pick-notes [cb picker-options ?options]
;;   (let [options (vim.tbl_extend :force {:select [:title :absPath :path]}
;;                                 (or ?options {}))]
;;     (M.list (fn [err notes]
;;               (assert (not err) (tostring err))
;;               (ui.show-note-picker cb notes picker-options))
;;             options options.notebook_path)))

;; (fn M.edit [picker-options options]
;;   (M.pick-notes (fn [notes]
;;                   (let [single-select? (and picker-options
;;                                             (= false
;;                                                picker-options.multi-select))
;;                         notes (if single-select? [notes] notes)]
;;                     (each [_ note (ipairs notes)]
;;                       (-> [:e note.absPath] (table.concat " ") (vim.cmd)))))
;;                 picker-options options))

(fn M.edit [picker-options options]
  ;; zk.edit + zk.pick_notes
  (let [options (vim.tbl_extend :force {:select [:title :absPath :path]}
                                options)]
    (M.list (fn [err notes]
              (assert (not err) (tostring err))
              (let [single-select? (and picker-options
                                        (= false picker-options.multi-select))
                    notes (if single-select? [notes] notes)]
                ;; use telescope here; REMINDER: picker-options contains :title
                (ui.show-note-picker #(do
                                        (each [_ note (ipairs $)]
                                          (-> [:e note.absPath]
                                              (table.concat " ")
                                              (vim.cmd))))
                                     notes picker-options))) ;; 
            options options.notebook_path)))

(fn M.back-links [?options]
  (let [;; referencee?
        title "Zk Backlinks"
        linkTo [(vim.api.nvim_buf_get_name 0)]
        o (vim.tbl_extend :force {: linkTo} (or ?options {}))]
    (M.edit {: title :multi-select true} o)))

(fn M.links [?options]
  (let [title "Zk Links"
        linkedBy [(vim.api.nvim_buf_get_name 0)]
        o (vim.tbl_extend :force {: linkedBy} (or ?options {}))]
    (M.edit {: title :multi-select true} o)))

;; TODO: check https://github.com/stsewd/tree-sitter-comment
;; check https://neovim.discourse.group/t/commentstring-for-terraform-files-not-set/4066/2
;(let [lspconfig (require :lspconfig)
;      configs (require :lspconfig.configs)
;      zkconf {:default_config {:filetypes [:markdown]}}] ; (tset configs :zk zkconf)
;  ;; lspconfig.zk
;  (when (= nil (L.external-client :zk))
;    (lspconfig.zk.setup {:settings {:zk zkconf}}))
;  (let [bufpath (vim.api.nvim_buf_get_name 0)
;        root (find-project-root bufpath :.zk)]
;    (vim.lsp.buf_request 0 :workspace/executeCommand
;                         {:command :zk.list :arguments [root {:select :path}]}
;                         (fn [err result]
;                           (assert (not err) (tostring err))
;                           (when result
;                             (let [paths (vim.tbl_map #($.path) result)]
;                               (vim.ui.select paths []
;                                              #(-> [:edit $] (table.concat " ")
;                                                   (vim.cmd))))))))
;  ;; (let [zklist-cmd (.  (. configs.zk.commands :ZkList) 1)] zklist-cmd) ; (((. (. configs.zk.commands :ZkList) 1)  {:match :notes})) ; configs.zk.list
;  )

M
