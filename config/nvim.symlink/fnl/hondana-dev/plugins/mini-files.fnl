(import-macros {: tc} :hondana-dev.macros)

(tc type string)
(local hidden-files-toggle-key :gh)
(tc type "integer,integer")
(local (width-focus width-preview) (values 30 30))

(λ minifiles-open-at-location-or-root []
  (let [{: open : close : get_explorer_state} (require :mini.files)]
    (if (not= nil (get_explorer_state))
        (close) ; close if already open
        (let [buf (vim.api.nvim_buf_get_name 0)
              (ok _) (pcall open buf true)]
          (when (not ok) ; not created yet?
            (print "Mini.files cannot open the current directory; open cwd instead")
            ;; open at the root of the project instead
            (open (vim.uv.cwd) true))))))

(tc type LazySpec)
(local P {1 :echasnovski/mini.files
          :keys [{1 :<leader><leader>
                  ;; open/close at the current file location if possible
                  2 #(minifiles-open-at-location-or-root)
                  :desc "Open mini.files (directory of the current file)"}
                 {1 :<leader>pv
                  2 #(let [{: open} (require :mini.files)]
                       (open (vim.uv.cwd) true))
                  :desc "Open mini.files (cwd)"}]
          :opts {:mappings {:reveal_cwd "@"}
                 :options {:use_as_default_explorer true}
                 :windows {:preview true
                           :width_focus width-focus
                           :width_preview width-preview}}
          :config (λ [_ opts]
                    (-> :mini.files
                        (require)
                        (#($.setup opts)))
                    (var show-dotfiles true)
                    (let [filter-show #true
                          filter-hide (λ [fs-entry]
                                        (not (vim.startswith fs-entry.name ".")))
                          toggle-dotfiles (λ []
                                            (set show-dotfiles
                                                 (not show-dotfiles))
                                            (let [{: refresh} (require :mini-files)
                                                  new-filter (or (and show-dotfiles
                                                                      filter-show)
                                                                 filter-hide)]
                                              (refresh {:content {:filter new-filter}})))
                          {:nvim_create_autocmd au
                           :nvim_create_augroup augroup} vim.api]
                      (let [group (augroup :Hondana_MiniFiles_BufferCreate {})
                            callback #(let [buf-id $.data.buf_id]
                                        (vim.keymap.set :n
                                                        hidden-files-toggle-key
                                                        toggle-dotfiles
                                                        {:buffer buf-id}))]
                        (au :User
                            {: group
                             : callback
                             :pattern :MiniFilesBufferCreate}))
                      (let [group (augroup :Hondana_MiniFiles_WindowUpdate {})
                            callback #(let [win_id $.data.win_id]
                                        (set (. vim.wo win_id :relativenumber)
                                             true))]
                        (au :User
                            {: group
                             : callback
                             :pattern :MiniFilesWindowUpdate}))))})

P
