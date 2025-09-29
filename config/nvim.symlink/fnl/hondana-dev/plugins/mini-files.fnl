(import-macros {: tc} :hondana-dev.macros)
(import-macros {: make-lazykeys!} :hondana-dev.macros.vim)

(tc type string)
(local hidden-files-toggle-key :gh)
(tc type "integer,integer")
(local (width-focus width-preview) (values 30 30))

(tc param ?skip boolean "true to switch to go to `cwd`")
(fn minifiles-open-at-location-or-root [?skip]
  (let [{: open : close : get_explorer_state} (require :mini.files)]
    (if (not= nil (get_explorer_state))
        (close) ; close if already open
        (do
          (when (not ?skip)
            (let [buf (vim.api.nvim_buf_get_name 0)
                  (ok _) (pcall open buf true)]
              (when ok (lua "return"))))
          (when (not ?skip)
            ;; something went wrong!
            (vim.notify "Mini.files cannot open the current directory; open cwd instead"
                        vim.log.levels.INFO))
          ;; open at the root of the project if skipped or buffer has no path
          (open (vim.uv.cwd) true)))))

;;; CONFIG
(tc type "fun(self:LazyPlugin, opts:table):nil")
(fn config [_ opts]
  (-> :mini.files
      (require)
      (#($.setup opts)))
  (var show-dotfiles true)
  (let [filter-show #true
        filter-hide (λ [fs-entry]
                      (not (vim.startswith fs-entry.name ".")))
        toggle-dotfiles (λ []
                          (set show-dotfiles (not show-dotfiles))
                          (let [{: refresh} (require :mini-files)
                                new-filter (or (and show-dotfiles filter-show)
                                               filter-hide)]
                            (refresh {:content {:filter new-filter}})))
        {:nvim_create_autocmd au :nvim_create_augroup augroup} vim.api]
    (let [group (augroup :Hondana_MiniFiles_BufferCreate {})
          callback #(let [buf-id $.data.buf_id]
                      (vim.keymap.set :n hidden-files-toggle-key
                                      toggle-dotfiles {:buffer buf-id}))]
      (au :User {: group : callback :pattern :MiniFilesBufferCreate}))
    (let [group (augroup :Hondana_MiniFiles_WindowUpdate {})
          callback #(let [win_id $.data.win_id]
                      (set (. vim.wo win_id :relativenumber) true))]
      (au :User {: group : callback :pattern :MiniFilesWindowUpdate}))))

;;; PLUGIN
(tc type LazySpec)
(local P {1 :echasnovski/mini.files
          :keys (make-lazykeys! [[:pf
                                  minifiles-open-at-location-or-root
                                  "Open mini.files (directory of the current file)"]
                                 [:pv
                                  #(minifiles-open-at-location-or-root true)
                                  "Open mini.files (cwd)"]])
          :opts {:mappings {:reveal_cwd "@"}
                 :options {:use_as_default_explorer true}
                 :windows {:preview true
                           :width_focus width-focus
                           :width_preview width-preview}}
          : config})

P
