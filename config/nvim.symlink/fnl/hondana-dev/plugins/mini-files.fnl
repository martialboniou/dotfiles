(local hidden-files-toggle-key :gh)
(local width-focus 30)
(local width-preview 30)

(λ minifiles-open-at-location-or-root []
  (local mini-files (require :mini.files))
  (if (not= nil (mini-files.get_target_window))
      (mini-files.close) ; close if already open
      (let [buf (vim.api.nvim_buf_get_name 0)
            (ok _) (pcall mini-files.open buf true)]
        (when (not ok) ; not created yet?
          (print "Mini.files cannot open the current directory; open cwd instead")
          ;; open at the root of the project instead
          (mini-files.open (vim.loop.cwd) true)))))

{1 :echasnovski/mini.files
 :keys [{1 :<leader><leader>
         ;; open/close at the current file location if possible
         2 #(minifiles-open-at-location-or-root)
         :desc "Open mini.files (directory of the current file)"}
        {1 :<leader>pv
         2 #(#($.open (vim.loop.cwd) true) (require :mini.files))
         :desc "Open mini.files (cwd)"}]
 :opts {:mappings {:reveal_cwd "@"}
        :options {:use_as_default_explorer true}
        :windows {:preview true
                  :width_focus width-focus
                  :width_preview width-preview}}
 :config (λ [_ opts]
           (#($.setup opts) (require :mini.files))
           (var show-dotfiles true)
           (λ filter-show [_] true)
           (λ filter-hide [fs-entry] (not (vim.startswith fs-entry.name ".")))
           (λ toggle-dotfiles []
             (set show-dotfiles (not show-dotfiles))
             (local new-filter (or (and show-dotfiles filter-show) filter-hide))
             (#($.refresh {:content {:filter new-filter}}) (require :mini.files)))
           (vim.api.nvim_create_autocmd :User
                                        {:callback (λ [args]
                                                     (local buf-id
                                                            args.data.buf_id)
                                                     (vim.keymap.set :n
                                                                     hidden-files-toggle-key
                                                                     toggle-dotfiles
                                                                     {:buffer buf-id}))
                                         :pattern :MiniFilesBufferCreate})
           (vim.api.nvim_create_autocmd :User
                                        {:callback (λ [args]
                                                     (tset (. vim.wo
                                                              args.data.win_id)
                                                           :relativenumber true))
                                         :pattern :MiniFilesWindowUpdate}))}
