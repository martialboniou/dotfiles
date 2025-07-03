(local api vim.api)
(local pattern :*)

;; F = utility functions
(local F {:set #(do
                  (api.nvim_set_hl 0 $...)
                  (let [not-reg-exec? (= "" (vim.fn.reg_executing))]
                    (when (and (not vim.o.lazyredraw) not-reg-exec?)
                      (vim.cmd "silent redraw"))
                    nil))
          :get #(api.nvim_get_hl_by_name $ true)
          :au api.nvim_create_autocmd
          :augrp #(api.nvim_create_augroup $ {:clear true})})

;; load this file before you set your colorscheme
(->> #(let [default-culnr-hl (F.get :CursorLineNr)
            group (F.augrp :Hondana_Mode_CursorLineNr)
            callback #(F.set :CursorLineNr default-culnr-hl)
            ;; suppose colors are set by mini.hues
            (ok new-color) (pcall F.get :MiniStatuslineModeInsert)]
        (local inserting-culnr-hl
               (if ok new-color ;;
                   ;; :else
                   ;; invert the original cursorline otherwise
                   (let [{:foreground background :background foreground} default-culnr-hl]
                     {:foreground (or foreground :#181818) : background})))
        (set inserting-culnr-hl.bold true)
        (local inserting-callback #(F.set :CursorLineNr inserting-culnr-hl))
        (F.au :InsertEnter {: pattern :callback inserting-callback : group})
        ;; restore original colors when leaving insert mode
        (F.au :InsertLeave {: pattern : callback : group}))
     (#{: pattern :callback $ :group (F.augrp :Hondana_ColorSchemeChange)})
     (F.au :ColorScheme))
