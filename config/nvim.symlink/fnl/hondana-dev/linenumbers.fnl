(local api vim.api)
(local pattern :*)

;; F = utility functions
(local F {:set #(do
                  (api.nvim_set_hl 0 $...)
                  (let [not-reg-exec? (= "" (vim.fn.reg_executing))]
                    (when (and (not vim.o.lazyredraw) not-reg-exec?)
                      (vim.cmd "silent redraw"))
                    nil))
          ;; :set #(api.nvim_set_hl 0 $...)
          :get #(api.nvim_get_hl_by_name $ true)
          :au api.nvim_create_autocmd
          :augrp #(api.nvim_create_augroup $ {:clear true})})

;; load this file before you set your colorscheme
(->> #(let [orig-cursorline-hl (F.get :CursorLineNr)
            group (F.augrp :Hondana_Mode_CursorLineNr)
            callback #(F.set :CursorLineNr orig-cursorline-hl)
            ;; suppose colors are set by mini.hues
            (ok insert-color) (pcall F.get :MiniStatuslineModeInsert)]
        (local insert-cursorline-hl
               (if ok insert-color ;;
                   ;; :else
                   ;; invert the original cursorline otherwise
                   (let [{:foreground background :background foreground} orig-cursorline-hl]
                     {:foreground (or foreground :background) : background})))
        (set insert-cursorline-hl.bold true)
        (local insert-callback #(F.set :CursorLineNr insert-cursorline-hl))
        (F.au :InsertEnter {: pattern :callback insert-callback : group})
        ;; restore original colors when leaving insert mode
        (F.au :InsertLeave {: pattern : callback : group}))
     (#{: pattern :callback $ :group (F.augrp :Hondana_ColorSchemeChange)})
     (F.au :ColorScheme))
