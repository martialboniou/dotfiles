(local {:new picker} (require :telescope.pickers))
(local {: new_async_job} (require :telescope.finders))
(local {: gen_from_vimgrep} (require :telescope.make_entry))
(local {: grep_previewer} (-> :telescope.config (require) (. :values)))

(local M {})

;; live multigrep based on TJ Devries' 2024 advent-of-nvim
;; from https://www.youtube.com/watch?v=xdXE1tOT-qg&t=621s
;; usage: <pattern><2-spaces><glob-pattern>
(fn live-multigrep [?opts]
  (local opts (or ?opts {}))
  (set opts.cwd (or opts.cwd (vim.uv.cwd)))
  (let [;; a little delay before refreshing the search
        debounce 100 ;;
        ;; two spaces for the prompt
        ;; [1]: normal_search, [2]: glob_pattern (ie `*.fnl` or `**/dir/**`)
        split-pattern "  "
        entry_maker (gen_from_vimgrep opts)
        command_generator #(when (and $ (not= "" $))
                             (let [pieces (-> $ (vim.split split-pattern))
                                   args ["rg"]]
                               (each [i o (ipairs [:-e :-g])]
                                 (-> pieces (. i)
                                     (#(when $
                                         (doto args
                                           (table.insert o)
                                           (table.insert $))))))
                               (-> args
                                   (#[$
                                      [:--color=never
                                       :--no-heading
                                       :--with-filename
                                       :--line-number
                                       :--column
                                       :--smart-case]])
                                   (vim.iter)
                                   (: :flatten)
                                   (: :totable))))
        finder (new_async_job {:cwd opts.cwd : entry_maker : command_generator})
        previewer (grep_previewer opts)
        sorter (-> :telescope.sorters (require) (#($.empty)))
        definitions {:prompt_title "Multi Grep"
                     : debounce
                     : finder
                     : previewer
                     : sorter}]
    (-> opts (picker definitions) (: :find))))

(fn M.setup []
  (vim.keymap.set :n :<leader>sm live-multigrep
                  {:desc "Open a multi-grep file picker"}))

M
