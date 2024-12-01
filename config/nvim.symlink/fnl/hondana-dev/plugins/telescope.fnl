(import-macros {: tc} :hondana-dev.macros)

(macro make-keymaps [keymaps]
  (let [out# []
        table?# #(-> $ (type) (= :table))
        length?# (fn [tbl size]
                   (and (table?# tbl) (= size (length tbl))))
        min-length?# (fn [tbl size]
                       (and (table?# tbl) (<= size (length tbl))))]
    (each [_ keymap# (ipairs keymaps)]
      (assert (length?# keymap# 3) "each element must be a table of 3")
      (var keys# (. keymap# 1))
      (when (= :string (type keys#)) (set keys# [keys#]))
      (assert (min-length?# keys# 1) "each key must be in a sequence")
      (for [i# 1 (length keys#)]
        (var key# (. keys# i#))
        ;; append <leader> when required
        (when (not (key#:match "^<")) (set key# (.. :<leader> key#)))
        (table.insert out# `{1 ,key# 2 ,(. keymap# 2) :desc ,(. keymap# 3)})))
    out#))

(tc type LazySpec)
(local P ;;
       {1 :nvim-telescope/telescope.nvim
        :dependencies [:nvim-lua/plenary.nvim
                       {1 :nvim-telescope/telescope-fzf-native.nvim
                        :build :make
                        :cond #(-> :make (vim.fn.executable) (= 1))}
                       :nvim-telescope/telescope-ui-select.nvim]
        :config #(let [{: load_extension : setup} (require :telescope)
                       {: get_dropdown} (require :telescope.themes)]
                   (setup {:extensions {:ui-select [(get_dropdown)]}})
                   (pcall load_extension :fzf)
                   (pcall load_extension :ui-select))
        :keys (let [{: find_files
                     : git_files
                     : help_tags
                     : keymaps
                     : builtin
                     : grep_string
                     : live_grep
                     : diagnostics
                     : resume
                     : oldfiles
                     : buffers
                     : current_buffer_fuzzy_find} (require :telescope.builtin)]
                (make-keymaps [;;; PROJECT KEYMAPS
                               ;; rg-filtered files (memo: project search)
                               [:ps
                                #(grep_string {:search (vim.fn.input "Grep > ")})
                                "Open a string-filtered file picker (cwd)"]
                               ;; files in git (memo: control project)
                               [:<C-p>
                                #(let [(ok _) (pcall git_files)]
                                   (when (not ok)
                                     (vim.notify "Not in a git project; use `<leader>cd` to reset your working directory"
                                                 vim.log.levels.WARN)))
                                "Open a git repo fuzzy file picker"]
                               ;; recent files (memo: view visited; vr = BAD ERGO)
                               [:vv oldfiles "Open a fuzzy recent file picker"]
                               ;; helptags (memo: view helptags; search helptags)
                               [[:vh :sh] help_tags "Open a help tag picker"]
                               ;; buffers (memo: view buffers + bb as alias; search other buffers)
                               [[:vb :bb :sb]
                                #(buffers {:ignore_current_buffer true})
                                "Open a fuzzy buffer picker"]
                               ;;; ADDITIONAL KEYMAPS (inspired by kickstart.nvim)
                               ;; / = fuzzy search (BEST ONE!)
                               [:/
                                #(let [{: get_dropdown} (require :telescope.themes)]
                                   (current_buffer_fuzzy_find (get_dropdown {:winblend 10
                                                                             :previewer false})))
                                "Open a fuzzy search picker"]
                               ;; resume selections from previous picker (memo: search resume; GREAT ONE!)
                               [:sr resume "Resume a previous picker"]
                               ;; files (memo: search files)
                               [:sf find_files "Open a fuzzy file picker"]
                               ;; diagnostics (memo: search diagnostics)
                               [:sd diagnostics "Open a diagnostic picker"]
                               ;; keymaps (memo: search keymaps)
                               [:sk keymaps "Open a keymap picker"]
                               ;; open file live_grep (memo: search /)
                               [:s/
                                #(live_grep {:grep_open_files true
                                             :prompt_title "Live Grep in Open Files"})
                                "Open a live grep picker in open files"]
                               ;; open Neovim configuration files (memo: search nvim)
                               [:sn
                                #(find_files {:cwd (vim.fn.stdpath :config)})
                                "Open a Neovim conf file picker"]
                               ;; builtin (memo: search builtin)
                               [:sb builtin "Open a Telescope selector"]
                               ;; generic grep_string (memo: search word)
                               ;; TODO: check if mergeable
                               [:sw grep_string "Open a word picker"]
                               ;; live grep (memo: search grep)
                               [:sg live_grep "Open a live grep picker"]]))})

P
