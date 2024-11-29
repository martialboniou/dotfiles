(import-macros {: tc} :hondana-dev.macros)

(tc param keys "string[]" param fun "fun()" param desc string)
(fn keys-keymaps [keys fun desc]
  (-> keys
      (#(icollect [_ key (ipairs $)]
          {1 (.. :<leader> key) 2 fun : desc}))))

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
                (local keymaps ;;
                       [;; files (memo: project files)
                        (-> [:pf :sf]
                            (keys-keymaps #(find_files)
                                          "Open a fuzzy file picker"))
                        ;; rg-filtered files (memo: project search)
                        (-> [:ps]
                            (keys-keymaps #(grep_string {:search (vim.fn.input "Grep > ")})
                                          "Open a string-filtered file picker (cwd)"))
                        ;; ;; files in git (memo: control project)
                        [{1 :<C-p>
                          2 #(git_files)
                          :desc "Open a git repo fuzzy file picker"}]
                        ;; recent files (memo: view visited; vr = BAD ERGO)
                        (-> [:vv]
                            (keys-keymaps #(oldfiles)
                                          "Open a fuzzy recent file picker"))
                        ;; helptags (memo: view helptags; search helptags)
                        (-> [:vh :sh]
                            (keys-keymaps #(help_tags) "Open a help tag picker"))
                        ;; buffers (memo: view buffers + bb as alias; search other buffers)
                        (-> [:vb :bb :sb]
                            (keys-keymaps #(buffers {:ignore_current_buffer true})
                                          "Open a fuzzy buffer picker"))
                        ;;; NEW KEYMAPS (inspired by kickstart.nvim; experimenting)
                        ;; / = fuzzy search (BEST ONE!)
                        (-> [:/]
                            (keys-keymaps #(let [{: get_dropdown} (require :telescope.themes)]
                                             (current_buffer_fuzzy_find (get_dropdown {:winblend 10
                                                                                       :previewer false})))
                                          "Open a fuzzy search picker"))
                        ;; resume selections from previous picker (memo: search resume; GREAT ONE!)
                        (-> [:sr]
                            (keys-keymaps #(resume) "Resume a previous picker"))
                        ;; diagnostics (memo: search diagnostics)
                        (-> [:sd]
                            (keys-keymaps #(diagnostics)
                                          "Open a diagnostic picker"))
                        ;; keymaps (memo: search keymaps)
                        (-> [:sk]
                            (keys-keymaps #(keymaps) "Open a keymap picker"))
                        ;; open file live_grep (memo: search /)
                        (-> [:s/]
                            (keys-keymaps #(live_grep {:grep_open_files true
                                                       :prompt_title "Live Grep in Open Files"})
                                          "Open a live grep picker in open files"))
                        ;; open Neovim configuration files (memo: search nvim)
                        (-> [:sn]
                            (keys-keymaps #(find_files {:cwd (vim.fn.stdpath :config)})
                                          "Open a Neovim conf file picker"))
                        ;; builtin (memo: search builtin)
                        (-> [:sb]
                            (keys-keymaps #(builtin)
                                          "Open a Telescope selector"))
                        ;; generic grep_string (memo: search word)
                        ;; TODO: check if mergeable
                        (-> [:sw]
                            (keys-keymaps #(grep_string) "Open a word picker"))
                        ;; live grep (memo: search grep)
                        (-> [:sg]
                            (keys-keymaps #(live_grep)
                                          "Open a live grep picker"))])
                (let [iter (vim.iter keymaps)]
                  (-> iter (: :flatten) (: :totable))))})

P
