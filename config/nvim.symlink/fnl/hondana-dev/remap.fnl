(import-macros {: tc : **!} :hondana-dev.macros)
(import-macros {: g!} :hibiscus.vim)

(local {:set keyset :del keydel} vim.keymap)

(macro ex-map! [...]
  (let [t# [...]]
    (local out# ;;
           (icollect [_ v# (ipairs t#)]
             `(keyset :n ,(.. :<leader> v#) vim.cmd.Ex {:desc "Explore files"})))
    `(do
       ,(unpack out#))))

;; keymap for NeoVim by ThePrimeagen
(tc type string)
(g! :mapleader " ")
(tc type string)
(g! :maplocalleader ",")

;; `<leader>pf`/`<leader>pv` = `:Ex` except when `hondana-dev.plugins.mini-files` is on
;; (so never used in the current setup)
(ex-map! :pf :pv)

;; fast nav

;; Q is removed (that's good!)
(keyset :n :Q :<nop>)

;; move the selection up & down with K & J
(keyset :v :J ":m '>+1<CR>gv=gv")
(keyset :v :K ":m '<-2<CR>gv=gv")

;; (keyset :n :Y :yg$)
;; NOTE: previous version by ThePrimeagen leaves marks:
;;       (keyset :n :J "mzJ`z")
(λ new-shift-j []
  ":join with position tracking (and without using marks)"
  {:author "https://gitlab.com/martialhb"}
  (let [[_ c] (-> 0 (vim.api.nvim_win_get_cursor))]
    (-> c
        (+ 1)
        (#[:join "|normal " $ "|"])
        (table.concat)
        (vim.cmd))))

(keyset :n :J new-shift-j)

;; doesn't move the cursor while appending line
;; the following one is bad <C-d> is delete (also used in terms)
;; page down
(keyset :n :<C-d> :<C-d>zz)

;; page up
(keyset :n :<C-u> :<C-u>zz)

;; keep the cursor in the middle during (back)search
(let [keys [:n :N]]
  (for [i 1 (length keys)]
    (local key (. keys i))
    (keyset :n key (.. key :zzzv))))

;; greatest remap ever
;;   paste a buffer but doesn't keep the deleted selection
;;   so you can paste the same again
(keyset :x :<leader>p "\"_dP"
        {:desc "Paste without keeping the replaced seletion"})

;; next greatest remap ever : asbjornHaland
;; yank to the clipboard
(keyset [:n :v] :<leader>y "\"+y" {:desc "Yank to the clipboard"})
(keyset :n :<leader>Y "\"+Y" {:desc "Yank line to the clipboard"})
;; delete for the clipboard
(keyset [:n :v] :<leader>d "\"_d" {:desc "Delete & keep it in the clipboard"})

;; ThePrimeagen thing; can be changed when foot controller is plugged
(keyset :i :<C-c> :<Esc>)

;; the following one works with the snippet forward keybindings
;; (as this bind is for the s mode)
;; NOTE: need https://github.com/ThePrimeagen/.dotfiles/tree/master/bin/.local/scripts/tmux-sessionizer
(keyset :n :<C-q> "<cmd>silent !tmux neww tmux-sessionizer<CR>")

;; quickfix navigation (inverted from ThePrimeagen version; more natural)
;; WARN: these functions (ie `cnext`...) have been updated to enable cycling
;; (check `hondana-dev.command`)
(each [key navi (pairs {:<C-j> :Cnext
                        :<C-k> :Cprev
                        :<leader>j :Lnext
                        :<leader>k :Lprev})]
  (keyset :n key (.. :<Cmd> navi :<CR>zz)))

;; gs => search-replace (in normal/visual mode) w/ confirmation
(let [cmds {:n (.. ":%s/\\<<C-r><C-w>\\>/<C-r><C-w>/cgI" (**! :<Left> 4))
            :v (.. ":s///cgI" (**! :<Left> 5))}]
  (each [mode cmd (pairs cmds)]
    (keyset mode :gs cmd {:desc "Search/Replace template"})))

;; <leader>cgn => use `cgn` to replace the current word (<dot> to propagate to the next one)
;; <leader>cc (alias)
(let [keys [:cgn :cc]]
  (for [i 1 (length keys)]
    (->> (. keys i)
         (.. :<leader>)
         (#(keyset :n $ ":let @/=expand('<cword>')<CR>cgn"
                   {:desc "Replace the current word using `cgn` (. to propagate)"})))))

;; g. => . to repeat a regular `c`-prefixed command as if it was performed using `cgn`
;; https://www.reddit.com/r/neovim/comments/sf0hmc/im_really_proud_of_this_mapping_i_came_up_with
(let [nmap #(keyset :n $1 $2 {:noremap true})]
  (nmap :g. "/\\V\\C<C-r>\"<CR>cgn<C-a><Esc>")
  (nmap :cg* "*Ncgn"))

;; <leader>cd => change local current directory
(keyset :n :<leader>cd ":lcd %:h<CR>"
        {:desc "Change local directory according to this file location"})

;; center the buffer vertically according to the cursor's position
(keyset :n "z;" ":<C-u>normal! zszH<CR>")

;; helix-kinda bindings beginning/end of line (like `^`/`g_`; not like `0`/`$`)
; NOTE: caret can be boring to type when you use dead keys
(keyset :n :gh :^)
(keyset :n :gl :g_)

;; <F4> => toggle completion in the buffer
;; NOTE: useful to get `i_CTRL-y` working to insert character which is above
;; the cursor; otherwise, `<C-y>` accepts the `blink.cmp`'s suggestion in
;; insert mode when the completion is enabled in the buffer
(keyset [:i :n :x] :<F4>
        (fn []
          ;; an undefined (nil) buffer completion may exist
          (when (= vim.b.completion nil) (set vim.b.completion true))
          (set vim.b.completion (not vim.b.completion))
          (vim.notify (if vim.b.completion "Completion restored!"
                          "Completion stopped in this buffer!")
                      vim.log.levels.INFO)))

;; <F3> => timestamp (oldie)
(let [cmd "<C-r>=strftime('%Y-%m-%d %a %H:%M')<CR>"]
  (each [m c (pairs {:n (.. :i cmd :<Esc>) :i cmd})]
    (keyset m :<F3> c)))

;; <F2> => switch vim numbers (column-wise)
(let [{: set-numbers} (require :hondana-dev.utils.globals)]
  (keyset [:i :n :x] :<F2> set-numbers))

;; mapping to move lines (legacy)
;; https://vim.fandom.com/wiki/Moving_lines_up_or_down
;; TODO: luaify this
(vim.cmd "nnoremap <M-j> :m .+1<CR>==")
(vim.cmd "nnoremap <M-k> :m .-2<CR>==")
(vim.cmd "inoremap <M-j> <Esc>:m .+1<CR>==gi")
(vim.cmd "inoremap <M-k> <Esc>:m .-2<CR>==gi")
(vim.cmd "vnoremap <M-j> :m '>+1<CR>gv=gv")
(vim.cmd "vnoremap <M-k> :m '<-2<CR>gv=gv")

;;; ** EXPERIMENTAL SECTION **
(local remap false)
(local silent true)
;; `<C-\>` is mandatory here so the cursor doesn't move back twice because of
;; the `<C-o>` at the EOL (otherwise `<C-o>` doesn't move the cursor if there's
;; at least one character after the cursor)
(local move-backwark-seq "<C-\\><C-o>h")

;; disable `<Tab>` default to vim.snippet.jump if active I use `<C-f>` for this
;; (via `blink.cmp` & `<C-b>` for the backwards alternative)
;; WARN: I don't mind to keep the default settings later but I often don't do
;; the last `vim.snippet.jump` forward and having the ability to use the
;; `<Tab>` key UNCONDITIONNALLY makes sense in my workflow
;; NOTE: check `hondana-dev.plugins.completion`
(keydel [:i :s] :<Tab>)

;; WARN: NEW: `comma period`/`comma comma` => semi-colon at eol
;; (works in a very short time duration on insert mode only)
;; INFO: `,.` sets the cursor after the newly added `;`
;;       `,,` sets the cursor before the newly added `;`
;; FIX: better code (TreeSitter?)
(let [pattern "<C-o>g_<C-o>a;"
      inskey #(keyset :i $...)]
  (inskey ",." pattern
          {: remap
           : silent
           :desc "Add semi-colon at the EOL before any trailing whitespace"})
  (inskey ",," (.. pattern move-backwark-seq)
          {: remap
           : silent
           :desc ;
           "Add semi-colon at the EOL before any trailing whitespace; the cursor is placed before"}))

;; WARN: NEW: `,p` simulates a auto-pairing of curly brackets (ie `{}`)
;; in insert mode by pulling the cursor back in between
(keyset :i ",p" (.. "{}" move-backwark-seq)
        {: remap
         : silent
         :desc "Put the cursor back after a fastly typed {} sequence"})

;; wordwise `<C-e>`/`<C-y>` in insert mode (restored from my old Vim 6.3 setup)
;; INFO: you might not like this one; check `:help i_CTRL-y`
;; WARN: `<C-e>` always work but to use `<C-y>` normally, you'll need to
;; disable the completion in the buffer (using `<F4>`) because `<C-y>` in
;; insert mode accepts the completion's suggestion when the completion is
;; enabled
(vim.cmd "inoremap <expr> <C-e> matchstr(getline(line('.')+1), '\\%' . virtcol('.') . 'v\\%(\\k\\+\\\\|.\\)')")
(vim.cmd "inoremap <expr> <C-y> matchstr(getline(line('.')-1), '\\%' . virtcol('.') . 'v\\%(\\k\\+\\\\|.\\)')")

;; `jk` in insert mode to exit insert mode
;; WARN: it won't work with the `multiple cursors` plugin (use `<C-c>` or
;; `<Esc>`)
(keyset :i :jk :<Esc> {: remap : silent :desc "Exit the insert mode"})
