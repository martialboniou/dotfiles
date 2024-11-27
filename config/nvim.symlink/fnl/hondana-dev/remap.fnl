(import-macros {: tc} :hondana-dev.macros)
(import-macros {: g!} :hibiscus.vim)
(import-macros {: **!} :hondana-dev.macros)

;; keymap for NeoVim by ThePrimeagen
(tc type string)
(g! :mapleader " ")
(tc type string)
(g! :maplocalleader ",")

;; these two lines will be remapped by plugins/mini-files
(each [_ key (ipairs [:pv :<leader>])]
  (vim.keymap.set :n (.. :<leader> key) vim.cmd.Ex))

;; fast nav

;; Q is removed (that's good!)
(vim.keymap.set :n :Q :<nop>)

;; move the selection up & down with K & J
(vim.keymap.set :v :J ":m '>+1<CR>gv=gv")
(vim.keymap.set :v :K ":m '<-2<CR>gv=gv")

;; (vim.keymap.set :n :Y :yg$)
;; NOTE: previous version by ThePrimeagen leaves marks:
;;       (vim.keymap.set :n :J "mzJ`z")
(λ new-shift-j []
  ":join with position tracking (and without using marks)"
  {:author "https://gitlab.com/martialhb"}
  (let [[_ c] (-> 0 (vim.api.nvim_win_get_cursor))]
    (-> c
        (+ 1)
        (#[:join "|normal " $ "|"])
        (table.concat)
        (vim.cmd))))

(vim.keymap.set :n :J new-shift-j)

;; doesn't move the cursor while appending line
;; the following one is bad <C-d> is delete (also used in terms)
;; page down
(vim.keymap.set :n :<C-d> :<C-d>zz)

;; page up
(vim.keymap.set :n :<C-u> :<C-u>zz)

;; keep the cursor in the middle during (back)search
(each [_ key (ipairs [:n :N])]
  (vim.keymap.set :n key (.. key :zzzv)))

;; greatest remap ever
;;   paste a buffer but doesn't keep the deleted selection
;;   so you can paste the same again
(vim.keymap.set :x :<leader>p "\"_dP"
                {:desc "Paste without keeping the replaced seletion"})

;; next greatest remap ever : asbjornHaland
;; yank to the clipboard
(vim.keymap.set [:n :v] :<leader>y "\"+y" {:desc "Yank to the clipboard"})
(vim.keymap.set :n :<leader>Y "\"+Y" {:desc "Yank line to the clipboard"})
;; delete for the clipboard
(vim.keymap.set [:n :v] :<leader>d "\"_d"
                {:desc "Delete & keep it in the clipboard"})

;; ThePrimeagen thing; can be changed when foot controller is plugged
(vim.keymap.set :i :<C-c> :<Esc>)

;; the following one works with the snippet forward keybindings
;; (as this bind is for the s mode)
;; NOTE: need https://github.com/ThePrimeagen/.dotfiles/tree/master/bin/.local/scripts/tmux-sessionizer
(vim.keymap.set :n :<C-q> "<cmd>silent !tmux neww tmux-sessionizer<CR>")

;; quickfix navigation (inverted from ThePrimeagen version; more natural)
(each [key navi (pairs {:<C-j> :cnext
                        :<C-k> :cprev
                        :<leader>j :lnext
                        :<leader>k :lprev})]
  (vim.keymap.set :n key (.. :<Cmd> navi :<CR>zz)))

;; <leader>s => search-replace (in normal/visual mode) w/ confirmation
(let [cmds {:n (.. ":%s/\\<<C-r><C-w>\\>/<C-r><C-w>/cgI" (**! :<Left> 4))
            :v (.. ":s///cgI" (**! :<Left> 5))}]
  (each [mode cmd (pairs cmds)]
    (vim.keymap.set mode :<leader>s cmd {:desc "Search/Replace template"})))

;; <leader>cgn => use `cgn` to replace the current word (<dot> to propagate to the next one)
;; <leader>cc (alias)
(each [_ v (ipairs [:cgn :cc])]
  (->> v
       (.. :<leader>)
       (#(vim.keymap.set :n $ ":let @/=expand('<cword>')<CR>cgn"
                         {:desc "Replace the current word using `cgn` (. to propagate)"}))))

;; added by https://gitlab.com/martialhb
; - change local current directory
(vim.keymap.set :n :<leader>cd ":lcd %:h<CR>"
                {:desc "Change local directory according to this file location"})

; - center the buffer vertically according to the cursor's position
(vim.keymap.set :n "z;" ":<C-u>normal! zszH<CR>")
;; TODO: test the following command; find a better keybinding
; - print the current filename at the cursor position
(vim.keymap.set :n :<leader>. ":put =expand('%:t')<CR>")
; - print a C #include guard at current the cursor position
(λ include-guard-scheme []
  (when (= (vim.fn.expand "%p") "")
    (error "Empty filename (save file and try again)"))
  ;; vimscript's toupper() is unicode (might not be a problem here: "a-z\.")
  (let [t #(-> $ (vim.fn.expand) (vim.fn.toupper))
        ext (t "%:t:e")
        guard (.. (t "%:t:r") "_" (if (= "" ext) "" (.. ext "_")))]
    (each [_ cmd (ipairs [(.. :O "#ifndef " guard)
                          (.. :o "#define " guard)
                          :o
                          (.. :o "#endif // " guard)
                          :k])]
      (vim.cmd.normal cmd))))

(vim.keymap.set :n :<leader>h include-guard-scheme
                {:desc "Print a C #include guard at the current cursor position"})

;; toggle the executability of the current file
(λ toggle-exec []
  (let [{: toggle-executable} (require :hondana-dev.utils)
        (ok res) (pcall toggle-executable)]
    (-> ok
        (not)
        (#(if $ "Error: toggle-executable in hondana-dev.remap: " "Success: "))
        (.. res)
        (print))))

(vim.keymap.set :n :<leader>x toggle-exec
                {:silent false :desc "Toggle the current file as executable"})
