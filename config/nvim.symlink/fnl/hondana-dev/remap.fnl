(import-macros {: g!} :hibiscus.vim)
;; keymap for NeoVim by ThePrimeagen
(g! :mapleader " ")
(g! :maplocalleader ",")

;; these two lines will be remapped by plugins/mini-files
(vim.keymap.set :n :<leader>pv vim.cmd.Ex)
(vim.keymap.set :n :<leader><leader> vim.cmd.Ex)

;; fast nav

;; Q is removed (that's good!)
(vim.keymap.set :n :Q :<nop>)

;; move the selection up & down with K & J
(vim.keymap.set :v :J ":m '>+1<CR>gv=gv")
(vim.keymap.set :v :K ":m '<-2<CR>gv=gv")

;; (vim.keymap.set :n :Y :yg$)
(vim.keymap.set :n :J "mzJ`z")

;; doesn't move the cursor while appending line
;; the following one is bad C-d is delete (also used in terms)
(vim.keymap.set :n :<C-d> :<C-d>zz)

;; page down (CHECK: any conflict?)
(vim.keymap.set :n :<C-u> :<C-u>zz)

;; page up
(vim.keymap.set :n :n :nzzzv)

;; keep the cursor in the middle during search
(vim.keymap.set :n :N :Nzzzv)

;; keep the cursor in the middle during backsearch

;; greatest remap ever
;;   paste a buffer but doesn't keep the deleted selection
;;   so you can paste the same again
(vim.keymap.set :x :<leader>p "\"_dP")

;; next greatest remap ever : asbjornHaland
;; yank for the clipboard
(vim.keymap.set [:n :v] :<leader>y "\"+y")
(vim.keymap.set :n :<leader>Y "\"+Y")
;; delete for the clipboard
(vim.keymap.set [:n :v] :<leader>d "\"_d")

;; ThePrimeagen thing; can be changed when foot controller is plugged
(vim.keymap.set :i :<C-c> :<Esc>)

;; the following one works with the snippet forward keybindings
;; (as this bind is for the s mode)
(vim.keymap.set :n :<C-f> "<cmd>silent !tmux neww tmux-sessionizer<CR>")

;; require https://github.com/ThePrimeagen/.dotfiles/blob/master/bin/.local/scripts/tmux-sessionizer in your path

;; quickfix navigation
(vim.keymap.set :n :<C-k> :<cmd>cnext<CR>zz)
(vim.keymap.set :n :<C-j> :<cmd>cprev<CR>zz)
(vim.keymap.set :n :<leader>k :<cmd>lnext<CR>zz)
(vim.keymap.set :n :<leader>j :<cmd>lprev<CR>zz)

(vim.keymap.set :n :<leader>s
                ":%s/\\<<C-r><C-w>\\>/<C-r><C-w>/gI<Left><Left><Left>")
(vim.keymap.set :v :<leader>s ":s///gI<Left><Left><Left><Left>")

;; added by https://gitlab.com/martialhb

;; toggle the executability of the current file
(λ toggle-exec []
  ;; use :make_executable if no back and forth
  (let [(ok res) (pcall (. (require :hondana-dev.utils) :toggle_executable))]
    (when (not ok)
      (print (.. "Error: toggle_executable in remap.lua: " res))
      (lua :return))
    (print (.. "Success: " res))))

(vim.keymap.set :n :<leader>x toggle-exec {:silent false} )
