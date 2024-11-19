(import-macros {: tc} :hondana-dev.macros)
(import-macros {: or=} :hibiscus.core)
(import-macros {: set! : set+ : setlocal! : g! : concat!} :hibiscus.vim)
(import-macros {: set!!} :hondana-dev.macros.vim)
(import-macros {: tc-source} :hondana-dev.macros.vim)

;; main functions & pattern
(local {:nvim_create_autocmd autocmd :nvim_create_augroup augroup} vim.api)

(tc type string)
(local pattern "*")

;; if activated, no background for cursorline
(let [group (augroup :Hondana_HiCursorLine {})
      callback #(vim.api.nvim_set_hl 0 :Cursorline {:bg :NONE})]
  ;; ensure no conflicts with any colorschemes in `hondana-dev.plugins.colors` 
  (autocmd [:ColorScheme :VimEnter] {: callback : group : pattern}))

;; global settings
(set!! true :termguicolors :nu :rnu :et :ic :sc :si :undofile :incsearch
       :cursorline)

(set!! false :wrap :swapfile :backup :hlsearch)
(set!! 4 :sw :ts :sts)
;; enable sidescrolloff for tiny monitors & tiling wm/terms
(set!! 10 :siso)

;; specific settings
(vim.opt.isfname:append "@-@")
(let [map {:guicursor ""
           :undodir (concat! "/" (os.getenv :HOME) :.vim :undodir)
           :scrolloff 8
           :colorcolumn :100
           :signcolumn :yes
           :updatetime 50}]
  (each [k v (pairs map)] (set! k v)))

;; these default keys may be remapped in `hondana-dev.remap`
(tc type string)
(g! :mapleader " ")
(tc type string)
(g! :maplocalleader ",")

;; you can use lambda in this programming language (see below)
(vim.cmd.iab ",\\ λ")

;; 2-space indentation for some filetypes
(let [group (augroup :Hondana_SpecialIndentation {})
      options [:sw :ts :sts]
      {: lisp-ft?} (require :hondana-dev.utils)
      callback #(when (or ;; I'm a Common Lisp & Fennel/Lua user
                          (lisp-ft? vim.bo.ft) ;;
                          ;; others
                          (or= vim.bo.ft :jsonc :json :haskell :ocaml))
                  (each [_ o (ipairs options)] (setlocal! o 2)))]
  (autocmd :BufWinEnter {: callback : group : pattern}))

;; shen programming language comments
(let [group (augroup :Hondana_ShenComments {})
      callback #(setlocal! :commentstring "\\\\ %s")]
  (autocmd [:BufNewFile :BufRead] {: callback : group :pattern :*.shen}))

;; restore last position (check ShaDa for other session/buffer restoration)
(let [group (augroup :Hondana_LastPosRestoration {})]
  (autocmd :BufReadPost {:callback #(when (-> "%" (vim.fn.bufname)
                                              (not= :.git/COMMIT_EDITMSG))
                                      (vim.cmd "silent! normal g`\"zv"))
                         : group
                         : pattern}))

;; additional rtp (temporary: not really needed, see ./plugins/telescope.fnl)
(tc type string)
(local fzf-macos :/opt/homebrew/opt/fzf)
(when (= 1 (vim.fn.isdirectory fzf-macos))
  (set+ :rtp fzf-macos)
  (vim.keymap.set :n :<leader>tn ":FZF<CR>"))
