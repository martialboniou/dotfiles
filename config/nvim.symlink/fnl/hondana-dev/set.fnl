(import-macros {: set! : set+ : setlocal! : g!} :hibiscus.vim)
(import-macros {: or=} :hibiscus.core)
(local autocmd vim.api.nvim_create_autocmd)

(set! :guicursor "")

(set! :number)
(set! :relativenumber)

(set! :shiftwidth 4)
(set! :tabstop 4)
(set! :softtabstop 4)
(set! :expandtab)

(set! :ignorecase)
(set! :smartcase)
(set! :smartindent)

(set! :wrap false)

(set! :swapfile false)
(set! :backup false)
(set! :undodir (.. (os.getenv :HOME) :/.vim/undodir))
(set! :undofile true)

(set! :hlsearch false)
(set! :incsearch true)

(set! :termguicolors)

(set! :scrolloff 8)
(set! :signcolumn :yes)
(vim.cmd "set isfname+=@-@")

(set! :updatetime 50)

(set! :colorcolumn :80)

;; these default keys may be remapped in ./remap.fnl
(g! :mapleader " ")
(g! :maplocalleader ",")

;; you can use lambda in this programming languagea (see below)
(vim.cmd.iab ",\\ λ")

;; special indentation for some filetypes
(let [group (vim.api.nvim_create_augroup :Hondana_SpecialIndentation {})]
  (autocmd :BufWinEnter {:callback (λ []
                                     ;; short indent
                                     (when (or= vim.bo.ft :jsonc :json :haskell
                                                :ocaml)
                                       (each [_ option (ipairs [:sw :ts :sts])]
                                         (setlocal! option 2))))
                         ;; long indent example
                         ;; (when (or= vim.bo.ft :go)
                         ;;   (each [_ option (ipairs [:sw :ts :sts])]
                         ;;     (setlocal! option 8)))
                         :pattern "*"
                         : group}))

;; restore last position (check ShaDa for other session/buffer restoration)
(let [group (vim.api.nvim_create_augroup :Hondana_LastPosRestoration {})]
  (autocmd :BufReadPost {:callback #(when (-> "%" (vim.fn.bufname)
                                              (not= :.git/COMMIT_EDITMSG))
                                      (vim.cmd "silent! normal g`\"zv"))
                         :pattern "*"
                         : group}))

;; additional rtp (temporary: not really needed, see ./plugins/telescope.fnl)
(local fzf-macos :/opt/homebrew/opt/fzf)
(when (= 1 (vim.fn.isdirectory fzf-macos))
  (set+ rtp fzf-macos)
  (vim.keymap.set :n :<leader>t ":FZF<CR>"))
