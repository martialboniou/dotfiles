(import-macros {: set! : setlocal! : g!} :hibiscus.vim)
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

;; shorten indentation for some filetypes
(local hondana-short-indentation
       (vim.api.nvim_create_augroup :Hondana_ShortIndentation {}))

(autocmd :BufWinEnter {:callback (λ []
                                   (when (or= vim.bo.ft :jsonc :json :haskell
                                              :ocaml)
                                     (each [_ option (ipairs [:sw :ts :sts])]
                                       (setlocal! option 2))))
                       :group hondana-short-indentation
                       :pattern "*"})
