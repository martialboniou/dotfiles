(import-macros {: set! : set+ : g! : color!} :hibiscus.vim)

(set! :guicursor "")

(set! :number)
(set! :relativenumber)

(set! :tabstop 4)
(set! :softtabstop 4)
(set! :shiftwidth 4)
(set! :expandtab)

(set! :ignorecase)
(set! :smartcase)
(set! :smartindent)

(set! :wrap false)

(set! :swapfile false)
(set! :backup false)
;; (set! :undodir (.. (os.getenv :HOME) :/.vim/undodir))
(set! :undofile true)

(set! :hlsearch false)
(set! :incsearch true)

(set! :termguicolors)

(set! :scrolloff 8)
(set! :signcolumn :yes)
;; (set+ :isfname "@~@")

(set! :updatetime 50)

(set! :colorcolumn :80)

(g! :mapleader " ")
(g! :maplocalleader ",")

; (color! :hondana-tomorrow-night)