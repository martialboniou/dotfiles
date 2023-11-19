(import-macros {: set! : set+ : setlocal! : g! : concat!} :hibiscus.vim)
(import-macros {: or=} :hibiscus.core)

(local autocmd vim.api.nvim_create_autocmd)

(macro set!! [x ...]
  "set the vim.opt options to x"
  (icollect [_ opt (ipairs [...])]
    (assert-compile (= :string (type opt)) "expected string for every vararg"))
  (let [out []]
    (each [_ o (ipairs [...])]
      (table.insert out `(tset vim.opt ,o ,x)))
    `(do
       ,(unpack out))))

;; global settings
(set!! true :termguicolors :nu :rnu :et :ic :sc :si :undofile :incsearch)
(set!! false :wrap :swapfile :backup :hlsearch)
(set!! 4 :sw :ts :sts)

;; specific settings
(vim.opt.isfname:append "@-@")
(let [map {:guicursor ""
           :undodir (concat! "/" (os.getenv :HOME) :.vim :undodir)
           :scrolloff 8
           :colorcolumn :80
           :signcolumn :yes
           :updatetime 50}]
  (collect [k v (pairs map)] (set! k v)))

;; these default keys may be remapped in ./remap.fnl
(g! :mapleader " ")
(g! :maplocalleader ",")

;; you can use lambda in this programming language (see below)
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
  (set+ :rtp fzf-macos)
  (vim.keymap.set :n :<leader>t ":FZF<CR>"))
