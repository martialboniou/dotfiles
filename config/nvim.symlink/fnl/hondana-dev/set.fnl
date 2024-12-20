(import-macros {: tc} :hondana-dev.macros)
(import-macros {: or=} :hibiscus.core)
(import-macros {: set! : set+ : setlocal! : g!} :hibiscus.vim)
(import-macros {: set!!} :hondana-dev.macros.vim)

;; main functions & pattern
(local {:nvim_create_autocmd au :nvim_create_augroup augroup} vim.api)

(tc type string)
(local pattern "*")

;; if activated, no background for cursorline
(let [group (augroup :Hondana_HiCursorLine {})
      callback #(vim.api.nvim_set_hl 0 :Cursorline {:bg :NONE})]
  ;; ensure no conflicts with any colorschemes in `hondana-dev.plugins.colors` 
  (au [:ColorScheme :VimEnter] {: callback : group : pattern}))

;; global settings
(set!! true :termguicolors :nu :rnu :et :ic :sc :si :undofile :incsearch
       :cursorline)

(set!! false :wrap :swapfile :backup :hlsearch)
;; 2-space indentation is the default
;; Sleuth or any formatters managed with `conform` will change the next line
(set!! 2 :sw :ts)
;; `softtabstop` at -1 mirrors `shiftwidth` (abbrev. `sw`)
(set!! -1 :sts)
;; enable sidescrolloff for tiny monitors & tiling wm/terms
(set!! 10 :siso)

;; specific settings
(vim.opt.isfname:append "@-@")
(let [map {:guicursor ""
           :undodir (-> [(os.getenv :HOME) :.vim :undodir] (table.concat "/"))
           :scrolloff 8
           :colorcolumn :100
           :signcolumn :yes
           :updatetime 50
           :timeoutlen 3000
           :list true
           :listchars {:tab "» " :trail "·" :nbsp "␣"}
           :inccommand :split}]
  (each [k v (pairs map)] (set! k v)))

;; these default keys may be remapped in `hondana-dev.remap`
(tc type string)
(g! :mapleader " ")
(tc type string)
(g! :maplocalleader ",")

;; netrw (use right splitting & tree by default)
(g! netrw_altv 1)
(g! netrw_liststyle 3)

;; diagnostic
(vim.diagnostic.config {:update_in_insert false
                        :float {:focusable true
                                :border :rounded
                                :source :always}
                        ;; `vim.diagnostic.severity.*` are strings so key-able
                        :signs {:text {vim.diagnostic.severity.ERROR "✘"
                                       vim.diagnostic.severity.WARN "▲"
                                       vim.diagnostic.severity.HINT "⚑"
                                       vim.diagnostic.severity.INFO "»"}}})

;; you can use lambda in this programming language (see below)
(vim.cmd.iab ",\\ λ")

;; 4-space indentation for some filetypes
;; REMINDER: each project should have its own setup
(let [group (augroup :Hondana_Fantastic4Indentation {})
      options [:sw :ts]
      pattern [:c :cpp :zig :rust :go]
      callback #(each [_ o (ipairs options)] (setlocal! o 4))]
  (au :FileType {: callback : group : pattern}))

;; visible yank
(let [group (augroup :Hondana_Highlight_Yank {:clear true})
      callback #(vim.highlight.on_yank)]
  (au :TextYankPost {: callback : group}))

;; shen programming language comments
(let [group (augroup :Hondana_ShenComments {})
      callback #(setlocal! :commentstring "\\\\ %s")]
  (au :FileType {: callback : group :pattern :shen}))

;; TEST: restore last position (check ShaDa for other session/buffer restoration)
(let [group (augroup :Hondana_LastPosRestoration {})]
  (au :BufReadPost {:callback #(when (-> "%" (vim.fn.bufname)
                                         (not= :.git/COMMIT_EDITMSG))
                                 (vim.cmd "silent! normal g`\"zv"))
                    : group
                    : pattern}))
