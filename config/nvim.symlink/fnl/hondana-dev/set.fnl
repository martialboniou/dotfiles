(import-macros {: tc} :hondana-dev.macros)
(import-macros {: or=} :hibiscus.core)
(import-macros {: set! : g!} :hibiscus.vim)
(import-macros {: set!! : setlocal!!} :hondana-dev.macros.vim)

;; main functions & pattern
(local {:api {:nvim_create_autocmd au :nvim_create_augroup augroup} : keymap}
       vim)

(tc type string)
(local pattern "*")

;; if activated, no background for cursorline
(let [group (augroup :Hondana_HiCursorLine {:clear true})
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

;; very specific case of timeoutlen (far shorter in insert mode; check `hondana-dev.remap` for a use case)
(let [group (augroup :Hondana_TimeoutLen {:clear true})
      pattern :*
      shortest-timeoutlen 200 ; ms
      default-timeoutlen (or vim.opt.timeoutlen 3000)]
  (au :InsertLeave
      {: group : pattern :callback #(set! :timeoutlen default-timeoutlen)})
  (au :InsertEnter
      {: group : pattern :callback #(set! :timeoutlen shortest-timeoutlen)}))

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
(keymap.set :ia ",\\" "λ")

;; 4-space indentation for some filetypes
;; REMINDER: each project should have its own setup
(let [group (augroup :Hondana_Fantastic4Indentation {:clear true})
      pattern [:roc :zig :c :cpp :h :rust :go]
      callback #(setlocal!! 4 :sw :ts)]
  (au :FileType {: callback : group : pattern}))

;; visible yank
(let [group (augroup :Hondana_Highlight_Yank {:clear true})
      callback #(vim.highlight.on_yank)]
  (au :TextYankPost {: callback : group}))

;; additional languages' setup
;; - shen programming language comments (from Shen Technology (c) Mark Tarver; https://shenlanguage.org)
;; NOTE: the language syntax has been embedded in this setup
(let [group (augroup :Hondana_ShenComments {:clear true})
      callback #(setlocal!! "\\\\ %s" :commentstring)]
  (au :FileType {: callback : group :pattern :shen}))

;; others
;; TEST: restore last position (check ShaDa for other session/buffer restoration)
(let [group (augroup :Hondana_LastPosRestoration {:clear true})]
  (au :BufReadPost {:callback #(when (-> "%" (vim.fn.bufname)
                                         (not= :.git/COMMIT_EDITMSG))
                                 (vim.cmd "silent! normal g`\"zv"))
                    : group
                    : pattern}))
