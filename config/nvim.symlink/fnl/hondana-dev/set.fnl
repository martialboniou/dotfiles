;; checked: 2025-05-21
(import-macros {: tc} :hondana-dev.macros)
(import-macros {: or=} :hibiscus.core)
(import-macros {: set! : g!} :hibiscus.vim)
(import-macros {: set!! : setlocal!!} :hondana-dev.macros.vim)

;; main functions & pattern
(local {:api {:nvim_create_autocmd au :nvim_create_augroup augroup} : keymap}
       vim)

;; shortcut for the group created here
(local augrp #(augroup $ {:clear true}))

(tc type string)
(local pattern "*")

;; if activated, no background for cursorline
(let [group (augrp :Hondana_HiCursorLine)
      callback #(vim.api.nvim_set_hl 0 :Cursorline {:bg :NONE})]
  ;; ensure no conflicts with any colorschemes in `hondana-dev.plugins.colors` 
  (au [:ColorScheme :VimEnter] {: callback : group : pattern}))

;; global settings
(set!! true :termguicolors :et :ic :sc :si :undofile :incsearch :cursorline)
(let [{: set-numbers} (require :hondana-dev.utils.globals)]
  ;; should do a `:= set nu rnu` in Lua at startup
  (set-numbers))

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
(set vim.opt.fillchars {:eob " "})
;; `:lua vim.opt.fillchars:remove("eob")` to restore
;; nrformats has alpha so `<C-a>` or `g<C-a>` works with characters
(vim.opt.nrformats:append "alpha")
(let [map {:guicursor ""
           ;; shared with Vim
           :undodir (-> [(os.getenv :HOME) :.vim :undodir] (unpack)
                        (vim.fs.joinpath))
           :scrolloff 8
           :colorcolumn :100
           :signcolumn :yes
           ;; :winborder :rounded
           :updatetime 50
           :timeoutlen 3000
           ;; try ↵  for eol
           :listchars {:tab "» " :trail "·" :nbsp "␣" :eol "¶"}
           ;; `list` after `listchars`
           :list true
           :inccommand :split}]
  (each [k v (pairs map)] (set! k v)))

;; very specific case of timeoutlen (far shorter in insert mode; check `hondana-dev.remap` for a use case)
(let [group (augrp :Hondana_TimeoutLen)
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

;; koka case: ensure `*.kk` is known as the file extension for the `koka` filetype
;; check `fnl/after/lsp/koka.fnl`
(vim.filetype.add {:extension {:kk :koka}})

;; markdown case: disable the recommended style
;; WARN: MARKDOWN must use this setup's default 2-space style; otherwise
;; comment the next line (check `:help ft-markdown-plugin`)
(set vim.g.markdown_recommended_style 0)

;; diagnostic
(let [text (-> :hondana-dev.utils.globals (require) (. :icons :diagnostic))]
  (vim.diagnostic.config {:virtual_text {:prefix ""}
                          :update_in_insert false
                          :float {:prefix ""
                                  :header ""
                                  :focusable true
                                  :border :rounded
                                  :source true
                                  :severity_sort true}
                          ;; `vim.diagnostic.severity.*` are strings so key-able
                          :signs {: text}}))

;; you can use lambda in this programming language (see below)
;; NOTE: /, is a lambda sign in Shen
(let [shortcuts [",\\" "/,"]]
  (for [i 1 (length shortcuts)]
    (keymap.set :ia (. shortcuts i) "λ")))

;; 4-space indentation for some filetypes
;; REMINDER: each project should have its own setup
(let [group (augrp :Hondana_Fantastic4Indentation)
      ;; NOTE: ft-zig-plugin, ft-go-plugin & ft_rust already reset their local indentation
      pattern [:roc :c :cpp]
      callback #(setlocal!! 4 :sw :ts)]
  (au :FileType {: callback : group : pattern}))

;; disable `o` in `formatoptions` for every filetype so inserting a line with
;; `o` or `O` won't prepend a comment header if we are inside a line comment
(let [group (augrp :Hondana_FormatOptionsWithoutO)
      callback #(vim.opt_local.formatoptions:remove :o)]
  (au :FileType {: callback : group}))

;; HACK: TEMPORARY: for Fennel, missing `r` formatoptions
(let [group (augrp :Hondana_FormatOptions_Fennel)
      callback #(vim.opt_local.formatoptions:append :r)]
  (au :FileType {: callback : group :pattern :fennel}))

;; visible yank
(let [group (augrp :Hondana_Highlight_Yank)
      callback #(vim.highlight.on_yank)]
  (au :TextYankPost {: callback : group}))

;; additional languages' setup
;; - shen programming language comments (from Shen Technology (c) Mark Tarver; https://shenlanguage.org)
;; NOTE: the language syntax has been embedded in this setup
(let [group (augrp :Hondana_ShenComments)
      callback #(setlocal!! "\\\\ %s" :commentstring)]
  (au :FileType {: callback : group :pattern :shen}))

;; others
;; TEST: restore last position (check ShaDa for other session/buffer restoration)
(let [group (augrp :Hondana_LastPosRestoration)]
  (au :BufReadPost {:callback #(when (-> "%" (vim.fn.bufname)
                                         (not= :.git/COMMIT_EDITMSG))
                                 (vim.cmd "silent! normal g`\"zv"))
                    : group
                    : pattern}))
