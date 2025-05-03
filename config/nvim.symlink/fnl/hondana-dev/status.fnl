(import-macros {: tc} :hondana-dev.macros)
(import-macros {: set!} :hibiscus.vim)

(macro highlight-status! [tbl]
  (let [out []]
    (each [type [bg fg] (pairs tbl)]
      (table.insert out `(vim.cmd ,(table.concat ["hi"
                                                  (tostring type)
                                                  (.. "guibg=" bg)
                                                  (.. "guifg=" fg)]
                                                 " "))))
    `(do
       ,(unpack out))))

(macro make-stamps! []
  (icollect [_ s (ipairs [:type
                          :file
                          :modified
                          :norm
                          :buffer
                          :location
                          :percent])]
    (.. "%#Status" ;;
        ;; capitalize an ascii word
        (-> (s:sub 1 1) (: :upper) (.. (s:sub 2))) "#")))

(local api vim.api)

;; F = utility functions
(local F {:au api.nvim_create_autocmd
          :augrp #(api.nvim_create_augroup $ {:clear true})})

(tc type boolean)
(var flip true)

;; highlight status by side-effect (background first)
(highlight-status! {:StatusType ["#b16286" "#1d2021"]
                    :StatusFile ["#fabd2f" "#1d2021"]
                    :StatusModified ["#1d2021" "#d3869b"]
                    :StatusBuffer ["#98971a" "#1d2021"]
                    :StatusLocation ["#458588" "#1d2021"]
                    :StatusPercent ["#1d2021" "#ebdbb2"]})

(F.au :ColorScheme
      {:group (F.augrp :Hondana_RestoreStatusLine)
       :callback #(vim.cmd "hi StatusLine guibg=#458588 guifg=#1d2021")})

;; F = utility functions

(tc return string)
(fn bump []
  "alternate the chevron's orientation"
  (set flip (not flip))
  (if flip
      ">>"
      ;; start at false w/ `"<<"`
      "<<"))

(tc type "string[]")
(local stamps (make-stamps!))

(tc return "string")
(fn stamp []
  "yields next status"
  (or (table.remove stamps 1) ""))

;; TODO: Luaify this
(vim.cmd "function! StatuslineGitBranch()\nlet b:gitbranch=\"\"\nif &modifiable\ntry\nlcd %:p:h\ncatch\nreturn\nendtry\nlet l:gitrevparse=system(\"git rev-parse --abbrev-ref HEAD\")\nlcd -\nif l:gitrevparse!~\"fatal: not a git repository\"\nlet b:gitbranch=\"(\".substitute(l:gitrevparse, '\\n', '', 'g').\") \"\nendif\nendif\nendfunction")

(tc type string)
(set! statusline (-> [" "
                      "%l"
                      (.. " " (stamp) (bump))
                      "%Y  "
                      (.. (bump) (stamp) (bump))
                      "%F %{b:gitbranch}"
                      (.. (bump) (stamp))
                      "%h%m%r"
                      (.. (stamp) "%=" (stamp) (bump))
                      (.. "﬘ " "%n")
                      (.. (bump) (stamp) (bump))
                      "燐"
                      ;; no need %l AKA line (always known)
                      "%c"
                      (.. (bump) (stamp) (bump))
                      "%p%%  "
                      (.. (bump) " ")]
                     (table.concat " ")))

(vim.cmd "augroup Hondana_GetGitBranch\nautocmd!\nautocmd VimEnter,WinEnter,BufEnter * cal StatuslineGitBranch()\naugroup END")

;; TEST: remove me
(vim.cmd "function StatusLineMode()\nlet l:mode=mode()\nif l:mode==#\"n\"\nreturn \"NOR\"\nelseif l:mode==?\"v\"\nreturn \"VIS\"\nelseif l:mode==#\"i\"\nreturn \"INS\"\nelseif l:mode==#\"R\"\nreturn \"REP\"\nelse\nreturn \" \"\nendif\nendfunction")
