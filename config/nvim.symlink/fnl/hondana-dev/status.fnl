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

;; TODO: check for the proper api.nvim_set_hl
(F.au :ColorScheme {:group (F.augrp :Hondana_RestoreStatusLine)
                    :callback #(vim.cmd "hi StatusLine guibg=#458588 guifg=#1d2021\nhi winbar guibg=NONE")
                    ;; #(do
                    ;;              (api.nvim_set_hl :StatusLine {:bg "#458588" :fg "#1d2021"})
                    ;;              (api.nvim_set_hl :winbar {:bg :NONE}))
                    })

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

;; TODO: make it async + (set vim.b.gitbranch "")
(fn statusline-git-branch []
  "replace the buffer variable `b:gitbranch`"
  (set vim.b.gitbranch "")
  (when vim.o.modifiable
    (local git-rev-parse (vim.fn.system [:git :rev-parse :--abbrev-ref :HEAD]))
    (when (= 0 vim.v.shell_error)
      (set vim.b.gitbranch (.. " (" (git-rev-parse:gsub "\n" "") ")")))))

(tc type string)
(set! statusline (-> [" "
                      "%l"
                      (.. " " (stamp) (bump))
                      "%Y  "
                      (.. (bump) (stamp) (bump))
                      "%F%{b:gitbranch}"
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

;; MANDATORY to set `b:gitbranch` as used in statusline (remove the MANDATORY clause when async)
(F.au [:VimEnter :WinEnter :BufEnter]
      {:group (F.augrp :Hondana_GetGitBranch) :callback statusline-git-branch})
