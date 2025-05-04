(import-macros {: tc} :hondana-dev.macros)
(import-macros {: set!} :hibiscus.vim)

(macro highlight-status0! [tbl]
  (let [out []]
    (each [type [bg fg] (pairs tbl)]
      (table.insert out `(vim.cmd ,(table.concat ["hi"
                                                  (tostring type)
                                                  (.. "guibg=" bg)
                                                  (.. "guifg=" fg)]
                                                 " "))))
    `(do
       ,(unpack out))))

(macro highlight-status! [tbl]
  "generate the highlights with a dict of tag as keys and a sequence of 1 or 2 Vim color codes
  as values as value. The first color code is background, the second one as foreground is optional"
  (let [out []]
    (each [type colors (pairs tbl)]
      (assert (-> colors (length) (< 3)))
      (local args (collect [k v (pairs {:bg (. colors 1) :fg (. colors 2)})]
                    (when v
                      (values k (tostring v)))))
      ;; the order is irrelevant
      (table.insert out `(api.nvim_set_hl 0 ,(tostring type) ,args)))
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

(local {: api : uv} vim)

;; F = utility functions
(local F {:au api.nvim_create_autocmd
          :augrp #(api.nvim_create_augroup $ {:clear true})})

(tc type boolean)
(var flip true)

;; highlight status by side-effect (background first, foreground is optional)
(highlight-status! {StatusType ["#b16286" "#1d2021"]
                    StatusFile ["#fabd2f" "#1d2021"]
                    StatusModified ["#1d2021" "#d3869b"]
                    StatusBuffer ["#98971a" "#1d2021"]
                    StatusLocation ["#458588" "#1d2021"]
                    StatusPercent ["#1d2021" "#ebdbb2"]})

(F.au :ColorScheme
      {:group (F.augrp :Hondana_RestoreStatusLine)
       :callback #(highlight-status! {StatusLine ["#458588" "#1d2021"]
                                      winbar [NONE]})})

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

(fn statusline-git-branch []
  "Async'ly replace the buffer variable `b:gitbranch`"
  (when vim.o.modifiable
    (tc type :uv.uv_handle_t)
    (var handle nil)
    (local {:new_pipe new : spawn :read_start start :read_stop stop : close} uv)
    (local [cmd & args] [:git :rev-parse :--abbrev-ref :HEAD])
    (local stdio [nil (new) (new)])
    (local options {: args : stdio})
    (local on-exit
           #(do
              (for [i 2 3]
                (let [p (. stdio i)]
                  (when p (stop p) (close p))))
              (close handle)
              ;; default 
              (when (not= 0 $) (set vim.b.gitbranch ""))))
    (set handle (spawn cmd options on-exit))
    ;; default initial message in the statusline
    (set vim.b.gitbranch "")
    (for [i 2 3]
      (let [p (. stdio i)]
        (when p
          (start p
                 (fn [_ data]
                   (when data
                     ;; format the detected branch like " ($)"
                     (set vim.b.gitbranch (.. " (" (data:gsub "\n" "") ")"))))))))))

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

;; MANDATORY: set `b:gitbranch` as used in statusline each time we enter a buffer, window...
(F.au [:VimEnter :WinEnter :BufEnter]
      {:group (F.augrp :Hondana_GetGitBranch) :callback statusline-git-branch})
