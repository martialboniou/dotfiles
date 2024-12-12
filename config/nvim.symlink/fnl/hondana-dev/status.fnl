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

(tc type boolean)
(var flip true)

;; highlight status by side-effect
(highlight-status! {:StatusType ["#b16286" "#1d2021"]
                    :StatusFile ["#fabd2f" "#1d2021"]
                    :StatusModified ["#1d2021" "#d3869b"]
                    :StatusBuffer ["#98971a" "#1d2021"]
                    :StatusLocation ["#458588" "#1d2021"]
                    :StatusPercent ["#1d2021" "#ebdbb2"]
                    :StatusNorm ["none" "white"]})

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
  (let [n (table.remove stamps 1)]
    (if n n "")))

(tc type string)
(set! statusline (-> [" "
                      "%l"
                      (.. " " (stamp) (bump))
                      "%Y  "
                      (.. (bump) (stamp) (bump))
                      "%F"
                      (.. (bump) (stamp))
                      "%m"
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
