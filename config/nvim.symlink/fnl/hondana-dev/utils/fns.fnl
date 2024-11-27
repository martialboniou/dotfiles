;; some utility functions
;; no goal for now but may merge the content of `root-pattern` here later
(import-macros {: tc} :hondana-dev.macros)

;; utility: tbl-reverse
(tc generic "U: any")
(tc generic "V: any")
(tc param tbl "table<integer, U>" param ?fun "fun(U): V")
(tc return "table<integer, V>")
(fn tbl-reverse [tbl ?fun]
  (let [rev []
        fun (or ?fun #$)
        tally (length tbl)]
    (for [i tally 1 -1]
      (->> i (. tbl) (fun) (table.insert rev)))
    rev))
