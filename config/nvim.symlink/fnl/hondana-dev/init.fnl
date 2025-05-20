(local root (or ... :hondana-dev))

(local P [:status :set :command :remap :linenumbers :lsp])
(let [tally (length P)]
  (for [i 1 tally]
    (-> root
        (.. "." (. P i))
        (require))))

;; your private settings (written in Fennel or Lua)
(local (_ _) (pcall require (.. root :.private)))
