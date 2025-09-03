(local root (or ... :hondana-dev))

(local P [:cosmetics.status :set :command :remap :cosmetics.linenumbers :lsp])

(let [{: roles} (-> root (.. :.utils.globals) (require))]
  (when (roles:check :commentator)
    (table.insert P :cosmetics.comments)))

(let [tally (length P)]
  (for [i 1 tally]
    (-> root (.. "." (. P i)) (require))))

;; your private settings (written in Fennel or Lua)
(local (_ _) (pcall require (.. root :.private)))
