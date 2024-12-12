(local root (or ... :hondana-dev))

(local P [:status :set :remap])
(for [i 1 (length P)]
  (-> root
      (.. "." (. P i))
      (require)))

;; your private settings (written in Fennel or Lua)
(local (_ _) (pcall require (.. root :.private)))
