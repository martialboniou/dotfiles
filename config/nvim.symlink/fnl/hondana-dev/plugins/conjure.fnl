(import-macros {: g!} :hibiscus.vim)

;; for now, conjure is used in cmp only

{1 :Olical/conjure
 :ft [:fennel :scheme :rust :clojure]
 :init #(g! "conjure#mapping#doc_word" :<localleader>K)
 :dependencies [:Olical/aniseed
                {1 :PaterJason/cmp-conjure :dependencies :hrsh7th/nvim-cmp}]}
